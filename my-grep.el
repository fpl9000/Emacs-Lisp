;; my-grep.el
;;
;; Code to allow grepping through files in the current directory (and also
;; recurisively under the current directory).

(require 'compile)
(require 'grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq next-error-highlight t		;; Always highlight next error
      grep-use-null-device nil)		;; Needed because I pass my own command to function grep.

(defvar my-grep-window-height 20
  "Height of the *grep* window in lines.")

(defvar my-grep-input-history nil
  "Minibuffer input history used by my-grep.")

(defvar my-grep-recursively nil
  "If non-nil, recursively grep all files under the default directory.  Do not
manually set this variable.  It is used by function my-grep-recursive to
communicate with my-grep.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (facep 'match)
    ;; This is the face used to highlight the regexp in the *grep* bufer.
  (set-face-background 'match "blue")
  (set-face-foreground 'match "white"))

(when (facep 'next-error)
  ;; This is the face used to highlight the regexp in the file buffer.
  (set-face-background 'next-error "blue")
  (set-face-foreground 'next-error "white"))

(if (facep 'compilation-info)
    ;; This is the face used for the filename.
    (set-face-foreground 'compilation-info "green"))

(if (facep 'compilation-line-number)
    ;; This is the face used for the line number.
    (set-face-foreground 'compilation-line-number "#4ff"))

;; This is the face used for ordinary text (i.e., none of the above stuff).
(setq compilation-message-face 'default)

;; This is the face used for lines announcing a binary file matches.
(set-face-foreground (make-face 'my-grep-binary-file-match-face) "#999")

(defvar my-orig-grep-mode-font-lock-keywords nil
"Holds the original value of grep-font-lock-keywords before any changes made by
this file.")

(if (null my-orig-grep-mode-font-lock-keywords)
    (setq my-orig-grep-mode-font-lock-keywords grep-mode-font-lock-keywords))

;; Tweak the value of grep-mode-font-lock-keywords.
(setq grep-mode-font-lock-keywords
      `(("^Searching for '.*:$" 0 'font-lock-function-name-face nil)
	,@my-orig-grep-mode-font-lock-keywords
	("^Grep finished .*$" 0 'font-lock-function-name-face t)
	("^[^:]+\\(:\\)[1-9]" 1 'my-grep-invisible-face t)
	("^[^:]+:[0-9]+\\(:\\)" 1 'my-grep-invisible-face t)
	("^Binary file .*$" 0 'my-grep-binary-file-match-face t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks.

(defun my-grep-setup-hook ()
  "Added to grep-setup-hook.  Sets up keybindings in the *grep* buffer."

  ;; This face is used as a hack to hide the colon between the filename and the
  ;; line number.  Need to do this here instead of at load-time, because the
  ;; background color of face 'default has not been set at load-time.
  (set-face-foreground (make-face 'my-grep-invisible-face) (face-background 'default))

  (local-set-key (kbd "n") 'compilation-next-error)
  (local-set-key (kbd "p") 'compilation-previous-error)
  (local-set-key (kbd "M-n") 'my-next16-line)
  (local-set-key (kbd "M-p") 'my-previous16-line)
  (local-set-key (kbd "SPC") 'compile-goto-error))

(add-hook 'grep-setup-hook 'my-grep-setup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-grep (regexp &optional case-sensitive)
  "Runs grep to search for REGEXP in all non-binary files in current directory.
If CASE-SENSITIVE is non-nil (interactively, a prefix argument is given), the
search is case-sensitive."
  (interactive (list (with-syntax-table (copy-syntax-table (syntax-table))
		       (modify-syntax-entry ?_ "w" (syntax-table))
		       (modify-syntax-entry ?- "w" (syntax-table))
		       (read-from-minibuffer
			(if current-prefix-arg
			    (if my-grep-recursively "Recursive egrep (case-sensitive): " "egrep (case-sensitive): ")
			  (if my-grep-recursively "Recursive egrep: " "egrep: "))
			(if (eq major-mode 'dired-mode)
			    ""
			  (word-at-point))
			nil nil 'my-grep-input-history ""))
		     current-prefix-arg))

  (if (or (null regexp)
	  (member-ignore-case regexp '("" "." ".." "..." "?" ".?" ".*" "*" ".+" "+" "(" ")" "((" "))" "++"
				       "??" "**" "***")))
      (error "Invalid regexp: '%s'" regexp))

  ;; This allows me to use shell metacharacters like ', ", <, >, SPC, etc. in the regular
  ;; expression.
  (let ((orig-regexp regexp))
    (setq regexp (shell-quote-argument regexp))

    ;; Make sure we end up with just this window and the *grep* buffer window.  IMPORTANT:
    ;; The advice (below) on function next-error and previous-error depends on this.
    (delete-other-windows)

    (ignore-errors (kill-buffer "*grep*"))
    (set-window-text-height (get-buffer-window (pop-to-buffer "*grep*"))
			    my-grep-window-height)

    ;; The current buffer and window are now the *grep* buffer/window.

    (let ((my-grep-options "-n -H -I --exclude='*~'")
	  (my-files-to-search (if my-grep-recursively
				  "."
				(if (file-expand-wildcards ".[!.]*")
				    ".[!.]* *"
				  "*"))))

      (if my-grep-recursively
	  (setq my-grep-options (concat my-grep-options " -r")))

      (if (not case-sensitive)
	  (setq my-grep-options (concat my-grep-options " -i")))

      ;; Spawn egrep asynchronously.
      (let ((my-grep-command (concat "egrep " my-grep-options " -- " regexp " " my-files-to-search
				     " 2>/dev/null"
				     (if my-grep-recursively
					 " | sed -e 's;^\./;;' -e 's;^\\(Binary file \\)\./;\\1;'"))))
	(grep my-grep-command)))

    (bury-buffer (current-buffer))
    (setq truncate-lines t)
    (goto-char (point-min))
    (forward-line 3)
    (end-of-line)

    (let ((inhibit-read-only t))
      (delete-region (point-min) (point))
      (insert (concat "Searching for '" orig-regexp "'"
		      (if (or my-grep-recursively case-sensitive) " (")
		      (if my-grep-recursively "recursively")
		      (if (and my-grep-recursively case-sensitive) ", ")
		      (if case-sensitive "case-sensitively")
		      (if (or my-grep-recursively case-sensitive) ")")
		      ":\n")))

    (goto-char (point-min))
    (recenter 0)

    ;; Fontify the *grep* buffer.
    (my-toggle-font-lock-mode 1)))
  
(defun my-grep-recursive (regexp &optional ignore-case)
  "Same as function grep, but recurses to search every file under the current directory."
  (interactive (list nil nil))
  (let ((my-grep-recursively t))
    (if (called-interactively-p 'any)
	(call-interactively 'my-grep)
      (my-grep regexp ignore-case))))

(defun my-grep-show-grep-buffer ()
  "Displays the *grep* buffer if it exists.  Deletes other windows first so the frame ends
up showing only the current buffer and the *grep* buffer."
  (interactive)
  (let ((my-grepbuf (get-buffer "*grep*")))
    (if (null my-grepbuf)
	(error "No *grep* buffer found!"))
    (delete-other-windows)
    (set-window-text-height (get-buffer-window (pop-to-buffer "*grep*"))
			    my-grep-window-height)))

(defun my-grep-pre-command-hook ()
  "Added to pre-command-hook by advice on next-error and previous-error.  If the
current command is not next-error or previous-error, this function deletes all
overlays at point (thus removing the next-error-induced highlight), hides the
*grep* buffer, then removes itself from the hook so it won't run on the next
command."
  (when (not (memq this-command '(next-error previous-error)))
    (let ((modstate (buffer-modified-p)))

      (mapc 'delete-overlay (overlays-at (point)))
      (remove-hook 'pre-command-hook 'my-grep-pre-command-hook)

      ;; Hack!!!
      (when (and (stringp (this-command-keys))
		 (string= "q" (this-command-keys)))
	(let ((grepwin (get-buffer-window "*grep*")))
	  (if grepwin
	      (delete-window grepwin)))

	(run-at-time 0 nil
		     `(lambda ()
			(delete-backward-char 1) ;; Delete the 'q' from the buffer.
			(set-buffer-modified-p ,modstate)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-grep-common-advice ()
  "..."
  (let ((my-grep-buffer-window (get-buffer-window "*grep*")))
    (when (windowp my-grep-buffer-window)
      ;; Make sure the *grep* window stays the same height.
      (set-window-text-height my-grep-buffer-window my-grep-window-height)
      
      ;; Delete all windows other than the *grep* window and the window showing the file
      ;; we just displayed.
      (mapc (lambda (window)
	      (if (and (not (eq window (selected-window)))
		       (not (eq window my-grep-buffer-window)))
		  (delete-window window)))
	    (window-list))

      ;; Add a self-removing pre-command-hook to unhighlight the target line.
      (add-hook 'pre-command-hook 'my-grep-pre-command-hook))))

(define-advice next-error (:around (origfun &rest args) my-ad-around-next-error)
  "..."
  (if (memq major-mode '(grep-mode xref--xref-buffer-mode))
      (other-window -1))
  (prog1
      (apply origfun args)
    (my-grep-common-advice)))

(define-advice previous-error (:around (origfun &rest args) my-ad-around-previous-error)
  "..."
  (if (memq major-mode '(grep-mode xref--xref-buffer-mode))
      (other-window -1))
  (prog1
      (apply origfun args)
    (my-grep-common-advice)))

(define-advice compile-goto-error (:around (origfun &rest args) my-ad-around-compile-goto-error)
  "..."
  (prog1
      (apply origfun args)
    (my-grep-common-advice)))
