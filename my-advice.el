;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Calls to define-advice go here, unless they are very specific to a mode or subsystem, in which
;; case they may appear elsewhere (e.g., ~/.gnus, my-hooks.el, my-frame.el, my-erc.el, etc.).
;;
;; WARNING: Advice on function fundamental-mode does not execute when new buffers are created in
;; Fundamental Mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'help-mode)
(require 'vc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice-related functions here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-unadvise-function (function-name)
  "Deletes one piece of named advice from FUNCTION-NAME.  This cannot delete unnamed advice."
  (interactive "aFunction from which to delete advice: ")

  ;; Get the function symbol.
  (let ((my-func (intern-soft function-name)))
    (if (null my-func)
	(error "No such function: %s!" function-name))
    (if (null (functionp my-func))
	(error "%s is not the name of a function!" function-name))

    ;; Read the name of the advice function to be deleted.
    (let (advice-function-names)
      (advice-mapc (lambda (advice-func &rest ignored)
		     (add-to-list 'advice-function-names (symbol-name advice-func)))
		   my-func)

      (if (null advice-function-names)
	  (error "There is no named advice on function %s!" function-name))

      (let* ((advice-function-name (completing-read "Advice to remove: "
						    advice-function-names nil t))
	     (advice-function (intern-soft advice-function-name)))
	(if (null advice-function)
	    (error "No advice named %s on function %s!" advice-function-name function-name))

	;; Remove the advice.
	(advice-remove my-func advice-function)
	(message "Removed advice %s from %s!" advice-function-name function-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advise functions below here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some /bin/ls implementations don't grok the -G switch I have in dired-listing-switches, and
;; ange-ftp-ls uses dired-listing-switches to construct the ls command it sends.  This advice hacks
;; around that problem.
(define-advice ange-ftp-ls (:around (origfun &rest args) my-ad-around-ange-ftp-ls)
  "Rebinds dired-listing-switched to '-alq' during call to ange-ftp-ls."
  (let ((dired-listing-switches "-alq"))
    (apply origfun args)))

(define-advice apropos-command (:after (&rest args) my-ad-after-apropos-command)
  "Selects the *Apropos* buffer after apropos-command completes.  For some reason,
this also affects function apropos-variable, so that function doesn't need to be advised."
  (other-window 1))

(define-advice browse-url-url-at-point (:override (&rest args) my-ad-around-browse-url-url-at-point)
  "Prevents Emacs from fetching a Web page unless I click on a real URL."
  (thing-at-point 'url t))

(define-advice Buffer-menu-bury (:after (&rest args) my-ad-after-Buffer-menu-bury)
  "Fixes the fontification in the Buffer Menu buffer after burying a buffer."
  (ignore-errors (my-buffer-menu-fontify)))

(define-advice c-indent-new-comment-line (:before (&rest args) my-ad-before-c-indent-new-comment-line)
  "Makes comment auto-filling work right."
  (setq fill-prefix nil))

(define-advice gnus-group-startup-message (:override (&rest args) my-ad-around-group-startup-message)
  "Avoids the invalid-face error that I sometimes get when using the version built into
Gnus."
  (erase-buffer)
  (insert (make-string 10 ?\n)
	  "
_    ___ _             _
_ ___ __ ___  __    _ ___
__   _     ___    __  ___
    _           ___     _
   _  _ __             _
   ___   __            _
	 __           _
	  _      _   _
	 _      _    _
	    _  _    _
	__  ___
       _   _ _     _
      _   _
    _    _
   _    _
  _
__")
  (goto-char (point-min))
  (set-text-properties (point-min) (point-max)
		       `(face (:foreground ,(my-random-element '("purple" "cyan" "green"
								 "yellow" "white" "pink"
								 "blue" "orange" "lightgreen")))))
  (indent-rigidly (point-min) (point-max)
		  (/ (max (- (window-width) 25) 0) 2)))

(define-advice help-mode-finish (:after (&rest args) my-ad-after-help-mode-finish)
  "Replaces fancy Unicode quotes with single-quote characters."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "[‘’]" nil t)
	(replace-match "'")))))

(define-advice kill-buffer (:after (&rest args) my-ad-after-kill-buffer)
  "This is similar to the advice on function other-buffer: this prevents an ERC buffer
from being displayed when killing a buffer.  This is necessary because kill-buffer is
written in C, and though it calls other-buffer, that call does not execute my advice on
other-buffer."
  (if (and (not (or (eq major-mode 'erc-mode)
		      (string= (buffer-name) " *ERC Pre-connect Buffer*")
		      (my-stack-match "^erc-")))
	   (eq major-mode 'erc-mode))
      (let ((my-new-buffer
	     (catch 'my-dolist-exit
	       (dolist (my-buffer (buffer-list))
		 (if (and (not (eq my-buffer (current-buffer)))
			  (not (minibufferp my-buffer))
			  (with-current-buffer my-buffer
			    (not (eq major-mode 'erc-mode)))
			  (not (eq my-buffer (get-buffer " **lose**"))))
		     (throw 'my-dolist-exit my-buffer))))))
	(if (not (null my-new-buffer))
	    (switch-to-buffer my-new-buffer)))))

(define-advice list-directory (:after (&rest args) my-ad-after-list-directory)
  "Binds 'q' in the directory listing buffer to hide the buffer."
  (pop-to-buffer "*Directory*")
  (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer))

(define-advice list-processes (:after (&rest args) my-ad-after-list-processes)
  "Binds 'q' in the process list buffer to hide the buffer."
  (pop-to-buffer "*Process List*")
  (fundamental-mode)
  (my-toggle-truncate-lines)
  (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer))

(define-advice man (:before (&rest args) my-ad-before-man)
  "Sets Man-frame-parameters every time function man is called.  This copes with changes
to the display resolution (e.g., due to RDP and such)."
  (setq Man-width 85)	;; This variable is not honored on Linux, but ...
  (setenv "MANWIDTH" (int-to-string Man-width))  ;; ... this one is!  Simple, no?

  (setq Man-frame-parameters (copy-alist default-frame-alist))
  (setcdr (assoc 'top Man-frame-parameters) -10)
  (setcdr (assoc 'left Man-frame-parameters) (my-frame-rightmost-left))
  (setcdr (assoc 'width Man-frame-parameters) Man-width)
  (setcdr (assoc 'height Man-frame-parameters) (my-screen-percent-to-frame-height 70)))

;; TODO: See if this is no longer needed.
(define-advice modify-frame-parameters (:around (origfun &rest args) my-ad-around-modify-frame-parameters)
  "If the frame parameters include 'width or 'height, change the size of the frame before
changing any other parameters.  This enables frames to be positioned correctly on Windows
when the new parameters include both size and position changes."
  (if (not my-win32)
      (apply origfun args)

    (let* ((my-frame (nth 0 args))
	   (my-params (nth 1 args))
	   (my-size-params (delete nil
				   (mapcar (lambda (param)
					     (if (memq (car param) '(width height))
						 param))
					   my-params))))
      (if (or (null my-size-params)
	      (and (not (assoc 'left my-params))
		   (not (assoc 'top my-params))))
	  ;; There was no mixture of size and position parameters, so just do it.
	  (apply origfun args)

	;; There were _both_ size and position parameters.  First, resize the
	;; frame.
	(modify-frame-parameters my-frame my-size-params)

	;; Make Emacs see the size change.
	(my-frame-redisplay-hack my-frame 'try-hard)

	;; Change the non-size parameters.
	(apply origfun args)))))

;; This advice causes other-buffer to return an empty string (a no-no), which causes
;; read-buffer-to-switch to return an empty string, which causes switch-to-buffer to throw an error.
;;
;;(define-advice other-buffer (:filter-return (retval) my-ad-after-other-buffer)
;;  "After advice that prevents other-buffer from returning an ERC buffer.  This prevents
;;switch-to-buffer from switching to an ERC buffer, though you can still type the name of an
;;ERC buffer to switch-to-buffer and it works as expected.  Unfortunately, this does NOT
;;prevent kill-buffer from displaying an ERC buffer after killing a buffer, because
;;kill-buffer is written in C and directly calls other-buffer, which does not execute this
;;advice."
;;  (let ((my-target-buffer retval))
;;    (if (and (not (or (eq major-mode 'erc-mode)
;;		      (string= (buffer-name) " *ERC Pre-connect Buffer*")
;;		      (my-stack-match "^erc-")))
;;	     (with-current-buffer my-target-buffer
;;	       (eq major-mode 'erc-mode)))
;;	(let ((my-new-target-buffer
;;	       (catch 'my-dolist-exit
;;		 (dolist (my-buffer (buffer-list))
;;		   (if (and (not (eq my-buffer (current-buffer)))
;;			    (not (minibufferp my-buffer))
;;			    (with-current-buffer my-buffer
;;			      (not (eq major-mode 'erc-mode)))
;;			    (not (eq my-buffer (get-buffer " **lose**"))))
;;		       (throw 'my-dolist-exit my-buffer))))))
;;
;;	  ;; Generate the return value.
;;	  (or my-new-target-buffer retval)))))

(define-advice read-passwd (:before (&rest args) my-ad-before-read-passwd)
  "Redisplays before reading a password.  This is a hack to make the Gnus frame redisplay
when reading my NNTP password."
  (redisplay 'force))

(define-advice shell-command (:around (origfun &rest args) my-ad-around-shell-command)
  "Displays *Shell Command Output* buffer if it contains more than one line of text.
Displays a visually distinct prompt when the current buffer is a TRAMP or Ange-FTP remote
file or directory.  Also binds 'q' to delete-window in buffer *Shell Command Output*."
  (interactive (progn
		 (when (string-match-p "[a-z]:/windows" default-directory)
		   (if (not (yes-or-no-p (format "Default directory is %s -- continue? " default-directory)))
		       (error "Aborted!")))
		 (list (read-from-minibuffer
			(if (and (fboundp 'tramp-tramp-file-p)
				 (tramp-tramp-file-p default-directory))
			    (concat (propertize "Remote shell command:"
						'face 'my-alert-face2) " ")
			  "Shell command: ")
			nil nil nil 'shell-command-history)
		       current-prefix-arg
		       shell-command-default-error-buffer)))

    (when (and (string-match-p "^\\s-*ssh\\b" (nth 0 args))
	       (null (getenv "SSH_AUTH_SOCK")))
      (ding)
      (if (not (y-or-n-p "ssh-agent not running!  Really execute command? "))
	  (error "Aborted!")))
    
    (let ((my-return-value (apply origfun args)))
      (if (string-match "&\\s-*$" (nth 0 args))
	  (let ((my-output-buf (get-buffer "*Async Shell Command*")))
	    (when my-output-buf
	      (bury-buffer my-output-buf)
	      (with-current-buffer my-output-buf
		(local-set-key (kbd "q") 'my-delete-window-and-bury-buffer))))

	(let ((my-output-buf (get-buffer "*Shell Command Output*")))
	  (when my-output-buf
	    (with-current-buffer my-output-buf
	      (bury-buffer (current-buffer))
	      (when (> (count-lines (point-min) (point-max)) 1)
		(pop-to-buffer my-output-buf)
		(message ""))
	      (goto-char (point-max))
	      (if (memq (selected-window) (get-buffer-window-list my-output-buf))
		  (recenter -1))
	      (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer)))))

      ;; Return the value returned by shell-command.
      my-return-value))

;;(define-advice vc-dired-reformat-line (:override (&rest args) my-ad-override-vc-dired-reformat-line)
;;  "Reformat a directory-listing line.  Replace various columns with version control
;;information, VC-INFO.  This code, like dired, assumes UNIX -l format."
;;  (beginning-of-line)
;;  (when (re-search-forward
;;	 ;; Match link count, owner, group, size.  Group may be missing,
;;	 ;; and only the size is present in OS/2 -l format.
;;	 "^..[drwxlts+-]+ \\( *[0-9]+\\( [^ ]+ +\\([^ ]+ +\\)?[0-9]+\\)?\\) "
;;	 (line-end-position) t)
;;    (replace-match (substring (concat vc-info "          ") 0 10)
;;		   t t nil 1))
;;  
;;  (move-to-column 12)
;;  (if (looking-at-p "\\+")
;;      (delete-char 1)))

(define-advice view-echo-area-messages (:after (&rest args) my-ad-after-view-echo-area-messages)
  "Selects the window showing the *Messages* buffer."
  (other-window 1)
  (my-show-max-text)
  (my-scroll-up-one-line 1))
