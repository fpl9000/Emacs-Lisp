;; my-misc.el
;; Miscellaneous functions.

(require 'time-stamp)
(require 'calc)
(require 'calc-ext)
(require 'comint)
(require 'cc-mode)
(require 'dired)
(require 'socks)

;; Silence byte-compiler warnings about these functions not being defined.
;; on non-Windows systems.
(declare-function set-message-beep ".c" (sound))
(declare-function w32-shell-execute ".c" (operation document &optional parameters show-flag))
(declare-function my-fixperm "my-cygwin.el" (&rest files))

(defvar my-emacs-elisp-file-check-timer nil
  "Holds the timer used to peridically check for changed Elisp files.")

(defun my-interactive-startup ()
  "Perform startup tasks that only happen in interactive sessions (i.e., not in batch
mode).  This is called from ~/.emacs."
  ;; Start the timer to periodically check for changed Elisp files.
  (if (timerp my-emacs-elisp-file-check-timer)
      (cancel-timer my-emacs-elisp-file-check-timer))

  (let ((interval (* 15 60))) ;; 15 minutes
    (setq my-emacs-elisp-file-check-timer
	  (run-at-time interval interval 'my-check-for-newer-elisp-files)))

  ;; The below code only executes the very first time Emacs starts.
  (when (not (boundp 'my-reload-my-elisp-files-running))
    (if (file-directory-p "~")
	(dired "~"))

    (if (and (file-exists-p "~/todo-work.txt")
	     (string= (downcase (system-name)) "izsystem023"))
	(find-file "~/todo-work.txt")
      (if (file-exists-p "~/todo.txt")
	  (find-file "~/todo.txt")))

    ;; This must be done after the above find-file/dired calls to make the warning buffer
    ;; visible in some window.
    (my-check-for-out-of-date-elc-files)

    ;; Try to start the server.  Warn if it's already running.
    (let ((my-server-already-running nil))
      (my-with-advice ((display-warning (lambda (&rest args)
					  (setq my-server-already-running t))))
	(server-start))

      (run-at-time 1.25 nil `(lambda ()
			       (if (display-graphic-p)
				   (my-frame-center))
			       (if ,my-server-already-running
				   (my-message-highlight "WARNING: Emacs server is already running!")))))))

(defmacro my-with-advice (func-advice-list &rest forms)
  "Evaluates FORMS with each function named in FUNC-ADVICE-LIST temporarilly advised with
the given advice.  FUNC-ADVICE-LIST is a non-quoted list with elements of the
form (FUNCSYM ADVICE), where FUNCSYM is a non-quoted function symbol and ADVICE is a
non-quoted lambda expression or non-quoted function symbol that is added to FUNCSYM
as :around advice.  When FORMS terminate (normally or abnormally), the advice is removed
from all functions.  Returns the value returned by the last form in FORMS.

Examples:

  ;; Advise message with my-message-advice-func and y-or-n-p with my-yorn-advice-func.
  (my-with-advice ((message my-message-advice-func)
                   (y-or-n-p my-yornp-advice-func))
    (some-code)
    (+ 42 (other-code)))

  ;; Prepend \">> \" to all output from message.
  (my-with-advice ((message (lambda (origfun &rest args)
                              (apply origfun (concat \">> \" (car args))
                                             (cdr args)))))
    (some-code)
    (+ 42 (other-code)))

  ;; Make message and ding do nothing.
  (my-with-advice ((message ignore)
                   (ding ignore))
    (some-code)
    (+ 42 (other-code)))"
  (declare (indent defun))

  ;; Validate parameters.
  (if (not (listp func-advice-list))
      (error "my-with-advice: First parameter '%s' is not a list!" func-advice-list))

  (dolist (elt func-advice-list)
    (if (or (not (listp elt))
	    (/= 2 (length elt)))
	(error "my-with-advice: First argument is malformed: '%s'" func-advice-list))

    (let ((my-func (car elt))
	  (my-advice (nth 1 elt)))
      (if (or (not (symbolp my-func))
	      (not (functionp my-func)))
	  (error "my-with-advice: '%s' is not a symbol with a function definition!" my-func))
      (if (and (listp my-advice)
	       (memq (car my-advice) '(quote function)))
	  (error "my-with-advice: First argument must not contain quoted advice!"))
      (if (not (functionp my-advice))
	  (error "my-with-advice: '%s' is not a function!" my-advice))))

  (if (null forms)
      (error "my-with-advice: Too few parameters: no forms to evaluate!"))

  ;; Generate the macro result.
  `(unwind-protect
       (let (my-result)
	 ;; Add the temporary advice.
	 (dolist (elt ',func-advice-list)
	   (advice-add (car elt) :around (nth 1 elt)))

	 ;; Evaluate FORMS.
	 (dolist (my-form-to-eval ',forms my-result)
	   (setq my-result (eval my-form-to-eval))))
     
     ;; Remove the temporary advice.
     (dolist (elt ',func-advice-list)
       (ignore-errors (advice-remove (car elt) (nth 1 elt))))))

(defmacro my-eval-silently (&rest forms)
  "Evalutates FORMS with functions message and ding advised to do nothing."
  (declare (indent defun))
  `(my-with-advice ((message ignore)
		    (ding ignore))
     ,@forms))

(defun my-toggle-tabify-region (start end &optional my-tab-width)
  "Untabifies or tabifies the region (depending on whether it contains any tabs).
Interactively, parameters are buffer locations as integers or markers."
  (interactive "r\np")
  (let (my-tabs-found)
    (save-excursion
      (goto-char start)
      (setq my-tabs-found (search-forward "	" end t)))

    (if my-tabs-found
	;; Found a TAB, so untabify region.
	(progn
	  (untabify start end)
	  (message "Untabified region!"))

      ;; Found no TABs, so tabify region.
      (tabify start end)
      (message "Tabified region (tab width = %d)!" tab-width))))

(defun my-magnet-url (torrenz-url)
  "Inserts a magnet URL from a Torrenz page URL."
  (interactive "sTorrentz URL: ")
  (let ((magnet-url (my-command (concat "mkmag " torrenz-url)))
	(start (point)))
    (setq magnet-url (delete ?\n magnet-url))
    (kill-new magnet-url)
    (insert magnet-url)
    (goto-char start)))

(defun my-base64-decode ()
  "Decodes the base64-encoded block that point is within.  The lines of theblock can have
whitespace before and after but not within a line."
  (interactive)
  (let* ((my-base64-chars-re "[a-zA-Z0-9=/+]\\{2,76\\}")
	 (my-base64-line-re (concat "^[ \t]*" my-base64-chars-re "[ \t]*$"))
	 (my-origin (point))
	 (my-start nil)
	 (my-match nil)
	 (my-end nil))

    (if (not (looking-at-p my-base64-chars-re))
	(error "Point is not within a base64-encoded block!"))

    ;; Find the start of the base64-encoded block.
    (beginning-of-line)
    (setq my-match (looking-at-p my-base64-line-re))

    (if (null my-match)
	(error "No base64-encoded region found!"))

    (while (and (not (bobp))
		my-match)
      (forward-line -1)
      (setq my-match (looking-at-p my-base64-line-re)))

    ;; We could be one line above the first line of the base64-encoded block.
    (if (not my-match)
	(forward-line 1))

    (setq my-start (point))

    ;; Find the end of the base64-encoded block.
    (goto-char my-origin)
    (beginning-of-line)
    (setq my-match (looking-at-p my-base64-line-re))

    (while (and (not (eobp))
		my-match)
      (forward-line 1)
      (setq my-match (looking-at-p my-base64-line-re)))

    ;; We could be one line below the last line of the base64-encoded block.  Don't test (eobp)
    ;; here, because if the last line of the encoded block has no newline, we're at the EOB but we
    ;; don't want to move up one line.
    (when (bolp)
      (forward-line -1)
      (end-of-line))

    (setq my-end (point))
    (push-mark my-end 'nomsg)

    (goto-char my-start)
    (base64-decode-region my-start my-end)))

(defun my-copy-current-directory-to-clipboard ()
  "Copies the current directory of the current buffer to the kill-ring/clipboard.  This is
not necessarilly the value of default-directory, which can be changed with function cd."
  (interactive)
  (let (result)
    (if (eq major-mode 'dired-mode)
	(setq result dired-directory)
      (let ((filename (buffer-file-name)))
	(if (null filename)
	    (error "This buffer has no directory!"))
	(setq result (file-name-directory filename))))

    (setq result (substring result 0 -1))
    (kill-new result)
    (message "Copied directory name: %s" result)
    result))

(defun my-revert-log-file-unconditionally ()
  "Reverts the current buffer unconditionally, widens the buffer, moves point to the end
of the buffer, turns off Font Lock mode, and toggles line truncation."
  (interactive)
  (with-temp-message ""
    (my-revert-buffer-unconditionally)
    (my-toggle-font-lock-mode 0)
    (widen)
    (goto-char (point-max))
    (toggle-truncate-lines 1)))

(defmacro my-block (&rest body)
  "Defines a block that can be exited by evaluating the form (my-block-exit).  This is useful
when used around the body of a dolist or while form, for example:

(dolist (x '(a b c d e))
  (my-block
   ...
   (if CONDITION
       (my-block-exit))
   ...))"
  nil
  `(cl-block my-block
	 (cl-macrolet ((my-block-exit () `(cl-return-from my-block)))
	   ,@body)))

(defun my-make-file-writable ()
  "Makes the current file writable."
  (interactive)
  (let ((filename (buffer-file-name))
	(chmod (concat my-systemdrive "/apps/cygwin/bin/chmod.exe")))
    (if (null filename)
	(error "Buffer %s has no associated file!" (buffer-name)))

    (if (and my-win32
	     (not (file-exists-p chmod)))
	(error "Can't find chmod.exe!  Is Cygwin installed?"))

    (if (null (y-or-n-p "Really set this buffer's file modes to 644? "))
	(error "Aborted!"))

    ;; Operate on the basename of the file.
    (let ((filename (file-name-nondirectory filename)))
      (if my-win32
	  ;; On Windows, don't use set-file-modes.	It doesn't work for some reason.
	  (call-process chmod nil nil nil "644" filename)
	(set-file-modes filename #o644))

      (message "File %s permissions set to 644." filename)

      ;; Update the buffer-local value of my-cygwin-original-file-acl to match the file's
      ;; new permissions.
      (my-cygwin-get-buffer-file-acl))))

(defun my-flip-slashes (start end)
  "Flips slashes for backslashes and vice verse in the region."
  (interactive "r")
  (let ((string (buffer-substring start end))
	orig
	new)
    (if (string-match-p "/" string)
	(setq orig ?/ new ?\\)
      (setq orig ?\\ new ?/))
    (subst-char-in-region start end orig new)))

(defun my-launch-mp3tag ()
  "Launches MP3Tag on the current directory."
  (interactive)
  (start-process "mp3tag" nil (concat my-systemdrive "/" my-progfilesx86 "/mp3tag/mp3tag.exe") default-directory))

(defun my-select-current-word (&optional prefix)
  "Selects the current word that point is in and copies it to the front of the kill ring."
  (interactive "P")
  (let ((my-word-regexp (if prefix
			    "[^ \t\n\r]"
			  (if (eq major-mode 'emacs-lisp-mode)
			      "[a-zA-Z_0-9-]"
			    "[a-zA-Z_0-9]")))
	(my-word nil))
    (if (not (looking-at-p my-word-regexp))
	(error "Point is not on a word!"))

    (save-excursion
      (while (and (not (bobp))
		  (looking-at-p my-word-regexp))
	(backward-char 1))

      (if (not (looking-at-p my-word-regexp))
	  (forward-char 1))
      (push-mark)

      (while (and (not (eobp))
		  (looking-at-p my-word-regexp))
	(forward-char 1))

      (setq my-word (kill-new (buffer-substring (mark t) (point)))))

    (let* ((my-max-display-length (- (window-width) 28))
	   (my-msg-word (if (> (length my-word) my-max-display-length)
			   (concat (substring my-word 0 my-max-display-length) " ...")
			 my-word)))
      (message "'%s' pushed on kill ring!" my-msg-word))))

(defun my-reload-my-elisp-files ()
  "Reloads all of my personal elisp files by loading ~/.emacs again."
  (interactive)

  (if (not (yes-or-no-p "Really re-load ~/.emacs? "))
      (error "Aborted!"))

  (let ((my-reload-my-elisp-files-running t))
    (message "================================================================================")
    (message "Reloading ~/.emacs ...")
    (load "~/.emacs")))

(defun my-explore-directory (&optional directory)
  "Opens Explorer (Windows) or Nautilus (Gnome) showing DIRECTORY (default
directory if nil).	Interactively, a prefix argument means prompt for the
directory location."
  (interactive (list (if current-prefix-arg
			 (expand-file-name (read-directory-name "Explore directory: ")))))
  (if (null directory)
      (setq directory default-directory))

  (if my-win32
      (w32-shell-execute "explore" directory)
    (error "Non-Windows systems not yet supported!")))

(defvar my-win32-elevated nil
  "Non-nil if this instance of Emacs is running with elevated privileges (i.e., the full
administrative token).")

(defun my-find-file-elevated (&optional filename)
  "Opens FILENAME (or, if FILENAME is nil or the empty string, the file
associated with the current buffer) in a new Emacs process that is elevated to
have the full administrative user token."
  (interactive (list (read-from-minibuffer "Find file elevated: " (buffer-file-name))))
  (if (not my-win32)
      (error "This function is only usable on Windows."))

  (when (or (null filename)
	    (string= "" filename))

    (if (null (buffer-file-name))
	(error "Current buffer is not visiting a file!"))

    (setq filename (buffer-file-name)))

  (if my-win32-elevated
      (find-file filename)

    ;; This is needed because pathnames supplied to Emacs on the command-line do
    ;; not trigger the advice on find-file-noselect for some reason.
    (setq filename (my-cygwin-pathname-to-emacs-pathname filename))

    (message "Launching elevated Emacs ...")
    (start-process "Elevated Emacs" nil (concat my-systemdrive "/franl/bin/win32/x86/elevate.exe")
		   (concat my-systemdrive "/apps/emacs/bin/runemacs.exe") "-f" "my-win32-elevated " filename)))

(defun my-win32-elevated ()
  "..."
  (run-at-time 0.5 nil (lambda ()
			 (let ((bgcolor "#400"))
			   (setcdr (assoc 'background-color default-frame-alist) bgcolor)
			   (setcdr (assoc 'background-color initial-frame-alist) bgcolor)
			   (set-face-background 'default bgcolor)
			   (set-face-background 'fringe bgcolor)
			   (set-frame-parameter nil 'background-color bgcolor))))
  (setq my-win32-elevated t))

(defun my-launch-truecrypt (filename)
  "Launches TrueCrypt on the given filename, which should be a TrueCrypt volume."
  (interactive "fTrueCrypt volume: ")
  (let ((tcprog (if my-win32
		    (concat my-systemdrive "/franl/bin/win32/x86/truecrypt-7.0a/truecrypt.exe")
		  "/usr/local/bin/truecrypt")))
    (start-process "TrueCrypt" nil tcprog
		   (subst-char-in-string ?/ ?\\ (expand-file-name filename)))))

(defun my-insert-ucs-character (codepoint)
  "Inserts the UCS character having CODEPOINT at point.	 CODEPOINT is the UCS
code point _not_ the Emacs internal code point of the character.  See
http://unicode.org/ for code point charts."
  (interactive "sUCS code point value (in decimal, use prefix 'x' for hex): ")
  (insert-char (if (= ?x (aref codepoint 0))
		   (substring codepoint 1)
		 (format "%x" (string-to-number codepoint)))))

(defun my-delete-window-and-bury-buffer ()
  "Buries the current buffer and deletes the current window."
  (interactive)
  (bury-buffer)
  (if (> (length (window-list nil)) 1)
      (delete-window)))

(defun my-delete-window-and-kill-buffer ()
  "Kills the current buffer and deletes the current window."
  (interactive)
  (kill-buffer)
  (if (> (length (window-list nil)) 1)
      (delete-window)))

(defun my-insert-separator (prefix)
  "Inserts a line of '='s.	If prefix is C-u, inserts a line '-'s instead."
  (interactive "P")
  (insert (make-string 80 (if (equal prefix '(4)) ?- ?=))))

;; This is my poor-man's TRAMP ...

(defvar-local my-edit-remote-file-remote-file-spec nil
  "Buffer local variable that holds the remote file spec for my-edit-remote-file.")

(defvar my-edit-remote-file-temp-dir (concat my-systemdrive "/temp/emacs-remote-edit")
  "Pathname of the temporary directory used by my-edit-remote-file-push-back.")

;; Clean the temporary directory at Emacs start time.
(if (and (not (featurep 'my-misc))
	 (file-directory-p my-edit-remote-file-temp-dir))
    (dolist (file (directory-files my-edit-remote-file-temp-dir 'full))
      (if (not (file-directory-p file))
	  (delete-file file))))

(defun my-edit-remote-file-push-back ()
  "..."
  (interactive)
  (if (not (stringp my-edit-remote-file-remote-file-spec))
      (error "ERROR: Variable my-edit-remote-file-remote-file-spec is not set!"))

  (if (buffer-modified-p)
      (error "This buffer has unsaved changes!"))

  (message "Pushing file back to %s" my-edit-remote-file-remote-file-spec)

  (let ((retval (shell-command (concat "scp '" (file-name-nondirectory (buffer-file-name))
				       "' '" my-edit-remote-file-remote-file-spec "'") nil nil)))
    (if (/= 0 retval)
	(error (format "scp failed with errno %s" retval))
      (message "File pushed back!"))))

(defun my-edit-remote-file (file)
  "Edits a remote file.  FILE has the same form as scp's remote file specification:
USER@HOST:PATH.  Use my-edit-remote-file-push-back to push the local copy of the file to
the remote system."
  (interactive "sRemote file: ")

  (if (not (string-match-p "\\([a-zA-Z0-9_]+@\\)?\\([a-zA-Z0-9_-]+\\):\\(.+\\)$" file))
      (error "Invalid remote file syntax!"))

  (let ((remote-host (match-string 2 file))
	(remote-path (match-string 3 file))
	(local-dir-cygwin (concat "/cygdrive/" my-sysdl "/temp/emacs-remote-edit"))
	(local-dir-emacs (concat my-systemdrive "/temp/emacs-remote-edit"))
	local-path-cygwin
	local-path-emacs
	remote-file)
    (if (string-match "^.*/\\([^/]+\\)$" remote-path)
	(setq remote-file (match-string 1 remote-path))
      (setq remote-file remote-path))

    ;; Make sure the temporary directory exists.
    (mkdir local-dir-emacs t)

    ;; Create the pathnames of the local file.
    (setq local-path-cygwin (concat local-dir-cygwin "/" remote-file)
	  local-path-emacs (concat local-dir-emacs "/" remote-file))

    ;; Confirm with the user if the local file already exists.
    (if (and (file-exists-p local-path-emacs)
	     (not (y-or-n-p (format "Local copy of %s already exists!  Overwrite? " remote-file))))
	(error "Aborted!"))

    ;; Fetch the remote file.
    (ignore-errors (delete-file local-path-emacs))
    (message "Fetching remote file ...")
    (shell-command (concat "scp '" file "' '" local-path-cygwin "'") nil nil)

    (if (not (file-exists-p local-path-emacs))
	(error "Failed to fetch remote file: %s" file))

    ;; Display the local file.
    (find-file local-path-emacs)

    ;; Enable push-back command.
    (setq my-edit-remote-file-remote-file-spec file)

    (local-set-key (kbd "C-z C-p") 'my-edit-remote-file-push-back)))

(defvar-local my-toggle-highlight-word-at-point-state nil
  "Non-nil if my-toggle-highlight-word-at-point has been used to highlight a
word, nil otherwise.  When non-nil, the value is the regexp that was passed to
highlight-regexp, which can be passed to unhighlight-regexp to remove the
highlighting.")

(make-face 'my-toggle-highlight-word-at-point-face)
(set-face-foreground 'my-toggle-highlight-word-at-point-face "#000")
(set-face-background 'my-toggle-highlight-word-at-point-face "#ff0")

(defun my-toggle-highlight-word-at-point ()
  "Uses functions highlight-regexp and unhighlight-regexp (and hi-lock-mode) to
highlight/unhighlight all occurrances of the word under point in the current
buffer.	 Using this function turns on Font Lock mode in the current buffer."
  (interactive)
  (if my-toggle-highlight-word-at-point-state
      (progn
	(unhighlight-regexp my-toggle-highlight-word-at-point-state)
	(setq my-toggle-highlight-word-at-point-state nil)
	(message "Highlighting disabled."))
    (let* ((word (symbol-name (or (symbol-at-point) (error "No word found at point!"))))
	   (regexp (concat "\\b" word "\\b")))
      (setq my-toggle-highlight-word-at-point-state regexp)
      (hi-lock-mode 1)
      (highlight-regexp regexp 'my-toggle-highlight-word-at-point-face)
      (message (concat "Highlighting '" word "'")))))

(defun my-vnc-clip-put ()
  "..."
  (interactive)
  (let ((selection (funcall interprogram-paste-function)))
    (if (and selection
	     (stringp selection))
	(write-region selection nil "~/.myclip")
      (write-region (car kill-ring) nil "~/.myclip")))
  (message "Done."))

(defun my-vnc-clip-get ()
  "..."
  (interactive)
  (message "Fetching ...")
  (if (/= 0 (shell-command "scp -q vm:.myclip ~ 2>&1"))
      (error "scp command failed!"))

  (switch-to-buffer (get-buffer-create "*My Clip*"))
  (if (not (string= (buffer-name) "*My Clip*"))
      (error "Failed to switch to buffer *My Clip*!"))

  ;; Must provide an argument to bury-buffer or Emacs switches to another buffer.
  (bury-buffer (current-buffer))
  (insert-file-contents "~/.myclip" nil nil nil 'replace)
  (kill-ring-save (point-min) (point-max)))

(defun my-ssh-environment-setup ()
  "Sets environment variable SSH_AUTH_SOCK based on the contents of
~/.ssh-agent/HOSTNAME.	If the variable is already set, nothing happens.
Interactively, the variable is always set from the file."
  (interactive)
  ;; Get rid of any SSH_AUTH_SOCK variable that exists previously.	This prevents
  ;; me from using another ssh-agent that I did not start.
  (setenv "SSH_AUTH_SOCK" nil)

  (let ((agentfile (concat "~/.ssh-agent/"
			   (downcase (substring (system-name) 0 (cl-position ?. (system-name)))))))
    (when (file-exists-p agentfile)
      (with-temp-buffer
	(insert-file-contents agentfile)
	(goto-char (point-min))
	(when (looking-at (concat "^SSH_AUTH_SOCK=\\([^;]+\\)"))
	  (let ((varvalue (match-string 1)))
	    (setenv "SSH_AUTH_SOCK" varvalue)
	    (if (called-interactively-p 'any)
		(message "Set SSH_AUTH_SOCK to %s" varvalue))))))))

(defun my-duration-to-string (time)
  "Converts an Emacs time value (a list of two integers as returned by function
time-since) or a single integer number of seconds into English."
  (let (days hours minutes seconds format-args output)
    (setq seconds (if (listp time)
		      (truncate (float-time time))
		    (if (integerp time)
			time
		      (error "Argument is not an integer or Emacs time value!")))
	  days (/ seconds 86400)
	  seconds (% seconds 86400)
	  hours (/ seconds 3600)
	  seconds (% seconds 3600)
	  minutes (/ seconds 60)
	  seconds (% seconds 60)
	  format-args (if (> days 0)
			  `("%d days, %d hours, %d minutes, %d seconds"
			    ,days ,hours ,minutes ,seconds)
			(if (> hours 0)
			    `("%d hours, %d minutes, %d seconds" ,hours ,minutes ,seconds)
			  (if (> minutes 0)
			      `("%d minutes, %d seconds" ,minutes ,seconds)
			    `("%d seconds" ,seconds))))
	  output (apply 'format format-args))
    ;; Change all "1 units" to "1 unit".
    (while (string-match "\\([^0-9]\\|^\\)1 \\S-+\\(s\\)" output)
      (setq output (replace-match "" nil nil output 2)))
    output))

(defvar my-tail-current-buffer-timer nil "...")

(defun my-tail-current-buffer ()
  "Repeatedly calls my-revert-buffer-unconditionally every 5 seconds.  Call this
function again to toggle this behavhior."
  (interactive)
  (if (and my-tail-current-buffer-timer
	   (timerp my-tail-current-buffer-timer))
      (progn
	(cancel-timer my-tail-current-buffer-timer)
	(setq my-tail-current-buffer-timer nil)
	(message "Tail timer cancelled."))

    ;; Create the timer.  Cancel the timer if an error occurs.
    (setq my-tail-current-buffer-timer
	  (run-at-time 5 5
		       `(lambda ()
			  (condition-case nil
			      (with-current-buffer ,(current-buffer)
				(my-revert-buffer-unconditionally)
				(dolist (window (get-buffer-window-list (current-buffer) nil t))
				  (with-selected-window window
				    (goto-char (point-max))
				    (recenter -1)
				    (sit-for 0))))
			    (error
			     (cancel-timer my-tail-current-buffer-timer))))))
    (message "Tailing current buffer every 5 seconds ...")))

(defvar my-revert-buffer-unconditionally-emergency-backup nil
  "...")

(defun my-revert-buffer-unconditionally ()
  "Reverts the current buffer without prompting the user, positions point at the
end of the buffer, and displays the maximum amount of text."
  (interactive)
  (let ((revert-without-query '(".")))
    (save-restriction
      (widen)
      (setq my-revert-buffer-unconditionally-emergency-backup (buffer-substring (point-min) (point-max))))
    (revert-buffer)
    (message "Reverted current buffer.")))

(defun my-highlight-compiler-errors ()
  "..."
  (interactive)
  ;; Highlight compiler error messages that appear near the bottom of the
  ;; current buffer.
  (save-excursion
    (goto-char (point-max))
    (forward-line -50)
    (while (search-forward-regexp "\\([A-Za-z_0-9]+\\.\\([ch]\\|[ch]pp\\):[0-9]+\\(:[0-9]+\\)?:\\)\\(\\s-*\\(error\\|note\\|warning\\):.*\\)$" nil t)
      (put-text-property (match-beginning 1) (match-end 1) 'face 'font-lock-preprocessor-face)
      (put-text-property (match-beginning 4) (match-end 4) 'face 'highlight)
      (end-of-line))))

(defun my-comint-beginning-of-line ()
  "Bind this to C-a in comint-mode.	 If within command input area, move point to
start of input.	 Otherwise move point to the true beginning of the current
line."
  (interactive)
  (if (>= (point) (process-mark (get-buffer-process (current-buffer))))
      (beginning-of-line)
    (let ((inhibit-field-text-motion t))
      (beginning-of-line))))

(defun my-comint-next-line ()
  "Bind this to C-n in comint-mode."
  (interactive)
  (if (>= (point) (process-mark (get-buffer-process (current-buffer))))
      (call-interactively 'comint-next-input)
    (call-interactively 'next-line)))

(defun my-comint-previous-line ()
  "Bind this to C-p in comint-mode."
  (interactive)
  (if (>= (point) (process-mark (get-buffer-process (current-buffer))))
      (call-interactively 'comint-previous-input)
    (call-interactively 'previous-line)))

(defun my-comint-recenter ()
  "Bind this to C-l in comint-mode."
  (interactive)
  (if (>= (point) (process-mark (get-buffer-process (current-buffer))))
      (my-show-max-text)
    (call-interactively 'recenter)))

(defun my-randomize-region-lines (start end)
  "Randomly shuffle the lines in the region."
  (interactive "r")
  (message "Randomizing lines in the region ...")
  (shell-command-on-region start end "randomize" nil 'replace)
  (message "Randomizing lines in the region ...	 Done!"))

(defun my-cat-url (url &optional prefix)
  "Runs 'wcat URL' using shell-command."
  (interactive "sFetch URL: 
P")
  (if (member ?' (string-to-list url))
      (error "URL cannot contain single-quotes."))
  (if prefix
      (shell-command (concat "wcat -S '" url "'"))
    (shell-command (concat "wcat '" url "'"))))

(defun my-recenter-all-frames (prefix)
  "Recenters (and thus redisplays) all frames."
  (interactive "P")

  (dolist (frame (frame-list))
    (with-selected-frame frame
      ;; For some reason, calling (redisplay 'force) instead of recenter
      ;; doesn't redisplay every frame on Windows.
      (recenter prefix)
      
      ;; Keep ERC buffers from leaving the last line of text in the middle of the frame.
      (if (eq major-mode 'erc-mode)
	  (my-show-max-text)))))

(defvar my-calc-history nil
  "Holds the minibuffer input history for my-calc.")

(setq calc-multiplication-has-precedence nil
      calc-display-sci-low -9999
      calc-display-sci-high 9999)

(defun my-calc (precision)
  "Perform an aribtrary precision math calculation.	 The precision of the result
is 20 digits by default.  Interactively, a prefix greater than 1 specifies the
precision."
  (interactive "p")

  ;; Need to call calc-create-buffer, otherwise calc-precision throws an error
  ;; if the *Calculator* buffer existed once and has been deleted.
  (if (not (get-buffer "*Calculator*"))
      (calc-create-buffer))

  (my-eval-silently
   (if (> precision 1)
       (calc-precision precision)
     (calc-precision 20)))

  (let* ((result (calc-eval (read-from-minibuffer "Calculate: " nil nil nil 'my-calc-history))))
    (if (listp result)
	(error "Error: %s at %d" (car (cdr result)) (car result))
      (kill-new result)
      (message "Result: %s" result))))

(defun my-insert-timestamp ()
  "Inserts the current date and time at point followed by a colon and a blank line."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %-I:%M %p")))

(defun my-insert-text-heading ()
  "..."
  (interactive)
  (insert "================================================================================\n")
  (my-insert-timestamp)
  (insert "\n")
  (insert "================================================================================\n\n")
  (end-of-line -2))

(defvar my-socks-servers
  '(("localhost:9150" "localhost" 9150 5)
    ("localhost:18181" "localhost" 18181 5))
  "A list of lists each of which describes a SOCKS server.	Each element of this
list is a suitable value for variable socks-server (defined in socks.el).")

(setq socks-server (car my-socks-servers))

(if my-win32
    (setq socks-services-file (concat my-systemdrive "/windows/system32/drivers/etc/services")))

(defvar my-socks-in-use nil
  "t if Emacs is socksified via my-toggle-socksify-emacs.  nil otherwise.")

(defun my-toggle-socksify-emacs (&optional prefix)
  "Toggles Emacs use of the SOCKS server specified by variable socks-server.
Interactively, a prefix of C-u means to query for a SOCKS server."
  (interactive "P")

  (if prefix
      (let* ((socks-choice (completing-read "Choose SOCKS server: " my-socks-servers nil nil))
	     (server-item (assoc-string socks-choice my-socks-servers)))
	(if server-item
	    (setq socks-server server-item)
	  (if (string-match "^\\([^:]+\\):\\([0-9]+\\)$" socks-choice)
	      (setq socks-server (list socks-choice
				       (match-string 1 socks-choice)
				       (string-to-number (match-string 2 socks-choice))
				       5))
	    (error "Invalid SOCKS server specification!")))
	(message "Set SOCKS server to %s" (car socks-server))))

  (if (or prefix (not my-socks-in-use))
      ;; Socksify Emacs ...
      (progn
	(fset 'open-network-stream (symbol-function 'socks-open-network-stream))
	(setq my-socks-in-use t)
	(force-mode-line-update 'all)
	(message "Emacs is now using the SOCKS server at %s." (car socks-server)))

    ;; De-socksify Emacs ...
    (setq my-socks-in-use nil)
    (force-mode-line-update 'all)
    (fset 'open-network-stream (symbol-function 'socks-original-open-network-stream))
    (message "Emacs is no longer using a SOCKS server.")))

(defun my-virus-scan (pathname)
  "Scan PATHNAME for viruses.  This function simply runs my script named 'scan',
which must be smart enough to do the virus scan."
  (if (string-match "^\\(.*\\)/$" pathname)
      (setq pathname (match-string 1 pathname)))

  (if (and (file-directory-p pathname)
	   (not (yes-or-no-p (format "Recursively scan directory '%s'? " pathname))))
      (error "Aborted!"))

  (if (file-directory-p pathname)
      (progn
	(message "Scanning directory '%s' for viruses in background." pathname)
	(my-background-shell-command (format "scan '%s'" pathname)))
    (message "Scanning file '%s' for viruses ..." pathname)
    (shell-command (format "scan '%s'" pathname))))

(defun my-subtract-from-number-at-point (value)
  "Subtracts VALUE from the number at point."
  (interactive "sValue to subtract: ")
  (if (not (string-match-p "[+-]?[0-9]*\\.?[0-9]+" value))
      (error "%s is not a number!" value))

  (save-excursion
    (let (numstart numend)
      (skip-chars-backward "-0-9." (point-min)) 
      (setq numstart (point))
      (skip-chars-forward "-0-9." (point-max))
      (setq numend (point))
      (my-subtract-from-region numstart numend value))))

(defun my-subtract-from-region (start end value)
  "Subtracts VALUE (a string containing a number) from the number in the region
between START and END."
  (interactive "r\nsValue to subtract: ")
  (let ((number (buffer-substring start end))
	result)
    (setq result (with-temp-buffer
		   (erase-buffer)
		   (insert (format "(%s) - (%s)\n" number value))
		   (call-process-region (point-min) (point-max)
					(concat my-systemdrive "/apps/cygwin/bin/bc.exe")
					'delete t nil)
		   (buffer-substring (point-min) (1- (point-max)))))
    (kill-region start end)
    (insert result)))

(defun my-show-window-dimensions ()
  "Displays the width and height of the current window."
  (interactive)
  (message "Window is %d columns by %d lines." (window-width) (window-height)))

(defun my-tab-to-replaceable-text ()
  "Positions the cursor on the first character of the next text matching the
regexp '![a-z0-9_-]+!'."
  (interactive)
  (let ((regexp "![A-Z0-9-]+"))
    (if (looking-at regexp)
	;; Skip over the one we're positioned on now.
	(forward-char (length (match-string 0))))

    (if (search-forward-regexp regexp nil t)
	(backward-char (length (match-string 0)))
      (let ((origin (point)))
	(goto-char (point-min))
	(if (search-forward-regexp regexp nil t)
	    (progn
	      (backward-char (length (match-string 0)))
	      (message "Wrapped search found replaceable text!"))
	  (goto-char origin)
	  (message "No replaceable text found!"))))))

(defun my-read-one-char (prompt chars)
  "Prompts the user with PROMPT (which should end with a space), then reads one
character of input that must be one of the characters in the list CHARS.  Does
not return until a valid character is typed.  Returns the character that was
typed."
  (let ((done nil)
	(cursor-in-echo-area t)
	input)
    (while (not done)
      (setq input (read-char-exclusive prompt))
      (if (memq input chars)
	  (setq done t)
	(ding)
	(let ((msg "Please type '"))
	  (setq msg (concat msg
			    (mapconcat 'char-to-string (cl-subseq chars 0 -1) "', '")
			    "', or '"
			    (char-to-string (car (last chars)))
			    "'."))
	  (message msg)
	  (sit-for 2))))
    input))

(defun my-rot13-region (start end)
  "Does a ROT13 on the text between START and END (interactively, the region)."
  (interactive "r")
  (let ((origpoint (point))
	(string (buffer-substring start end))
	(index 0)
	(charmap (concat (make-string 65 0)
			 "NOPQRSTUVWXYZABCDEFGHIJKLM"
			 (make-string 6 0)
			 "nopqrstuvwxyzabcdefghijklm"))
	oldchar
	newchar)
    (while (< index (length string))
      (setq oldchar (aref string index))
      (setq newchar (if (or (and (>= oldchar ?A)
				 (<= oldchar ?Z))
			    (and (>= oldchar ?a)
				 (<= oldchar ?z)))
			(aref charmap oldchar)
		      oldchar))
      (aset string index newchar)
      (incf index))
    (delete-region start end)
    (insert string)
    (goto-char origpoint)
    (if (= (point) start)
	(set-mark end)
      (set-mark start))))

(defvar my-background-shell-command-history nil
  "Holds the command history used by my-background-shell-command.")

(defun my-background-shell-command (command)
  "Runs COMMAND in the background without displaying an output buffer."
  (interactive (list (read-from-minibuffer "Background shell command: " nil nil nil
					   'my-background-shell-command-history)))
  (let ((minibuf-active (minibuffer-window-active-p (selected-window)))
	(active-window (selected-window)))
    ;; Prevent shell-command from popping open a window on the *Async Shell Command* buffer.
    (my-with-advice ((display-buffer ignore))
      (shell-command (concat command " &")))

    (bury-buffer "*Async Shell Command*")
    (if minibuf-active
	(select-window active-window))))

(defun my-run-doxygen (&optional prefix)
  "Runs doxygen."
  (interactive "P")
  (message "Running doxygen ...")
  (ignore-errors (kill-buffer "*Doxygen Output*"))
  (let ((status (shell-command "doxygen doxygen.conf")))
    (if (not (= 0 status))
	(pop-to-buffer "*Shell Command Output*")
      (with-current-buffer "*Shell Command Output*"
	(rename-buffer "*Doxygen Output*"))

      (if (and my-win32
	       (null prefix))
	  (shell-command "launch doc/html/index.html"))

      (bury-buffer (get-buffer "*Doxygen Output*"))
      (delete-window)
      (message "Running doxygen ...	 Done."))))

(defvar my-beep-enabled t
  "Non-nil if set-message-beep has been used to enable a beep sound, otherwise nil.")

(defun my-toggle-beep ()
  "Toggles whether Emacs will beep or not."
  (interactive)
  (if (not my-win32)
      (error "This command only works on Windows!"))
  (if my-beep-enabled
      (set-message-beep 'silent)
    (set-message-beep (if (file-exists-p (concat my-systemdrive "/windows/"))
			  'hand			;; For XP.
			'asterisk)))	;; For non-XP.
  (setq my-beep-enabled (not my-beep-enabled))
  (message "Emacs is now %s" (if my-beep-enabled "noisy." "silent.")))

(defun my-java-compile (&optional prefix)
  "Compiles the current Java source file (or all Java source files in the current
directory if PREFIX is non-null)."
  (interactive "P")
  (let* ((classpath (save-excursion
		      (goto-char (point-min))
		      (if (not (search-forward-regexp "^package\\s-+\\(.*\\);" nil t))
			  "."
			(let* ((package-name (match-string 1))
			       (numdirs (1+ (- (length package-name)
					       (length (delete ?. package-name))))))
			  (mapconcat 'identity (make-list numdirs "..") "/")))))
	 ;; For Sun's javac use: "javac -classpath %s %s".
	 ;; For GNU's gcj use: "gcj -C --classpath=%s %s".
	 (cmd (format "javac -classpath %s %s" classpath
		      (if prefix
			  "*.java"
			(file-name-nondirectory (buffer-file-name))))))
    (message "Compiling with: %s" (propertize cmd 'face 'my-alert-face))
    (my-compile cmd)))

(defun my-java-run ()
  "Runs the class contained in the current Java source file."
  (interactive)
  (let* (package-name
	 (bufname (buffer-name))
	 (classpath (save-excursion
		      (goto-char (point-min))
		      (if (not (search-forward-regexp "^package\\s-+\\(.*\\);" nil t))
			  "."
			(setq package-name (match-string 1))
			(let ((numdirs (1+ (- (length package-name)
					      (length (delete ?. package-name))))))
			  (mapconcat 'identity (make-list numdirs "..") "/")))))
	 (cmd (format "java -classpath %s %s" classpath
		      (if (string-match "^\\(.*\\)\\.java$" bufname)
			  (if package-name
			      (concat package-name "." (match-string 1 bufname))
			    (match-string 1 bufname))
			(error "Cannot determine name of class to execute!")))))
    (message "Running with: %s" (propertize cmd 'face 'my-alert-face))
    (delete-other-windows)
    (shell-command (concat cmd " &"))
    (other-window 1)))

(defun my-java-clean ()
  (interactive)
  (when (yes-or-no-p (format "Delete *.class in %s? "
			     ;; propertize doesn't work with y-or-n-p.	:-(
			     (propertize default-directory 'face 'my-alert-face)))
    (message "Deleting *.class ...")
    (if (null (shell-command "rm *.class"))
	(message "Done."))))

(defun my-java-generate-documentation ()
  "Runs javadoc on the current source file and launches a browser to display the generated
documentation."
  (interactive "")
  (if (buffer-modified-p)
      (error "Please save this buffer first!"))
  (shell-command (format "javadoc -d doc -private -quiet -header '%s' %s"
			 (file-name-nondirectory (buffer-file-name))
			 (file-name-nondirectory (buffer-file-name))
			 ))
  (my-launch-file (concat (file-name-directory (buffer-file-name))
			  "doc/index.html")))

(defun my-random-element (list)
  "Returns a random element from LIST (or nil if LIST is empty)."
  (if list
      (nth (random (length list)) list)))

(defun my-time-to-string (time)
  "Converts an Emacs time value (as returned by function time-since or
time-subtract) into English."
  (let (days hours minutes seconds format-args output)
    (setq seconds		(truncate (float-time time))
	  days			(/ seconds 86400)
	  seconds		(% seconds 86400)
	  hours			(/ seconds 3600)
	  seconds		(% seconds 3600)
	  minutes		(/ seconds 60)
	  seconds		(% seconds 60)
	  format-args	(if (> days 0)
			    `("%d days, %d hours, %d minutes, %d seconds"
			      ,days ,hours ,minutes ,seconds)
			  (if (> hours 0)
			      `("%d hours, %d minutes, %d seconds" ,hours ,minutes ,seconds)
			    (if (> minutes 0)
				`("%d minutes, %d seconds" ,minutes ,seconds)
			      `("%d seconds" ,seconds))))
	  output		(apply 'format format-args))
    ;; Change all "1 units" to "1 unit".
    (while (string-match "\\([^0-9]\\|^\\)1 \\S-+\\(s\\)" output)
      (setq output (replace-match "" nil nil output 2)))
    output))

(defun my-revert-all-buffers ()
  "Reverts all buffers that are assoicated with a filename."
  (interactive)
  (when (yes-or-no-p (propertize "Do you *REALLY* want revert all buffers? "
				 'face 'my-alert-face))
    (let ((count 0))
      (dolist (buf (buffer-list))
	(let ((bufname (buffer-name buf)))
	  (if (and (buffer-file-name buf)
		   (not (buffer-modified-p buf)))
	      (with-current-buffer buf
		(message "Reverting buffer %s ..." bufname)
		(ignore-errors
		  (revert-buffer 'ignore-autosave 'noconfirm))
		(setq count (1+ count))))))

      (message "Reverted %d buffers!" count))))

(defun my-check-for-newer-elisp-files ()
  "Displays a popup message if any of my Elisp source files are newer than the time at
which my the source (or .elc) file was last loaded.  This function is called periodically
in a timer."
  (if my-elisp-load-times
      (catch 'return
	(let ((files `("~/.emacs" "~/.gnus" ,@(directory-files "~/elisp" 'fullpath "^my-.*\\.el$")))
	      msg file-loadtime)
	  (dolist (pathname files)
	    (setq pathname (expand-file-name pathname)  ;; QUESTION: Maybe used file-truename instead (or both)?
		  file-loadtime (cdr (assoc pathname my-elisp-load-times)))

	    (when (and file-loadtime
		       (time-less-p file-loadtime
				    (nth 5 (file-attributes pathname))))
	      (my-message-highlight (format "File '%s' has changed!  Reload ~/.emacs!"
					    (file-name-nondirectory pathname)))
	      (sleep-for 1)  ;; To be sure I see the message even if I'm typing.
	      (sit-for 5)
	      (throw 'return nil)))))))

(defun my-check-for-out-of-date-elc-files ()
  "Scans buffer *Messages* for warnings about out-of-date .elc files having been
loaded, and displays the warnings."
  (interactive)
  (let ((msg "")
	sol eol msglines)
    (with-current-buffer "*Messages*"
      (save-excursion
	(goto-char (point-max))

	;; Start the search from the most recent message about loading my-constants.el or the
	;; beginning of the buffer if that message isn't found.  This is to cope with the
	;; possibility that my-reload-my-elisp-files may have been executed.
	(if (not (search-backward-regexp "^Loading my-constants\\.\\.\\.done$" nil t))
	    (goto-char (point-min)))

	(while (search-forward-regexp "source file is newer" nil t)
	  (setq sol (progn (beginning-of-line) (point))
		eol (progn (end-of-line) (point))
		msg (concat msg (buffer-substring sol eol) "\n")))))

    (when (> (length msg) 0)
      (pop-to-buffer "*Out of Date Elisp Files*")
      (erase-buffer)

      (setq msglines (cl-count ?\n msg))
      (insert (propertize
	       (concat (format "\nWARNING: %d out of date .elc %s loaded:\n\n"
			       msglines (if (> msglines 1) "files were" "file was"))
		       msg)
	       'face 'my-alert-face))

      (fundamental-mode)
      (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer)
      (goto-char (point-min)))))

(defun my-do-later (delay &rest forms)
  "Evaluates FORMS after DELAY seconds.	 DELAY can be either an integer or
floating point number."
  (run-at-time delay nil `(lambda () ,@forms)))

(defun my-popup (title message &optional use-traytip)
  "Pops up a dialog box having the given TITLE and display MESSAGE.  This function
requires that a program named \"popup\" exists in the the user's PATH. The caller does not
have to strip single-quotes from the arguments, because this function does that.  If
optional parameter USE-TRAYTIP is non-null and this is a Windows system, a system tray
balloon is used instead."

  ;; BUG: Using my-eval-silently here is defeated by the fact that the popup command is run in the
  ;; background.  By the time popup finishes, the advice placed on message in my-eval-silently has
  ;; been removed.
  (my-eval-silently
   (if my-win32
       ;; For some reason, the stdout/stderr redirections are needed to keep
       ;; Windows Emacs from waiting for the command to complete.
       (let ((cmd (if use-traytip "traytip" "popup")))
	 (shell-command (format "(%s '%s' '%s' >/dev/null 2>&1 &)"
				cmd (delete ?' title) (delete ?' message))))
     ;; For some reason, under FreeBSD (and what other UNIXes?), using a subshell
     ;; makes bash hang eating CPU.
     (shell-command (format "popup '%s' '%s' >/dev/null 2>&1"
			    (delete ?' title) (delete ?' message))))))

(defun my-switch-to-other-buffer ()
  "Returns the first buffer after this one in the buffer-list whose name doesn't start
with a space, isn't named '*Buffer List*', isn't named '*grep*', and isn't an ERC buffer."
  (interactive)
  (let ((buffers (cdr (buffer-list)))
	done
	buf
	bufname)
    (while (not done)
      (setq buf (car buffers))
      (setq bufname (buffer-name buf))
      (if (and (not (eq buf (current-buffer)))
	       (not (= 32 (aref bufname 0)))
	       (not (string= "*Buffer List*" bufname))
	       (not (string= "*grep*" bufname))
	       (not (with-current-buffer buf
		      (memq major-mode '(erc-mode Buffer-menu-mode)))))
	  (setq done t)
	(setq buffers (cdr buffers))))

    (if (null buf)
	(setq buf (other-buffer)))
    (switch-to-buffer buf)))

(defun my-stack-match (regexp)
  "Returns t if the name of any function currently active in the Lisp call stack
matches REGEXP, nil otherwise.	Matching is done with string-match-p."
  (let ((backtrace-func nil)
	(funcstring nil)
	(frame 1)
	(logbuf nil)
	;;(logbuf (get-buffer-create "*stackmatch*"))	;; Set to nil to turn off logging.
	(match nil))
    (when logbuf
      (bury-buffer logbuf)
      (with-current-buffer logbuf
	(erase-buffer)
	(insert (concat (format-time-string "%H:%M:%S\n")))))
    (while (setq backtrace-func (cadr (backtrace-frame frame)))
      (setq funcstring (prin1-to-string backtrace-func)
	    match (or match (not (null (string-match-p regexp funcstring))))
	    frame (1+ frame))
      (if logbuf
	  (with-current-buffer logbuf
	    (goto-char (point-max))
	    (insert (format "%2d: %s\n" (1- frame) funcstring)))))
    match))

(defun my-random-quote (&optional prefix)
  "Echoes a random pithy quote to the message area.	 If prefix is non-null, this
inserts the quote at point in the current buffer and nothing appears in the echo
area."
  (interactive "P")
  (let ((inhibit-redisplay t)
	(quote "")
	(lines 0))
    (save-window-excursion
      (set-buffer (get-buffer-create "*Quotes*"))
      (insert-file-contents "~/.quotes" nil nil nil 'replace)
      (setq lines (count-lines (point-min) (point-max)))
      (goto-char (point-min))
      (forward-line (random lines))
      (setq quote (buffer-substring (line-beginning-position) (line-end-position)))
      (kill-buffer "*Quotes*"))
    (if prefix
	(insert quote)
      (message quote))
    quote))

(defun my-confirm-kill-emacs (unused-prompt)
  "Bound to variable confirm-kill-emacs."
  (y-or-n-p (propertize "Really kill Emacs? " 'face 'my-alert-face)))

(defun my-buffer-menu-cleanup ()
  "Deletes the buffers *Completions*, *GPG Messages*, *Help*, *Compile-Log*, *vc*,
*vc-diff*, *Apropos*, and all Man-mode buffers.	 Also buries all buffers whose names
start with '*', .newsrc-dribble, sent-usenet, and gnus-mbox."
  (interactive)
  (my-list-buffers)  ;; Seems to avoid the below 'Non-string bufname' error.

  (dolist (buffer (buffer-list))
    (let ((bufname (buffer-name buffer)))
      (if (not (stringp bufname))
	  (error "my-buffer-menu-cleanup: Non-string bufname!  How can this happen?"))

      ;; Kill some buffers ...
      (if (or (member bufname '("*Completions*" "*Help*" "*Compile-Log*" "*Apropos*" "*vc*"
				"*SourceSafe log*" "*vc-diff*" "*GPG Messages*" "*Doxygen Output*"
				"*Calendar*" "*Calculator*" "*Calc Trail*"))
	      (string-match-p "^\\*diff" bufname)
	      (and (string= "*Async Shell Command*" bufname)
		   (null (process-list)))
	      (and (string= "*Shell Command Output*" bufname)
		   (= 0 (buffer-size buffer))))
	  (kill-buffer buffer)
	(if (string-match-p "^\\*Man " bufname)
	    (kill-buffer buffer)))

      ;; Bury some buffers ...
      (if (member bufname '("*Group*" ".newsrc-dribble" "sent-usenet" "gnus-mbox" "*scratch*"))
	  (bury-buffer buffer)
	(if (and (string-match-p "^\\*" bufname)
		 (buffer-live-p buffer))
	    (bury-buffer buffer)))))

  ;; Regenerate the buffer-menu buffer.
  (my-list-buffers))

(defun my-insert-footnote ()
  "Inserts an ISO Latin-1 superscripted 1, 2, or 3, depending on the keypath
used to invoke this command."
  (interactive)
  (let ((keys (this-single-command-keys)))
    (cond ((equal keys (string-to-vector "\C-z1"))
	   (insert "..."))
	  ((equal keys (string-to-vector "\C-z2"))
	   (insert "..."))
	  ((equal keys (string-to-vector "\C-z3"))
	   (insert "..."))
	  (t (error "oops")))))

(defun my-describe-variable (variable)
  "Just like describe-variable, except the minibuffer history is restricted to
past input read by this function (instead of all input read by any call to
completing-read)."
  (interactive 
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if (symbolp v)
				    (format "Describe variable (default %s): " v)
				  "Describe variable: ")
				obarray 'boundp t nil
				'my-describe-variable-history
				(if (symbolp v) (symbol-name v))))
     (list (if (equal val "")
	       v (intern val)))))
  (describe-variable variable))

(defvar my-describe-variable-history nil
  "History list used by function my-describe-variable.")

(defun my-describe-function (function)
  "Just like describe-function, except the minibuffer history is restricted to
past input read by this function (instead of all input read by any call to
completing-read)."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)			 
	 val)
     (setq val (completing-read (if fn
				    (format "Describe function (default %s): " fn)
				  "Describe function: ")
				obarray 'fboundp t nil
				'my-describe-function-history
				(symbol-name fn)))
     (list (if (equal val "")
	       fn (intern val)))))
  (describe-function function))

(defvar my-describe-function-history nil
  "History list used by function my-describe-function.")

(defun my-turn-off-backup-this-buffer ()
  "Turns off backups for this buffer."
  (interactive)
  (set (make-local-variable 'backup-inhibited) t)
  (if (called-interactively-p 'any)
      (message "Backup is disabled in this buffer!")))

(defun my-describe-face-at-point (&optional prefix)
  "Describes the face of the character at point.  Interactively, a prefix argument means
to read the face name in the minibuffer."
  (interactive "P")
  (let* ((face (if prefix
		   (read-from-minibuffer "Face name: " nil nil 'lisp 'minibuffer-history)
		 (get-char-property (point) 'face))))
    (if (and prefix (not (facep face)))
	(error "%s is not a face!" (prin1-to-string face)))
    (if (null face)
	(error "The character at point has no face property."))
    (if (listp face)
	(message "Multiple faces: %s" face)
      (describe-face face)
      (other-window 1)
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(insert (propertize (format "Description of face %s.\n\n" face) 'face 'my-alert-face)))
      (goto-char (point-min)))))

(defun my-scroll-up-one-line (&optional prefix)
  "Scrolls the current buffer up one line."
  (interactive "p")
  (scroll-up prefix))

(defun my-scroll-down-one-line (&optional prefix)
  "Scrolls the current buffer down one line."
  (interactive "p")
  (scroll-down prefix))

(defun my-find-non-ascii-char ()
  "..."
  (interactive)
  (search-forward-regexp "[^\000-\377]"))

(defun my-delete-trailing-whitespace ()
  "Deletes all trailing spaces and tabs from every line in the buffer.	This is not
exactly the same as the builtin function delete-trailing-whitespace, which takes START
and END paramters and can also delete blank lines at the end of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[		 ]+$" nil t)
      (replace-match ""))))

(defun my-shell (&optional new-frame)
  "Launches a shell in a uniquely-named buffer.	 With a prefix arg (interactively,
a non-nil argument, NEW-FRAME) a new frame is created."
  (interactive "P")
  (if new-frame
      (select-frame (make-frame '((top . 335)
				  (left . 25)
				  (width . 85)
				  (height . 28)))))
  ;; No longer needed.
  ;;;; Trick function shell into setting TERM=emacs.
  ;;(let ((system-uses-terminfo nil))
  ;;  (shell))

  (shell))

(defvar my-shell-prompt-regexp "^[-A-Za-z0-9_]+:[^:]+: "
  "A regexp that matches my Bash shell prompt.")

(defun my-shell-clear-buffer ()
  "Deletes the entire contents of the current shell buffer.	 See my-shell."
  (interactive)
  ;; Make sure we are at a shell prompt.  This function should not do anything
  ;; if we are not at a shell prompt, because it sends a newline to the process.
  (let ((origin (point))
	(start-of-input (process-mark (get-buffer-process (current-buffer)))))
    (goto-char start-of-input)

    (let ((inhibit-field-text-motion t))
      (beginning-of-line))

    (when (not (looking-at-p my-shell-prompt-regexp))
      (goto-char origin)
      (error "Aborted: Bottom of buffer doesn't look like a shell prompt!"))

    (goto-char (point-max))

    ;;	  (let ((inhibit-read-only t)
    ;;		  (killed-unsent-input nil))
    ;;		(when (/= (point) start-of-input)
    ;;		(comint-kill-input)
    ;;		(setq killed-unsent-input t))
    ;;
    ;;		;; Erase the entire buffer.
    ;;		(delete-region (point-min) (point-max))
    ;;
    ;;		;; Send a newline to make a new shell prompt to appear.
    ;;		(comint-send-input)
    ;;
    ;;		;; Put back the unsent shell input.
    ;;		(if killed-unsent-input
    ;;		  (yank)))

    (let ((inhibit-field-text-motion t)
	  (inhibit-read-only t))
      (beginning-of-line)
      (delete-region (point-min) (point))
      (goto-char (point-max)))))

(defun my-show-max-text (&optional gap)
  "Display the maximum amount of text in the current buffer by positioning the
bottom line of the buffer at the bottom line of the window.	 Use a prefix
argument to specify the number of blank lines to leave at the bottom.  This
function moves point to the beginning of the last line of text in the buffer."
  (interactive "P")
  (goto-char (point-max))
  (if (and (bolp) (> (point) 1))
      (forward-line -1))
  (recenter (if gap
		(prefix-numeric-value (- (1+ gap)))
	      -1)))

(defun my-make-windows-app (file)
  (interactive "fFile: ")
  (let ((winfile (find-file-noselect file)))
    (set-buffer winfile)
    (goto-char 0)
    (cond ((search-forward "PE" 1000)
	   (forward-char 90)
	   (cond ((looking-at-p "\C-b")
		  (message "%s is already a Windows non-console app." file))
		 ((looking-at-p "\C-c")
		  (delete-char 1)
		  (insert "\C-b")
		  (save-buffer))
		 (t
		  (message "file %s does not look like a console app." file))))
	  (t
	   (message "file %s does not look like a console app." file)))
    (kill-buffer (current-buffer))))

(defun my-cycle-eol-conventions ()
  "Cycles through three EOL conventions (DOS, Unix, and Mac) for the
current buffer without altering the character set coding."
  (interactive)
  (let* ((sysname (symbol-name buffer-file-coding-system))
	 (offset (string-match-p "\\(dos\\|unix\\|mac\\)$" sysname))
	 (prefix (substring sysname 0 offset))
	 (suffix (substring sysname offset))
	 (newname (concat prefix
			  (cond ((string= "unix" suffix) "mac")
				((string= "mac" suffix) "dos")
				((string= "dos" suffix) "unix")
				(t (error "Unrecognized coding system: %s"
					  sysname))))))
    (setq buffer-file-coding-system (intern newname))
    (set-buffer-modified-p t)
    (message "New coding system is %s" newname)))

(defvar my-print-onscreen nil
  "If non-nil, my-print-region and my-print-buffer pass -Pdisplay to a2ps, causing
data to be display on-screen instead of being printed.")

(defun my-print-toggle-onscreen-display (n)
  "Called interactively, toggles the value of my-print-onscreen.  Non-interactively,
if N is negative, turns off onscreen printing, otherwise turns it on."
  (interactive "P")
  (if (called-interactively-p 'any)
      (progn
	(setq my-print-onscreen (not my-print-onscreen))
	(message (if my-print-onscreen
		     "Printing to screen."
		   "Printing to default printer.")))

    ;; Called non-interactively.
    (if (listp n)
	(setq n (car n)))
    (setq my-print-onscreen (>= n 0))))

(defvar my-print-font-size nil
  "Is this used anywhere?")				;; ISSUE: ???


(defun my-print-region (start end &optional 2up tabwidth title nomessage edit)
  "Prints the contents of the current region using the program \"a2ps\".  The
name of print request is the name of the buffer.

If optional argument 2UP is non-nil, prints 2-up in a 6-point font.	 Optional
argument TABWIDTH is the numeric tab-width value to pass to a2ps via the
--tabsize option.  Optional argument TITLE is a string to pass to a2ps via the
--center-title option.	If optional argument NOMESSAGE is non-nil, no messages
are displayed in the minibuffer.  If optional argument EDIT is non-nil (interactively,
prefix is C-u), the user is given a chance to edit the print command before it
executes.

Returns a string containing the command used to perform the print operation."
  (interactive (list (region-beginning) (region-end) nil nil nil nil
		     (if (equal '(4) current-prefix-arg) t)))
  (or nomessage (message "Printing region..."))

  (let ((result "")
	(tmpfile (if my-win32
		     (concat my-systemdrive "/temp/emacs-print.tmp")
		   "/tmp/emacs-print.tmp")))
    (write-region start end tmpfile)
    (let* ((font-arg (format "-f%d "
			     (if (and (boundp 'my-print-font-size)
				      (integerp my-print-font-size))
				 my-print-font-size
			       (if 2up 7 9))))
	   (command (concat "a2ps "
			    (if (or my-print-onscreen edit) "-Pdisplay ")
			    (if 2up "-2 ")
			    font-arg
			    (format "--tabsize=%d " (or tabwidth tab-width))
			    "--center-title='" (delete ?' (or title
							      (buffer-name))) "' "
							      ;; This next bit lets Emacs run without waiting for
							      ;; a2ps to terminate.
							      (format "<%s >/dev/null 2>&1 &" tmpfile))))
      (if edit
	  (setq command (read-from-minibuffer "Print command: " (cons command 5))))
      (setq result command)
      (shell-command-on-region start end command nil))
    result))

(defun my-print-buffer (&optional 2up tabwidth title nomessage edit)
  "Prints the contents of the current buffer (_not_ the on-disk file, but the
contents of the buffer) using the program \"a2ps\".	 The name of print request
is the name of the buffer.

If optional argument 2UP is non-nil, prints 2-up in a 6-point font.	 Optional
argument TABWIDTH is the numeric tab-width value to pass to a2ps via the
--tabsize option.  Optional argument TITLE is a string to pass to a2ps via the
--center-title option.	If optional argument NOMESSAGE is non-nil, no messages
are displayed in the minibuffer.  If optional argument EDIT is non-nil (interactively,
prefix is C-u), the user is given a chance to edit the print command before it
executes."
  (interactive (list nil nil nil nil (if (equal '(4) current-prefix-arg) t)))
  (if (not nomessage)
      (message "Printing buffer..."))
  (message (my-print-region (point-min) (point-max) 2up tabwidth title t edit)))

(defun my-backup-enable-predicate (name)
  "Prevents files named \".emacs-places\" from being backed up.	 All other files are
backed up.	Variable backup-enable-predicate should be set to this function."
  (save-match-data
    (cond ((string-match-p "\\.emacs-places$" name) nil)
	  (t t))))

(defun my-remove-crs ()
  "Removes all CR (^M) characters from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\C-m" nil t)
      (replace-match "" nil t))))

(defun my-deactivate-mark ()
  "Deactivates the mark in the current buffer.	This function exists because
deactivate-mark is not interactive."
  (interactive)
  (deactivate-mark))

(if (< emacs-major-version 21)
    (defun next-line (arg)
      "A variation on the standard next-line function.

Move cursor vertically down ARG lines.

If there is no character in the target line exactly under the current
column, the cursor is positioned after the character in that line
which spans this column, or at the end of the line if it is not long
enough.

If there is no line in the buffer after this one, behavior depends on
the value of next-line-add-newlines.  If non-nil and not eq to 'if-modified
or if eq to 'if-modified and the buffer has been modified, a newline
character is inserted to create a line and the cursor moves to that line,
otherwise the cursor is moved to the end of the buffer (if already at
the end of the buffer, an error is signaled).

The command \\[set-goal-column] can be used to create a semipermanent
goal column to which this command always moves.	 Then it does not try
to move vertically.	 This goal column is stored in `goal-column',
which is nil when there is none.

If you are thinking of using this in a Lisp program, consider using
forward-line instead.  It is usually easier to use and more reliable (no
dependence on goal column, etc.)."
      (interactive "p")
      (let ((opoint (point)))
	(if (cond ((eq next-line-add-newlines 'if-modified) (buffer-modified-p))
		  (t next-line-add-newlines))
	    (if (/= arg 1)
		(line-move arg)
	      (forward-line 1)
	      (if (or (= opoint (point)) (not (eq (preceding-char) ?\n)))
		  (insert ?\n)
		(goto-char opoint)
		(line-move arg)))
	  (if (and (> arg 0)
		   (eobp))
	      (signal 'end-of-buffer nil))
	  (line-move arg)
	  (if (= opoint (point))
	      (end-of-line))))
      nil))

(defun my-split-window-vertically (&optional prefix)
  "Split window vertically the way God intended.  Interactively, a prefix argument
means to split the window horizontally instead."
  (interactive "P")
  (if prefix
      (split-window-horizontally)
    (split-window-vertically))
  (other-window 1))

(defun my-next16-line (&rest any)
  "Move the cursor down 16 lines."
  (interactive)
  (with-no-warnings
    ;; Yes, I really want to call next-line here, so suppress the compiler warnings about
    ;; using it in Elisp.
    (next-line 16))
  (if (eq major-mode 'dired-mode)
      (dired-move-to-filename)))

(defun my-previous16-line (&rest any)
  "Move the cursor up 16 lines."
  (interactive)
  (with-no-warnings
    ;; Yes, I really want to call previous-line here, so suppress the compiler warnings
    ;; about using it in Elisp.
    (previous-line 16))
  (if (eq major-mode 'dired-fode)
      (dired-move-to-filename)))

(defvar my-insert-buffer-name-last-insertion-length 0
  "Holds the number of characters inserted by the most recent call to
my-insert-buffer-name.")

(defvar my-insert-buffer-name-last-inserted-buffer-window nil
  "Holds the window displaying the buffer whose name was inserted by the most
recent call to my-insert-buffer-name.")

(defun my-insert-buffer-name ()
  "Inserts the name of the currently selected buffer.  Pressing the key bound to
this function multiple times in sequence will replace the last-inserted buffer
name with the name of the buffer displayed in the next window in the current
frame, eventually cycling around to the window displaying the current buffer."
  (interactive)
  (let (buffer-name
	window)
    (if (and (eq last-command 'my-insert-buffer-name)
	     (windowp my-insert-buffer-name-last-inserted-buffer-window))
	(progn
	  ;; Remove the previously inserted buffer name.
	  (delete-region (point) (- (point) my-insert-buffer-name-last-insertion-length))

	  ;; Get the name of the buffer in the next window in this frame.
	  (setq window (next-window my-insert-buffer-name-last-inserted-buffer-window
				    'ignoreminibuffer)
		buffer-name (buffer-name (window-buffer window))))

      ;; Get the current window and buffer name.
      (setq window (selected-window))

      (if (minibuffer-window-active-p window)
	  ;; window is the minibuffer window, so skip it.
	  (setq window (next-window window)))

      (setq buffer-name (buffer-name (window-buffer window))))

    (setq my-insert-buffer-name-last-insertion-length (length buffer-name)
	  my-insert-buffer-name-last-inserted-buffer-window window)
    (insert buffer-name)))

(defun my-n-column-tabs (columns)
  "Sets tab-width and tab-stop-list (local to current buffer) so that tabs are
every N columns."
  (interactive "p")
  (if (<= columns 0)
      (error "Invalid tab width: %d!" columns))

  (if (= columns 1)
      (error "Can't set tab width to 1 column!"))

  (make-local-variable 'tab-stop-list)
  (setq tab-width columns
	tab-stop-list (number-sequence columns 120 columns))
  ;; This call to recenter causes error "`recenter'ing a window that does not display
  ;; current-buffer." when this function is called from a Local Variables block.
  ;;(recenter)
  )

(defun my-yank-quoted ()
  "Does a (yank) and then quotes the yanked lines with a leading '> '."
  (interactive)
  (yank)
  (my-indent-region-with-string "> "))

(defun my-launch-file (pathname)
  "Launches a file."
  (interactive "fFile to launch: ")
  (if my-win32
      (progn
	(message "Launching %s ..." pathname)

	;; Sleep for 1 second to give me time to release the shift key, because Winamp enqueues
	;; a song rather than playing it if it is opened with the shift key down.
	(sleep-for 1)
	(w32-shell-execute "open" (expand-file-name pathname)))

    (cond
     ((string-match-p "\\.\\(ogg\\|wav\\|mp3\\)$" pathname)
      (message "Opening %s with xmms ..." pathname)
      (start-process "xmms-play" nil "xmms" "--play" pathname))

     ((string-match-p "\\.\\(xbm\\|xpm\\|png\\|gif\\|jpg\\|bmp\\)$" pathname)
      (message "Opening %s with Eye of Gnome ..." pathname)
      (start-process "eog" nil "eog" pathname))

     ((string-match-p "\\.\\(html\\|htm\\)$" pathname)
      (message "Opening %s with firefox ..." pathname)
      ;; Must not have a space after the comma in the argument list to openURL()!!!
      (start-process "firefox" nil "firefox" "-remote"
		     (format "openURL(file://%s,new-window)" pathname)))

     (t
      (shell-command (concat "gnome-open " pathname " >/dev/null 2>&1"))
      (message "Opening %s with gnome-open ..." pathname)
      ;; Why doesn't this work?
      ;;(start-process "gnome-open" nil "gnome-open" pathname)
      ))))

(defun my-kill-ring-save (beg end)
  "Does kill-ring-save normally, then deactivates the mark."
  (interactive "r")
  (kill-ring-save beg end)
  (setq deactivate-mark t))

(defun my-emacs-lisp-mode-newline ()
  "Inserts a newline then indents using lisp-indent-line."
  (interactive)
  (newline)
  (lisp-indent-line))

(defun my-auto-indent-newline ()
  "Inserts a newline then indents using indent-for-tab-command.	 In some modes,
does additional special processing."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^[ \t]+$")
	(delete-horizontal-space)))

  ;; Special case: ruby-mode.
  (if (and (eq major-mode 'ruby-mode)
	   (save-excursion
	     (beginning-of-line)
	     (looking-at-p "^\\s-*end\\s-*$")))
      (indent-according-to-mode))

  ;; Special case: java-mode.
  (if (and (eq major-mode 'java-mode)
	   (save-excursion
	     (beginning-of-line)
	     (looking-at-p "\\s-*/\\*\\*\\s-*$")))
      (progn
	(my-newline-with-indent)
	(insert " * ")
	(my-newline-with-indent)
	(insert "*/")
	(forward-line -1)
	(end-of-line))

    ;; All other cases ...
    (newline)
    (indent-for-tab-command)))

;;(defun my-perl-mode-newline ()
;;	"Inserts a newline then indents using perl-indent-command."
;;	(interactive)
;;	(save-excursion
;;	  (beginning-of-line)
;;	  (if (looking-at-p "^[ \t]+$")
;;		(delete-horizontal-space)))
;;	(newline)
;;	(perl-indent-command))

(defun my-count-lines-page ()
  "Just like count-lines-page with the off-by-1 bug."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (end-of-line)
      (let ((this-line-buffer (count-lines (point-min) (point)))
	    (buffer-total (count-lines (point-min) (point-max))))
	(narrow-to-page)
	(let ((this-line-page (count-lines (point-min) (point)))
	      (page-start-line (1+ (count-lines 1 (point-min))))
	      (page-total (count-lines (point-min) (point-max))))
	  (message
	   "Line %d of %d in buffer -- %d of %d in page -- page starts at line %d"
	   this-line-buffer buffer-total this-line-page page-total
	   page-start-line))))))

(defun my-indent-region-with-string (indent-string &optional number-lines)
  "Prefixes every line in the region with a user-supplied string."
  (interactive (if current-prefix-arg
		   '("" t)
		 (list (read-from-minibuffer "Indent region with string: "
					     "" nil nil nil)
		       nil)))
  (if (and (null number-lines)
	   (equal indent-string ""))
      (setq indent-string "> "))
  (save-excursion
    (let ((reglines	   (count-lines (region-beginning) (region-end)))
	  (line		   1))
      (goto-char (region-beginning))
      (while (<= line reglines)
	(beginning-of-line)
	(if number-lines
	    (insert (format "%5d		" line))
	  (insert indent-string))
	(setq line (+ line 1))
	(forward-line 1)))))

(defun my-mouse-split-window-vertically (event)
  "Split window vertically the way God intended."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (split-window-vertically)
  (other-window 1))

(defun my-toggle-truncate-lines ()
  "Toggles truncate-lines between t and nil for current buffer."
  (interactive)
  ;; I don't use toggle-truncate-lines because it puts annoying messages in the
  ;; minibuffer.
  (my-with-advice ((message ignore))
    (toggle-truncate-lines)))

(defun my-kill-to-bottom ()
  "Kills all text between point and bottom of buffer."
  (interactive)
  (kill-region (point) (point-max))
  (recenter))

(defvar my-show-all-buffers nil
  "Local in Buffer Menu buffers.  Non-nil if my-list-buffers should omit unimportant
buffers from the list, otherwise all buffers are shown.")

(defun my-list-buffers (&optional show-all-buffers)
  "Display a buffer list in the present window."
  (interactive "P")
  (let ((temp-buffer-show-function nil)) ;; Why this let?
    (switch-to-buffer "*Buffer List*" 'norecord)
    (list-buffers)
    (goto-char (point-min))
    (set (make-local-variable 'my-show-all-buffers) (not (null show-all-buffers)))
    (local-set-key "\C-x\C-b" 'my-list-buffers)))

(defun my-newline-with-indent ()
  "Inserts a newline then indents using indent-relative-maybe."
  (interactive)
  ;;(open-line 1)
  ;;(forward-line 1)
  (insert "\n")
  (indent-relative-maybe))

(defun my-newline-basic ()
  "Inserts a newline without triggering any other activity, such as line
filling, etc."
  (interactive)
  (open-line 1)
  (forward-line 1))

(defun my-kill-buffer-and-window ()
  "Kills the current buffer and deletes the current window."
  (interactive)
  (kill-buffer nil)
  (delete-window))

(defun my-kill-buffer-window-and-frame ()
  "Kills the current buffer; deletes the current window and frame."
  (interactive)
  (kill-buffer nil)
  (delete-frame))

(defun my-squeeze-blank-lines ()
  "Squeezes all multiple blank lines into one in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\C-j\C-j\C-j" nil t)
      (forward-line -1)
      (delete-blank-lines))))

(defun my-debug-on-entry (function)
  "Cause the Lisp debugger to be invoked whenever FUNCTION is called."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if fn
				    (format "Debug function (default %s): " fn)
				  "Debug function: ")
				obarray 'fboundp t))
     (list (if (equal val "")
	       fn (intern val)))))
  (debug-on-entry function)
  (message "Debugger will be entered at call to %s" (prin1-to-string function)))

(defun my-replace-function (function replacement &optional orig-function-holder)
  "Replace the function value of FUNCTION with the form REPLACEMENT (which can
be either a symbol with a valid function binding or a lambda expression).  If
optional third argument ORIG-FUNCTION-HOLDER is present, it must be a symbol,
and if it is not bound as a function, it's function value becomes the original
function value of FUNCTION.

The original function value of FUNCTION is returned if the replacement
actually occurred, otherwise nil is returned.

It is safe to call this function with identical arguments multiple times --
only one replacement ever occurs (until the FUNCTION's original function value
is restored -- then replacement will happen again))."

  (if (not (symbolp function))
      (error "my-replace-function: Argument #1 is not a symbol!"))
  (if (and (not (symbolp replacement))
	   (not (eq 'lambda (car-safe replacement))))
      (error "my-replace-function: Argument #2 is not a symbol or lambda expression!"))
  (if (and orig-function-holder
	   (not (symbolp orig-function-holder)))
      (error "my-replace-function: Argument #3 is not a symbol!"))
  (if (not (fboundp function))
      (error "my-replace-function: Symbol %s not bound as a function!"
	     (prin1-to-string function)))
  (if (eq (symbol-function function) replacement)
      ;; FUNCTION has already been replaced by REPLACEMENT.
      ;;
      nil
    (let ((fval (symbol-function function)))
      (if (eq 'autoload (car-safe fval))
	  (progn
	    (if (not (stringp (car (cdr-safe fval))))
		(error "my-replace-function: Argument 1 is not a valid autoload form!"))
	    (load (car (cdr fval)))))
      (if (not (fboundp orig-function-holder))
	  (fset orig-function-holder fval)
	(setq fval (symbol-function orig-function-holder)))
      (fset function replacement)
      fval)))

(defun my-Man-mode-quit ()
  "Quit from Man-mode buffer/frame, deleting the frame if necessary."
  (interactive)
  (if (not (display-graphic-p))
      (bury-buffer)
    (bury-buffer (current-buffer))
    (delete-frame)))

(defun my-shell-command-on-buffer (command)
  "Runs COMMAND giving it the contents of the current buffer on stdin.
The command's output is available afterwards just as with
shell-command-on-region (i.e., in buffer *Shell Command Output*)."
  (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) command nil))

(defun my-byte-compile-current-file ()
  "Byte-compile current file."
  (interactive)
  (byte-compile-file (buffer-file-name))
  (if my-win32
      (my-fixperm (if (string= ".gnus" (file-name-nondirectory (buffer-file-name)))
		      ".gnus.elc"
		    (concat (buffer-file-name) "c"))))

  (with-current-buffer "*Compile-Log*"
    (widen)
    (my-toggle-font-lock-mode 1)
    (goto-char (point-max))
    (narrow-to-page)
    (if (> (count-lines (point-min) (point-max)) 1)
	(pop-to-buffer "*Compile-Log*")
      (widen))))

(defun my-load-current-elisp-file ()
  "Loads the .elc file that corresponds to the current .el file.  If there's no .elc file,
simply performs eval-current-buffer."
  (interactive)
  (if (buffer-modified-p)
      (error "Please save this buffer first!"))

  (let* ((el-filename (buffer-file-name))
	 (my-load-current-elisp-file-running t)
	 elc-filename)

    (if (null el-filename)
	(error "Current buffer has no associated filename!"))

    (if (and (not (string-match-p "/\\.gnus$"  el-filename))
	     (not (string-match-p "\\.el$" el-filename)))
	(error "This file name does not have a .el suffix!"))

    (setq elc-filename
	  (if (string-match-p "/\\.gnus$" el-filename)
	      (concat el-filename ".elc")
	    (concat el-filename "c")))

    (if (file-exists-p elc-filename)
	(progn
	  (if (file-newer-than-file-p el-filename elc-filename)
	      (error "The byte-compiled file is out of date!"))
	  (load elc-filename))
      (ding)
      (message "WARNING: There is no byte-compiled version of this file!")
      (sleep-for 2)
      (load el-filename))))

(defun my-command (shellcmd &optional inputstr no-stderr)
  "Execute SHELLCMD with INPUTSTR as standard input.  If INPUTSTR is nil, it is
taken to be the empty string.  Returns the command's standard output and
standard error as a string.	 If NO-STDERR is non-nil, returns only the command's
standard output."
  (save-excursion
    (let ((stderr-buffer (get-buffer-create " *my-command stderr*")))
      (if (null (get-buffer " *my-command stderr*"))
	  (error "my-command: failed to create temporary buffer for stderr!"))

      (with-current-buffer stderr-buffer
	(erase-buffer))

      (set-buffer (get-buffer-create " *my-command output*"))
      (if (not (string= (buffer-name) " *my-command output*"))
	  (error "my-command: cannot create temporary buffer!"))

      (erase-buffer)
      (insert (or inputstr ""))

      ;; Silence call to push-mark on line 1443 of simple.el.
      (my-with-advice ((push-mark ignore))
	(condition-case nil
	    (shell-command-on-region (point-min)
				     (point-max)
				     shellcmd
				     'insert	;; Insert output in this buffer.
				     'replace	;; Replace region with output.
				     stderr-buffer)
	  (error "")))

      (my-remove-crs)
      (prog1
	  (if no-stderr
	      (buffer-string)
	    (concat (with-current-buffer stderr-buffer (buffer-string))
		    (buffer-string)))
	(kill-buffer (current-buffer))))))

(defun my-occur ()
  "To be used in place of occur.  Prevents temporary buffer from appearing in
a different frame."
  (interactive)
  (let ((temp-buffer-show-function nil))
    (call-interactively 'occur)))

(defun my-occur-mode-goto-occurrence ()
  "Like occur-mode-goto-occurrence, but leaves cursor in *Occur* window."
  (interactive)
  (occur-mode-goto-occurrence)
  (recenter (/ (window-height) 2))
  (switch-to-buffer-other-window "*Occur*"))

(defun my-clean-log ()
  "Cleans cruft characters out of a typescript or Xterm log file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "[7m" nil t)
      (replace-match "" t))

    (goto-char (point-min))
    (while (search-forward "[m" nil t)
      (replace-match "" t))))

;; BUG: This function needs to handle the case where a symbol's function cell
;; contains another symbol.

(defun my-encapsulate (fsym pre-form post-form)
  "Takes three arguments: FSYM, PRE-FORM, POST-FORM.

Re-writes the function value of FSYM (the lambda expression bound to
FSYM's function cell) so that the function first evaluates PRE-FORM,
then performs its existing work, then evaluates POST-FORM.	Will
autoload FSYM if necessary.	 Operates on a copy of FSYM's function
value, so even read-only functions can be encapsulated.	 Preserves
FSYM's argument list, documentation string, interactive specification,
and return value, so this cannot be used to alter FSYM's functional
interface, only to add side-effects.

The previous function value is appended to a list which is put on
FSYM's property list under the property symbol encapsulate-prev-function."
  (interactive
   "aFunction name: \nxLisp form to prepend: \nxLisp form to append: ")
  (if (not (symbolp fsym))
      (error "Argument 1: must be a symbol."))
  (if (not (fboundp fsym))
      (error "Argument 1: value as function is void."))
  (let* ((fval			(symbol-function fsym))
	 (fval-orig		(if (not (listp fval))
				    (error "Argument 1: value as function is not a list.")
				  (copy-sequence fval))))
    (if (subrp fval)
	(error "Argument 1: is a builtin: can only encapsulate Lisp functions."))
    (if (eq 'autoload (car fval))
	(progn
	  (if (not (stringp (car (cdr fval))))
	      (error "Argument 1: not a valid autoload form."))
	  (load (car (cdr fval)))
	  (setq fval (symbol-function fsym))))
    (if (not (eq 'lambda (car fval)))
	(error "Argument 1: is not a function."))
    (setq fval (copy-sequence fval))
    (let* ((arglist		(nth 1 fval))
	   (docstring	(let ((element (nth 2 fval)))
			  (or (stringp element) (integerp element))))
	   (body		(nthcdr (if docstring 3 2) fval)))
      (if (not (listp arglist))
	  (error "Argument 1: function has no argument list."))
      (let ((body-len	(length body)))
	(if (zerop body-len)
	    (error "Argument 1: function has no forms in body."))
	(let ((last-form-in-body		(car (nthcdr (1- body-len) body))))
	  (setcar (nthcdr (1- body-len) body)
		  (nconc '(prog1) (list last-form-in-body) (list post-form))))
	(setcdr (nthcdr (if docstring 2 1) fval)
		(nconc (list pre-form) body))
	(fset fsym fval))
      (let ((encap-func-values	(get fsym 'encapsulate-prev-functions)))
	(progn
	  (if (not (listp encap-func-values))
	      (error "Internal error: encapsulate-functions property is not a list."))
	  (put fsym
	       'encapsulate-prev-functions
	       (nconc encap-func-values (list fval-orig)))))
      fsym)))
