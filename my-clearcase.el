;; my-clearcase.el
;; ClearCase interface code.

(define-key global-map [M-f1]	'my-cc-checkout-file)
(define-key global-map [M-f2]	'my-cc-show-diffs)
(define-key global-map [M-f3]	'my-cc-checkin-file)
(define-key global-map [M-f4]	'my-cc-edit-configspec)

(defun my-cc-set-configspec ()
  "Sets the ClearCase configspec for the view containing the current file to be
the contents of the current file.  This function is used by
my-cc-edit-configspec."
  (interactive)
  (if (buffer-modified-p)
      (error "Please save the buffer first!"))
  (shell-command (concat "cleartool setcs " (buffer-file-name)))
  (let ((my-output-buffer (get-buffer "*Shell Command Output*")))
    (if my-output-buffer
	(pop-to-buffer my-output-buffer)
      (message "Configspec set for current view."))))

(defun my-cc-edit-configspec ()
  "Edits the ClearCase configspec for the view containing the current file."
  (interactive)
  (let ((my-tmpfile (concat my-systemdrive "/temp/configspec.tmp")))
    (shell-command (concat "cleartool catcs > " my-tmpfile))
    (find-file-other-frame my-tmpfile)
    (local-set-key "\C-c\C-c" 'my-cc-set-configspec)))

(defun my-cc-checkout-file (unreserved)
  "Checks out the file shown in the current buffer.  Non-zero prefix arg
means do an unreserved checkout."
  (interactive "P")
  (if (buffer-modified-p)
      (error "The buffer is currently modified!"))
  (if (not buffer-read-only)
      (error "The buffer is writable!  Perhaps this file is already checked out?"))
  (let ((my-bufname (buffer-name))
	(restype (if unreserved "unreserved" "reserved"))
	(resopt (if unreserved "-unres" "-res")))
    (if (not (y-or-n-p (concat "Check out " my-bufname " (" restype ")? ")))
	(message "No checkout performed.")
      (shell-command (concat "ct co " resopt " -nc " my-bufname))
      (revert-buffer nil 'noconfirm))))

(defvar my-cc-checkin-history nil
  "History variable used when reading check-in comments with
my-cc-checkin-file.")

(defun my-cc-checkin-file ()
  "Checks in the file shown in the current buffer."
  (interactive)
  (if (buffer-modified-p)
      (error "Please save the buffer first!"))
  (let ((my-bufname (buffer-name))
	(comment ""))
    (if (not (y-or-n-p (concat "Check in " my-bufname "? ")))
	(message "No checkin performed.")
      (setq comment
	    (read-string "Check-in comment: " nil 'my-cc-checkin-history))
      (if (string-match-p "\"" comment)
	  (error "Check-in comment contains double-quote characters!"))
      (message (concat "Checking in " my-bufname " ..."))
      (shell-command (concat "ct ci -rm -c \"" comment "\" " my-bufname))
      (if (y-or-n-p (concat "Revert buffer? "))
	  (revert-buffer nil 'noconfirm nil)))))

(defun my-cc-show-diffs (context-lines)
  "Shows differences between the latest checked-in version of a file and
the checked-out version."
  (interactive "p")
  (if (buffer-modified-p)
      (progn
	(ding)
	(message "Please save the buffer first!"))
    (let* ((bufname (buffer-name))
	   (lines (if (<= context-lines 1) 2 context-lines))
	   (command (concat "diff -w -c" (int-to-string lines) " " bufname
			    "@@/main/LATEST " bufname)))
      (shell-command (concat command " | tr -d '\015'"))
      (if (get-buffer "*Shell Command Output*")
	  (progn
	    (switch-to-buffer-other-window "*Shell Command Output*")
	    (my-n-column-tabs 4)
	    ;; Use with-no-warnings to suppress byte compiler warning that mark-whole-buffer is for
	    ;; interactive use only.
	    (with-no-warnings (mark-whole-buffer))
	    (my-indent-region-with-string "  ")
	    (message command))
	(message "Diff produced no output.")))))
