;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my-sc.el
;;
;; Support for source control systems.  Currently supports ClearCase and
;; SourceSafe.  Emacs has native support for CVS, but this file sets some
;; variables that affect CVS.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'vc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq vc-diff-switches		"-uw"
      vc-make-backup-files	t)

(defvar my-sc-perforce-available
  (file-exists-p (concat my-systemdrive "/program files/perforce/p4.exe"))
  "...")

(defvar my-sc-clearcase-available
  (file-exists-p (concat my-systemdrive "/" my-progfilesx86 "/rational/clearcase/bin/cleartool.exe"))
  "...")

(defvar my-sc-sourcesafe-available
  (file-exists-p (concat my-systemdrive "/" my-progfilesx86 "/microsoft visual studio/vss/win32/ss.exe"))
  "...")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-sc-three-way-merge ()
  "Performs a 3-way merge between the current file (FILE) and FILE.franl and
FILE.anc (the common ancestor)."
  (let* ((filename (buffer-file-name))
	 (ancestorname (concat filename ".anc"))
	 (franlname (concat filename ".franl")))
    (if (not (and (file-exists-p filename)
		  (file-exists-p ancestorname)
		  (file-exists-p franlname)))
	(error "One or more of the three required files is missing!"))
    (shell-command (format "diff3 -m '%s' '%s' '%s'" franlname ancestorname filename))))

(defun my-sc-make-file-editable ()
  "Makes the current file and buffer writable.  This is used to edit read-only
source files without checking them out."
  (interactive)
  (if (not buffer-read-only)
      (error "This buffer is already writable!"))
  (read-only-mode 0)
  (let* ((filename (buffer-file-name))
	 (ancestorname (concat filename ".anc"))
	 (filemodes (file-modes filename)))
    (if (null filename)
	(error "This buffer has no associated file."))
    (if (not my-win32)
	(if (logand filemodes #o200)
	    (error "The disk file is already writable by you!")))
    (if (file-exists-p ancestorname)
	(error "The ancestor file (%s) already exists!" (file-name-nondirectory ancestorname)))

    (copy-file filename ancestorname)
    (set-file-modes filename (logior #o200 (file-modes filename)))

    (if (not my-win32)
	(if (= 0 (logand #o200 (file-modes filename)))
	    (error "Failed to make disk file writable!")))
    (message "Disk file is now writable.  Created ancestor file %s."
	     (file-name-nondirectory ancestorname))))

(defun my-sc-sanity ()
  ;; Check if there is more than one SC application isntalled on this machine!
  (if (> (length (delete nil
			 (mapcar (lambda (x) (not (null x)))
				 (list my-sc-clearcase-available
				       my-sc-sourcesafe-available
				       my-sc-perforce-available))))
	 1)
      (error "Multiple source control systems exist on this machine!")))


;; TODO: Replace these cascading if expressions with code that builds a function name
;; dynamically using intern-soft and concatenating strings to form the function name.

(defun my-sc-diff ()
  "..."
  (interactive)
  (my-sc-sanity)
  (if my-sc-perforce-available
      (my-sc-perforce-diff)
    (if my-sc-clearcase-available
	(my-sc-clearcase-diff)
      (if my-sc-sourcesafe-available
	  (my-sc-sourcesafe-diff)
	(error "No platform support for this command!")))))

(defun my-sc-update ()
  "..."
  (interactive)
  (my-sc-sanity)

  (if (buffer-modified-p)
      (error "Current buffer is modified!"))

  (if my-sc-clearcase-available
      (my-sc-clearcase-update)
    (if my-sc-sourcesafe-available
	(my-sc-sourcesafe-update)
      (error "No platform support for this command!"))))

(defun my-sc-checkout ()
  "..."
  (interactive)
  (my-sc-sanity)

  (if (buffer-modified-p)
      (error "Current buffer is modified!"))

  (if my-sc-perforce-available
      (my-sc-perforce-checkout)
    (if my-sc-clearcase-available
	(my-sc-clearcase-checkout)
      (if my-sc-sourcesafe-available
	  (my-sc-sourcesafe-checkout)
	(error "No platform support for this command!")))))

(defun my-sc-uncheckout (nokeep)
  "..."
  (interactive "P")
  (my-sc-sanity)

  (if (buffer-modified-p)
      (error "Current buffer is modified!"))

  (if my-sc-perforce-available
      (my-sc-perforce-uncheckout)
    (if my-sc-clearcase-available
	(my-sc-clearcase-uncheckout nokeep)
      (if my-sc-sourcesafe-available
	  (my-sc-sourcesafe-uncheckout nokeep)
	(error "No platform support for this command!")))))

(defun my-sc-checkin ()
  "..."
  (interactive)
  (my-sc-sanity)

  (if (buffer-modified-p)
      (error "Current buffer is modified!"))

  (if my-sc-clearcase-available
      (my-sc-clearcase-checkin)
    (if my-sc-sourcesafe-available
	(my-sc-sourcesafe-checkin)
      (error "No platform support for this command!"))))

(defun my-sc-history ()
  "..."
  (interactive)
  (my-sc-sanity)
  (if my-sc-clearcase-available
      (my-sc-clearcase-history)
    (if my-sc-sourcesafe-available
	(my-sc-sourcesafe-history)
      (error "No platform support for this command!"))))

(defun my-sc-status ()
  "..."
  (interactive)
  (my-sc-sanity)
  (if my-sc-clearcase-available
      (my-sc-clearcase-status)
    (if my-sc-sourcesafe-available
	(my-sc-sourcesafe-status)
      (error "No platform support for this command!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perforce support.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-sc-perforce-checkout ()
  "..."
  (message "Checking out current file ...")
  (let ((orig-buffer (current-buffer))
	(status (shell-command (format "p4co '%s'" (file-name-nondirectory (buffer-file-name))))))
    (if (= 0 status)
	(with-current-buffer orig-buffer
	  (revert-buffer nil 'noconfirm)))))

(defun my-sc-perforce-uncheckout ()
  "..."
  (if (not (yes-or-no-p "This can cause data loss!  Really un-checkout this file? "))
      (error "Aborted!")

    (message "Reverting current file ...")
    (let ((orig-buffer (current-buffer))
	  (status (shell-command (format "p4unco '%s'" (file-name-nondirectory (buffer-file-name))))))
      (if (= 0 status)
	  (with-current-buffer orig-buffer
	    (revert-buffer nil 'noconfirm))))))
  
(defun my-sc-perforce-diff ()
  "Runs a Perforce 'diff' command on the current file.  Produces a unified diff."
  (shell-command (format "p4 diff -du '%s'" (file-name-nondirectory (buffer-file-name))))
  (switch-to-buffer "*Shell Command Output*")
  (diff-mode)
  (goto-char (point-min)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ClearCase support.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-foreground (make-face 'my-sc-diff-header-face) "#0ff")

(defun my-sc-clearcase-diff ()
  "..."
  (message "Diffing current file with predecessor ...")
  (let ((file (buffer-file-name)))
    (if (null file)
	(error "There is no file associated with this buffer!"))
    (shell-command (format "cleartool diff -serial -pred -options '-blank_ignore' '%s'"
			   (file-name-nondirectory file))))

  (with-current-buffer "*Shell Command Output*"
    (ignore-errors (kill-buffer "*ClearCase Diff*"))
    (rename-buffer "*ClearCase Diff*")

    (goto-char (point-min))
    (while (search-forward-regexp "^\\(-----\\[.*\\]-----\\|---\\)$" nil t)
      (let ((eol (point)))
	(beginning-of-line)
	(add-text-properties (point) eol '(face my-sc-diff-header-face)))
      (end-of-line))
    (goto-char (point-min))
    
    (local-set-key [f1] (lambda () (interactive)
			  (when (search-forward-regexp "^-----\\[")
			    (if (or (looking-at ".*now at \\([0-9]+\\)")
				    (looking-at ".*after \\([0-9]+\\)"))
				(let ((linenumber (string-to-number (match-string 1))))
				  (recenter '(4))
				  (other-window 1)
				  (goto-char (point-min))
				  (forward-line (1- linenumber))
				  (other-window 1))))))
    (local-set-key "n" (lookup-key (current-local-map) [f1]))))

(defun my-sc-clearcase-update ()
  "..."
  (error "Not implemented yet!"))

(defun my-sc-clearcase-checkin ()
  "..."
  (error "Not implemented yet!"))

(defun my-sc-clearcase-history ()
  "..."
  (if (/= 0 (shell-command (format "cleartool lshistory '%s' 2>/dev/null" (buffer-file-name))))
      (error "This file does not appear to be under source control!")
    (with-current-buffer "*Shell Command Output*"
      (my-remove-crs)
      (goto-char (point-min))
      (let ((start (point))
	    (face1 '(:foreground "lightblue"))
	    (face2 '(:foreground "orange"))
	    (count 0))
	(while (search-forward-regexp "^[0-9]" nil t)
	  (setq count (1+ count))
	  (when (> (- (point) start) 0)
	    (backward-char 1)
	    (set-text-properties start (point)
				 `(face ,(if (cl-oddp count) face1 face2)))
	    (setq start (point)))
	  (end-of-line))
	(set-text-properties start (point-max)
			     `(face ,(if (cl-evenp count) face1 face2))))
      (toggle-truncate-lines 1)
      (goto-char (point-min)))))

(defun my-sc-clearcase-checkout ()
  "..."
  (message "Checking out current file ...")
  (let ((orig-buffer (current-buffer))
	(status (shell-command (format "cleartool co -reserved -nc '%s'"
				       (file-name-nondirectory (buffer-file-name))))))
    (if (= 0 status)
	(with-current-buffer orig-buffer
	  (revert-buffer nil 'noconfirm)))))
		   
(defun my-sc-clearcase-status ()
  "..."
  (error "Not implemented yet!"))

(defun my-sc-clearcase-uncheckout (nokeep)
  "..."
  (message "Unchecking out current file ...")
  (shell-command (format "cleartool unco %s '%s'"
			 (if nokeep "-rm" "-keep")
			 (file-name-nondirectory (buffer-file-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SourceSafe support.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'source-safe)

;; WARNINGS:
;;
;; 1. Don't put trailing '/'s on these pathnames or source-safe.el will
;;    malfunction.
;;
;; 2. If you change ss-project-dirs without quitting from Emacs, set
;;    ss-project-dirs-cache to nil for the change to be seen.

(setq ss-project-dirs		'(((concat "^" my-systemdrive "/franl/work/arcstream/src") . "$/Thomson/"))
      ss-program		(concat my-systemdrive "/program files/microsoft visual studio/vss/win32/ss.exe")
      ss-tmp-dir		temporary-file-directory
      ;; ss-update-new-frame	'((name . "Source Safe Check-in Comment"))
      ss-diff-program		'(DIFF "-u -w"))

(defun my-sc-sourcesafe-sanity ()
  "Performs some SourceSafe sanity checks.  Signals an error if any fail."
  (if (not (file-executable-p ss-program))
      (error "The file specified by ss-program does not exist or is not executable."))
  (if (not (file-directory-p ss-tmp-dir))
      (error "The directory specified by ss-tmp-dir does not exist.")))

(defun my-sc-sourcesafe-update ()
  "Runs the command 'ss get \*' in the current directory.  Confirms the action
with the user first."
  (my-sc-sourcesafe-sanity)
  (if (yes-or-no-p (format "Get latest for \"%s\"? " default-directory))
      (shell-command "ss get \\* &")))

(defun my-sc-sourcesafe-diff ()
  "Invokes ss-diff so that it runs without popping up new frames and
highlighting text int the buffer.."
  (my-sc-sourcesafe-sanity)
  (if (buffer-modified-p)
      (error "Buffer must be saved to disk first!"))

  (let ((buffers (buffer-list)))
    (while buffers
      (if (string-match-p "^\\*DIFF " (buffer-name (car buffers)))
	  (kill-buffer (car buffers)))
      (setq buffers (cdr buffers))))

  (ss-diff nil t)
  (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer)
  (run-at-time 2 nil (lambda () (diff-mode) (diff-hunk-next)))
  (bury-buffer (current-buffer)))

(defun my-sc-sourcesafe-history ()
  "Same as ss-history, except the original window remains selected and the *SS
History* buffer is buried."
  (my-sc-sourcesafe-sanity)
  (ss-history)
  (bury-buffer (current-buffer))
  (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer))

(defun my-sc-sourcesafe-status ()
  "Calls ss-status then buries buffer *SS Status*."
  (my-sc-sourcesafe-sanity)
  (ss-status)
  (bury-buffer (current-buffer))
  (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer))

(defun my-sc-sourcesafe-checkout ()
  "..."
  (my-sc-sourcesafe-sanity)
  (error "Not implemented yet."))

(defun my-sc-sourcesafe-uncheckout (nokeep)
  "..."
  (my-sc-sourcesafe-sanity)
  (error "Not implemented yet."))

(defun my-sc-sourcesafe-checkin ()
  "Calls ss-checkin then positions point at top of comment buffer."
  (my-sc-sourcesafe-sanity)
  (if (buffer-modified-p)
      (error "Buffer must be saved to disk first!"))
  (ss-checkin)
  (goto-char (point-min)))

;; Hooks.

(defun my-sc-sourcesafe-after-update-hook ()
  "Added to ss-after-update-hooks."
  (bury-buffer "*SourceSafe update comment*")
  (bury-buffer "*SourceSafe log*"))

(add-hook 'ss-after-update-hooks 'my-sc-sourcesafe-after-update-hook)

(defun my-sc-sourcesafe-before-update-hook ()
  "Added to ss-before-update-hooks."
  (define-key (current-local-map) "\M-\C-y" 'my-sc-sourcesafe-update-insert-changelog))

(add-hook 'ss-before-update-hooks 'my-sc-sourcesafe-before-update-hook)

(defun my-sc-sourcesafe-update-insert-changelog ()
  "Inserts the most recently killed or yanked text into the SourceSafe Update
Comment buffer, replacing whatever text is there.  Also removes C++ comments
from left edge of text lines."
  (interactive)
  (delete-region (point-min) (point-max))
  (yank)
  (goto-char (point-min))
  (while (re-search-forward "^\\(//\\|#\\)\\s-*" nil t)
    (replace-match "" nil nil))
  (goto-char (point-min)))
