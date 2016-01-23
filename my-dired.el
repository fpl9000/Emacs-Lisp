;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My dired-mode and vc-dired-mode customizations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)
(require 'dired-aux)
(require 'ls-lisp)
(require 'vc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ;; On Solaris "-o" is equivalent to "-G" on Linux.
      dired-listing-switches		(if my-solaris
					    "-alo"
					  "-alG")
      dired-ls-F-marks-symlinks		t
      dired-use-ls-dired		'unspecified)

;; Silence byte-compiler warnings about these variables not being defined
;; on non-Windows systems.
(defvar ls-lisp-use-insert-directory-program)
(defvar ls-lisp-ignore-case)
(defvar ls-lisp-verbosity)

;; On Windows, Emacs pre-loads ls-lisp.el, which prevents dired from using /bin/ls to generate a
;; dired buffer.  This would be OK if ls-lisp.el could obtain the Cygwin permissions, but it can't,
;; so these next settings force ls-lisp.el to revert to running /bin/ls.  Unfortunately, this makes
;; Dired mode depend on Cygwin's /bin/ls to function correctly.

(when my-win32

  (defvar my-dired-bin-ls-exists (file-exists-p "c:/apps/cygwin/bin/ls.exe")
    "Non-nil if file c:/apps/cygwin/bin/ls.exe exists, otherwise nil.")

  (if my-dired-bin-ls-exists
      ;; Use /bin/ls if it exists.  This is especially important on Windows, where ls-lisp
      ;; is not capable of showing Cygwin permissions correctly, so every file appears to have
      ;; either mode 644 or 755.
      (setq ls-lisp-use-insert-directory-program t)

    ;; Make ls-lisp use /bin/ls-style date/time formats.
    (setq ls-lisp-format-time-list '("%b %e %H:%M" "%b %e  %Y")
	  ls-lisp-use-localized-time-format t))

  (setq ls-lisp-ignore-case t  ;; Make dired sort case-insensitively.
	ls-lisp-verbosity '(links uid)))  ;; Make dired show hard link count and file owner.

(defvar my-dired-date-regexp " [JFMASOND][aepuco][nbrynlgptvc]\\s-+[0-9]+\\s-+[0-9][0-9]:?[0-9][0-9] "
  "A regular expression that matches the date field in a Dired buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (facep 'dired-warning)
    (set-face-foreground 'dired-warning "#fc8"))

(when (facep 'dired-flagged)
  (set-face-foreground 'dired-flagged "red")
  (set-face-bold 'dired-flagged nil))

(if (facep 'dired-directory-face)
    (copy-face 'default dired-directory-face))

(if (facep 'dired-symlink-face)
    (copy-face 'default dired-symlink-face))

(when (facep 'dired-marked)
  (set-face-foreground 'dired-marked "yellow")
  (set-face-bold 'dired-marked nil))

(make-face 'my-dired-summary-face)
(set-face-foreground 'my-dired-summary-face "pink")

(make-face 'my-dired-date-face)
(set-face-foreground 'my-dired-date-face "lightgreen")	;; f86

(make-face 'my-dired-size-face)
(set-face-foreground 'my-dired-size-face "orange")

(make-face 'my-dired-directory-face)
(set-face-foreground 'my-dired-directory-face "#0ff")

(make-face 'my-dired-backup-face)
(set-face-foreground 'my-dired-backup-face "#aaa")

(make-face 'my-dired-code-face)
(set-face-foreground 'my-dired-code-face "orange")

(make-face 'my-dired-symlink-face)
(set-face-foreground 'my-dired-symlink-face "#f0c")

(make-face 'my-dired-elc-face)
(set-face-foreground 'my-dired-elc-face "darkgrey")

(make-face 'my-dired-executable-face)
(set-face-foreground 'my-dired-executable-face "yellow")

(make-face 'my-dired-media-face)
(set-face-foreground 'my-dired-media-face "#e6a")

(make-face 'my-dired-archive-face)
(set-face-foreground 'my-dired-archive-face "#f8f")

(make-face 'my-dired-class-face)
(set-face-foreground 'my-dired-class-face "#f9f")

(make-face 'my-dired-torrent-face)
(set-face-foreground 'my-dired-torrent-face "#faa")

(make-face 'my-dired-document-face)
(set-face-foreground 'my-dired-document-face "#fc8")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-dired-mode-hook ()
  "Added to dired-mode-hook.  Sets truncate-lines to t, then modifies
dired-mode-map.  This hook also runs when vc-dired-mode starts."
  (setq truncate-lines t)

  ;; Earlier entries in font-lock-keywords prevent later entries from being
  ;; applied to the same text, unless the later entry has OVERRIDE set to t.

  (font-lock-add-keywords
   nil
   `(
     ("^\\s-+total used.*$"
      0 'my-dired-summary-face nil)

     (,(concat "^..\\(d\\S-+\\).*" my-dired-date-regexp "\\(.*\\)$")
      (1 'my-dired-directory-face nil)
      (2 'my-dired-directory-face nil))

     (,(concat "^..-.*" my-dired-date-regexp "\\(.*\\.elc\\)$")
      1 'my-dired-elc-face nil)

     (,(concat "^..-.*" my-dired-date-regexp
	       "\\(.*\\.\\(pdf\\|vsd\\|ppt\\|txt\\|html\\|doc\\|docx\\|xls\\|dot\\|dotx\\|hlp\\|chm\\)\\)$")
      1 'my-dired-document-face t)

     (,(concat "^..-.*" my-dired-date-regexp 
	       "\\(.*\\.\\(mp3\\|ogg\\|flac\\|wma\\|wav\\|avi\\|mp4\\|m4v\\|mpg\\|wmv\\|asf\\|mov\\|mkv\\|flv\\)\\)$")
      1 'my-dired-media-face nil)

     (,(concat "^..-.*" my-dired-date-regexp "\\(.*\\.\\(exe\\|dll\\|so\\|class\\)\\)$")
      1 'my-dired-executable-face nil)

     (,(concat "^..-.*" my-dired-date-regexp "\\(.*\\(~\\|\\.wbk\\)\\)$")
      1 'my-dired-backup-face nil)

     (,(concat "^..-.*" my-dired-date-regexp "\\(.*\\.tar\\(\\.gz\\|\\.bz2\\|\\.xz\\)?\\)$")
      1 'my-dired-archive-face nil)

     (,(concat "^..-.*" my-dired-date-regexp "\\(.*\\.\\(zip\\|iso\\|dss\\|rar\\|deb\\|rpm\\)\\)$")
      1 'my-dired-archive-face nil)

     (,(concat "^..-.*" my-dired-date-regexp "\\(.*\\.class\\)$")
      1 'my-dired-class-face nil)

     (,(concat "^..-.*" my-dired-date-regexp "\\(.*\\.torrent\\)$")
      1 'my-dired-torrent-face nil)

     (,(concat "^..\\(-\\S-+\\).*" my-dired-date-regexp "\\(.*\\.lnk\\)$")
      (1 'my-dired-symlink-face nil)
      (2 'my-dired-symlink-face nil))

     (,(concat "^..-.*" my-dired-date-regexp "\\(.*\\.\\(cpp\\|c\\|h\\(pp\\)?\\|cs\\|java\\|mof\\)\\)$")
      1 'my-dired-code-face nil)

     (,(concat "^..\\(l\\S-+\\).*" my-dired-date-regexp "\\(.*\\)$")
      (1 'my-dired-symlink-face nil)
      (2 'my-dired-symlink-face nil))

     ("^..\\S-+\\s-+\\S-+\\s-+\\S-+\\s-+\\([0-9]+\\)"
      1 'my-dired-size-face t)

     (,(concat "^.*\\(" my-dired-date-regexp "\\)")
      1 'my-dired-date-face t)

     (,(concat "^..-..x.*" my-dired-date-regexp "\\(.*\\)$")
      1 'my-dired-executable-face t)

     (,(concat "^\\(D\\).*" my-dired-date-regexp "\\(.*\\)$")
      (1 'dired-flagged t)
      (2 'dired-flagged t))

     (,(concat "^\\(\\*\\).*" my-dired-date-regexp "\\(.*\\)$")
      (1 'dired-marked t)
      (2 'dired-marked t))

     ("^.* (modified) .*$"
      0 'font-lock-warning-face t)
     )
   'set) ;; Replace all Dired mode entries with mine.

  ;;(turn-on-font-lock)
  (my-toggle-font-lock-mode 1)

  ;; Thus must be done _after_ we turn on Font Lock mode!
  (setq font-lock-keywords-case-fold-search t)

  ;; ...
  (if (facep 'dired-ignored)
      (copy-face 'default 'dired-ignored))

  (when (display-graphic-p)
    (local-set-key [backspace] 'dired-unmark-backward)
    (local-set-key [mouse-2] 'my-dired-mouse-find-file))
  (local-set-key (kbd "/") (lambda ()
			     (interactive)
			     (kill-new default-directory)
			     (message "Copied current directory path to clipboard.")))
  (local-set-key (kbd "C-h") 'dired-unmark-backward)
  (local-set-key (kbd "<M-backspace>") 'dired-unmark-all-marks)
  (local-set-key (kbd "M-=") 'count-words-region)
  (local-set-key (kbd "M-s") 'my-scroll-down-one-line)
  (local-set-key (kbd "B") (lambda () (interactive)
			     (call-process (concat my-systemdrive "/apps/cygwin/bin/bash.exe") nil nil nil
					   "rxvt" "--nohome" "-geometry" "105x34+200+200")))
  (local-set-key (kbd "c") 'dired-do-copy)	;; Normally bound to 'C'.
  (local-set-key (kbd "C") 'my-dired-do-copy-with-suggestion)
  (local-set-key (kbd "E") (lambda () (interactive)
			     (call-process "explorer.exe" nil nil nil
					   (subst-char-in-string ?/ ?\\ (dired-current-directory)))))
  (local-set-key (kbd "f") 'my-dired-find-alternate-file)
  (local-set-key (kbd "h") 'my-dired-sha1sum)
  ;;(local-set-key (kbd "H") 'my-dired-hyphenate-filename)
  (local-set-key (kbd "L") 'my-dired-launch-file)
  (if my-win32
      (local-set-key (kbd "M") 'my-dired-chmod))
  (local-set-key (kbd "P") 'my-dired-fixperm)
  (local-set-key (kbd "Q") 'my-dired-winamp-enqueue)
  (local-set-key (kbd "r") 'dired-do-rename)	;; Normally bound to 'R'.
  (local-set-key (kbd "R") 'my-dired-do-rename-with-suggestion)
  (if my-win32
      (local-set-key (kbd "S") 'my-dired-make-desktop-shortcut))
  (local-set-key (kbd "t") 'my-dired-touch)
  (local-set-key (kbd "u") 'my-dired-go-up-and-kill-current)
  (local-set-key (kbd "v") 'my-dired-scan-for-viruses)
  (local-set-key (kbd "V") 'my-dired-toggle-vc-directory)
  (local-set-key (kbd "W") 'my-dired-winamp-play))

(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(defun my-dired-after-readin-hook ()
  "Added to dired-after-readin-hook."
  (if my-win32
      (let ((inhibit-read-only t)
	    (sort-fold-case t))

	;; On Windows, Cygwin's /bin/ls has no switch to sort case-insensitively, so I do it
	;; here.  Sort the Dired buffer case-insensitively (so items are ordeored like in Explorer).
	(if (or (not (eq this-command 'dired-sort-toggle-or-edit))
		(not (member ?t (string-to-list dired-actual-switches))))
	    (save-excursion
	      (goto-char (point-min))
	      (forward-line 2)
	      (sort-regexp-fields nil (concat "^.*" my-dired-date-regexp "\\(.*\\)$") "\\1" (point) (point-max))))

;;	;; Get rid of all '+' characters showing the presence of non-UNIX ACLs.
;;	(save-excursion
;;	  (goto-char (point-min))
;;	  (when (search-forward-regexp "^............\\+ " nil t)
;;	    (goto-char (point-min))
;;	    (while (search-forward-regexp "^............\\(.\\) " nil t)
;;	      (replace-match "" nil nil nil 1))))

	;; Clean up the Dired buffer if it's excessively wide due to the owner of my home directory
	;; under Cygwin.  NOTE: This assumes the length of user-login-name is always less than the
	;; string "TrustedInstaller   " (which is 19 characters).  Probably a safe bet.
	(save-excursion
	  (let ((changed nil))
	    (goto-char (point-min))

	    (if (looking-at-p (concat "  " my-systemdrive "/franl:"))
		(let* ((trustedinstaller "NT SERVICE+TrustedInstaller")
		       (trustedinstaller-short "TrustedInstaller   ")
		       (my-userid (concat user-login-name
					  (make-string (- (length trustedinstaller)
							  (length user-login-name)) 32)))
		       (my-userid-short (concat user-login-name
						(make-string (- (length trustedinstaller-short)
								(length user-login-name)) 32))))
						     
		  ;; First, shorten the items owned by NT SERVICE+TrustedInstaller.
		  (while (search-forward trustedinstaller nil t)
		    (setq changed t)
		    (replace-match trustedinstaller-short nil t))

		  ;; Next, if the above loop changed anything, shorten the whitespace after
		  ;; my userid.
		  (when changed
		    (goto-char (point-min))
		    (while (search-forward my-userid nil t)
		      (replace-match my-userid-short)))))))

	(set-buffer-modified-p nil))))

(add-hook 'dired-after-readin-hook 'my-dired-after-readin-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-dired-touch ()
  "Touches the current (or marked) files."
  (interactive)
  (let ((files (dired-get-marked-files 'no-dirs)))
    (if (not (y-or-n-p (format "Really touch %d file(s)? " (length files))))
	(error "No files touched."))
    (shell-command (concat "touch " (mapconcat 'identity files " ")))
    (dired-do-redisplay)
    (message "Touched %d files." (length files))))

(defun my-dired-sha1sum ()
  "Computes the SHA-1 hash of the marked files."
  (interactive)
  (let ((files (dired-get-marked-files 'no-dirs)))
    (message "Computing SHA1 hashes of %d file(s) ..." (length files))
    (shell-command (concat "sha1sum -b " (mapconcat 'shell-quote-argument files " ")))))

(defun my-dired-hyphenate-filename ()
  "..."
  (interactive)
  (let ((files (dired-get-marked-files 'no-dirs)))
    (dolist (file files)
      (let ((newname (copy-sequence file)))
	(subst-char-in-string 32 ?- newname 'inplace)
	(setq newname (downcase (replace-regexp-in-string "--+" "-" newname)))
	(setq newname (replace-regexp-in-string ",-" "-" newname))
	(rename-file file newname))))
  ;; This clears all the marks.
  (revert-buffer))

(defvar my-dired-chmod-input-history nil
  "...")

(defun my-dired-chmod (newperms)
  "Changes the permissions on the current (or marked) item(s)."
  (interactive (list (read-from-minibuffer "New file modes: " nil nil nil
					   'my-dired-chmod-input-history nil nil)))

  (if (not my-win32)
      (error "Function my-dired-chmod called on a non-Windows OS!"))

  (if (not (string-match-p "^[01234567]+$" newperms))
      (error "Not a valid octal permission value: %s" newperms))

  (let ((files (dired-get-marked-files 'no-dirs))
	(origin (point))
	(outbuf (get-buffer-create " *my-dired-chmod output*")))
    (with-current-buffer outbuf
      (erase-buffer))

    (message "Changing permissions ...")
    (apply 'call-process "chmod" nil outbuf nil (cons newperms files))

    (if (> (buffer-size outbuf) 0)
	(progn
	  (pop-to-buffer outbuf)
	  (goto-char (point-min))
	  (let ((window-min-height 10))
	    (shrink-window-if-larger-than-buffer))
	  (fundamental-mode)
	  (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer)
	  (message "The above errors occurred while changing permissions!"))

      (my-eval-silently
       (dired-do-redisplay))
      (goto-char origin) ;; For some reason, save-excursion doesn't work here.
      (message "Changing permissions ... Done!"))))

(defun my-dired-fixperm ()
  "Runs my fixperm script on the current (or marked) item(s)."
  (interactive)
  (if (not my-win32)
      (error "This command is not available on non-Windows platforms!"))

  (if (and (string-match-p "/.ssh/$" default-directory)
	   (not (y-or-n-p (format "Directory is %s -- are you sure? " default-directory))))
      (error "Aborted!"))

  (let ((files (dired-get-marked-files 'no-dirs)))
    (dolist (file files)
      (if (member-ignore-case file '(".netrc" ".authinfo"))
	  (if (not (y-or-n-p (format "Really fix permissions on %s? " file)))
	      (error "Aborted!"))))

    (message "Fixing permissions on %d file%s."
	     (length files) (if (> (length files) 1) "s" ""))

    (apply 'my-fixperm files)))

(defun my-dired-scan-for-viruses ()
  "Scans the current file or directory for viruses."
  (interactive)
  (let ((filename (dired-get-filename 'no-dir)))
    (my-virus-scan filename)))

(defun my-dired-make-desktop-shortcut ()
  "Makes a desktop shortcut to the current file."
  (interactive)
  (if (not my-win32)
      (error "This function is only available on Windows!"))
  (if (not (eq major-mode 'dired-mode))
      (error "This doesn't look like a Dired buffer!"))
  (let* ((path (dired-get-filename nil))  ;; Raises error if no file on line.
	 (file (dired-get-filename t))    ;; t means get just the plain filename.
	 (name (read-from-minibuffer "Shortcut name: " file))
	 (cmd (format "mkshortcut --name=\"%s\" \"%s\"" name file)))
    ;;(shell-command cmd)
    ;; The above shell-command call causes mkshortcut to hang and/or crash for some reason, so instead
    ;; I make the shortcut in the same directory as the file and move it to the desktop.
    (call-process (concat my-systemdrive "/apps/cygwin/bin/mkshortcut.exe") nil nil nil (concat "--name=" name) file)
    (call-process (concat my-systemdrive "/apps/cygwin/bin/mv.exe") nil nil nil (concat name ".lnk")
		  (format "/cygdrive/%s/users/%s/desktop" my-sysdl user-login-name))
    (message "Created desktop shortcut '%s'." name)))

;;(defun my-dired-delete-force ()
;;  "Deletes a file in dired mode, even if that file is read-only."
;;  (interactive)
;;  (if (not (eq major-mode 'dired-mode))
;;      (error "This doesn't look like a Dired buffer!"))
;;  (let ((path (dired-get-filename nil))		;; Raises error if no file on line.
;;	(file (dired-get-filename t))
;;	(confirm-func (function y-or-n-p)))
;;    (if (eq t (car (file-attributes path)))
;;	(error "Cannot forcibly delete directories!"))
;;    (if (= ?. (aref file 0))
;;	(setq confirm-func (function yes-or-no-p)))
;;    (if (null (apply confirm-func (list (concat "Delete " path "? "))))
;;	(message "File not deleted.")
;;      (shell-command (concat "rm -f " path))
;;      (revert-buffer))))

(defun my-dired-do-copy-with-suggestion ()
  "Copies the file on the current line, offering it's current name as the
suggested new name.  You should edit the name to be different.  In the
minibuffer, space is a self-inserting character, so use TAB to do filename
completion."
  (interactive)
  (if (not (eq major-mode 'dired-mode))
      (error "This is not a dired buffer!"))
  (let ((newname)
	(currentfile (dired-get-filename 'no-dir))
	(minibuffer-local-filename-completion-map
	 (copy-keymap minibuffer-local-filename-completion-map))
	(orig-spc-binding (lookup-key minibuffer-local-filename-completion-map " ")))
    (define-key minibuffer-local-filename-completion-map " " 'self-insert-command)
    (setq newname (read-file-name "Copy to: " (dired-current-directory) nil nil currentfile))
    (copy-file (dired-get-filename) newname 1)
    (dired-revert)))

(defun my-dired-do-rename-with-suggestion ()
  "Renames the file on the current line offering it's current name as the
suggested new name.  You should edit the name to be different.  In the
minibuffer, space is a self-inserting character, so use TAB to do filename
completion."
  (interactive)
  (if (not (eq major-mode 'dired-mode))
      (error "This is not a dired buffer!"))
  (let ((newname)
	(currentfile (dired-get-filename 'no-dir))
	(minibuffer-local-filename-completion-map
	 (copy-keymap minibuffer-local-filename-completion-map))
	(orig-spc-binding (lookup-key minibuffer-local-filename-completion-map " ")))
    (define-key minibuffer-local-filename-completion-map " " (lambda () (interactive)
							       (insert 32)))
    (setq newname (read-file-name "Rename to: " (dired-current-directory) nil nil
				  currentfile))
    (rename-file (dired-get-filename) newname 1)
    (dired-revert)))

(defun my-dired-find-alternate-file ()
  "..."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (if (null filename)
	(error "No filename on current line!"))

    ;; If it's a file or the current Dired buffer is showing my home directory,
    ;; then find it without destroying the current Dired buffer.
    (if (or (not (file-directory-p filename))
	    (and (eq major-mode 'dired-mode)
		 (or (string= (buffer-name) "franl")
		     (string= (buffer-name) "~"))))
	(dired-find-file)
      (dired-find-alternate-file))))

(defun my-dired-go-up-and-kill-current ()
  "Displays parent directory in dired mode.  Deletes current dired buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (dired-up-directory)
    (if (and (not (eq (current-buffer) buf))
	     (not (string= "franl" (buffer-name buf))))
	(kill-buffer buf))))

(defun my-dired-launch-file ()
  "Launches the file on the current line of a Dired buffer."
  (interactive)
  (let ((pathname (dired-get-filename)))
    (my-launch-file pathname)))

;; Silence byte-compiler warning about this function not being defined
;; on non-Windows systems.
(declare-function w32-shell-execute ".c" (operation document &optional parameters show-flag))

(defun my-dired-launch-program-as-root ()
  "Launches the program on the current line of the Dired buffer as user root on UNIX or
as the local Administrator on Windows."
  (interactive)
  (let ((pathname (subst-char-in-string ?/ ?\\ (dired-get-filename))))
    (if my-win32
	(progn
	  (if (/= 0 (shell-command (format "getfacl \"%s\" | grep -q '^other:r.x$'" pathname)))
	      (error "The program is not readable and executable by others!"))

	  ;; Can't use start-process here or runas.exe doesn't get a console (thanks
	  ;; to how Emacs spawns sub-processes on Windows).
	  (w32-shell-execute "open" (concat my-systemdrive "/windows/system32/runas.exe")
			     (concat "/user:Administrator \"" pathname "\"")))
      (error "Not yet implemented on UNIX!"))))

(defun my-dired-toggle-vc-directory ()
  "Toggles the current dired view between vc-dired-mode and dired-mode."
  (interactive)
  (rename-buffer "xyzzy" 'unique)
  (let ((oldbuf (current-buffer))
	(my-toggle-vc-dired-mode-in-progress t))
    (if (eq major-mode 'dired-mode)
	(vc-dir ".")
      (dired "."))
    (kill-buffer oldbuf)))

(defun my-dired-mark-by-size (prefix size)
  "..."
  (interactive (list current-prefix-arg
		     (string-to-number
		      (read-string "File size: " "2000" minibuffer-history))))
  (save-excursion
    (goto-char (point-min))
    (beginning-of-line 5)
    (while (not (eobp))
      (goto-char (+ (point) 35))
      (let ((start (point))
	    (cmpfunc (if prefix (function >=) (function <))))
	(forward-word 1)
	(if (apply cmpfunc
		   (list (string-to-number (buffer-substring start (point))) size))
	    (progn
	      (dired-mark 1)
	      (beginning-of-line))
	  (beginning-of-line 2))))))

(defun my-dired-mouse-find-file (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (let (file)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

(defun my-dired-winamp-play (&optional prefix)
  "Plays the file or directory on the current line in Winamp or XMMS.  If a directory
is played, all files under that directory (recursively) are played."
  (interactive "P")
  (let* ((pathname (dired-get-filename))
	 (msg (format "Playing \"%s\"" (file-name-nondirectory pathname))))

    (if (not my-win32)
	;; Non-Windows system ...
	(start-process "xmms-enqueue" nil "xmms" pathname)

      ;; Windows ...
      (let ((winamp-script (expand-file-name "~/bin/win32/winamp"))
	    (bash (concat my-systemdrive "/apps/cygwin/bin/bash.exe")))

	(if (not (file-exists-p bash))
	    (error "File not found: %s" bash))
	(if (not (file-exists-p winamp-script))
	    (error "File not found: %s" winamp-script))

	(start-process "winamp-play" nil bash winamp-script pathname)))
    (message msg)))

(defun my-dired-winamp-enqueue (&optional prefix)
  "Enqueues the file or directory on the current line in Winamp or XMMS.  If a directory
is enqueued, all files under that directory (recursively) are enqueued."
  (interactive "P")
  (let* ((pathname (dired-get-filename))
	 (msg (format "Queued \"%s\"" (file-name-nondirectory pathname))))

    (if (not my-win32)
	;; Non-Windows system ...
	(start-process "xmms-enqueue" nil "xmms" "--enqueue" pathname)

      ;; Windows ...
      (let ((winamp-script (expand-file-name "~/bin/win32/winamp"))
	    (bash (concat my-systemdrive "/apps/cygwin/bin/bash.exe")))

	(if (not (file-exists-p bash))
	    (error "File not found: %s" bash))
	(if (not (file-exists-p winamp-script))
	    (error "File not found: %s" winamp-script))

	(start-process "winamp-enqueue" nil bash winamp-script "/ADD" pathname)))
    (message msg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vc-dired stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define-key vc-dired-mode-map "u" 'my-vc-dired-go-up)
;;(define-key vc-dired-mode-map "U" 'my-vc-dired-go-up-and-kill-current)
;;(define-key vc-dired-mode-map "f" 'my-vc-dired-find-file)

(defun my-vc-dired-go-up ()
  "Like my-dired-go-up, but for VC Dired buffers."
  (interactive)
  (let* ((name (save-excursion
		 (goto-char (point-min))
		 (forward-line 2)
		 (dired-get-filename)))
	 (parentdir (if (string-match "^\\(.*\\)/[^/]+/[^/]+$" name)
			(match-string 1 name))))
    (if parentdir
	(if (and (file-directory-p (concat parentdir "/CVS"))
		 (file-regular-p (concat parentdir "/CVS/Root")))
	    (vc-dir parentdir)
	  (dired parentdir))
      (error "Couldn't find parent directory!"))))

(defun my-vc-dired-find-file ()
  "Like dired-find-file but uses VC Dired mode to display the new
directory."
  (interactive)
  (let ((name (dired-get-filename)))
    (if (file-directory-p name)
	(vc-dir name)
      (dired-find-file))))

(defun my-vc-dired-go-up-and-kill-current ()
  "Like my-dired-go-up-and-kill-current but works with VC Dired buffers."
  (interactive)
  (let ((buf (current-buffer)))
    (my-vc-dired-go-up)
    (if (not (eq (current-buffer) buf))
	(kill-buffer buf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-advice dired-create-directory (:after (&rest args) my-ad-after-dired-create-directory)
  "On Windows, runs fixperm on the new directory."
  (when my-win32
    (my-fixperm (car args))))

(define-advice vc-dired-mode (:after (&rest args) my-ad-after-vc-dired-mode)
  "Sets mouse-face to 'highlight on all listed names."
  (run-at-time 0.1 nil (lambda ()
			 (save-excursion
			   (goto-char (point-min))
			   (dired-next-line 2)
			   (let ((column (current-column))
				 (inhibit-read-only t))
			     (dired-previous-line 1)
			     (while (dired-next-line 1)
			       (add-text-properties (point) (progn (end-of-line) (point))
						    '(mouse-face highlight))))))))
