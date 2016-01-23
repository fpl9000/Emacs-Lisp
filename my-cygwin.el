;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code to make Emacs work better on Windows and with Cygwin.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Things to do:
;;
;; o Factor the common code from the advice on find-file-noselect and dired into
;;   a separate function.

(if (not my-win32)
    (error "Attempt to load my-cygwin.el on a non-Windows system!"))

;; Silence byte-compiler warnings about functions defined in other files.
(declare-function my-block "my-misc.el")
(declare-function my-block-exit "my-misc.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local my-cygwin-original-file-acl nil
  "A buffer-local variable that holds the ACLs for each buffer's file (or nil if the
buffer has no file).  Used by my-cygwin-after-save-hook and my-cygwin-find-file-hook
to preserve a file's ACLs across saves, because Emacs seems to reset a files executable
bits when saving it.")

;; The default value of null-device is "NUL", but as of Cygwin 1.7.x, "NUL" is a
;; valid filename, so we use /dev/null instead.
(setq null-device "/dev/null")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-cygwin-is-symlink (file)
  "Returns non-nil if FILE is a Cygwin symlink, nil otherwise."
  (if (and (file-exists-p file)
	   (not (file-directory-p file)))
      (with-temp-buffer
	(insert-file-contents-literally file nil 0 10)
	(string= "!<symlink>" (buffer-string)))))

(defun my-cygwin-symlink-to-emacs-pathname (symlink-pathname)
  "Converts a pathname of a Cygwin symlink file into a pathname to the target of the
symlink.  NOTE: The argument to this function must exist, and it must be an Emacs-style
pathname not a Cygwin-style pathname."
  (save-match-data
    (with-temp-buffer
      ;; Read the symlink contents into a temporary buffer.  This raises an error
      ;; if symlink-pathname does not exist, which is desirable.
      (insert-file-contents-literally symlink-pathname)

      (let (symlink-content symlink-target)
	(setq symlink-content (buffer-string))

	(if (string-match "^!<symlink>\377\376\\(.*\\)$" symlink-content)
	    ;; It's a new-style Cygwin symlink.  QUESTION: Do we still need to
	    ;; replace '\' with '/'?
	    (setq symlink-target
		  (subst-char-in-string ?\\ ?/ (delete ?\000 (match-string 1 symlink-content))))

	  ;; It's either an old-style Cygwin symlink or a pure Windows shortcut.
	  (setq symlink-content (delete ?\n symlink-content))
	  (if (string-match "^L\000\000\000.*\000\\([^\000-\037\177-\277]+\\)$" symlink-content)
	      (setq symlink-target
		    (subst-char-in-string ?\\ ?/ (match-string 1 symlink-content)))

	    ;; This will help me find unsupported symlinks.
	    (error "my-cygwin-symlink-to-emacs-pathname: Unsupported symlink type: '%s'" symlink-pathname)))
	
	;; Compute the Emacs-style pathname to return.
	(if (or (= (aref symlink-target 0) ?/)
		(and (= (aref symlink-target 1) ?:)
		     (= (aref symlink-target 2) ?/)))
	    ;; The symlink target is an absolute pathname of the form "/x/y" or "c:/x/y",
	    ;; so just return it.
	    symlink-target

	  ;; The symlink target is a relative pathname, so return the symlink-pathname
	  ;; with the rightmost pathname component replaced by the symlink target.
	  (if (null (cl-position ?/ symlink-pathname))
	      symlink-target

	    (if (string-match "^\\(.+\\)/[^/]+$" symlink-pathname)
		(concat (match-string 1 symlink-pathname) "/" symlink-target)

	      ;; This should never happen.
	      (error "my-cgywin-symlink-pathname-to-emacs-pathname: INTERNAL ERROR!"))))))))

(defun my-cygwin-pathname-to-emacs-pathname (pathname)
  "Converts a Cygwin-style pathname (.e.g, /tmp, /c/franl/file, /c, c:/foo/bar,
/cygdrive/c/franl) to a pathname that Emacs can understand, recursively expanding Cygwin
symlinks in every component of the pathname.  The file or directory referenced by the
pathname does not have to exist.  The pathname can be relative or absolute.  The pathname
'/' is converted to Cygwin's root directory not to the root of the system drive.
Pathanames starting with '~/' are left with that prefix, because Emacs groks it."

  (if (string= pathname "")
      (error "my-cygwin-pathname-to-emacs-pathname: Argument PATHNAME is the empty string!"))

  (subst-char-in-string ?\\ ?/ pathname 'inplace)

  (save-match-data
    ;; TODO: Someday handle pathnames starting with "~USER/".
    (if (string-match-p "^~[a-z]+" pathname)
	(error "my-cygwin-pathname-to-emacs-pathname: Pathnames starting with ~USER not yet supported!"))

    ;; Special case pathname "/", which would confuse the below algorithm, which works by
    ;; splitting pathname into component strings at the '/'s.  Be sure to return an
    ;; Emacs-style pathname that ends with a '/', so that minibuffer completion (below) will
    ;; complete "/et" into "/etc".
    (if (string= pathname "/")
	(concat my-systemdrive "/apps/cygwin/")

      ;; Leave pathnames of the form "//..." unchanged.
      (if (string-match-p "^//" pathname)
	  pathname

	;; Convert the pathname from left to right.  Break the pathname into its components,
	;; and do the conversion by iteratively concatenating each pathname component to the
	;; previous and re-converting the result.
	(let ((case-fold-search t)
	      (pathname-starts-with-slash (= (aref pathname 0) ?/))
	      (pathname-ends-with-slash (= (aref pathname (1- (length pathname))) ?/))
	      (path-components (split-string pathname "/" t))
	      (result ""))

	  (dolist (component path-components)
	    (my-block

	     ;;(message ">> 0 result='%s' component='%s'" result component)

	     ;; Append the next component to result and convert it.  Yes, we re-convert
	     ;; the _entire_ result string again, because symlinks to absolute pathnames
	     ;; can completely transform it.
	     (if (string= result "")
		 (if pathname-starts-with-slash
		     (setq result (concat "/" component))
		   (setq result component))
	       (setq result (concat result "/" component)))
	     
	     ;;(message ">> 1 '%s'" result)

	     ;; NOTE: The rest of this algorithm must not set result to a string ending
	     ;; with '/', because that will cause the above component concatenation
	     ;; code to create a pathname with two adjacent '/'s.  Check for that.
	     (if (string-match-p "//" result)
		 (error "my-cygwin-pathname-to-emacs-pathname: ERROR: Two '/'s in variable result!"))

	     (let ((prev-result ""))
	       ;; Attempt to transform result.  We repeatedly transform the pathname using
	       ;; this loop until it stops changing.  This is necessary because symlinks can
	       ;; change the pathname.
	       (while (not (string= result prev-result))

		 ;; Use a my-block to keep the below if-statements from becoming
		 ;; horribly deeply nested.
		 (my-block

		  ;; Re-convert all '\'s to '/'s because a symlink may have introduced new
		  ;; '\'s.  QUESTION: Can this really happen?  It looks like Cygwin symlinks
		  ;; always contain '/'s.  Keep this just in case we have to support Windows
		  ;; shortcuts at some point.
		  (subst-char-in-string ?\\ ?/ result 'inplace)

		  ;; Remember previous value of result.
		  (setq prev-result result)

		  ;;(message ">> 2 '%s'" result)

		  ;; First, make sure this isn't a TRAMP-style pathname, ange-ftp-style filename, or a
		  ;; pathname of the form "//server/share".  In those cases, do nothing.
		  (if (or (string-match-p "^/[^/:]+:" result)	;; TRAMP/ange-ftp
			  (string-match-p "^//[^/]+/" result))	;; SMB share
		      (my-block-exit))

		  ;;(message ">> 3 '%s'" result)

		  ;; This handles pathnames of the form "/X/...", where X is a drive letter.
		  (when (string-match "^/\\([a-z]\\)\\(/.*\\)?$" result)
		    (setq result (concat (match-string 1 result) ":" (match-string 2 result)))
		    (my-block-exit))

		  ;;(message ">> 4 '%s'" result)

		  ;; This handles pathnames of the form "/cygdrive/X/..." or
		  ;; "/cygdrive/X", where X is a drive letter.  If result is
		  ;; "/cygdrive" here, don't transform it, which causes the dolist to
		  ;; append another componet to it and re-enter this transformation
		  ;; loop.
		  (if (string= result "/cygdrive")
		      (my-block-exit))

		  (when (string-prefix-p "/cygdrive/" result)
		    (if (not (string-match "^/cygdrive/\\([a-z]\\)\\(/.*\\)?$" result))
			(error "Malformed Cygwin pathname: %s" pathname)
		      (setq result (concat (match-string 1 result) ":" (match-string 2 result)))
		      (my-block-exit)))

		  ;;(message ">> 5 '%s'" result)

		  ;; This handles "/tmp", "/var/log/sshd.log", "/bin", etc.
		  (when (= ?/ (aref result 0))
		    (setq result (concat my-systemdrive "/apps/cygwin" result))
		    (my-block-exit))

		  ;;(message ">> 6 '%s'" result)

		  ;; This handles the two pathnames "c:/apps/cygwin/usr/bin"
		  ;; and "c:/apps/cygwin/usr/lib".  They don't exist and are
		  ;; implemented using Cygwin mounts.  Emacs can't see them.
		  (when (string-match (concat "^" my-systemdrive "/apps/cygwin/usr/\\(bin\\|lib\\)$") result)
		    (setq result (concat my-systemdrive "/apps/cygwin/" (match-string 1 result)))
		    (my-block-exit))

		  ;;(message ">> 7 '%s'" result)

		  ;; This handles old-style Cygwin symlinks.  It would be nice if it could
		  ;; also handle pure Windows shortcuts, but that's not working yet.
		  (when (and (not (file-exists-p result))
			     (file-exists-p (concat result ".lnk")))
		    (setq result (my-cygwin-symlink-to-emacs-pathname (concat result ".lnk")))
		    (my-block-exit))

		  ;;(message ">> 8 '%s'" result)

		  ;; This handles new-style Cygwin symlinks.
		  (when (my-cygwin-is-symlink result)
		    (setq result (my-cygwin-symlink-to-emacs-pathname result))
		    (my-block-exit)))))))

	  (if (string= result "/cygdrive")
	      (error "Malformed Cygwin pathname: %s" result))

	  (if pathname-ends-with-slash
	      (concat result "/")
	    (if (string-match-p "^[a-z]:$" result)
		;; Turn "c:" into "c:/".
		(concat result "/")
	      result)))))))

(defun my-fixperm (&rest files)
  "Runs fixperm on FILES.  Does not wait for the fixperm process to finish."
  (let ((fixperm (concat my-systemdrive "/franl/bin/win32/fixperm"))
	(bash (concat my-systemdrive "/apps/cygwin/bin/bash.exe")))
    (if (or (not (file-exists-p fixperm))
	    (not (file-exists-p bash)))
	(progn
	  (message "my-fixperm: Either Bash or fixperm does not exist!")
	  (sleep-for 1))

      (my-eval-silently
       (let ((expanded-files-list (mapcar (lambda (file)
					    ;; No need to quote the filenames, because we're using
					    ;; start-process, which bypasses shell parsing and passes
					    ;; the expanded filenames directly to the script.
					    (expand-file-name file))
					  files)))
	 (apply 'start-process "fixperm" nil bash fixperm expanded-files-list))))))

(defun my-cygwin-get-buffer-file-acl ()
  "If the current buffer is associated with a file, get that file's ACL into buffer-local variable
my-cygwin-original-file-acl.  If file doesn't exist or if getfacl is not available, set the variable
to nil."
  (let ((filename (buffer-file-name))
	(getfacl (concat my-systemdrive "/apps/cygwin/bin/getfacl.exe")))
    (setq my-cygwin-original-file-acl nil)

    (let ((getfacl-buffer (get-buffer-create " *My getfacl Output*"))
	  (directory (file-name-directory filename)))
      (when (and filename
		 (file-exists-p getfacl)
		 (bufferp getfacl-buffer))
	(with-current-buffer getfacl-buffer
	  (erase-buffer))

	(let ((default-directory directory))
	  ;; Fill getfacl-buffer with output of getfacl.
	  (call-process getfacl nil `(,getfacl-buffer nil) nil (file-name-nondirectory filename))

	  (if (> (buffer-size getfacl-buffer) 2)
	      ;; Must set my-cygwin-original-file-acl outside of with-current-buffer, because it
	      ;; is a buffer-local variable.
	      (setq my-cygwin-original-file-acl
		    (with-current-buffer getfacl-buffer
		      ;; Delete non-ACL output of getfacl.
		      (goto-char (point-min))
		      (while (search-forward-regexp "^#" nil t)
			(beginning-of-line)
			(let ((start (point)))
			  (forward-line 1)
			  (delete-region start (point))))

		      ;; Delete the two newlines at the end of the buffer.
		      (goto-char (point-max))
		      (delete-region (point) (- (point) 2))

		      ;; Save the ACL as a string with commas between each component of the ACL, which is
		      ;; acceptable as input to setfacl (see function my-cygwin-after-save-hook).
		      (subst-char-in-string ?\n ?, (buffer-substring (point-min) (point-max)) 'inplace)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-cygwin-after-save-hook ()
  "Added to after-save-hook on Windows.  Fixes permissions on saved files (to cover
newly-created files) and backup files."
  (let ((setfacl (concat my-systemdrive "/apps/cygwin/bin/setfacl.exe")))
    (if (file-exists-p setfacl)
	(let* ((filename (buffer-file-name))
	       (backup-filename (concat filename "~"))
	       (directory (file-name-directory filename))
	       (acl-string (or my-cygwin-original-file-acl
			       (if (string-match-p "/bin/" directory)
				   "user::rwx,group::r-x,other:r-x"
				 "user::rw-,group::r--,other:r--"))))

	  ;; We set default-directory and pass the basename of the file name to setfacl, because
	  ;; it doesn't seem to work all the time on a pathname that starts with a drive letter.
	  (let ((default-directory directory))
	    ;; First, do the file that was just saved.
	    (start-process "setfacl" nil setfacl "-s" acl-string (file-name-nondirectory filename))

	    ;; Next, do the backup file, if it exists (backup files don't always get created).
	    (if (file-exists-p backup-filename)
		(start-process "setfacl" nil setfacl "-s" acl-string
			       (file-name-nondirectory backup-filename))))))))

(add-hook 'after-save-hook 'my-cygwin-after-save-hook)

(defun my-cygwin-find-file-hook ()
  "Added to find-file-hook on Windows.  Records the permissions of a file so that
they can be put back whenever the file is saved."
  (my-cygwin-get-buffer-file-acl))

(add-hook 'find-file-hook 'my-cygwin-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-advice find-file-literally (:around (origfun &rest args) my-ad-around-find-file-literally)
  "Makes find-file-literally ignore Cygwin symlinks (i.e., it loads the contents of the
symlink itself not the file to which it links."
  (my-with-advice ((my-cygwin-is-symlink ignore))
    (apply origfun args)))

(define-advice find-file-noselect (:filter-args (args) my-ad-filter-args-find-file-noselect)
  "Support for Cygwin pathnames and symlinks in find-file and its cousins."
  (when my-win32
    (cons (my-cygwin-pathname-to-emacs-pathname (car args))
	  (cdr args))))

(define-advice minibuffer-complete (:before (&rest args) my-ad-before-minibuffer-complete)
  "Supports use of Cygwin-style pathnames when doing minibuffer completion.  Supports
completion when point is not at the end of the minibuffer."
  (let* ((origin (set-marker (make-marker) (point)))
	 (goto-end (eobp))
	 (pathtocomplete (save-excursion
			   (move-beginning-of-line nil)
			   (subst-char-in-string ?\\ ?/ (buffer-substring (point) origin) 'inplace))))

    ;; Make origin a before-insertion marker so that it stays to the right of
    ;; the replacement text this function inserts.
    (set-marker-insertion-type origin t)

    ;; Covert the part of the minibuffer up to the right-most '/' to the left of point.
    (if (not (string= pathtocomplete ""))
	(save-match-data
	  (if (string-match "^.*/" pathtocomplete)
	      (let* ((pathtoconvert (match-string 0 pathtocomplete))
		     (convertedpath (replace-match (my-cygwin-pathname-to-emacs-pathname pathtoconvert)
						   t nil pathtocomplete)))
		;; Don't use (point-min) here, because there may be intangible text at the start of
		;; the line and that will cause this call to delete-region to try to delete it,
		;; which signals an error.
		(move-beginning-of-line nil)
		(delete-region (point) origin)
		(insert convertedpath)))))))

(define-advice dired (:filter-args (args) my-ad-filter-args-dired)
  "Support for Cygwin pathnames (e.g., /tmp, /, /usr/bin, /c/apps, /e/music, etc.)."
  (let ((my-pathname (my-cygwin-pathname-to-emacs-pathname (car args))))
    (if (not (file-directory-p my-pathname))
	(error "Directory not found: %s" (car args)))
    (cons my-pathname (cdr args))))

(define-advice dired-find-file (:around (origfun &rest args) my-ad-around-dired-find-file)
  "Support for Cygwin symlinks in Dired mode."
  (if (save-excursion
	(back-to-indentation)
	(looking-at-p "l"))
      (find-file (my-cygwin-pathname-to-emacs-pathname (dired-get-filename)))
    (apply origfun args)))

(define-advice insert-directory (:filter-args (args) my-ad-filter-args-insert-directory)
  "Make insert-directory (and thus dired) show '+' for files with ACLs.
This is necessary because Cygwin's /bin/ls leaves out the '+' when passed an argument
starting with a drive letter (e.g., c:/franl).  We advise insert-directory instead of
dired, because dired does other things with the directory name that cause errors when it
starts with /cygdrive."
  (let ((my-dirname (car args)))
    ;; Turn x:/... into /cygdrive/x/...
    (if (and my-dired-bin-ls-exists
	     (string-match "^\\([a-zA-Z]\\):\\(/.*\\)$" my-dirname))
	(cons (concat "/cygdrive/" (match-string 1 my-dirname)
		      (match-string 2 my-dirname))
	      (cdr args))
      args)))

;; Not needed anymore, because Cygwin's gunzip is now a script that runs /bin/gzip.
;; TODO: Delete this if we never need it again. :)
;;
;;(define-advice dired-call-process (:filter-args (args) my-ad-filter-args-dired-call-process)
;;  "Makes dired-do-compress work when uncompressing .gz files under Cygwin.  This is needed
;;because Cygwin's gunzip is a symlink to /bin/gzip, and dired-call-process uses
;;call-process to run it, and call-process doesn't grok Cygwin symlinks."
;;  (let ((my-program (car args))
;;	(my-args (nth 2 args)))
;;    (if (string= "gunzip" my-program)
;;	(setf (nth 0 args) "gzip"
;;	      (nth 2 args) (cons "-d" my-args)))
;;    args))
