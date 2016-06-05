;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Software development stuff.  This is mostly for C and C++ code.  Don't forget
;; about the hooks in my-hooks.el.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAGS and xref support.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'xref)
(require 'etags)

;; TODO: Remove this when find-tag is finally eliminated!
;; Suppress byte compiler warninga about find-tag being obsolete.
(eval-when-compile
   (setq byte-compile-warnings '(not obsolete)))

(setq tags-case-fold-search t        ;; Do case-insensitive TAGS searching.
      tags-revert-without-query t)   ;; Don't prompt me to reload TAGS files from disk.

(defun my-show-tags-files ()
  "Displays the TAGS files in variable tags-table-list."
  (interactive)
  (if tags-table-list
      (message "TAGS files:\n%s" (mapconcat 'identity tags-table-list "\n"))
    (message "tags-table-list is nil!")))

(defvar my-xref-pulse-delay 3
  "The number of seconds that the source line should be highlighted after various xref
functions move point to that line.")

(define-advice xref-find-definitions (:around (origfun &rest args) my-ad-around-xref-find-definitions)
  "When xref-find-definitions will use a TAGS file, this searches for a TAGS file in the
current directory and its ancestors and, if one is found, prepends it to tags-table-list."
  (interactive '("mydummy"))

  ;; If xref uses TAGS files in this file, look for a TAGS file in this directory and all of its
  ;; ancestors, adding each one to tags-table-list.  This condition is needed to cope with major
  ;; modes (such as Emacs-Lisp) that do xref'ing without TAGS tables.
  (when (eq (xref-find-backend) 'etags)

    ;; Search for TAGS files.
    (let ((my-new-tags-table-list nil)
	  (my-curdir (substring default-directory 0 -1)))

      ;; Walk up the directory tree looking for a file named "TAGS".  my-curdir is always a
      ;; directory name without the trailing '/'.  If we've reached the root of the filesystem, it
      ;; is "" (UNIX) or "x:" (Windows).
      (while (and (not (string= "" my-curdir))
		  (or (not my-win32)
		      (not (string-match-p "^[a-zA-Z]:$" my-curdir))))

	(let ((my-tags-file-name (concat my-curdir "/TAGS"))) 
	  (if (file-exists-p my-tags-file-name)
	      (add-to-list 'my-new-tags-table-list my-tags-file-name 'append))

	  (setq my-curdir (substring (file-name-directory my-curdir) 0 -1))))

      ;; QUESTION: Maybe change this to use loaded TAGS files even if tags-table-list is empty?

      (if (null my-new-tags-table-list)

	  ;; We didn't find any TAGS files ...
	  (if (null tags-table-list)
	      ;; ... and there are no TAGS files in tags-table-list, so show an error.
	      (error "No TAGS files found in or above current directory (and tags-table-list is empty)!")

	    ;; Ask the user if we should use the TAGS files in tags-table-list.
	    (if (let* ((my-index 0)
		       (max-mini-window-height 10)
		       (my-prompt (concat (mapconcat (lambda (my-tags-file)
						       (prog1
							   (format "%d: %s" my-index my-tags-file)
							 (incf my-index)))
						     tags-table-list "\n")
					  "\n\nNo TAGS files found for this directory!  "
					  "Use above TAGS files?")))
		  (not (y-or-n-p my-prompt)))
		(error "Aborted!")))

	;; We found TAGS files.  If the list of found TAGS files is different from tags-table-list,
	;; update tags-table-list to contain the newly found files.  This code doesn't set
	;; tags-file-name, but it gets set somehow by my use of xref-find-definitions, and it
	;; overrides tags-table-list, so set it to nil here.
	(if (not (equal (sort my-new-tags-table-list 'string-lessp)
			(sort tags-table-list 'string-lessp)))
	    (setq tags-file-name nil
		  tags-table-list my-new-tags-table-list)))

      ;; Now tags-table-list contains the TAGS files to use.  Load all the TAGS files and build the
      ;; tags completion table in each TAGS buffer now, so that it doesn't happen when I type the
      ;; first character at the below prompt.  Function tags-completion-table only computes the
      ;; completion table if it doesn't already exist, so it doesn't always happen.
      (if tags-table-list
	  (save-excursion
	    (visit-tags-table-buffer)
	    (tags-completion-table)

	    (while (visit-tags-table-buffer t)
	      (tags-completion-table))))))

  ;; Prompt for an identifier and update parameter ARGS.  QUESTION: Is there a reason I don't
  ;; just call xref--read-identifier here?
  (let* ((my-backend (xref-find-backend))
	 (my-identifier (if (memq major-mode '(dired-mode Buffer-menu-mode))
			    ""
			  (or (xref-backend-identifier-at-point my-backend) "")))
	 ;; Don't ask the user if it's OK to update the tags table list.
	 (tags-add-tables nil))
    (setq args (list (completing-read "Find definitions of: "
				      ;; If tags-table-list has been changed, this re-builds the
				      ;; completion table for this completing-read call.
				      (xref-backend-identifier-completion-table my-backend)
				      nil nil (cons my-identifier 0) 'xref--read-identifier-history
				      my-identifier))))

  ;; Call the advised function and save its return value.
  (let* ((my-retval (apply origfun args))
	 (my-xref-window (get-buffer-window "*xref*" (selected-frame))))

    (if (window-live-p my-xref-window)
	(progn
	  (set-window-text-height my-xref-window 20)

	  ;; The foreground I set in my-visual.el for this face doesn't stick, so force it here.
	  (set-face-foreground 'compilation-info "orange"))

      ;; An *xref* buffer was not displayed, so show the pathname of the file we're in.
      (let ((my-filename (replace-regexp-in-string (concat "^" (or (getenv "HOME")
								   (if my-win32 "c:/franl" "/home/franl")))
						   "~" (file-truename (buffer-file-name)))))
	(my-message-highlight "File: %s" my-filename))

      ;; ... and optionally recenter if point is too close to the bottom of the window.  This
      ;; sit-for is needed to let the display update so that line-number-at-pos computes the correct
      ;; value.
      (sit-for 0)
      (if (< (- (line-number-at-pos (window-end))
		(line-number-at-pos (point)))
	     10)
	  (recenter '(4)))

      ;; Return the value returned by the advised function.
      my-retval)))

(define-advice xref-pop-marker-stack (:around (origfun &rest args) my-ad-around-xref-pop-marker-stack)
  "Binds pulse-iterations and pulse-delay to highlight the target source line to my liking."
  (let ((pulse-iterations 1)
	 (pulse-delay my-xref-pulse-delay))
    (apply origfun args)
    (my-message-highlight "File: %s" (buffer-file-name))))

(define-advice xref-goto-xref (:around (origfun &rest args) my-ad-around-xref-goto-xref)
  "Binds pulse-iterations and pulse-delay to highlight the target source line to my liking."
  (let ((pulse-iterations 1)
	 (pulse-delay my-xref-pulse-delay))
    (apply origfun args)
    (my-message-highlight "File: %s" (buffer-file-name))))

(define-advice xref-next-line (:after (&rest args) my-ad-after-xref-next-line)
  "Displays the file name."
  (my-message-highlight "File: %s" (buffer-file-name)))

(define-advice xref-prev-line (:after (&rest args) my-ad-after-xref-prev-line)
  "Displays the file name."
  (my-message-highlight "File: %s" (buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-advice csharp-maybe-insert-codedoc (:around (origfun &rest args) my-ad-around-csharp-maybe-insert-codedoc)
  "... TODO ..."
  (let ((origin (point))
	retval)
    (if (and (or (back-to-indentation) t)
	     (string= "//" (buffer-substring (point) origin)))

	;; The '/' was the 3rd consecutive '/', so maybe do something special depending on the
	;; contents of the previous line.
	(if (not (= 0 (forward-line -1)))
	    ;; forward-line didn't return 0, we were unable to move to the previous line, so we are
	    ;; at the first line of the file, and we want the normal behavior of
	    ;; csharp-maybe-insert-codedoc.
	      (progn
		(goto-char origin)
		(setq retval (apply origfun args)))

	  (if (not (looking-at-p "\\s-*/"))
	      ;; The previous line has no comment on it, so let the normal function execute (it will
	      ;; insert a <summary> element).
	      (progn
		(goto-char origin)
		(setq retval (apply origfun args))
		(just-one-space))

	    ;; In all other case, this advice controls what is inserted.
	    (if (or (looking-at-p "\\s-*/// </")
		    (looking-at-p "\\s-*/// [^<\n]"))
		;; The previous line was an end tag or "/// normal text", so just insert a '/' to
		;; create a separator line ("///").
		(progn
		  (goto-char origin)
		  (insert "/"))

	      (goto-char origin)
	      (let* ((cursor-in-echo-area t)
		     (answer (downcase (read-char "Type P (param), R (returns), E (exception), S (seealso), C (code), / (slash): "))))
		(cond ((= answer ?p)
		       (insert "/ <param name=\"\">")
		       (my-auto-indent-newline)
		       (insert "/// ")
		       (my-auto-indent-newline)
		       (insert "/// </param>")
		       (forward-line -2)
		       (end-of-line))

		      ((= answer ?r)
		       (insert "/ <returns></returns>")
		       (backward-char 10))

		      ((= answer ?e)
		       (insert "/ <exception cref=\"\">")
		       (my-auto-indent-newline)
		       (insert "/// ")
		       (my-auto-indent-newline)
		       (insert "/// </exception>")
		       (forward-line -2)
		       (end-of-line))

		      ((= answer ?s)
		       (insert "/ <seealso cref=\"\"/>")
		       (backward-char 3))

		      ((= answer ?c)
		       (insert "/ <code>")
		       (my-auto-indent-newline)
		       (insert "/// ")
		       (my-auto-indent-newline)
		       (insert "/// </code>")
		       (forward-line -1))

		      ((= answer ?/)
		       (insert "/")))))))

      ;; This '/' was not the 3rd consecutive '/' in a series, so let the normal function insert it.
      (goto-char origin)
      (setq retval (apply origfun args)))
    
    ;; Return the value returned by the advised function.
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-insert-brace-block ()
  "Inserts a C/C++ brace block, leaving point positioned to type the first line of nested code."
  (interactive)
  (let ((start (point)))
    (insert "{\n\n}")
    (indent-region start (point))
    (forward-line -1)
    (indent-according-to-mode)))

(defun my-find-error (&optional prefix)
  "Finds the next build error in the current buffer."
  (interactive "P")
  (if (overlayp my-highlight-line-overlay)
      (delete-overlay my-highlight-line-overlay))

  (if (null (search-forward-regexp "error:\\|error - \\|\\([1-9][0-9]*\\|:\\|fatal\\|signtool\\) +error" nil t))
      (error "No errors found!"))
  
  (my-highlight-line))

(defun my-compile-and-run (&optional filename)
  "Compiles and runs FILENAME or the current file if FILENAME is nil."
  (interactive)
  (let* ((file-to-compile (or filename (buffer-file-name)))
	 (basename (file-name-nondirectory file-to-compile)))
    (if (null file-to-compile)
	(error "No file to compile!"))

    (message "Compiling and running %s ..." basename)

    (cond ((string-match-p "\\.cs$" file-to-compile)
	   (shell-command (format "csc '%s' && './%s'" basename basename)))

	  ((string-match-p "\\.cpp$" file-to-compile)
	   (shell-command (format "g++ '%s' && './%s'" basename basename)))

	  ((string-match-p "\\.c$" file-to-compile)
	   (shell-command (format "gcc '%s' && './%s'" basename basename)))

	  (t
	   (error "Don't know how to compile '%s'" basename)))))

(defun my-compile (&optional command)
  "Just like function compile, except leaves point at bottom of *compilation*
buffer."
  (interactive)
  (if (buffer-modified-p)
      (error "Save the current buffer first!"))

  ;; Call compile without offering to save any modified buffers.
  (my-with-advice ((save-some-buffers ignore))
    (if command
	(compile command)
      (call-interactively #'compile)))

  (pop-to-buffer "*compilation*")
  (set-window-text-height (selected-window) my-grep-window-height)
  (goto-char (point-max)))

(defun my-ruby-rdoc (prefix)
  "Runs rdoc to generate and then display the Rdoc documentation for the current
directory.  Interactively, a prefix argument means to pass the current file name
to rdoc, which prevents rdoc from processing other Ruby source files in the
current directory."
  (interactive "p")
  (message "Generating Rdoc documentation ...")
  (if prefix
      (shell-command (format "rdoc -q '%s' && launch doc/index.html"
			     (file-name-nondirectory (buffer-file-name))))
    (shell-command (format "rdoc -q && launch doc/index.html"))))

(defun my-c-fill-paragraph (prefix)
  "Bound to M-q in C/C++/C# buffers (see my-hooks.el)."
  (interactive "P")
  (let* ((paragraph-content "\\s-*\\(///?\\|\\*\\|/\\*\\|\\*/\\)\\s-*.+$")
	 (id-to-skip "\\s-*\\(//\\|/\\*\\)\\s-*\\([a-z_][a-z0-9_.]+\\|.*(.*)\\)$")
	 (csharp-xml-to-skip "\\s-*///\\s-*<[^<]+>")
	 (origin (set-marker (make-marker) (point)))

	 ;; Find the start of the paragraph.  This is one of: the first line of this block
	 ;; of comments, the first line above the current location that contains a C# XML
	 ;; tag, or the first line above the current location that follows a blank comment
	 ;; line.
	 (pstart (progn (while (progn (beginning-of-line)
				      (and (not (bobp))
					   (looking-at-p paragraph-content)
					   (not (looking-at-p csharp-xml-to-skip))))
			  (forward-line -1))
			(if (not (looking-at-p paragraph-content))
			    (forward-line 1))

			;; Skip some special first lines so these don't get filled.
			(if (or (looking-at-p id-to-skip)
				(looking-at-p csharp-xml-to-skip))
			    (forward-line 1))

			(point)))

	 ;; Find the end of the paragraph.  Don't need to skip back over C# XML end tags
	 ;; (e.g., </param>) because c-fill-paragraph groks them and does the right thing.
	 (pend (progn (while (looking-at-p paragraph-content)
			(forward-line 1))
		      (point))))

    ;; Narrow buffer to the paragraph and fill it using c-fill-paragraph.
    (save-restriction
      (narrow-to-region pstart pend)
      (goto-char pstart)
      (c-fill-paragraph))

    (goto-char origin)))

(defun my-insert-public-section-comment ()
  "Inserts a C/C++/C#-style comment marking public stuff."
  (interactive)
  (let ((start (point))
	(comment (concat "// ================================================================================\n"
			 "// Public methods"
			 (if (eq major-mode 'csharp-mode)
				", properties, fields, and types"
			      (if (eq major-mode 'c++-mode)
				  " and data members"
				""))
			 ".\n// ================================================================================\n\n")))
    (insert comment)
    ;;(my-auto-indent-newline)
    (indent-region start (point))))

(defun my-insert-internal-section-comment ()
  "Inserts a C#-style comment marking internal stuff."
  (interactive)
  (let ((start (point))
	(comment (concat "// ================================================================================\n"
			 "// Internal methods, properties, fields, and types.\n"
			 "// ================================================================================\n")))
    (insert comment)
    ;;(my-auto-indent-newline)
    (indent-region start (point))))

(defun my-insert-protected-section-comment ()
  "Inserts a C/C++/C#-style comment marking public stuff."
  (interactive)
  (let ((start (point))
	(comment (concat "// ================================================================================\n"
			 "// Protected methods"
			 (if (eq major-mode 'csharp-mode)
				", properties, fields, and types"
			      (if (eq major-mode 'c++-mode)
				  " and data members"
				""))
			 ".\n// ================================================================================\n")))
    (insert comment)
    ;;(my-auto-indent-newline)
    (indent-region start (point))))

(defun my-insert-private-section-comment ()
  "Inserts a C/C++/C#-style comment marking public stuff."
  (interactive)
  (let ((start (point))
	(comment (concat "// ================================================================================\n"
			 "// Private methods"
			 (if (eq major-mode 'csharp-mode)
				", properties, fields, and types"
			      (if (eq major-mode 'c++-mode)
				  " and data members"
				""))
			 ".\n// ================================================================================\n")))
    (insert comment)
    ;;(my-auto-indent-newline)
    (indent-region start (point))))

(defun my-insert-under-construction-shell ()
  "Inserts a shell/Tcl/Ruby/Perl comment saying \"UNDER CONSTRUCTION\"."
  (interactive)
  (insert "############################################################")
  (my-newline-with-indent)
  (insert "###################  UNDER CONSTRUCTION  ###################")
  (my-newline-with-indent)
  (insert "############################################################"))

(defun my-insert-under-construction-C ()
  "Inserts a C comment saying \"UNDER CONSTRUCTION\"."
  (interactive)
  (insert "/***********************************************************")
  (my-newline-with-indent)
  (insert " ******************  UNDER CONSTRUCTION  *******************")
  (my-newline-with-indent)
  (insert "***********************************************************/"))

(defun my-insert-under-construction-java ()
  "Inserts a Java comment saying \"UNDER CONSTRUCTION\"."
  (interactive)
  (insert "// **********************************************************")
  (my-newline-with-indent)
  (insert "// ******************* UNDER CONSTRUCTION *******************")
  (my-newline-with-indent)
  (insert "// **********************************************************"))

(defun my-insert-under-construction-lisp ()
  "Inserts a C comment saying \"UNDER CONSTRUCTION\"."
  (interactive)
  (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (my-newline-with-indent)
  (insert ";;                   UNDER CONSTRUCTION                   ;;")
  (my-newline-with-indent)
  (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"))

(defun my-c++-switch-source-for-header ()
  "If the current buffer is a .cpp file, find the corresponding .h file (or vice versa)."
  (interactive)
  (if (null (buffer-file-name))
      (error "This buffer has no file!"))

  (save-match-data
    (let ((this-filename (file-name-nondirectory (buffer-file-name)))
	  filename-no-extension)
      (if (not (string-match "^\\(.*\\.\\)\\(cpp\\|c\\|h\\|hpp\\)$" this-filename))
	  (error "Unsupported file suffix: %s" this-filename))

      (setq filename-no-extension (match-string 1 this-filename))

      (if (= ?h (aref (match-string 2 this-filename) 0))
	  ;; Look for the source file.  Check existing buffers first ...
	  (let ((cfile (concat filename-no-extension "c"))
		(cppfile (concat filename-no-extension "cpp")))
	    (if (get-buffer cfile)
		(switch-to-buffer cfile)
	      (if (get-buffer cppfile)
		  (switch-to-buffer cppfile)

		;; Then check the current directory ...
		(if (file-exists-p cfile)
		    (find-file cfile)
		  (if (file-exists-p cppfile)
		      (find-file cppfile)

		    ;; Then check the "src" subdirectory ...
		    (if (file-exists-p (concat "src/" cfile))
			(find-file (concat "src/" cfile))
		      (if (file-exists-p (concat "src/" cppfile))
			  (find-file (concat "src/" cppfile))

			;; Then use find-tag ...
			(if (not (or (ignore-errors (find-tag cfile))
				     (ignore-errors (find-tag cppfile))))
			    (error "Can't find source file!")))))))))

	;; Look for the header file.  Check existing buffers first ...
	(let* ((hfile (concat filename-no-extension "h"))
	       (hppfile (concat filename-no-extension "hpp"))
	       (hfileorig hfile)
	       (hppfileorig hppfile))
	  (if (get-buffer hfile)
	      (switch-to-buffer hfile)
	    (if (get-buffer hppfile)
		(switch-to-buffer hppfile)

	      ;; Then check the current directory ...
	      (if (file-exists-p hfile)
		  (find-file hfile)
		(if (file-exists-p hppfile)
		    (find-file hppfile)

		  ;; Then check the parent directory ...
		  (if (file-exists-p (concat "../" hfile))
		      (find-file (concat "../" hfile))
		    (if (file-exists-p (concat "../" hppfile))
			(find-file (concat "../" hppfile))

		      ;; Then look in ../include ...
		      (setq hfile (concat "../include/" hfileorig)
			    hppfile (concat "../include/" hppfileorig))
		      (if (file-exists-p hfile)
			  (find-file hfile)
			(if (file-exists-p hppfile)
			    (find-file hppfile)

			  ;; Then look in ../../include ...
			  (setq hfile (concat "../../include" hfileorig)
				hppfile (concat "../../include" hppfileorig))
			  (if (file-exists-p hfile)
			      (find-file hfile)
			    (if (file-exists-p hppfile)
				(find-file hppfile)

			      ;; Then use find-tag ...
			      (if (not (or (ignore-errors (find-tag hfileorig))
					   (ignore-errors (find-tag hppfileorig))))
				  (error "Can't find header file!")))))))))))))))))

(defun my-insert-logger-call ()
  "Inserts a call to a logger function and leaves point between the double quotes."
  (interactive)
  (let ((keys (this-command-keys)))
    (cond
     ((string= keys "\C-zl")
      (insert "CEMTRACE_DEBUG(MODULE_NAME, \"\");")
      (backward-char 3))
     
     ((string= keys "\C-z0")
      (insert "syslog(LOG_ERR, \"\");")
      (backward-char 3)
      (ignore-errors
	(my-insert-function-name)
	(insert ": "))))))

(defun my-insert-function-name ()
  "Inserts the name of the C, C++, or Elisp function that point is currently
in."
  ;; BUG: Doesn't do Elisp yet.
  (interactive)
  (if (not (memq major-mode '(c++-mode c-mode)))
      (error "This command does not support %s mode!" mode-name))

  (save-match-data
    (let (funcname)
      (save-excursion
	(if (not (search-backward-regexp "^{" nil t))
	    (error "Point is not in a C or C++ function!")
	  (if (not (search-backward-regexp "^[a-zA-Z_]" nil t))
	      (error "Cannot find function head!")
	    (if (not (looking-at "^.*?\\([A-Za-z_0-9]+\\)\("))
		(error "I'm confused -- where's the damn function name?")
	      (setq funcname (match-string 1))))))
      (insert funcname))))
