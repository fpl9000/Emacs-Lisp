;; My Emacs configuration file.  This file loads all of my other .el files.  Use
;; command-line switch --debug-init to debug errors that occur during startup.

;; (setq debug-on-error t debug-on-quit t force-load-messages t)

(package-initialize)

(let ((my-lowest-version "25.1"))
  (if (version< emacs-version my-lowest-version)
      (error "Sorry, Emacsen prior to major version %s are not supported!" my-lowest-version)))

;; Prepend my personal directories to load-path.  This is idempotent.
(dolist (my-dir '("~/elisp" "~/elisp/fpl-moo" "~/cvs/docbookxml"))
  (add-to-list 'load-path my-dir))

;; Go home.
(if (not (boundp 'my-byte-compiling-all-files))
    (dolist (my-buf '("*scratch*" "*Messages*"))
      (with-current-buffer my-buf
	(cd "~"))))

(defvar my-elisp-load-times nil
  "An alist mapping the name of each of my personal Elisp files to the time at which it
was loaded.  Each element of this list has the form (PATHNAME . TIME), where PATHNAME is
the absolute pathname an Elisp file (with the .el suffix) and TIME is the time it was
loaded.  This is used by function my-check-for-newer-elisp-files.")

(setq my-elisp-load-times nil)  ;; For when this file is reloaded.

(defun my-update-elisp-load-time (pathname)
  "Added to after-load-functions.  Updates my-elisp-load-times to indicate that PATHNAME
has just been loaded.  PATHNAME is always absolute."
  ;; Record the load time of the .el file not the .elc file.  Those are the files I edit.
  (when (file-in-directory-p pathname "~")
    (if (string-match "\\(.*\\.el\\)c$" pathname)
	(setq pathname (match-string 1 pathname)))

    (let ((my-path-and-load-time (assoc pathname my-elisp-load-times)))
      (if my-path-and-load-time
	  (setcdr my-path-and-load-time (current-time))
	(add-to-list 'my-elisp-load-times (cons pathname (current-time)))))))

(add-to-list 'after-load-functions 'my-update-elisp-load-time)

;; Load the rest of my personal Elisp files.
(let ((my-files `("my-constants" "my-compat" "my-misc" "my-autoload" "my-general" "my-frame"
		  "my-visual" "my-dired" "my-mouse" "my-dev" "my-message" "my-hooks"
		  "my-crypt" "my-anon" "my-grep" "my-sc" "my-advice" "my-erc" "my-moo"
		  "my-crumbs" "my-abbrevs" "~/.gnus"
		  ,@(if (eq system-type 'windows-nt) '("my-cygwin"))
		  ;;"my-mods" ;; Must load late!
		  "my-keys")))
  ;; Don't use require here, because when this file is reloaded, I want all of my personal
  ;; Elisp files to reload too, which won't happen with require.
  (mapc 'load my-files))

;; Do interactive startup tasks.
(if (null noninteractive)
    (my-interactive-startup))

;; Local variables:
;; fill-column: 90
;; End:
