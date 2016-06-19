;; General Emacs configuration.

(require 'ange-ftp)
(require 'bytecomp)
(require 'arc-mode)  ;; Needed because there's no archive-mode-hook.
(require 'warnings)
(require 'shell)
(require 'browse-url)
(require 'vc-hooks)
(require 'savehist)
(require 'xref)
(require 'tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seed the random number generator.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(random t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Silence byte-compiler warning about set-message-beep not being defined
;; on non-Windows machines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not my-win32)
    (declare-function set-message-beep "w32fns.c" (sound)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Silence the bell on Windows.  Why not everywhere?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if my-win32
    (set-message-beep 'silent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup icomplete mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(icomplete-mode 1)
(if (boundp 'icomplete-prospects-height)
    (setq icomplete-prospects-height 2))

(define-advice icomplete-simple-completing-p (:filter-return (retval) my-ad-filter-return-icomplete-simple-completing-p)
  "Disables icomplete-mode for some commands."
  ;; Must use member instead of memq for this test!
  (if (member this-command '(find-file find-alternate-file write-file dired dired-create-directory
			     dired-do-rename dired-do-copy my-dired my-dired-do-rename-with-suggestion
			     my-dired-do-copy-with-suggestion man find-tag))
      ;; Make icomplete-simple-completing-p return nil to disable icomplete-mode for this command.
      nil
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save places.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(save-place-mode t)
(setq save-place-limit 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure minibuffer history saving between sessions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring my-grep-input-history
        my-background-shell-command-history my-calc-history my-describe-function-history
	my-describe-variable-history my-dired-chmod-input-history my-erc-connect-history
	my-erc-join-channel-history)
      savehist-file "~/.emacs.d/savehist"
      savehist-autosave-interval 60)	;; In seconds.

(savehist-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set environment variable SSH_AUTH_SOCK to a working value or unset it.  We
;; don't use any inherited value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(my-ssh-environment-setup)	;; This function is defined in my-misc.el.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure TRAMP.  IMPORTANT: This must come after the above call to
;; my-ssh-environment-setup.  If SSH_AUTH_SOCK is misconfigured, TRAMP
;; can malfunction or hang.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Speed up TRAMP by putting auto-save files in a local directory instead of
;; writing them back to the remote host.  TRAMP makes this directory if it
;; does not exist.
(setq tramp-verbose 2
      tramp-auto-save-directory "~/.emacs.d/tramp-autosave")

;; On Windows, use method sshx, because method ssh doesn't force SSH to allocate
;; a tty, and because methods scp and scpx don't work.
(if my-win32
    (setq tramp-default-method "sshx"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol properties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable some functions that are disabled by default.

(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left		'disabled nil)
(put 'eval-expression		'disabled nil)
(put 'narrow-to-page		'disabled nil)
(put 'narrow-to-region		'disabled nil)
(put 'downcase-region		'disabled nil)

;; Disable some functions that are enabled by default.

(put 'erase-buffer	'disabled t)	;; Too dangerous.
(put 'set-goal-column	'disabled t)	;; An evil function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; As of Emacs 22.0, dired checks TZ instead of /etc/localtime to find the time
;; zone.

(setenv "TZ" "EST5EDT")

;; This makes ssh and scp work under Emacs when they have to spawn a proxy command
;; (e.g., socat).
(setenv "SHELL" "/bin/bash")

;; Set BASH_ENV so that a shell spawned by shell-command (e.g., "bash -c '...'")
;; will source .bash_profile and .bashrc (the latter because we set UNDER_EMACS
;; below).  QUESTION: Is this needed on UNIX or just on Windows?

(if (null (getenv "BASH_ENV"))
    (setenv "BASH_ENV" (concat (getenv "HOME") "/.bash_profile")))

;; Set UNDER_EMACS to clue ~/.bash_profile that we're running under Emacs.

(setenv "UNDER_EMACS" "1")

;; Set CVS_RSH.  This is set by .bash_profile, but on Windows and when running
;; in Gnome on UNIX, Emacs isn't a descendant of a Bash shell, so it doesn't
;; have this in its environment.

(setenv "CVS_RSH" "ssh")

(when my-win32
  ;; This must always match what I set in .bash_profile, otherwise "M-x man" in
  ;; Emacs won't find the same man pages as the interactive "man" command finds.
  (let ((my-cyghome (getenv "CYGHOME")))
    (setenv "MANPATH" (concat my-cyghome "/man:" my-cyghome
			      "/local/man:/usr/local/man:/usr/local/share/man:/usr/share/man:/usr/man"))))

;; Needed for Man-mode.

(setenv "COLUMNS" "95")

;; Set LANG to "C.ISO-8859-1" so that:
;;
;; 1. Dired mode (i.e., /bin/ls) sorts in a sane way.
;;
;; 2. Dired mode displays timestamps the same as /bin/ls does in a terminal.
;;
;; 3. Dired mode displays ISO Latin 1 characters (e.g., 'é') correctly.

(setenv "LANG" "C.ISO-8859-1")

;; Set environment variable TERM to "emacs" so my shell startup scripts know
;; the shell is spawned by Emacs.

(setenv "TERM" "emacs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar orig-Info-default-directory-list Info-default-directory-list
  "Holds the original value of Info-default-directory-list.")

(if my-win32
    ;; Windows systems.
    (setq w32-quote-process-args	34	;; 34 = '"'.  Why does t not work?
	  w32-recognize-altgr		nil	;; Default is t.

	  ;; On Windows, VC mode cannot run bzr using call-process, because bzr
	  ;; is a Python script not a Windows executable.
	  vc-handled-backends		(delete 'Bzr vc-handled-backends)
	  )

  ;; Non-Windows systems.
  (setq browse-url-browser-function	'browse-url-firefox
	browse-url-firefox-program	"/usr/local/firefox/firefox"))

(add-to-list 'safe-local-eval-forms	'(my-n-column-tabs 2))
(add-to-list 'safe-local-eval-forms	'(my-n-column-tabs 4))
(add-to-list 'safe-local-eval-forms	'(my-n-column-tabs 8))
(add-to-list 'safe-local-eval-forms	'(auto-fill-mode 0))
(add-to-list 'safe-local-eval-forms	'(my-toggle-truncate-lines))
(add-to-list 'safe-local-variable-values '((indent-tabs-mode . nil)))

(setq ange-ftp-binary-file-name-regexp	"."
      ange-ftp-default-user		"ftp"
      ange-ftp-generate-anonymous-password "flitterio@gmail.com"
      ange-ftp-retry-time		20
      ange-ftp-send-hash		t
      ange-ftp-try-passive-mode		t	;; New in version 21.
      ange-ftp-ftp-program-name		(if my-win32
					    (concat my-systemdrive "/apps/cygwin/bin/ftp.exe")
					  "ftp")
      ange-ftp-tmp-name-template	(if my-win32
					    (concat my-systemdrive "/temp/ange-ftp")
					  "/tmp/ange-ftp")
      apropos-do-all			t
      archive-zip-extract		'("unzip" "-p")
      archive-zip-expunge		'("zip" "-d")
      archive-zip-update		'("zip" "-u")
      auth-source-save-behavior		nil
      auto-mode-alist			(append '(("\\.bat\\'" . fundamental-mode)
						  ("\\.vcx?proj\\'" . fundamental-mode)
						  ("\\.cs\\'" . csharp-mode)
						  ("\\.[ch]\\'" . c++-mode)
						  ("\\.idl\\'" . c++-mode)
						  ("\\.asc\\'" . text-mode)
						  ("\\.rb\\'" . ruby-mode)
						  ("\\.mof\\'" . my-mof-mode)
						  ("\\.\\(ism\\|xml\\)\\'" . fundamental-mode))
						auto-mode-alist)
      auto-save-interval		600
      backup-by-copying			(if my-win32 t nil)
      backup-by-copying-when-linked	t
      backup-by-copying-when-mismatch	nil
      backward-delete-char-untabify-method nil
      backup-enable-predicate		(function my-backup-enable-predicate)
      bc-bookmark-limit			128
      buffers-menu-max-size		nil
      byte-compile-verbose		nil
      calc-multiplication-has-precedence nil
      calc-display-sci-low		-9999
      calc-display-sci-high		9999
      confirm-kill-emacs		#'my-confirm-kill-emacs
      comint-scroll-to-bottom-on-input	'this
      completion-ignore-case		t
      completion-ignored-extensions	(delete "CVS/" completion-ignored-extensions)
      completion-styles			(if (boundp 'completion-styles)
					  (delq 'partial-completion completion-styles))
      debugger-bury-or-kill		'kill
      delete-active-region		nil
      diff-switches			"-uw"
      emacs-lisp-docstring-fill-column	90
      enable-recursive-minibuffers	t
      explicit-shell-file-name		"bash"
      font-lock-verbose			nil
      icon-title-format			frame-title-format
      grep-command			"egrep -n "
      help-window-select		t
      hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-line
					 try-expand-line-all-buffers
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill)

      ;; As of Emacs 21, the Elisp reference manual is part of the Emacs CVS repository.
      Info-default-directory-list	`("~/einfo/"
					  ,@(if my-win32
						(list (concat my-systemdrive "/apps/emacs/share/info/")
						      (concat my-systemdrive "/apps/cygwin/usr/local/info/")
						      (concat my-systemdrive "/apps/cygwin/usr/share/info/"))
					      (list "/usr/local/info/"
						    "/usr/local/share/info/"))
					  ,@orig-Info-default-directory-list)

      ;; This next assigment prevents environment variable INFOPATH from being used to
      ;; initialize Info-directory-list.
      Info-directory-list		Info-default-directory-list
      inhibit-startup-message		t
      ;;ispell-command-options		(list "-p" (expand-file-name "~/.ispell-words"))
      ;;ispell-window-configuration	nil
      large-file-warning-threshold	15000000
      list-directory-brief-switches	"-aCF"
      list-directory-verbose-switches	"-alF"
      lpr-command			"a2ps"
      lpr-switches			nil
      Man-notify-method			'newframe
      message-log-max			t
      message-signature			nil
      minibuffer-message-timeout	0.75
      mouse-1-click-follows-link	nil
      next-line-add-newlines		'nil	;; Default in v21.
      read-file-name-completion-ignore-case t
      remote-shell-program		"ssh"	;; Affects ange-ftp-shell-command.
      save-abbrevs			'silently
      scroll-step			20
      search-whitespace-regexp          nil     ;; Make SPC match just one space interactively.
      ;; Here, "-ic" causes bash spawned from Emacs to eat CPU under FreeBSD.
      shell-command-switch		"-c"
      shell-file-name			"bash"
      shell-prompt-pattern		"^.*[:>] "  ;; Used only by TRAMP because comint-use-prompt-regexp is nil.
      split-width-threshold		nil
      temporary-file-directory		(if my-win32
					    (concat my-systemdrive "/temp")
					  "/tmp")
      text-quoting-style		'straight   ;; Don't change 's into Unicode quotes (U+2018, U+2019).
      time-stamp-format			"%:y-%02m-%02d %02H:%02M:%02S %s"
      user-full-name			"Francis Litterio"
      user-mail-address			"flitterio@gmail.com"
      version-control			'never
      warning-suppress-types		(cons '(undo discard-info) warning-suppress-types)
      )

(setq-default fill-column 100
	      buffer-file-coding-system	'undecided-unix
	      cursor-in-non-selected-windows nil)
