;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; my-anon.el
;;
;; SUMMARY
;; -------
;;
;; This package provides functions for sending mail and posting news articles
;; anonymously using Mixmaster, a Type II remailer.  This package uses the
;; external program Mixmaster, which is available from:
;;
;;	http://mixmaster.sourceforge.net/
;;
;;
;; INSTALLATION AND USAGE
;; ----------------------
;;
;; To use this package, put this file in a directory (e.g., ~/elisp) that is
;; named in your Emacs load-path variable, and load it into Emacs.  You can do
;; that by putting code similar to this in your ~/.emacs file:
;;
;;	(add-to-list 'load-path "~/elisp")
;;	(require 'my-anon)
;;
;; Follow the above code with code to change the values of any of the
;; user-visible variables (below).  For instance:
;;
;;	(setq my-anon-mixdir "~/mymix")
;;
;; IMPORTANT: Before you use this package for the first time, you _MUST_ change
;; the values of the following user-visible variables:
;;
;;	my-anon-mixmaster-hash
;;	my-anon-mixconfig-hash
;;	my-anon-pubring-hash
;;
;; These variables contain SHA-1 hashes of the Mixmaster executable, the
;; Mixmaster configuration file, and the Type II remailers keyring file
;; (respectively).  The current values are guaranteed not to work.  When
;; computing the SHA-1 hashes of these files, use the "-b" switch, as follows:
;;
;;	sha1sum -b ~/mix/pubring.mix
;;
;; If you don't (or can't) do this, you cannot use this package.
;;
;;
;; FACES
;; -----
;;
;; The following faces can be configured to change the appearance of the
;; composition buffer:
;;
;;	my-anon-to-face			Controls the "To:" header.
;;	my-anon-subject-face		Controls the "Subject:" header.
;;	my-anon-date-face		Controls the "Date:" header.
;;	my-anon-mixchain-face		Controls the "Chain:" header.
;;	my-anon-mixargs-face		Controls the "Mixmaster args:" header.
;;	my-anon-sent-message-face	Controls the "Sent:" header.
;;	my-anon-note-face		Controls the statistics.
;;
;; For instance, to change the foreground color of the Subject header, put this
;; code into your ~/.emacs file after loading this package:
;;
;;	(set-face-foreground 'my-anon-subject-face "green")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'time-stamp)
(require 'message)
(require 'mail-utils)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User-visible variables.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-anon-mixdir "~/mix"
  "Pathname of the Mixmaster install directory.  This is the value that matters
when using this package, _not_ the value of environment variable MIXPATH.
Environment variable MIXPATH is temporarilly set to this value when this package
runs Mixmaster.  Afterward, it is restored to its original value.")

(defvar my-anon-mixmaster (concat my-anon-mixdir "/mixmaster.exe")
  "Pathname of Mixmaster executable.")

(defvar my-anon-mixconfig (concat my-anon-mixdir "/mix.cfg")
  "Pathname of Mixmaster configuration file.")

(defvar my-anon-pubring (concat my-anon-mixdir "/pubring.mix")
  "Pathname of the Type II remailer keyring.")

(defvar my-anon-mixmaster-hash "eeb0da44db0ccc5bf51139f5c3e695e002d443a2"
  "The sha1sum hash of the Mixmaster executable.")

(defvar my-anon-mixconfig-hash "fa78ca067911fee16c34903d289ebd14b406ea00"
  "The sha1sum hash of the Mixmaster config file.")

(defvar my-anon-pubring-hash "c848a59b2fa4d8cff8def5cf207f5ba85694f00d"
  "The sha1sum hash of the Type II remailer keyring.")

(defvar my-anon-typeII-stats-file (concat my-anon-mixdir "/mlist2.txt")
  "The pathname of the file containing version 2 of the Type II remailer
statistics.  Update this file using a command like this:

    wget -O ~/mix/mlist2.txt http://stats.mixmaster.it/mlist2.txt")

(defvar my-anon-compose-in-new-frame t
  "If non-nil, mail messages and USENET postings are composed in a new frame.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Internal variables.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-foreground (set (make-face 'my-anon-to-face) 'my-anon-to-face) "#faf")
(set-face-foreground (set (make-face 'my-anon-subject-face) 'my-anon-subject-face) "#faf")
(set-face-foreground (set (make-face 'my-anon-date-face) 'my-anon-date-face) "#faf")
(set-face-foreground (set (make-face 'my-anon-mixchain-face) 'my-anon-mixchain-face) "#2f2")
(set-face-foreground (set (make-face 'my-anon-mixargs-face) 'my-anon-mixargs-face) "#2f2")
(set-face-foreground (set (make-face 'my-anon-sent-message-face) 'my-anon-sent-message-face) "#ccc")
(set-face-foreground (set (make-face 'my-anon-note-face) 'my-anon-note-face) "#ccc")

(defvar my-anon-mixlog (concat (getenv "HOME") "/mix/.mixlog")
  "Pathname of the log file to which stdout and stderr from Mixmaster is redirected.
The contents of this file is displayed to the user after Mixmaster processed the outgoing
message.")

(defvar-local my-anon-sent nil
  "Non-nil in a message composition buffer if the message has been sent.")

(defvar my-anon-config-options nil
  "If non-nil, holds dynamically-computed Mixmaster configuration options.
User's should not use this variable -- it's value is overwritten.")

(defvar my-anon-separator
  "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
  "Separator used around help text.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User-visible functions.
;;
;; Users should bind these to keys for convenient access.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-anon-compose-mail ()
  "Compose a new anonymous mail message."
  (interactive)
  (my-anon-compose-message))


(defun my-anon-compose-news ()
  "Compose a new anonymous USENET posting."
  (interactive)
  (my-anon-compose-message 'news))


(defun my-anon-add-date-header (prefix)
  "Adds a Date header to the message being composed.  You are free to edit the
date value.  Interactively, a prefix of C-u means to remove the Date header."
  (interactive "P")

  (save-excursion
    (goto-char (point-min))
    (if (not (search-forward-regexp "^$" nil t))
	(error "Message is malformed!"))

    (if (search-backward-regexp "^Date: " nil t)
	(if (not (equal '(4) prefix))
	    (error "Date header already exists!")
	  ;; Remove the Date header.
	  (beginning-of-line)
	  (let ((inhibit-read-only t))
	    (kill-line 1)))

      (goto-char (point-min))
      (forward-line 2)
      (let ((inhibit-read-only t))
	(insert (propertize "Date:" 'face 'my-anon-date-face 'read-only t)
		(propertize " " 'face 'my-anon-date-face 'read-only t 'rear-nonsticky t)
		(concat (format-time-string "%a") ", " (mail-rfc822-date))
		(propertize "\n" 'read-only t 'rear-nonsticky t))))))


(defun my-anon-send-message (prefix)
  "Sends the anonymous mail message or news posting being composed in the
current buffer.  If the message has a \"To:\" header, it is a mail message,
otherwise it is a news posting."
  (interactive "P")

  (set-buffer-modified-p nil)

  (when prefix
    (let ((my-smtp-host (read-from-minibuffer "Enter SMTP host (default is localhost): ")))
      (setq my-anon-config-options (concat "--SMTPRELAY='" my-smtp-host "'"))))

  (delete-other-windows)

  ;; Make sure there's an SMTP tunnel on localhost port 25.

  (message "Checking for SMTP tunnel ...")
  (if (/= 0 (shell-command "listening | grep -q ' 127\\.0\\.0\\.1:25 '"))
      (error "No SMTP tunnel on localhost:25!"))

  ;; Remove the Sent header (this is needed when resending an already-sent message).

;;  (save-excursion
;;    (goto-char (point-min))
;;    (when (search-forward-regexp "^Sent: " nil t)
;;      (beginning-of-line)
;;      (let ((inhibit-read-only t))
;;	(kill-line 1))
;;      (set-buffer-modified-p nil)))

  (let ((my-to (message-fetch-field "To"))
	(my-newsgroups (message-fetch-field "Newsgroups"))
	(my-subject (message-fetch-field "Subject"))
	(my-chain (message-fetch-field "Chain"))
	(my-mixargs (message-fetch-field "Mixmaster args")))

    (if (not (or my-to my-newsgroups))
	(error "Missing recipient or newsgroup!"))
    (if (not my-subject)
	(error "Missing subject!"))
    (if (not my-chain)
	(error "Missing remailer chain!"))

    ;; Get confirmation if the message has already been sent.

    (when (if my-anon-sent
	      (progn
		(ding)
		(yes-or-no-p "This message has already been sent!  Send again? "))
	    (yes-or-no-p (propertize (if my-to
					 "Really send this mail anonymously? "
				       "Really post this article anonymously? ")
				     'face 'my-alert-face)))

      ;; Perform sanity checks.
      (my-anon-sanity-checks)

      (setq my-anon-sent t)
      (my-anon-extract-and-send (current-buffer) my-to my-newsgroups my-mixargs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Internal functions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-anon-sanity-checks ()
  "Perform sanity and security checks."
  (message "Performing sanity checks ...")

  (if (not (file-directory-p my-anon-mixdir))
      (error "Directory %s not found!" my-anon-mixdir))

  (if (not (file-exists-p my-anon-typeII-stats-file))
      (error "File %s not found!" my-anon-typeII-stats-file))

  (if (> (- (float-time)
	    (float-time (nth 5 (file-attributes my-anon-typeII-stats-file))))
	 (* 24 60 60))
      (error "Remailer statistics are older than one day!  Please update them."))

  (if (not (file-executable-p my-anon-mixmaster))
      (error "File %s not found or not executable!" my-anon-mixmaster))

  (let ((my-hash (car (split-string (shell-command-to-string (concat "sha1sum -b " my-anon-mixmaster))))))
    (if (and (stringp my-hash)
	     (not (string= my-hash my-anon-mixmaster-hash)))
	(error "File %s failed hash check!" my-anon-mixmaster)))
  
  (let ((my-hash (car (split-string (shell-command-to-string (concat "sha1sum -b " my-anon-mixconfig))))))
    (if (and (stringp my-hash)
	     (not (string= my-hash my-anon-mixconfig-hash)))
	(error "File %s failed hash check!" my-anon-mixconfig)))

  (let ((my-hash (car (split-string (shell-command-to-string (concat "sha1sum -b " my-anon-pubring))))))
    (if (and (stringp my-hash)
	     (not (string= my-hash my-anon-pubring-hash)))
	(error "File %s failed hash check!" my-anon-pubring)))
  
  (message "Sanity checks passed."))


(defun my-anon-compose-message (&optional news)
  "Compose an anoymous mail message or news posting.  If optional argument NEWS
is non-nil (interactively, a prefix is supplied), compose a news posting, otherwise
compose a mail message."
  (interactive "P")

  ;; Perform sanity checks.
  (my-anon-sanity-checks)

  ;; Maybe create a new frame to display the composition buffer.
  (if my-anon-compose-in-new-frame
      (select-frame (make-frame)))

  (switch-to-buffer (generate-new-buffer-name (concat "*Anonymous "
						      (if news "Posting*" "Mail*"))))

  (if my-anon-compose-in-new-frame
      (set-frame-name (buffer-name)))

  ;; Prepare the composition buffer's initial contents.
  (my-anon-prepare-buffer news)

  (goto-char (point-min))
  (end-of-line)
  (set-buffer-modified-p nil))


(defun my-anon-prepare-buffer (&optional news)
  "Prepares the message buffer by inserting headers and instructions.  If optional argument
NEWS is non-nil, this is a news posting, otherwise this is a mail message."
  (text-mode)

  ;; Make sure we don't inherit an old value of my-anon-config-options.
  (setq my-anon-config-options nil)

  ;; Turn off backups for this buffer.
  (set (make-local-variable 'backup-inhibited) t)

  (add-hook 'after-save-hook
	    (lambda ()
	      ;; set-file-modes doesn't work on Windows, so we use chmod ...
	      (if my-win32
		  (start-process "anon-chmod" nil "chmod" "600" (buffer-file-name))
		(set-file-modes (buffer-file-name) #o600)) nil 'local))

  (setq buffer-file-name (format "%s/mix/anon%s-%s.txt"
				 (getenv "HOME")
				 (if news "post" "mail")
				 (time-stamp-string "%:y%02m%02d-%02H%02M%02S")))

  (let ((my-anon-mode-map (copy-keymap (current-local-map))))
    (define-key my-anon-mode-map (kbd "C-c C-c") 'my-anon-send-message)
    (define-key my-anon-mode-map (kbd "C-c C-d") 'my-anon-add-date-header)
    (define-key my-anon-mode-map (kbd "C-x C-s") 'my-anon-maybe-save-buffer)
    (use-local-map my-anon-mode-map))

  ;; Build the composition buffer, with some text read-only.
  
  (let ((read-only-rear-nonsticky-space (propertize " " 'read-only t 'rear-nonsticky t))
	(inhibit-read-only t))
    (insert (propertize (if news "Newsgroups:" "To:")
			'face 'my-anon-to-face 'read-only t)

	    read-only-rear-nonsticky-space
	    (propertize "\nSubject:" 'face 'my-anon-subject-face 'read-only t)

	    read-only-rear-nonsticky-space
	    (propertize "\nChain:" 'face 'my-anon-mixchain-face 'read-only t)

	    read-only-rear-nonsticky-space
	    (propertize "\nMixmaster args:" 'face 'my-anon-mixargs-face 'read-only t)

	    read-only-rear-nonsticky-space
	    (propertize
	     (concat "\n\n[The text below will not appear in the sent message.]\n"
		     ;; Only show remailers whose reliability is 50% or greater.
		     (shell-command-to-string (concat "/bin/egrep '[5-90][0-9]\\.[0-9]% ' "
						      my-anon-typeII-stats-file))
		     "\n"
		     my-anon-separator
		     "\nEnter body of mail below.  Type \"C-c C-c\" to send message.\n"
		     "Type \"C-c C-d\" to add a Date header.  "
		     "Type \"C-u C-x C-s\" to save this buffer.\n"
		     my-anon-separator "\n\n")
	     'face 'my-anon-note-face 'read-only t)))

  ;; Set the rear-nonsticky property on the last newline to t.
  (let ((inhibit-read-only t))
    (add-text-properties (1- (point)) (point) '(rear-nonsticky t))))


(defun my-anon-maybe-save-buffer (prefix)
  "..."
  (interactive "P")
  (when (not (equal '(4) prefix))
    (set-buffer-modified-p nil)
    (error "Buffer was not saved!  Type \"C-u C-x C-s\" to force a save."))

  (save-buffer))


(defun my-anon-extract-and-send (msgbuffer to newsgroups mixargs)
  "Extracts a message from the composition buffer and sends it using Mixmaster."
  (let ((my-mixmaster-command))
    (save-excursion
      (with-temp-buffer
	(insert-buffer-substring msgbuffer)
	
	;; Cleanup the message.

	(let ((inhibit-read-only t))
	  (remove-text-properties (point-min) (point-max) '(read-only nil)))

	(goto-char (point-min))
	(if (not (search-forward-regexp "^$" nil t))
	    (error "This message is malformed -- cannot send!")
	  ;; Leave the blank line following the headers.
	  (beginning-of-line 2)
	  (let ((my-start (point)))
	    (if (not (search-forward-regexp (concat "^" my-anon-separator "$") nil t))
		(error "This message is malformed -- cannot send!")
	      (end-of-line)
	      (if (not (search-forward-regexp (concat "^" my-anon-separator "$") nil t))
		  (error "This message is malformed -- cannot send!")
		(forward-line 2))
	      (delete-region my-start (point)))))

	(goto-char (point-min))
	(when (search-forward-regexp "^Mixmaster args:" nil t)
	  (beginning-of-line)
	  (kill-line 1))

	;; Build the Mixmaster command.

	(setq my-mixmaster-command
	      (concat my-anon-mixmaster " -v"
		      (if my-anon-config-options (concat " " my-anon-config-options))
		      (if mixargs (concat " " mixargs))
		      (if newsgroups " -p")
		      " >'" my-anon-mixlog "' 2>&1"))

	;; Now run Mixmaster to send the anonymous message.

	(let ((my-old-mixpath-envvar (getenv "MIXPATH")))
	  (setenv "MIXPATH" my-anon-mixdir)
	  (message "Running: %s" (propertize my-mixmaster-command 'face 'my-alert-face))
	  (unwind-protect
	      (shell-command-on-region (point-min) (point-max) my-mixmaster-command)
	    (setenv "MIXPATH" my-old-mixpath-envvar)))))
  
    ;; Insert Sent: header.
    (goto-char (point-min))
    (when (search-forward-regexp
	   (concat "^\\[The text below will not appear in the sent message.\\]$") nil t)
      (beginning-of-line 2)
      (let ((inhibit-read-only t))
	(insert (propertize (concat "Sent: " (format-time-string "%a") ", " (mail-rfc822-date) "\n")
			    'face 'my-anon-sent-message-face 'read-only t))))

    (set-buffer-modified-p nil)

    (ignore-errors (kill-buffer ".mixlog"))
    (find-file-other-window my-anon-mixlog)
    (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer)
    (delete-file my-anon-mixlog)

    (message (if to "Mailed with: %s" "Posted with: %s")
	     (propertize my-mixmaster-command 'face 'my-alert-face))))
