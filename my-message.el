;; Configuration file for sending mail and posting news using the message.el
;; package that comes with Gnus.  This code is written so that mail can be sent
;; without requiring Gnus to be started.  Outgoing mail is archived in
;; ~/mail/outgoing, and outgoing news is archived in ~/news/outgoing.

(require 'gnus)
(require 'nntp)
(require 'smtpmail)
(require 'message)
(require 'mm-util)

(setq mail-aliases			t
      mail-signature-file		"~/.signature"
      mail-yank-prefix			"> "
      mail-header-separator
	"== == == == == == == == == == == == == == == == == == == == == == == =="
      message-auto-save-directory	"~/mail" ;; Used only when Gnus isn't running.
      message-default-headers		(concat "Fcc: ~/news/outgoing\n")
      message-deletable-headers		(delq 'Date message-deletable-headers)
      message-directory			"~/news"

      ;; This causes these headers to be stripped from the message before
      ;; posting, but the headers are not stripped so early that they don't have
      ;; their specified effect (e.g., the Fcc header still appends the article
      ;; to my outgoing message file).
      message-ignored-news-headers	"^Fcc:\\|^Sender:"
      message-sendmail-envelope-from	'header
      message-send-mail-function	#'smtpmail-send-it

      ;; 1. Prevent message.el from generating a Sender header.
      ;; 2. Allow lines longer than 79 characters.
      ;; 3. Allow me to post to unknown groups without interactive confirmation.
      message-syntax-checks		'((sender . disabled)
					  (long-lines . disabled)
					  (existing-newsgroups . disabled))
      send-mail-function		'smtpmail-send-it

      smtpmail-debug-info		t

      ;; This next set of variables expects an stunnel client listening on localhost port 25
      ;; and forwarding to smtp.gmail.com port 465.  Create that tunnel with this command:
      ;;
      ;;	tunnel --sslclient 25:smtp.gmail.com:465
      
      smtpmail-local-domain		"gmail.com"
      smtpmail-smtp-server		"localhost"
      smtpmail-smtp-service		25
      smtpmail-smtp-user		"flitterio@gmail.com"

      ;; This next variable doesn't exist in Emacs 24.3.  Need to use ~/.authinfo and
      ;; variable smtpmail-smtp-user instead.
      ;;smtpmail-auth-credentials		'(("localhost" 25 "flitterio@gmail.com" nil))

      ;; Emacs on Windows isn't ready to do STARTTLS for SMTP mail.  Too much code needs
      ;; to change.  Leave this in case one day it's usable.
      ;;
      ;;starttls-process-connection-type	nil	;; nil => pipe, t => pty
      ;;smtpmail-smtp-server		"smtp.gmail.com"
      ;;smtpmail-smtp-service		25
      ;;smtpmail-stream-type		'starttls
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-advice mm-append-to-file (:filter-args (args) my-ad-filter-args-mm-append-to-file)
  "Forces all archived outgoing messages be written using the iso-latin-1-unix coding
system."
  (if (< (length args) 4)
      (add-to-list 'args 'iso-latin-1-unix 'append)
    (setf (nth 3 args) 'iso-latin-1-unix))
  args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-message-mode-hook ()
  "Added to message-mode-hook, which runs when message-mode is turned on in the
message buffer -- the buffer is almost completely empty at this point."
  (font-lock-add-keywords nil '(("^>.*$" 0 'font-lock-comment-face t)
				("^== .* ==$" 0 'font-lock-function-name-face t)))

  (when (facep 'message-header-to-face)
    (set-face-foreground 'message-header-name-face "orange")
    (set-face-bold 'message-header-name-face nil)

    (set-face-foreground 'message-header-to-face "yellow")
    (set-face-bold 'message-header-to-face nil)

    (set-face-foreground 'message-header-cc-face "pink")
    (set-face-bold 'message-header-cc-face nil)

    (set-face-foreground 'message-header-subject-face "lightblue")
    (set-face-bold 'message-header-subject-face nil)

    (set-face-foreground 'message-header-newsgroups-face "yellow")
    (set-face-bold 'message-header-newsgroups-face nil)

    (set-face-foreground 'message-header-other-face "lightgreen")
    (set-face-bold 'message-header-other-face nil)

    ;; This next setting is ignored due to a bug in Gnus.
    (set-face-foreground 'message-separator-face "blue3")
    (set-face-bold 'message-separator-face nil))

  (abbrev-mode 1)
  (modify-syntax-entry ?' "." message-mode-syntax-table)

  (setq fill-column			72
	fill-paragraph-function		(function mail-mode-fill-paragraph)
	indent-tabs-mode		t
	paragraph-start			(concat "-+$\\|" paragraph-start))

  (if (fboundp 'mml-mode)  ;; mml-mode isn't in 20.5 or 20.6.
      (mml-mode 0))	;; Turn off MML minor mode to get back "\M-m" binding.

  (local-set-key "\C-c\C-e" 'my-message-encrypt-body)
  (local-set-key "\C-c\C-d" 'my-message-decrypt-body)
  (local-set-key "\C-c\C-s" 'my-message-clearsign-body)

  ;; This next hack is to cope with the fact that gnus-article-edit-mode is
  ;; derived from message-mode, so this hook runs when I type 'e' to edit an
  ;; article.  Ugh.
  (if (eq major-mode 'gnus-article-edit-mode)
      (local-set-key "\C-c\C-c" 'gnus-article-edit-done)
    (local-set-key "\C-c\C-c" 'my-message-send-and-exit))

  (local-set-key "\C-c\C-f\C-v" (lambda () (interactive) (message-position-on-field "Via")))
  (local-set-key "\C-c\C-f\C-c" 'my-message-insert-cc-to-author)
  (local-set-key "\C-c\C-w" 'my-message-signature)
  (local-set-key "\C-zc" 'my-message-toppost-citation)
  ;;(local-set-key "\C-zf" 'my-message-toggle-from-header)
  (local-set-key "\C-zr" 'my-message-attach-resume))

(add-hook 'message-mode-hook 'my-message-mode-hook)

(defun my-message-setup-hook ()
  "Added to message-setup-hook, a hook run as the last thing when the message
buffer has been initialized, but before yanked text is inserted."

  ;; If mail is being composed in a dedicated frame, make the composition window the
  ;; only window.  This is needed because sometimes message-pop-to-buffer acts like
  ;; switch-to-buffer and sometimes it acts like pop-to-buffer.
  (if (and (string= "Outgoing Message" (cdr (assoc 'name (frame-parameters))))
	   (frame-parameter nil 'my-delete-this-frame))
      (delete-other-windows))

  (my-message-massage-subject)  ;; Frob the Subject header.

  ;; Change the syntax of "'" to punctuation (see my-abbrev.el for why).
  (modify-syntax-entry ?' "." (syntax-table))

  ;; Insert X-Random-Quote header.
  (if (file-exists-p "~/.quotes")
      (save-excursion
	(message-goto-eoh)
	(let ((start (point))
	      (fill-prefix "\t"))
	  (insert "X-Random-Quote: ")
	  (my-random-quote 'insert)
	  (insert "\n")
	  (fill-region start (point)))))

  ;; Frob the From header.
  (let ((from (concat user-mail-address " (Francis Litterio)"))
	eol)

    (if (and (message-news-p)
	     (not (message-mail-p))  ;; Can't forge my email address over SMTP.
	     (save-excursion
	       (save-restriction
		 (goto-char (point-min))
		 (message-narrow-to-headers)
		 (not (string-match-p "\\bgmane\\." (message-fetch-field "Newsgroups"))))))
	;; Non-Gmane news servers don't get my real email address.  Gmane requires it.
	(setq from "Fran <email@not.available>"))

    (message-goto-from)
    (setq eol (point))
    (message-beginning-of-line)
    (delete-region (point) eol)
    (insert from)

    (when (message-mail-p)
      (message-goto-fcc)
      (beginning-of-line)
      (if (looking-at "^Fcc: .*/\\(news\\)/outgoing$")
	  (replace-match "mail" nil nil nil 1))))

  ;; Insert a Date header.  This is not regenerated by message.el, becaue I've
  ;; removed the symbol 'Date from message-deletable-headers.
  
  (message-goto-subject)
  (beginning-of-line)
  (end-of-line)
  (insert (concat "\nDate: " (message-make-date)))

  (my-toggle-font-lock-mode 1)

  ;; Mark buffer not modified.
  (set-buffer-modified-p nil))

(add-hook 'message-setup-hook 'my-message-setup-hook)

(defun my-message-sent-hook ()
  "Bound to message-sent-hook, a hook called after a mail message is sent."
  (delete-auto-save-file-if-necessary 'force)

  ;; Rename most recent SMTP trace buffer to *smtptrace* and kill all
  ;; others.  Does nothing if the first argument names a nonexistent
  ;; buffer.

  (my-message-move-buffer (format "*trace of SMTP session to %s*" smtpmail-smtp-server)
			  "*smtptrace*")

  ;; Arrange for USENET posting buffers to be named "*sent news*" after
  ;; they are sent.  Gnus will change the leading "*" in the buffer name
  ;; to "*sent ".

  (let ((bufname (buffer-name)))
    ;; Is it a news article?
    (if (or (message-news-p)
	    ;; Just in case message-news-p fails to recognize a news article ...
	    (string-match-p "^\\*news on .+\\*" bufname)
	    (string-match-p "^\\*posting on .+\\*" bufname)
	    (string-match-p "^\\*followup to .+\\*" bufname))
	(progn
	  ;; Arrange to keep the last 3 articles posted.
	  (my-message-move-buffer "*sent news 2*" "*sent news 3*")
	  (my-message-move-buffer "*sent news 1*" "*sent news 2*")
	  (my-message-move-buffer bufname "*news 1*"))

      ;; Is it a mail message?
      (if (or (message-mail-p)
	      ;; Just in case message-mail-p fails to recognize a mail message ...
	      (string-match-p "^\\*mail\\*" bufname)
	      (string-match-p "^\\*reply to .+\\*" bufname))
	  (progn
	    ;; Arrange to keep the last 3 mail messages sent.
	    (my-message-move-buffer "*sent mail 2*" "*sent mail 3*")
	    (my-message-move-buffer "*sent mail 1*" "*sent mail 2*")
	    (my-message-move-buffer bufname "*mail 1*")))))

  ;; Bury the archive buffers.

  (if (get-buffer "sent-mail")
      (bury-buffer "sent-mail"))
  (if (get-buffer "sent-usenet")
      (bury-buffer "sent-usenet")))
    
(add-hook 'message-sent-hook 'my-message-sent-hook)

(defun my-message-insert-citation-line ()
  "Function that inserts a simple citation line.  See variable
message-citation-line-function."
  (when message-reply-headers
    (if (and (string-match-p "^nnfolder:" gnus-newsgroup-name)
	     (not (string-match-p "followup" (symbol-name this-command)))) ;; Hack!
	(insert "You wrote:\n\n")
      (let ((name (mail-header-from message-reply-headers)))
	(if (string-match "^\"?\\(.*?\\)\"?\\s-*<.*@.*>" name)
	    (setq name (match-string 1 name))
	  (if (string-match ".*@.*?\\s-*(\\(.*\\))" name)
	      (setq name (match-string 1 name))))
	(insert name " wrote:\n\n")))))

(setq message-citation-line-function #'my-message-insert-citation-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-hook functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-message-compose-emacs-bug-report (&optional prefix)
  "Compose a new Emacs bug report in a new frame.  Interactively, a prefix of C-u
means not to create a new frame."
  (interactive "P")
  (my-message-mail prefix)
  (message-goto-to)
  (insert "bug-gnu-emacs@gnu.org")
  (message-goto-subject))

(defun my-message-toppost-citation ()
  "Switches the current reply to using 'top post' style citation."
  (interactive)
  (message-goto-body)
  (if (not (looking-at-p "^You wrote:$"))
      (error "Can't find start of citation!")

    (beginning-of-line)
    (delete-region (point) (point-max))
    (let ((message-yank-prefix "")
	  (message-yank-cited-prefix ""))
      (insert "\n\n-----Original Message-----\n"
	      "From: " (mail-header-from message-reply-headers) "\n"
	      "Subject: " (mail-header-subject message-reply-headers) "\n"
	      "Date: " (mail-header-date message-reply-headers) "\n\n")
      (my-with-advice ((my-message-insert-citation-line ignore))
	(message-yank-original)))
    (message-goto-body)))

(defun my-message-attach-resume ()
  "Attached my resume at point."
  (interactive)
  (if (not (bolp))
      (ignore-errors (forward-line 1)))
  (mml-attach-file "~/info/personal/franl/resume.doc" "application/msword"
		   "Francis Litterio's resume"))

(defun my-message-mail2news (&optional gateway)
  "Composes a mail message to the specified mail2news GATEWAY (default is
mail2news_nospam@dizum.com)"
  (interactive)
  (my-message-mail)
  (font-lock-mode 0)	;; Turn off font-lock mode so below faces show up.
  (message-goto-to)
  (insert (or gateway "mail2news_nospam@dizum.com"))
  (message-goto-subject)
  (save-excursion (insert (concat "\nNewsgroups: "
				  (propertize "<newsgroup list>" 'face 'my-alert-face)))))

(defun my-message-encrypt-body ()
  "Encrypts the body of the current message using function my-crypt-encrypt-region."
  (interactive)
  (message-goto-body)
  (my-crypt-encrypt-region (point) (point-max)))

(defun my-message-decrypt-body ()
  "Decrypts the body of the current message using function my-crypt-decrypt-region."
  (interactive)
  (message-goto-body)
  (my-crypt-decrypt-region (point) (point-max)))

(defun my-message-clearsign-body ()
  "Clearsigns the body of the current message using function my-crypt-clearsign-region."
  (interactive)
  (message-goto-body)
  (my-crypt-clearsign-region (point) (point-max)))

(defun my-message-insert-cc-to-author ()
  "Inserts a Cc: header containing the email address of the author of the
message to which this message is a reply."
  (interactive)
  (if (message-news-p)
      (save-excursion
	(message-goto-cc)
	(insert (mail-header-from message-reply-headers)))
    (message-goto-cc)))

(defun my-message-mail (&optional prefix)
  "Calls message-mail in a new frame.  Interactively, a prefix of C-u means don't
create a new frame."
  (interactive "P")
  (if (equal prefix '(4))
      (message-mail)
    (my-frame-call-in-new-frame (function message-mail)
				'((name . "Outgoing Message")))))

(defun my-message-signature (prefix)
  "Inserts either ~/.signature or ~/.signature-news at point, depending on
whether the current message is email or a news posting."
  (interactive "P")
  (let ((mail-sig-file mail-signature-file)
	(news-sig-file "~/.signature-news")
	(which-file nil))
    (save-excursion
      (goto-char (point-min))
      (if (and (not prefix)
	       (search-forward-regexp "^Newsgroups: " 1000 t)
	       ;;(not (looking-at-p "gmane\\."))
	       ;;(not (looking-at-p "wstd\.[a-z.]+$"))
	       )
	  (setq which-file news-sig-file)
	(setq which-file mail-sig-file)))

    (save-excursion
      (insert-file-contents which-file))))

;;(defun my-message-toggle-from-header ()
;;  "Toggles the From: header between various values."
;;  (interactive)
;;  (save-excursion
;;    (goto-char (point-min))
;;    (if (not (search-forward-regexp "^From: " 1000 t))
;;	(error "No From: header in this message!")
;;
;;      ;; Found a From header ...
;;      (if (eolp)
;;	  ;; From header is empty ...
;;	  (insert "Francis Litterio <flitterio@gmail.com>")
;;
;;	;; Rotate From header ...
;;	(if (looking-at-p ".* <franl@\\(world\\.std\\|theworld\\)\\.com>")
;;	    (progn
;;	      (kill-line)
;;	      (insert "Francis Litterio <flitterio@gmail.com>"))
;;	    (kill-line)
;;	    (insert "Francis Litterio <franl@TheWorld.com>"))))))

(defun my-message-send-and-exit ()
  "Calls message-send-and-exit, then decides whether to delete the current frame."
  (interactive)
  (let* ((message-mail-p)
	 (outgoing-archive (concat "~/"
				   (if (message-mail-p) "mail" "news")
				   "/outgoing"))
	 (temporarilly-desocksify (and (message-mail-p) my-socks-in-use)))

    (if (and (get-buffer "outgoing")
	     (equal (file-attributes outgoing-archive)
		    (file-attributes (buffer-file-name (get-buffer "outgoing")))))
	(error "There is a buffer visiting %s!  Message not sent." outgoing-archive))

;;    (when (message-mail-p)
;;      ;; Make sure there's an SMTP tunnel on localhost port 25.
;;      (message "Checking for SMTP tunnel ...")
;;      (if (/= 0 (shell-command "listening | grep -q ' 127\\.0\\.0\\.1:25 '"))
;;	  (error "No SMTP tunnel on localhost:25!")))

    (unwind-protect
	(progn
	  (if temporarilly-desocksify
	      (fset 'open-network-stream (symbol-function 'socks-original-open-network-stream)))

	  (if (not (yes-or-no-p "Really send this message? "))
	      (error "Message not sent!"))

	  ;; Set user-mail-address to the string specified in the From: header
	  ;; of the mail composition buffer.  Arrange to restore it to
	  ;; whatever value it had before this message was sent (that task is
	  ;; completed by my-message-send-hook, which runs after the message
	  ;; is sent).

	  (let ((from-header (my-message-get-header "From"))
		(via-header (my-message-get-header-and-delete "Via")))
	    (set-text-properties 0 (length from-header) nil from-header)
	    (set-text-properties 0 (length via-header) nil via-header)

	    (let ((user-mail-address (or from-header user-mail-address))
		  (smtpmail-smtp-server (or via-header smtpmail-smtp-server)))

	      (if (and (message-send-and-exit)
		       (frame-parameter nil 'my-delete-this-frame))
		  (delete-frame)))))

      (if temporarilly-desocksify
	  (fset 'open-network-stream (symbol-function 'socks-open-network-stream))))))

(defun my-message-massage-subject ()
  "Massages the Subject: header of an outgoing mail message so that it
has at most one leading \"Re:\" string."
  (save-excursion
    (message-goto-subject)
    (beginning-of-line)
    (save-match-data
      (if (looking-at "Subject:[ \t]+\\([Rr]e:[ \t]*\\)+")
	  (replace-match "Subject: Re: ")))))

(defun my-message-get-header (header &optional delete)
  "Returns nil if HEADER doesn't exist, otherwise contents of the header.
If optional argument DELETE is non-nil, delete the header too."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-fetch-field header))))

(defun my-message-get-header-and-delete (header)
  "Returns nil if HEADER doesn't exist.  Otherwise, this function returns the
contents HEADER and deletes the header."
  (let ((header (my-message-get-header header)))
    (when header
      (goto-char (point-min))
      (when (search-forward-regexp (concat "^" header ":") nil t)
	(beginning-of-line)
	(let (bol (point))
	  (forward-line)
	  (delete-region bol (point)))))
    header))

(defun my-message-move-buffer (existing new)
  "Like the UNIX \"mv\" command.  Renames buffer named EXISTING to be named NEW, destroying
buffer NEW if it exists.  Arguments are names of buffers.  Buffer EXISTING is also marked
unmodified and buried."
  (let ((existingbuf (get-buffer existing)))
    (when (bufferp existingbuf)
      (if (get-buffer new)
	  (kill-buffer new))
      (bury-buffer existingbuf)
      (with-current-buffer existingbuf
	(set-buffer-modified-p nil)
	(rename-buffer new)))))
