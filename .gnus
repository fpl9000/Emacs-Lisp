;; -*- Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My personal configuration file for Gnus.
;;
;; Things To Do:
;;
;; o My echo area message doesn't appear if the drafts group has one article
;;   with no subject.
;;
;; o Make it so this file can be byte-compiled without being loaded first.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (null (assoc-string gnus-version-number '("5.11" "5.13")))
    (error "Unsupported Gnus version: %s!" gnus-version-number))


(require 'gnus)
(require 'gnus-cache)
(require 'gnus-score)
(require 'gnus-async)
(require 'gnus-group)
(require 'gnus-sum)
(require 'gnus-art)
(require 'nntp)
(require 'canlock)
(if window-system
    (require 'gnus-cite))		;; For citation face definitions.

;; Silence byte-compiler warnings about some free variables.
(eval-when-compile
  (defvar gnus-tmp-qualified-group)
  (defvar nntp-connection-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Where to get the news.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gnus-select-method '(nntp "news.gmane.org"))

(setq gnus-secondary-select-methods '((nntp "aioe.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move this section further down eventually.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gnus-directory "~/news/")

(defvar my-gnus-frame-parameters
  `((height . ,(if (<= (x-display-pixel-height) 1024) 68 77))
    (width . ,(if (<= (x-display-pixel-height) 1024) 127 127))
    (font . ,(if my-win32
		 "sixbyten"
	       ;; X's 6x10 font ...
	       "-Misc-Fixed-Medium-R-Normal--10-100-75-75-C-60-ISO8859-1"))
    (name . "gnus"))
    "The frame-parameters used when created the Gnus frame.")

(defun my-start-gnus (&optional prefix)
  "Calls function gnus with gnus-batch-mode bound to t to silence the y/n
question about continuing if the NNTP server cannot be contacted.  If
window-system is non-nil, creates and selects a new frame (after a two second
delay).  Interactively, a prefix means to just read mail and not connect to
an NNTP server."
  (interactive "P")
  (if (not (file-exists-p "~/.gnus"))
      (error "~/.gnus does not exist!"))

  (if (and (file-exists-p "~/.gnus.elc")
	   (file-newer-than-file-p "~/.gnus" "~/.gnus.elc"))
      (error "~/.gnus is newer than ~/.gnus.elc -- Gnus not started!"))

  (let* ((groupbuf (get-buffer "*Group*"))
	 (gnus-batch-mode t)
	 (just-mail prefix))
    (if (and groupbuf (> (buffer-size groupbuf) 10))
	(error "Gnus seems to be started already!"))

    (if (and (not my-socks-in-use)
	     (not (y-or-n-p "Not using SOCKS!  Really start Gnus? ")))
	(error "Gnus startup aborted!"))
    
    (when (and window-system
	       (not (string= "gnus" (frame-parameter nil 'name))))
      (sit-for 2 nil)
      (select-frame (make-frame my-gnus-frame-parameters))
      (modify-frame-parameters nil '((my-gnus-dedicated-frame . t)))
      (my-frame-center))

    (if just-mail
	(gnus-no-server)
      (gnus))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq canlock-password			"stop making sense"
      canlock-password-for-verify	canlock-password
      gnus-agent			nil
      gnus-article-sort-functions	'(gnus-article-sort-by-date)
      gnus-asynchronous			nil
      gnus-auto-expirable-newsgroups	"^nndraft:"
      gnus-auto-center-summary		nil
      gnus-auto-select-first		nil
      gnus-auto-select-next		nil
      gnus-check-new-newsgroups		nil
      gnus-default-article-saver	'gnus-summary-save-in-file
      gnus-dribble-directory		"~/news"
      gnus-extra-headers		nil
      gnus-goto-next-group-when-activating nil
      gnus-group-goto-unread		nil
      gnus-group-sort-function		#'gnus-group-sort-by-real-name
      gnus-ignored-from-addresses	nil	;; Show my From: address.
      gnus-large-newsgroup		100
      gnus-message-archive-group	nil
      gnus-message-archive-method	nil
      ;;gnus-nntp-server		nil ;; Obsolete: use gnus-select-method instead.
      gnus-novice-user			nil
;; Is this needed?
;;    gnus-parameters			'(("." (display . default)))
      gnus-permanently-visible-groups	"."
      gnus-prompt-before-saving		t
      gnus-read-active-file		nil
      gnus-save-all-headers		nil
      gnus-save-killed-list		nil
      gnus-saved-headers		"^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Sender:\\|^To:\\|^Gnus-Warning:"

      gnus-summary-goto-unread		nil
      gnus-thread-ignore-subject	t
      gnus-thread-indent-level		2
      gnus-thread-sort-functions	'(gnus-thread-sort-by-number
					  gnus-thread-sort-by-date)
      gnus-treat-fill-long-lines	nil
      gnus-treat-display-smileys	nil
      gnus-uncacheable-groups		"^nnml\\|^nnmbox\\|^nndraft\\|^nnfolder"
      gnus-use-cache			t	;; All ticked articles are cached.
      gnus-use-cross-reference		nil	;; Show cross-posted articles in every group.
      gnus-use-full-window		nil	;; See my-gnus-summary-mode-hook.

      ;; Scoring stuff ...
      gnus-use-scoring			t
      gnus-kill-files-directory		"~/news/"
      gnus-score-expiry-days		nil	;; Never expire score file entries.
      gnus-home-score-file		"~/news/all.SCORE"
      gnus-summary-expunge-below	0  ;; Hide all articles scored below 0.
;;    mail-sources			(if my-win32
;;					    '((file :path (concat my-systemdrive "/franl/.mail")
;;						    :prescript (concat my-systemdrive "/franl/bin/win32/mymovemail %t")))
;;					  (if (eq system-type 'berkeley-unix)
;;					      '((file :path "/var/mail/franl"))
;;					    '((file :path "/var/spool/mail/franl"))))
;;    mail-source-delete-incoming	t
      message-courtesy-message		"[This is a courtesy copy of an article that has been posted to\n these newsgroups: %s.]\n\n"
      message-shoot-gnksa-feet		'(multiple-copies  ;; Send multiple copies.
					  cancel-messages) ;; Cancel others' messages.
;;    my-gnus-ask-when-deleting-mail-with-attachments nil
      mm-decrypt-option			'never
      mm-discouraged-alternatives	'("image/*" "text/html" "text/richtext")
      mm-text-html-renderer		'lynx
      mm-uu-configure-list		'((pgp-signed . disabled))
;;    nnmail-crosspost			nil	;; Only the first split method applies.
;;    nnmail-extra-headers		gnus-extra-headers
;;    nnmail-expiry-wait		'immediate
;;    nnmail-tmp-directory		"/tmp"
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gnus-article-mode-line-format	"%~(form (my-gnus-mode-line-string))@"
      gnus-emphasis-alist		'(("\\(^\\|\\s-\\)\\([_*][a-z0-9]+[_*]\\)"
					   2 2 my-gnus-article-emphasis-face))
      gnus-group-line-format		"%S%p%P%5y %G %uD\n"
      gnus-group-mode-line-format	"%~(form (my-gnus-mode-line-string))@"
      gnus-summary-dummy-line-format	"                    . . . . . . . . . . . . . %S\n"
      gnus-summary-line-format		"%U%R %d %I%uF %s\n"	;; "%U%R %d %I%-25,25f %s\n"
      gnus-summary-make-false-root	'dummy
      gnus-summary-mode-line-format	"%~(form (my-gnus-mode-line-string))@"
      gnus-treat-buttonize-head		nil
      gnus-treat-highlight-signature	nil
      gnus-unseen-mark			32	;; Space.
      gnus-visual			'(summary-highlight article-highlight))

(when window-system
  ;; Group buffer faces ...
  (mapatoms #'(lambda (sym)
		(if (and (facep sym)
			 (string-match "^gnus-group-.*-face$" (symbol-name sym)))
		    (set-face-bold sym nil))))

  ;; Summary buffer faces ...
  (set-face-foreground 'gnus-summary-normal-read-face nil)
  (set-face-foreground 'gnus-summary-cancelled-face "#888")

  ;; Article faces ...
  (make-face 'my-gnus-article-emphasis-face)
  (set-face-foreground 'my-gnus-article-emphasis-face "#ff0")
  (setq gnus-article-button-face 'my-button-face)
  (setq widget-button-face 'highlight) ;; Maybe should be set locally in Article mode?
  (set-face-foreground 'gnus-header-name-face "#ffe0a0")
  (set-face-foreground 'gnus-header-content-face "#4d7")
  (set-face-foreground 'gnus-header-from-face "#ff6")
  (set-face-foreground 'gnus-header-subject-face "#ff6")
  (set-face-foreground 'gnus-header-newsgroups-face "#cde")

  ;; Citation faces ...
  (set-face-foreground 'gnus-cite-face-1 "#9ff")
  (set-face-foreground 'gnus-cite-face-2 "#8f8")
  (set-face-foreground 'gnus-cite-face-3 "lightsalmon")
  (set-face-foreground 'gnus-cite-face-4 "pink")
  (set-face-foreground 'gnus-cite-face-5 "#f0c")
  (set-face-foreground 'gnus-cite-face-6 "#9ff")
  (set-face-foreground 'gnus-cite-face-7 "#8f8")
  (set-face-foreground 'gnus-cite-face-8 "lightsalmon")
  (set-face-foreground 'gnus-cite-face-9 "pink")
  (set-face-foreground 'gnus-cite-face-10 "#f0c")
  (set-face-foreground 'gnus-cite-face-11 "#9ff")
  
  (set-face-background 'mm-uu-extract "#224"))

(defun my-morph-for-gnus ()
  "If running under a window system, morphs the selected frame to have the size
and position that I prefer for reading Gnus."
  (interactive)
  (when window-system
    (modify-frame-parameters (selected-frame) my-gnus-frame-parameters)
    (my-frame-center)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'gnus-save-quick-newsrc-hook 'my-turn-off-backup-this-buffer)
(add-hook 'gnus-save-standard-newsrc-hook 'my-turn-off-backup-this-buffer)

(defun my-gnus-group-mode-hook ()
  "Added to gnus-group-mode-hook."
  (setq default-directory "~/")

  (add-hook 'post-command-hook 'my-gnus-group-post-command-hook nil 'local)

  (local-set-key "/" 'my-gnus-group-respool-all)
  (local-set-key "~" 'my-gnus-group-cleanup)
  (local-set-key "a" 'my-gnus-group-post-news)
  (local-set-key "C" 'my-gnus-group-close-nntp-server)
  (local-set-key "F" 'my-gnus-group-find-new-groups)
  (local-set-key "g" 'my-gnus-group-get-new-news)
  (local-set-key (kbd "M-g") 'my-gnus-group-get-new-news-this-section)
  (local-set-key (kbd "M-C-g") 'gnus-group-get-new-news-this-group)
  (local-set-key "k" 'my-gnus-kill-nntp-process)
  (local-set-key "K" 'my-gnus-kill-all-nntp-processes)
  (local-set-key "m" 'my-gnus-group-mail)
  (local-set-key "n" 'gnus-group-next-group)
  (local-set-key "o" '(lambda () (interactive)
			(let ((proc (gnus-check-group (gnus-group-group-name))))
			  (if (and (processp proc)
				   (eq 'open (process-status proc)))
			      (message "Opened NNTP server '%s'!" (process-name proc))
			    (message "Failed to open NNTP server!")))))
  (local-set-key "p" 'gnus-group-prev-group)
  (local-set-key "q" 'my-gnus-group-exit-kill-frame)
  (local-set-key "Q" 'gnus-group-exit)
  (local-set-key "=" 'delete-other-windows)
  (local-set-key "\C-x\C-s" '(lambda () (interactive) (error "Huh?!")))
  (local-set-key "\M-<" '(lambda () (interactive) (beginning-of-buffer) (gnus-goto-colon)))
  (local-set-key "\M->" '(lambda () (interactive) (end-of-buffer) (gnus-group-prev-group 1)))
  (local-set-key "\M-a" 'my-scroll-up-one-line)
  (local-set-key "\M-s" 'my-scroll-down-one-line)
  (local-set-key "\M-n" (lambda () (interactive) (gnus-group-next-group 16)))
  (local-set-key "\M-p" (lambda () (interactive) (gnus-group-prev-group 16)))
  (local-set-key [down] 'gnus-group-next-group)
  (local-set-key [up] 'gnus-group-prev-group)
  ;;(local-set-key [down-mouse-1] 'gnus-mouse-pick-group)
  )

(add-hook 'gnus-group-mode-hook 'my-gnus-group-mode-hook)

(defun my-gnus-summary-mode-hook ()
  "Added to gnus-summary-mode-hook."
  (add-hook 'post-command-hook 'my-gnus-summary-post-command-hook nil 'local)
  (add-hook 'after-change-functions 'my-gnus-fontify-summary-buffer t 'local)  

  (set (make-local-variable 'scroll-conservatively) 0)

  (local-set-key (kbd "<return>") '(lambda () (interactive) (gnus-summary-scroll-up 2)))
  (local-set-key (kbd "<S-return>") '(lambda () (interactive) (gnus-summary-scroll-down 2)))
  (local-set-key "a" 'my-gnus-summary-post-news)
  (if (my-gnus-group-is-mailgroup)
      (local-set-key "C" 'gnus-summary-copy-article)
    (local-set-key "C" 'my-gnus-summary-cancel-article))
  (local-set-key "d" 'my-gnus-summary-d-command)
  (local-set-key "D" 'gnus-summary-mark-as-read-forward)
  (local-set-key "E" 'my-gnus-summary-expire-articles-now)
  (local-set-key "f" 'my-gnus-summary-followup-with-original)
  (local-set-key "F" 'my-gnus-summary-mail-forward)
  (local-set-key "g" 'gnus-summary-rescan-group)
  (local-set-key "h" 'gnus-summary-toggle-header)
  (local-set-key "k" 'my-gnus-summary-kill-thread)
  (local-set-key "l" 'my-gnus-summary-activate-widget)
  ;; This conflicts with the score lowering command.
  ;;(local-set-key "L" 'my-gnus-summary-follow-link-by-number)
  (local-set-key "m" 'my-gnus-group-mail) ;; No summary version!
  (local-set-key "M" 'gnus-summary-move-article)
  (local-set-key "n" 'my-gnus-summary-next-subject)
  (local-set-key "N" 'my-gnus-summary-next-thread)
  (local-set-key "p" 'my-gnus-summary-prev-subject)
  (local-set-key "P" 'my-gnus-summary-prev-thread)
  (local-set-key "r" 'my-gnus-summary-reply-with-original)
  (local-set-key "R" 'my-gnus-summary-wide-reply-with-original)
  (local-set-key "S" 'my-gnus-summary-report-gmane-spam)
  (local-set-key "t" 'gnus-summary-next-thread)
  (local-set-key "u" 'my-gnus-summary-clear-mark-forward)
  (local-set-key "v" 'gnus-article-inline-part)
  (local-set-key "w" 'gnus-article-fill-long-lines)
  (local-set-key "x" 'my-gnus-summary-limit-to-unread)
  (local-set-key " " 'my-gnus-summary-next-page)
  (local-set-key "=" 'my-gnus-summary-expand-window)
  (local-set-key "~" 'my-gnus-summary-toggle-rot13)
  (local-set-key "`" 'my-gnus-summary-goto-thread-root)
  (local-set-key "," 'my-gnus-summary-next-article-in-thread)
  (local-set-key "." 'gnus-summary-beginning-of-article)
  (local-set-key "!" 'my-gnus-summary-tick-article)
  (local-set-key "\M-!" 'my-gnus-summary-tick-thread)
  (local-set-key "\C-cf" 'my-gnus-summary-copy-from-header)
  (local-set-key "\C-h" 'gnus-summary-prev-page)
  (local-set-key (kbd "<S-return>") 'gnus-summary-scroll-down)
  (local-set-key "\M-+" 'my-gnus-summary-print-article)
  (local-set-key "\M-a" 'my-scroll-up-one-line)
  (local-set-key "\M-r" 'gnus-summary-show-raw-article)
  (local-set-key "\M-s" 'my-scroll-down-one-line)
  (local-set-key "\C-x\C-s" '(lambda () (interactive) (error "Huh?!")))
  (local-set-key (kbd "C-z k a") 'my-gnus-summary-kill-by-author)
  (local-set-key (kbd "C-z k s") 'my-gnus-summary-kill-by-subject)
  (local-set-key "\M-n" (lambda () (interactive) (gnus-summary-next-subject 16)))
  (local-set-key "\M-p" (lambda () (interactive) (gnus-summary-prev-subject 16)))
  (local-set-key [down] 'gnus-summary-next-subject)
  (local-set-key [up] 'gnus-summary-prev-subject)
  ;;(local-set-key [down-mouse-1] 'gnus-mouse-pick-article)
  )

(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-mode-hook)

(defun my-gnus-article-mode-hook ()
  "Added to gnus-article-mode-hook."
  (local-set-key "\M-+" 'gnus-article-summary-command)
  (local-set-key "\M-a" 'my-scroll-up-one-line)
  (local-set-key "\M-s" 'my-scroll-down-one-line))

(add-hook 'gnus-article-mode-hook 'my-gnus-article-mode-hook)

(defun my-gnus-subscribe-new-group (groupname)
  "Function assigned to gnus-subscribe-newsgroup-method.  This is called once
for each new newsgroup.  See also my-gnus-started-hook."
  (let ((list-buffer (get-buffer-create "*New Newsgroups*")))
    (with-current-buffer list-buffer
      (goto-char (point-max))
      (when (= (buffer-size) 0)
	(let ((foo-mode-map (make-sparse-keymap)))
	  (define-key foo-mode-map "q" #'my-kill-buffer-and-window)
	  (use-local-map foo-mode-map))
	(insert "The following new newsgroups were detected.\n"
		"Each has been subscribed as a zombie group.\n\n"))
      (insert (concat "	" groupname "\n"))
      (gnus-subscribe-zombies groupname))))

(setq gnus-subscribe-newsgroup-method 'my-gnus-subscribe-new-group)

(defvar my-gnus-keepalive-timer nil
  "Holds the timer object for sending keepalive commands to the NTTP server.")

(defun my-gnus-started-hook ()
  "Added to gnus-started-hook.  See also function my-gnus-subscribe-new-group."
  (fset 'gnus-summary-recenter #'my-gnus-summary-recenter)

  (my-gnus-show-new-groups)		;; Show new groups.
  (gnus-group-prev-group 99)		;; Position cursor on first group.

  ;; Start the keepalive timer.  This timer is cancelled in my-gnus-after-exiting-gnus-hook.
  (setq my-gnus-keepalive-timer (run-at-time 10 30 'my-gnus-keepalive))
  
  ;; This is a hack to make the Gnus frame display its contents.  Try to remove this
  ;; some time.
  (when (frame-parameter nil 'my-gnus-dedicated-frame)
    (let ((gnus-frame (selected-frame)))
      (run-at-time 1 nil `(lambda () (with-selected-frame ,gnus-frame
				       (redisplay 'force)))))))

(add-hook 'gnus-started-hook 'my-gnus-started-hook)

(make-variable-buffer-local
 (defvar my-gnus-article-is-raw nil
   "This variable is local to each Summary buffer.  If non-nil, the current article
is displayed in raw form."))

(defun my-gnus-mark-article-hook ()
  "Added to gnus-mark-article-hook.  If used in the drafts group, it unmarks
articles as they are read.  Despite the name of this function (and its hook), it
is called when an article is first displayed -- not when it is marked."
  (setq my-gnus-article-is-raw nil)
  (if (string= "nndraft:drafts" gnus-newsgroup-name)
      (progn
	(gnus-summary-mark-article-as-unread ? )
	(gnus-summary-tick-article nil 'untick))))

(add-hook 'gnus-mark-article-hook 'my-gnus-mark-article-hook 'append)

(defun my-gnus-summary-prepare-hook ()
  "Added to gnus-summary-prepare-hook.  In news groups, finds articles by me
and ticks them.  Also collapses multiple spaces in the subject."
  (my-gnus-delete-compile-log-buffer)	;; Why is this done???

  (if (string-match "^\\([^:]+\\|nntp\\+.*\\)$" gnus-newsgroup-name)
      (save-excursion

	;; Tick articles by me.
	(goto-char (point-min))
	(while (search-forward-regexp "Fran.*Litterio" nil t)
	  (if (not (= ?O (gnus-summary-article-mark)))
	      (let ((here (point)))
		(my-gnus-summary-tick-article)
		;; Ticking advances point to next message header, so go back.
		(goto-char here)
		(end-of-line))))

	;; Collapse multiple spaces in the subject.
	(goto-char (point-min))
	(while (= 0 (gnus-summary-next-subject 1))
	  (move-to-column 20)
	  (skip-chars-forward " ")
	  (goto-char (next-single-char-property-change (point) 'my-gnus-author-face))
	  (skip-chars-forward " ")

	  ;; We are now either on the first character of the subject or at the end of line.
	  (if (and (not (eolp))
		   (looking-at ".*  "))
	      (let ((subject-start (point))
		    (inhibit-read-only t)
		    ;;(query-replace-skip-read-only t)  ;; Shouldn't be necessary.
		    ;; This next binding prevents my fontification code from running when
		    ;; replace-regexp changes the Summary buffer.
		    (after-change-functions nil))
		(end-of-line)
		(save-restriction
		  (narrow-to-region subject-start (point))
		  (save-excursion
		    (goto-char (point-min))
		    (while (re-search-forward "  +" nil t)
		      (replace-match " "))))))))))

(add-hook 'gnus-summary-prepare-hook 'my-gnus-summary-prepare-hook)

(defun my-gnus-summary-prepared-hook ()
  "Added to gnus-summary-prepared-hook.  Positions on the first unread subject line.
Also performs special processing for the drafts and nndoc...world-mail.txt groups."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (move-to-column 3)
      (let ((inhibit-read-only t)
	    (start (point)))
	(move-to-column 19)
	(add-text-properties start (point) '(face font-lock-variable-name-face)))
      (forward-line 1)))

  ;; I do this because putting ("^nndoc" (display . [unseen])) into
  ;; gnus-parameters doesn't work for some reason.
  (if (and (null current-prefix-arg)
	   (string-match "^nndoc\\+.*world-mail.txt$" gnus-newsgroup-name))
      (gnus-summary-limit-to-marks "OKrRE" 'reverse))

  (if (null (gnus-summary-article-mark))
      (gnus-summary-exit)

    (if (= 32 (gnus-summary-article-mark)) ;; 32 is space (i.e., no mark).
	(gnus-summary-next-unread-subject 0)
      (goto-char (point-min))
      (gnus-summary-next-unread-subject 1))

    (when (/= 32 (gnus-summary-article-mark)) ;; 32 is space (i.e., no mark).
      (goto-char (point-max))
      (gnus-summary-next-unread-subject -1))

    (when (and (string= gnus-newsgroup-name "nndraft:drafts")
	       (> (buffer-size) 30))
      (message (propertize (concat "\n  Type \"e\" to edit a draft, "
				   "\"D s\" to send one without editting. \n\n")
			   'face 'my-alert-face2)))))

(add-hook 'gnus-summary-prepared-hook 'my-gnus-summary-prepared-hook)

(defun my-gnus-group-prepare-hook ()
  "Added to gnus-group-prepare-hook.  Does a catchup on the groups containing
outbound messages.  Fontifies the Group buffer."
  (my-gnus-delete-compile-log-buffer)   ;; Why is this done???

  (save-excursion
    (let ((archive-groups '("nnfolder:sent-usenet")))
      (while archive-groups
	(let ((group (car archive-groups)))
	  (when (gnus-group-goto-group group 'far)
	    (gnus-group-catchup group)
	    (gnus-group-update-group group)))
	(setq archive-groups (cdr archive-groups)))))

  ;;(my-gnus-kill-duplicate-nttp-connections)

  (my-gnus-fontify-group-buffer))

(add-hook 'gnus-group-prepare-hook 'my-gnus-group-prepare-hook)

(defun my-gnus-article-prepare-hook ()
  "Added to gnus-article-prepare-hook.  Changes some UTF-8 characters into ASCII equivalents,
so I don't see solid blocks instead of character glyphs in the fonts I tend to use."
  (save-excursion
    (let ((elipses (make-string 1 342438))		;; UTF-8 elipses
	  (left-double-quote (make-string 1 342396))	;; UTF-8 left-double-quote
	  (right-double-quote (make-string 1 342397))	;; UTF-8 right-double-quote
	  (left-single-quote (make-string 1 342392))	;; UTF-8 left-single-quote
	  (right-single-quote (make-string 1 342393))	;; UTF-8 right-single-quote
	  (ndash (make-string 1 342387))		;; UTF-8 ndash
	  (mdash (make-string 1 342388))		;; UTF-8 mdash
	  )

      (goto-char (point-min))
      (while (search-forward elipses nil t)
	(replace-match "..."))

      (goto-char (point-min))
      (while (search-forward left-double-quote nil t)
	(replace-match "\""))

      (goto-char (point-min))
      (while (search-forward right-double-quote nil t)
	(replace-match "\""))

      (goto-char (point-min))
      (while (search-forward left-single-quote nil t)
	(replace-match "'"))

      (goto-char (point-min))
      (while (search-forward right-single-quote nil t)
	(replace-match "'"))

      (goto-char (point-min))
      (while (search-forward ndash nil t)
	(replace-match "-"))

      (goto-char (point-min))
      (while (search-forward mdash nil t)
	(replace-match "--"))
      )))

(add-hook 'gnus-article-prepare-hook 'my-gnus-article-prepare-hook)

(defun my-gnus-after-exiting-gnus-hook ()
  "Added to gnus-after-exiting-gnus-hook.  Kills the buffer visiting the file
named in nnmbox-mbox-file.  Cancels any timers I have running."
  (if (and (boundp 'my-gnus-keepalive-timer)
	   (timerp my-gnus-keepalive-timer))
      (cancel-timer my-gnus-keepalive-timer)))

(add-hook 'gnus-after-exiting-gnus-hook 'my-gnus-after-exiting-gnus-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice gnus-button-push (after my-ad-after-gnus-button-push activate)
    "If the current buffer is a Gnus Article buffer, the Gnus Summary buffer is
selected."
    (if (eq major-mode 'gnus-article-mode)
	(gnus-article-show-summary)))

(defadvice gnus-goto-colon (around my-ad-around-gnus-goto-colon activate)
  "Positions point where I want it in group and summary buffers."
  (if (eq major-mode 'gnus-summary-mode)
      (move-to-column 20)
    (if (eq major-mode 'gnus-group-mode)
	(move-to-column 8)
      ad-do-it)))

(defadvice process-send-string (before my-ad-process-send-string activate)
  "Displays a message that Gnus is fetching new news from a specific group.  This only
works for native groups, because this function is not called when fetching new news
in foreign groups.

IMPORTANT: The reason this advice is not on nntp-send-command or some other
nntp.el function is that those functions are all defined using defsubst, so the
byte-compiler inlines them into their callers, which prevents advice on the
actual function definition from being executed."
  (let ((string (ad-get-arg 1)))
    (if (string= (substring string 0 (min 11 (length string)))
		 "LIST ACTIVE")
	(when (string-match "^LIST ACTIVE \\(\\S-+\\)" string)
	  (message "Getting new news in group %s ..."
		   (propertize (match-string 1 string) 'face 'my-alert-face))
	  (sit-for 0.1)))))

(defadvice nntp-request-group (before my-ad-before-nntp-request-group activate)
  "Displays a message that Gnus is fetching new news from a specific group.
This only works for foreign groups, because this function is not called when
fetching new news in native groups (see the advice on process-send-string for
how native groups are handled)."
  (message "Getting new news in group %s ..."
	   (propertize (ad-get-arg 0) 'face 'my-alert-face)))

(defadvice gnus-group-get-new-news-this-group
  (around my-ad-around-gnus-group-get-new-news-this-group activate)
  "Displays a message saying Gnus is getting new news in the group."
  ad-do-it
  (message (propertize "Done getting new news!" 'face 'my-alert-face)))

(defadvice gnus-group-make-doc-group (after my-ad-after-gnus-group-make-doc-group activate)
  "Changes the level of the newly-created nndoc group to 2."
  (gnus-group-set-current-level 1 2))

(defadvice gnus-draft-send-message (before my-ad-before-gnus-draft-send-message activate)
  "Closes the Article window before sending the draft."
  (delete-other-windows))

(defadvice gnus-summary-exit (after my-ad-after-gnus-summary-exit activate)
  "Makes sure the Group buffer is the only window."
  (if (string= (buffer-name) "*Group*")
      (delete-other-windows)))

(defadvice gnus-group-check-bogus-groups (before my-ad-before-gnus-group-check-bogus-groups
						 activate)
  "Asks the user if he/she is sure before running the advised function."
  (if (null (yes-or-no-p "Really check for bogus groups? "))
      (error "Aborted!")))

(defadvice gnus-summary-show-raw-article (around my-ad-around-gnus-summary-show-raw-article
						 activate)
  "Toggles raw display of the article.  Syntax-highlights the raw article headers."
  (if my-gnus-article-is-raw
      (progn
	(gnus-summary-expand-window)
	(if (get-buffer gnus-article-buffer)
	    (kill-buffer gnus-article-buffer))
	(gnus-summary-beginning-of-article)
	(setq my-gnus-article-is-raw nil))

    ad-do-it
    (setq my-gnus-article-is-raw t)
    (gnus-article-highlight-headers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gnus-draft)	;; Needed to define the mode map.
(define-key gnus-draft-mode-map "e" 'my-gnus-draft-edit-message)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions related to NNTP servers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BUG: This doesn't handle multiple connections through the same SOCKS proxy.
(defun my-gnus-kill-duplicate-nttp-connections ()
  "Kills duplicate NNTP connections."
  (let ((nntp-connections (my-gnus-nntp-processes))
	server-names
	servers-to-kill)

    ;; First, build a list of NNTP servers to be killed.
    (dolist (proc nntp-connections)
      (let ((server-name (car (process-contact proc))))
	(if (assoc-string server-name server-names)
	    ;; Duplicate!
	    (add-to-list 'servers-to-kill server-name))
	;; Not a duplicate yet, but remember it so we can check future servers
	(add-to-list 'server-names server-name)))

    (when servers-to-kill
      (ding)
      (message "WARNING: Duplicate NNTP servers: %s"
	       (mapconcat 'identity servers-to-kill " "))
      (sleep-for 2))

    ;; Now kill the duplicate connections, leaving one connection to each server.
    (dolist (proc nntp-connections)
      (let ((server-name (car (process-contact proc))))
	(when (assoc-string server-name servers-to-kill)
	  (message "Killing NNTP connection to %s" server-name)
	  (kill-buffer (process-buffer proc))
	  (setq servers-to-kill (delete server-name servers-to-kill)))))))

(defun my-gnus-nntp-processes ()
  "Returns a list of all open NNTP process objects."
  nntp-connection-list)

(defun my-gnus-kill-nntp-process ()
  "Kill the NNTP process associated with the current group."
  (interactive)
  (unless (yes-or-no-p "Really kill this server's NNTP process? ")
    (error "Aborted!"))

  (let ((server-name (nth 1 (gnus-group-name-to-method (gnus-group-group-name)))))
    (dolist (proc (my-gnus-nntp-processes))
      (if (string= server-name (car (process-contact proc)))
	  (kill-buffer (process-buffer proc))))))

(defun my-gnus-kill-all-nntp-processes ()
  "Kills all NNTP processes."
  (interactive)
  (unless (yes-or-no-p "Really kill all NNTP processes? ")
    (error "Aborted!"))

  (mapc #'(lambda (proc)
	    (kill-buffer (process-buffer proc)))
	(my-gnus-nntp-processes))

  (message "Killed all NNTP processes!"))

(defun my-gnus-keepalive ()
  "Sends a DATE command to each connected NNTP server.  This function is called
from a timer (see variable my-gnus-keepalive-timer).  The timer is cancelled in
my-gnus-after-exiting-gnus-hook.  This function only sends a command to a server
if there are no gnus* or nntp* functions active on the stack (since calling
interacting with the NNTP server while those functions are active can cause NNTP
protocol data corruption)."
  (dolist (proc (my-gnus-nntp-processes))
    (if (not (my-stack-match "^\\(nntp\\|gnus\\)"))
	(process-send-string proc "DATE\n"))))

(defun my-gnus-server-status ()
  "Returns an NNTP server status string to be displayed in the mode lines of
Gnus buffers."
  (let ((result ""))
    (dolist (proc (my-gnus-nntp-processes))
      (if (eq (process-status proc) 'open)
	  (setq result (concat result
			       (or (car-safe (process-contact proc))
				   "UNKNOWN")
			       " "))))

    ;; Remove the trailing space from the string.
    (if (string= " " (substring result -1))
	(setq result (substring result 0 -1)))

    (concat "[" result "]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions related to the Group buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-gnus-group-post-command-hook ()
  "Added to post-command-hook (locally) in the group buffer.  This function
calls my-gnus-fontify-group-buffer."
  ;; Just do this for now.  Might do more later.
  (my-gnus-fontify-group-buffer))

(defun gnus-user-format-function-D (header)
  "Returns a short descriptive string based on the name of a group.  This function is
called because of %uD in the value of variable gnus-group-line-format."
  "")

(defun my-gnus-group-method ()
  "Returns the method for the group on the current line."
  (car (gnus-find-method-for-group (gnus-group-group-name))))

(defun my-gnus-group-server ()
  "Returns the server name for the group on the current line."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   UNDER CONSTRUCTION                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  nil)

(defun my-gnus-group-is-mailgroup ()
  "Returns t if the current group is a mail group, nil otherwise."
  (not (gnus-news-group-p (if (eq major-mode 'gnus-group-mode)
			      (gnus-group-group-name)
			    gnus-newsgroup-name))))

;; These faces are used by function my-gnus-fontify-group-buffer below.

(progn
  (set-face-foreground (make-face 'my-gnus-group-drafts-empty-face) "#ccc")
  (set-face-foreground (make-face 'my-gnus-group-drafts-face) "#fff")
  (set-face-foreground (make-face 'my-gnus-group-mail-empty-face) "#a8a")
  (set-face-foreground (make-face 'my-gnus-group-mail-face) "#faf")
  (set-face-foreground (make-face 'my-gnus-group-mail-important-face) "#f44")
  (set-face-foreground (make-face 'my-gnus-group-news-primary-empty-face) "#7bb")
  (set-face-foreground (make-face 'my-gnus-group-news-primary-face) "#aee")
  (set-face-foreground (make-face 'my-gnus-group-news-secondary-empty-face) "#ba5")
  (set-face-foreground (make-face 'my-gnus-group-news-secondary-face) "#ee8")
  (set-face-foreground (make-face 'my-gnus-group-news-eclipse-empty-face) "#c62")
  (set-face-foreground (make-face 'my-gnus-group-news-eclipse-face) "#fa5")
  (set-face-foreground (make-face 'my-gnus-group-news-unknown-empty-face) "yellow")
  (set-face-foreground (make-face 'my-gnus-group-news-unknown-face) "yellow")
  "Updated Gnus faces.")

(defun my-gnus-fontify-group-buffer ()
  "Fontifies the Group buffer."
  (ignore-errors
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(beginning-of-line)
	(let ((inhibit-read-only t)
	      (start (point))
	      (name (gnus-group-group-name))
	      (empty (looking-at "^ +0 ")))
	  (end-of-line)
	  (cond ((string= "nndraft:drafts" name)
		 (add-text-properties start (point)
				      `(face ,(if empty
						  'my-gnus-group-drafts-empty-face
						'my-gnus-group-drafts-face))))

		((string-match "^nnfolder:" name)
		 (add-text-properties start (point)
				      `(face ,(if empty
						  'my-gnus-group-mail-empty-face
						'my-gnus-group-mail-face))))

		((string-match "^[^:]+$" name)
		 (add-text-properties start (point)
				      `(face ,(if empty
						  'my-gnus-group-news-primary-empty-face
						'my-gnus-group-news-primary-face))))

		((string-match "^nntp\\+" name)
		 (add-text-properties start (point)
				      `(face ,(if empty
						  'my-gnus-group-news-secondary-empty-face
						'my-gnus-group-news-secondary-face))))

		(t
		 (add-text-properties start (point)
				      `(face ,(if empty
						  'my-gnus-group-news-unknown-empty-face
						'my-gnus-group-news-unknown-face))))))
	(forward-line 1)))))

(defun my-gnus-group-cleanup ()
  "Removes all *~ files under gnus-directory."
  (interactive)
  (let ((wildcards '("nnfolder/*~" "nnfolder/archive/*~" "*~"))
	(wildcard nil)
	(tmpbuf " *My Gnus Cleanup List*")
	(files nil))
    (while (setq wildcard (car wildcards))
      (setq files (append files (file-expand-wildcards (concat gnus-directory wildcard))))
      (setq wildcards (cdr wildcards)))
    (if (null files)
	(message "No files to cleanup.")
      (save-window-excursion
	(pop-to-buffer tmpbuf)
	(if (not (string= (buffer-name) tmpbuf))
	  (error "Internal error in my-gnus-group-cleanup!"))
	(erase-buffer)
	(mapc #'(lambda (string) (insert (concat string "\n"))) files)
	(insert "\n")
	(shrink-window-if-larger-than-buffer)
	(when (yes-or-no-p "Really delete the listed files? ")
	  (mapc #'delete-file files)
	  (message "Done."))))))

(defun my-gnus-group-close-nntp-server ()
  "Closes the first NNTP server connection listed in the server buffer."
  (interactive)
  (let ((servername nil))
    (save-window-excursion
      (gnus-group-enter-server-mode) ;; Jump to server buffer.
      (goto-char (point-min))
      (if (not (search-forward "nntp:" nil t))
	  (error "No NNTP server found!"))
      (setq servername (gnus-server-server-name))
      (if (looking-at ".*(closed)$")
	  (error "Server %s already closed." servername))
      (gnus-server-close-server servername)
      (gnus-server-exit))
    (message "Closed %s." servername)))

(defun my-gnus-group-get-new-mail ()
  "Gets new mail only (even if an NNTP server is connected)."
  (interactive)
  (save-excursion
;;    (my-gnus-group-set-group-levels)
    (gnus-group-get-new-news 2))
  ;; Cope with the above wierd bug by positioned point on the first group.
  (gnus-group-prev-group 99))

(defun my-gnus-group-get-new-news (&optional arg)
  "Behaves just like gnus-group-get-new-news, except it copes with a bug in Gnus
that happens after the server closes the connection."
  (interactive "P")
  (message "Getting new news ...")
  (sit-for 0)
  (gnus-group-get-new-news arg)
  (message (propertize "Done getting new news!" 'face 'my-alert-face)))

(defun my-gnus-group-get-new-news-this-section ()
  "Gets new news in all groups in the same section as the group on the current
line."
  (interactive)

  ;; For some reason, save-excursion doesn't restore point when used here, so
  ;; we use "let".
  (let ((orig-point (point))
	(method (gnus-group-name-to-method (gnus-group-group-name)))
	someerror)

    (if (my-gnus-group-is-mailgroup)
	;; Mail groups not supported.
	(error "Mail groups not supported!"))

    ;; It's a news group.  Get new news in all groups having the same method (i.e.,
    ;; the same NNTP server).
    (save-excursion
      (goto-char (point-min))
      (catch 'abort-get-news
	(while (not (eobp))
	  (when (equal method (gnus-group-name-to-method (gnus-group-group-name)))
	    (gnus-group-get-new-news-this-group)
	    (beginning-of-line)
	    (if (looking-at "\\s-+\\*")
		;; If there was an error, don't continue checking other groups
		;; in this section.
		(throw 'abort-get-news nil)))
	  (forward-line 1))))

    (goto-char orig-point)
    (message (propertize "Done getting new news for %s!" 'face 'my-alert-face)
	     method)))

(defun my-gnus-group-exit-kill-frame ()
  "Exit from Gnus normally, then delete the frame (if it's not the last frame
aand it appears to be dedicated to Gnus."
  (interactive)
  (gnus-group-exit)
  (if (and (null (get-buffer "*Group*"))  ;; User answered "y" to quit Gnus.
	   window-system
 	   (> (length (frame-list)) 1)
 	   (string= (frame-parameter nil 'name) "gnus"))
      (delete-frame)))

(defun my-gnus-group-find-new-groups ()
  "Just like gnus-group-find-new-groups, except that it pops up a buffer showing
any new groups."
  (interactive)
  (call-interactively 'gnus-group-find-new-groups)
  (my-gnus-show-new-groups))

(defun my-gnus-show-new-groups ()
  "Lists any new groups by displaying the *New Newsgroups* buffer if it exists.
This function is tied closely to function my-gnus-subscribe-new-group."
  (let ((listbuffer (get-buffer "*New Newsgroups*")))
    (when listbuffer
      (pop-to-buffer listbuffer)
      (goto-char (point-min))
      (search-forward "\n\n")
      (sort-lines nil (point) (point-max))
      (goto-char (point-min))
      (search-forward "following")
      (insert (format " %d" (- (count-lines (point-min) (point-max)) 3)))
      (goto-char (point-min)))))

(defun my-gnus-group-mail ()
  "Compose a new mail message in a new frame.  Interactive arguments are the
same as for gnus-group-mail."
  (interactive)
  (my-gnus-call-in-composition-frame #'gnus-group-mail))

(defun my-gnus-group-post-news (&optional prefix)
  "Compose a new news posting to the group under point in a new frame.
Interactively, a prefix of C-u leaves the Newsgroups header initially blank."
  (interactive "P")
  (if (not (gnus-alive-p))
      (error "Gnus has not been started!  Please start Gnus first."))

  (let ((current-prefix-arg prefix))
    (if (null prefix)
	(setq current-prefix-arg '(4))
      (if (equal current-prefix-arg '(4))
	  (setq current-prefix-arg nil)))
    (my-gnus-call-in-composition-frame #'gnus-group-post-news)))

(defun my-gnus-post-news ()
  "Compose a new news posting in a new frame.  The Newsgroups header is
initially blank."
  (interactive)
  (my-gnus-group-post-news '(4)))

(defun my-gnus-draft-edit-message ()
  "In a new frame, continue editing a draft article.  Interactive arguments are
the same as for gnus-draft-edit-message."
  (interactive)
  (my-gnus-call-in-composition-frame #'gnus-draft-edit-message))

(defun my-gnus-group-respool-all ()
  "Respools all articles in the nndoc group on the current line using the
nnfolder backend."
  (interactive)
  (if (not (string-match "^nndoc" (gnus-group-group-name)))
      (error "Not an nndoc group!"))
  (gnus-group-read-group)
  (if (eq major-mode 'gnus-group-mode)
      (message "Group has no unread messages.")
    (goto-char (point-min))
    (goto-char (point-min))
    (gnus-summary-mark-as-read-forward 9999)
    (gnus-summary-exit)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions related to the Summary buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These faces are used by function my-gnus-fontify-summary-buffer below.

(progn
  (set-face-foreground (make-face 'my-gnus-summary-spam-face) "#888")
  (set-face-foreground (make-face 'my-gnus-summary-my-article-face) "orange")
  (set-face-foreground (make-face 'my-gnus-summary-linus-article-face) "green")
  (set-face-foreground 'gnus-summary-normal-ancient-face nil)
  (set-face-foreground 'gnus-summary-selected-face "#ff0")
  (set-face-underline 'gnus-summary-selected-face nil))

(defun my-gnus-fontify-summary-buffer (start end unused)
  "Added to after-change-functions locally in the Summary buffer.  Fontifies the
Summary buffer between START and END, two buffer positions.  The third argument
is unused by this function."
  (let ((inhibit-read-only t)
	(article-is-marked nil)
	(article-is-ticked nil))
    (save-excursion
      (goto-char start)
      (beginning-of-line)

      (while (< (point) end)

	;; Figure out if the article is ticked.
	(when (looking-at "^[^ ]")
	  (setq article-is-marked t)
	  (if (looking-at "^!")
	      (setq article-is-ticked t))
	  (add-text-properties (point) (+ 2 (point)) '(face my-gnus-summary-my-article-face)))

	;; Fontify the article date/time.
	(move-to-column 3)
	(let ((time-start (point)))
	  (move-to-column 19)
	  (add-text-properties time-start (point) '(face font-lock-variable-name-face)))

	;; Fontify the author's name and (maybe) the subject.
	(move-to-column 20)
	(skip-chars-forward " ")

	(if (looking-at "Fran\\w* Litterio.*$")
	    (add-text-properties (match-beginning 0) (match-end 0)
				 '(face my-gnus-summary-my-article-face))

	  ;; It's not one of my articles.
	  (let ((author-face (car (cdr (memq 'my-gnus-author-face
					     (text-properties-at (point))))))
		(author-start (point))
		author-end)
	    (when author-face ;; Is this ever nil?
	      (setq author-end
		    (next-single-char-property-change (point) 'my-gnus-author-face))
	      (if (> author-end author-start)
		  (add-text-properties author-start author-end
				       `(face ,(if article-is-ticked
						   'gnus-summary-normal-ticked
						 author-face)))))))
	(forward-line 1)))))

(defun my-gnus-summary-post-command-hook ()
  "Added to post-command-hook (locally) in the summary buffer.  This function
recenters point if point ends up too close to the bottom of the window."
  (if (not (memq last-command '(mwheel-scroll scroll-down scroll-up my-scroll-up-one-line
					      my-scroll-down-one-line)))
      (let ((window-line (- (line-number-at-pos (point))
			    (line-number-at-pos (window-start)))))
	(if (or (>= window-line (- (window-height) 3))
		(<= window-line 2))
	    (recenter (/ (window-height) 2))))))

(defun gnus-user-format-function-F (header)
  "Returns the contents of an article's From or To header, depending on which
group the article appears in.  This function is called because of %uF in the
value of variable gnus-summary-line-format."
  (let (result)
    (if (and (boundp 'gnus-newsgroup-name)
	     (stringp gnus-newsgroup-name))
	;; Unfortunately, header doesn't contain the newsgroup(s) to which an
	;; article was posted, so we have nothing to show for articles in
	;; nnfolder:sent-usenet.  For gmane.comp.security.cert.advisory, we hide
	;; the sender name to show as much of the subject as possible (because
	;; the sender is the same for all articles).
	(setq result
	      (if (not (assoc-string gnus-newsgroup-name
				     '("nnfolder:sent-usenet"
				       "nntp+news.gmane.org:gmane.comp.security.cert.advisory")))
		  (if (string= gnus-newsgroup-name "nndraft:drafts")
		      (cdr (assq 'To (mail-header-extra header)))
		    (gnus-summary-extract-address-component (gnus-header-from header)))))
      (setq result "???"))

    (if result
	(propertize (format "%-25s" (substring result 0 (min 25 (length result))))
		    'face 'font-lock-builtin-face
		    'my-gnus-author-face 'font-lock-builtin-face)
      "")))

(set-face-foreground 'gnus-summary-low-unread "#666")
(set-face-foreground 'gnus-summary-low-read "#666")
(set-face-foreground 'gnus-summary-low-ticked "#666")
(set-face-foreground 'gnus-summary-low-undownloaded "#666")

(defun my-gnus-summary-kill-by-author ()
  "..."
  (interactive)
  ;; Without this next call, the new score entry goes into a per-group score file.
  (gnus-score-load-file "~/news/all.SCORE")
  (gnus-summary-score-entry "From" (regexp-quote (aref (gnus-summary-article-header) 2))
			    'regexp -1000 nil 'prompt)
  (gnus-score-save)
  (gnus-summary-rescore))

(defun my-gnus-summary-kill-by-subject ()
  "..."
  (interactive)
  ;; Without this next call, the new score entry goes into a per-group score file.
  (gnus-score-load-file "~/news/all.SCORE")
  (gnus-summary-score-entry "Subject" (regexp-quote (gnus-summary-article-subject))
			    'regexp -1000 nil 'prompt)
  (gnus-score-save)
  (gnus-summary-rescore))

(defun my-gnus-summary-expand-window ()
  "Makes the Summary buffer window the only window in its frame.  Also deletes all overlays,
which removes the highlighing on the formerly-displayed article's summary line."
  (interactive)
  (delete-other-windows)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (delete-overlay overlay)))

(defun my-gnus-summary-report-gmane-spam ()
  "Reports the current article as spam to Gmane."
  (interactive)
  (my-gnus-summary-expand-window)
  (my-gnus-summary-d-command 1)
  (let ((article-number (gnus-summary-article-number))
	(groupname (if (string-match "^nntp\\+news\\.gmane\\.org:\\(.*\\)$" gnus-newsgroup-name)
		       (match-string 1 gnus-newsgroup-name))))
    (if (null groupname)
	(error "This is not a Gmane newsgroup!"))
    (shell-command
     (format "(wget -O - 'http://spam.gmane.org/%s:%d' >/dev/null 2>&1 &)"
	     groupname article-number)
     " *Gmane Spam Report Command Output*"))
  (message "Reported spam to Gmane."))

(defun my-gnus-summary-copy-from-header ()
  "Copies the contents of the current article's From: header to the front of the
kill ring."
  (interactive)
  (let ((article-was-visible (get-buffer-window "*Article*" (selected-frame)))
	(original-mark (gnus-summary-article-mark (gnus-summary-article-number)))
	(original-window-start (window-start))
	from-header)
    (gnus-summary-beginning-of-article)	;; Displays the article buffer.
    (with-current-buffer "*Article*"
      (setq from-header (mail-fetch-field "From")))
    (kill-new from-header)
    (when (not article-was-visible)
      (gnus-summary-expand-window)
      (set-window-start (selected-window) original-window-start))
    (gnus-summary-mark-article nil original-mark 'no-expire)
    (gnus-goto-colon)
    (message "Copied %s to kill ring." (propertize from-header 'face 'my-alert-face))))

(defun my-gnus-summary-limit-to-unread ()
  "Limits the Summary buffer to only unread articles and those articles having
the marks 'O' and '!'."
  (interactive)
  (gnus-summary-limit-to-marks "!O ")
  (message "Limited to unread."))

(defun my-gnus-summary-follow-link-by-number (number)
  "For use with HTML emails washed by Lynx.  Follows link NUMBER listed at the bottom
of the email in the References section."
  (interactive "p")
  (with-current-buffer gnus-article-buffer
    (goto-char (point-max))
    (if (not (search-backward "\C-jReferences\C-j\C-j"))
	(error "This message has no References section.  Is it really an HTML email?"))
    (if (not (search-forward (format " %d. " number)))
	(error "This message has no link numbered %d!" number))
    (let ((url (buffer-substring (point) (progn (end-of-line) (point)))))
      ;; Without delaying the call to browse-url like this, a new IE browser opens.
      ;; This makes IE use an existing browser window.  The Windows version of browse-url
      ;; doesn't honor the variable browse-url-new-window-flag.
      (run-at-time 0.5 nil 'browse-url url))))

(defun my-gnus-summary-save-parts (prefix)
  "Calls gnus-summary-save-parts on articles indicated by process mark or prefix
arg, then kills the Article buffer."
  (interactive "P")
  (save-window-excursion
    (gnus-summary-save-parts "image/.*" "~/info/misc/usenet" prefix))
  (let ((artbuf (get-buffer "*Article*")))
    (if (bufferp artbuf)
	(kill-buffer artbuf))))

(defun my-gnus-summary-uncache-article ()
  "Uncaches the article on the current line of the Summary buffer.  Leave point
unmoved."
  (if (gnus-cached-article-p (gnus-summary-article-number))
      (save-excursion
	(gnus-cache-remove-article 1))))

(defun my-gnus-summary-clear-mark-forward (n)
  "Same as gnus-summary-clear-mark-forward, except cached articles are uncached."
  (interactive "p")
  (my-gnus-summary-uncache-article)
  (gnus-summary-clear-mark-forward n))

(defun my-gnus-summary-activate-widget (index)
  "Activates the INDEXth widget in the current article (interactively, INDEX is
the prefix argument).  Leaves point in the article buffer at the start of the
widget."
  (interactive "p")
  (save-excursion
    (when (not (my-gnus-other-window-is-article))
      (gnus-summary-expand-window)
      (gnus-summary-beginning-of-article))
    (other-window 1)
    (goto-char (point-min))
    (widget-forward index)
    (widget-button-press (point)))
  (message "Widget activated."))

(defun my-gnus-summary-cancel-article ()
  "Just like gnus-summary-cancel-article, except that it will cancel articles
that were not posted by you."
  (interactive)
  (let ((orig-error-func (symbol-function 'error)))
    (fset 'error #'(lambda (&rest args)
		     (if (or (not (stringp (car args)))
			     (and (not (string= "This article is not yours" (car args)))
				  (not (string= "Failed to verify Cancel-lock: This article is not yours" (car args)))))
			 (apply orig-error-func args))))
    (unwind-protect
	(call-interactively 'gnus-summary-cancel-article)
      (fset 'error orig-error-func))))

(defun my-gnus-summary-next-subject ()
  "Moves to the next subject line in current group.  If article window is showing,
it changes to track the subject header line, otherwise no article window is
opened."
  (interactive "")
  (if (my-gnus-other-window-is-article)
      (gnus-summary-next-article)
    (gnus-summary-next-subject 1)))

(defun my-gnus-summary-prev-subject ()
  "Moves to the previous subject line in current group.  If article window is showing,
it changes to track the subject header line, otherwise no article window is
opened."
  (interactive "")
  (if (my-gnus-other-window-is-article)
      (gnus-summary-prev-article)
    (gnus-summary-prev-subject 1)))

(defun my-gnus-summary-next-page (&optional lines circular)
  "Just like gnus-summary-next-page, except that it never advances to the next
article."
  (interactive "P")
  (let* ((bottom-visible (if (my-gnus-other-window-is-article)
			     (save-window-excursion
			       (other-window 1)
			       (= (window-end) (point-max))))))
    (if (or (not bottom-visible)
	    (not (= (gnus-summary-article-number)
		    gnus-current-article)))	;; This var. is not documented!
	(gnus-summary-next-page lines circular)
      (message "End of article."))))

(defun my-gnus-summary-kill-thread (&optional unmark)
  "Kills all articles in the current thread, including articles above the
current article in the summary buffer.  Takes the same arguments and returns the
same value as gnus-summary-kill-same-subject."
  (interactive "P")
  (my-gnus-summary-expand-window)
  (my-gnus-summary-goto-thread-root)
  (let ((done nil)
	(ismailgroup (string-match "^\\(nnfolder:\\|nnmbox:\\|nnml:\\)" gnus-newsgroup-name)))
    (while (not done)
      ;; Never touch a ticked article.
      (if (not (= ?! (gnus-summary-article-mark)))
	  (if unmark
	      (gnus-summary-mark-article-as-unread ? )
	    (if ismailgroup
		(gnus-summary-mark-article nil ?E)	;; Mark as expirable.
	      (gnus-summary-mark-article-as-read ?K))))
      (forward-line 1) ;; Don't use gnus-summary-next-subject here!
      (setq done (or (eobp)
		     (<= (gnus-summary-thread-level) 0)))))

  ;; If unmarking, go back to the thread root, otherwise move on.
  (if unmark
      (progn
	(forward-line -1)
	(my-gnus-summary-goto-thread-root))

    ;; If we end up on a dummy thread-root, advance past it, otherwise stay here.
    (gnus-summary-goto-subject (gnus-summary-article-number))
  
    ;; If this article has been read, go to the next unread subject line.
    (if (not (= 32 (gnus-summary-article-mark)))
	(gnus-summary-next-unread-subject 1))))

(defun my-gnus-summary-goto-thread-root ()
  "Positions the cursor on the root article of the current thread."
  (interactive)
  (while (> (gnus-summary-thread-level) 0)
    (forward-line -1))
  (gnus-summary-goto-subject (gnus-summary-article-number)))

(defun my-gnus-summary-next-thread ()
  "Positions the cursor on the root article of the next thread."
  (interactive)
  (forward-line 1)
  (while (> (gnus-summary-thread-level) 0)
    (forward-line 1))
  ;; This next line moves point past dummy lines.
  (gnus-summary-goto-subject (gnus-summary-article-number)))

(defun my-gnus-summary-prev-thread ()
  "Positions the cursor on the root article of the previous thread."
  (interactive)
  (forward-line -1)
  (gnus-goto-colon)
  (when (looking-at "\\. \\. \\.")
    (forward-line -1)
    (gnus-goto-colon))
  (while (looking-at " ")
    (forward-line -1)
    (gnus-goto-colon))
  (gnus-summary-goto-subject (gnus-summary-article-number)))

(defun my-gnus-summary-tick-article (&optional article clear-mark)
  "Ticks the current article and then calls gnus-goto-colon to put point after
the ':'.  This exists purely because gnus-summary-tick-article leaves point in a
annoying column.  Takes the same arguments and returns the same value as
gnus-summary-tick-article."
  (interactive)
  (prog1
      (progn
	(gnus-summary-tick-article article clear-mark)
	;; Advance to next line in Summary buffer if this article's body is not
	;; visible.
	(if (not (my-gnus-other-window-is-article))
	    (gnus-summary-next-subject 1)))
    (gnus-goto-colon)))

(defun my-gnus-summary-tick-thread (&optional untick)
  "Ticks all articles in the current thread using my-gnus-summary-tick-article."
  (interactive "P")
  (my-gnus-summary-goto-thread-root)
  (let* ((articles (gnus-summary-articles-in-thread))
	 (numarticles (length articles))
	 article)
    (while (setq article (car articles))
      (message "Ticking article %d ..." article)
      (my-gnus-summary-tick-article (car articles))
      (message "Ticking article %d ...  Done." article)
      (setq articles (cdr articles)))))

(defun my-gnus-summary-toggle-rot13 ()
  "Toggles ROT13 display of the current article.  The article is displayed if
it is not already."
  (interactive)
  (when (not (my-gnus-other-window-is-article))
    (gnus-summary-expand-window)
    (gnus-summary-beginning-of-article))
  ;; Can't use save-window-excursion here -- it undoes the ROT13!
  (other-window 1)
  (toggle-rot13-mode)
  (other-window 1)
  (redraw-frame (selected-frame)))

(defun my-gnus-summary-post-news ()
  "Compose a new news posting in a new frame.  Interactive arguments are the same
as for gnus-summary-post-news."
  (interactive)
  (my-gnus-call-in-composition-frame #'gnus-summary-post-news))

(defun my-gnus-summary-followup-format-mail-addresses (&rest addresses)
  "..."
  ;; Remove any nil's from the list.
  (setq addresses (delq 'nil addresses))

  ;; Transform to a list of individual addresses.
  (setq addresses (mapconcat 'identity addresses ", "))
  (setq addresses (split-string addresses ","))

  ;; Remove my address from the list of addresses.
  (setq addresses (mapcar #'(lambda (addr) (if (string-match "Fran.*Litterio" addr) nil addr)) 
			  addresses))

  ;; Remove any nil's introduced by the previous step.
  (setq addresses (delq 'nil addresses))

  ;; Format the list of addresses.
  (if addresses
      (let ((result (mapconcat 'identity addresses ", ")))
	(setq result (replace-regexp-in-string "<mailto:\\([^>]*\\)>" "\\1" result))
	(setq result (replace-regexp-in-string " +" " " result))
	(setq result (replace-regexp-in-string "^ " "" result))
	(propertize result 'face 'my-alert-face))))

(defun my-gnus-summary-followup-with-original ()
  "In a new frame, compose a news followup including the original article.
Interactive arguments are the same as for gnus-summary-followup-with-original."
  (interactive)
  ;; Find out if we should send mail instead of posting news.

  (let (list-address reply-to)
    (with-current-buffer gnus-original-article-buffer
      (setq list-address (or (message-fetch-field "List-Post")
			     (message-fetch-field "List-ID"))
	    reply-to (message-fetch-field "Reply-To")))

    (if (and list-address
	     (y-or-n-p (concat "This group is a mailing list ("
			       (replace-regexp-in-string "<mailto:\\([^>]*\\)>" "\\1" list-address)
			       ").  Followups can be mailed to:\n"
			       (my-gnus-summary-followup-format-mail-addresses
				reply-to list-address)
			       "\nReply by mail to the above addresses? ")))
	(my-gnus-call-in-composition-frame #'gnus-summary-wide-reply-with-original)
      (my-gnus-call-in-composition-frame #'gnus-summary-followup-with-original))))

(defun my-gnus-summary-reply-with-original ()
  "In a new frame, compose a reply to the current article including the
original article.  Interactive arguments are the same as for
gnus-summary-reply-with-original."
  (interactive)
  (my-gnus-call-in-composition-frame #'gnus-summary-reply-with-original))

(defun my-gnus-summary-wide-reply-with-original ()
  "In a new frame, compose a wide reply to the current article including the
original article.  Interactive arguments are the same as for
gnus-summary-wide-reply-with-original."
  (interactive)
  (my-gnus-call-in-composition-frame #'gnus-summary-wide-reply-with-original))

(defun my-gnus-summary-reply ()
  "In a new frame, compose a mail reply to the current article without including
the original article.  Interactive arguments are the same as for
gnus-summary-reply."
  (interactive)
  (my-gnus-call-in-composition-frame #'gnus-summary-reply))

(defun my-gnus-summary-mail-forward ()
  "In a new frame, forward the current mail article to someone else.
Interactive arguments are the same as for gnus-summary-mail-forward."
  (interactive)
  (my-gnus-call-in-composition-frame #'gnus-summary-mail-forward))

(defun my-gnus-call-in-composition-frame (function)
  "Calls FUNCTION interactively in a new frame."
  (if window-system
      (let ((current-top (frame-parameter nil 'top))
	    (current-left (frame-parameter nil 'left)))
	(my-frame-call-in-new-frame function `((name . "Outgoing Message")
					       (left . ,(+ current-left 100))
					       (top . ,(+ current-top 100))))
	;;(my-frame-center)
	)
    (call-interactively function)))

(defun my-gnus-summary-next-article-in-thread ()
  "Displays the next article in the current thread.  If there is none, deletes the
article window."
  (interactive)
  (gnus-summary-next-subject 1)
  (if (= 0 (gnus-summary-thread-level))
      (gnus-summary-expand-window)
    (gnus-summary-beginning-of-article)))

(defun my-gnus-summary-recenter ()
  "Recenters the summary window the way I want it recentered."
  (if (> (count-windows) 1)
      ;; This call to recenter just recenters without redisplay to reduce
      ;; flashing of the screen.
      (recenter '(4))))

(defun my-gnus-summary-d-command (count)
  "Bound to 'd' in gnus-summary-mode.  This works the same as
gnus-summary-mark-as-read-forward, except in the nndraft:drafts group, where it
expires drafts.  It also uncaches cached articles."
  (interactive "p")
  (my-gnus-summary-uncache-article)
  (if (not (string= gnus-newsgroup-name "nndraft:drafts"))
      (gnus-summary-mark-as-read-forward count)
    (gnus-summary-mark-as-expirable count)
    (gnus-summary-expire-articles t)))

(defun my-gnus-summary-expire-articles-now ()
  "Bound to E in gnus-summary-mode.  Simply calls gnus-summary-expire-articles
after asking if the user is sure.  It uses y-or-n-p instead of yes-or-no-p,
which gnus-summary-expire-articles-now uses."
  (interactive)
  (if (y-or-n-p "Really expire all expirable articles now? ")
      (gnus-summary-expire-articles t)
    (message "No articles expired.")))

(defun my-gnus-summary-print-article (&optional edit)
  "Prints the currently-displayed article using my-print-buffer.  If optional
argument EDIT (interactively, a prefix of C-u) is non-nil, the user gets to edit
the print command before it executes."
  (interactive "P")
  (gnus-summary-beginning-of-article)
  (let ((subject (gnus-summary-article-subject))
	(result ""))
    (save-window-excursion
      (if (string-match "^\\([rR][eE]\\s-*:\\s-*\\)*\\(.*\\)\\s-*$" subject)
	  (setq subject (substring subject (match-beginning 2) (match-end 2))))
      (if (> (length subject) 53)
	  (setq subject (substring subject 0 53)))
      (other-window 1)
      (setq result (concat "Printed with: "
			   (let ((my-print-font-size (if (and (boundp 'my-print-font-size)
							      (integerp my-print-font-size))
							 my-print-font-size
						       10)))
			     (my-print-buffer nil nil subject nil edit)))))
    (message result)))

(defun my-gnus-other-window-is-article ()
  "Returns t if the other window, as selected by (other-window 1), is an article
window, nil otherwise."
  (if (> (count-windows) 1)
      (save-window-excursion
	(other-window 1)
	(eq major-mode 'gnus-article-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous code and code that applies to multiple buffers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-gnus-mode-line-string ()
  "Returns a string used to identify the current group in summary and article buffer
mode lines.  See variables gnus-summary-mode-line-format and gnus-article-mode-line-format."
  (concat ;;"Gnus:  "
          (if gnus-newsgroup-name
	      (concat (gnus-group-short-name gnus-newsgroup-name) " "))))

;; Overrides the same function defined in gnus-util.el.
(defun gnus-dd-mmm (messy-date)
  "Return a string like DD-MMM from a big messy string."
  (condition-case ()
      (let* ((time (substring (format-time-string "%d-%b-%y %l:%M%p"
						  (safe-date-to-time messy-date))
			      0 -1))
	     (lastchar (1- (length time))))
	(aset time lastchar (downcase (aref time lastchar)))
	time)
    (error "??-???-?? ??:?? ")))

(defun my-gnus-listed-groups ()
  "Returns a list of all listed groups (including mail groups)."
  (with-current-buffer gnus-group-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((groups nil)
	    (name nil))
	(while (setq name (gnus-group-group-name))
	  (setq groups (cons name groups))
	  (forward-line 1))
	groups))))

(defun my-gnus-unread-mail-count ()
  "Returns the number of unread mail messages in the following set of mail
groups: inbox, e15, xsaber, oldfriends, work."
  (let ((groups (my-gnus-listed-groups))
	(group nil)
	(count 0))
    (while groups
      (setq group (car groups))
      ;; NOTE: Change the docstring above if you change this regexp!
      (if (string-match "^nnfolder:\\(inbox\\|e15\\|xsaber\\|oldfriends\\|work\\)"
			group)
	  (setq count (+ count (gnus-group-unread group))))
      (setq groups (cdr groups)))
    count))

(defun my-gnus-delete-compile-log-buffer ()
  "Deletes the buffer named *Compile-Log* if it is empty."
  (let ((buf (get-buffer "*Compile-Log*")))
    (if (and (bufferp buf)
	     (= 0 (buffer-size buf)))
	(kill-buffer buf))))
