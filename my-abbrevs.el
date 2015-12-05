;; my-abbrevs.el
;; Abbrev-related stuff.

(require 'erc)
(require 'mailabbrev)

(set-default 'abbrev-mode t)
(kill-all-abbrevs)	;; Kills all global and mode-specific abbrevs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global abbrevs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((my-abbrev-defs
       '(("'quote"	"" (lambda () (my-random-quote 'insert)))
	 ("teh"		"the")
	 ("uncon"	"UNDER CONSTRUCTION"))))

  (clear-abbrev-table global-abbrev-table)

  (dolist (info my-abbrev-defs)
    (apply 'define-abbrev global-abbrev-table info))
  "Redefined global abbrevs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC Mode abbrevs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((my-abbrev-defs
       '(("'bashman"	"the Bash Manual at http://www.gnu.org/software/bash/manual/")
	 ("'cppfaq"	"the C++ FAQ Lite at http://new-brunswick.net/workshop/c++/faq/")
	 ("'cpplib"	"the Dinkumware C++ Library Reference Manual at http://www.dinkumware.com/manuals/default.aspx#Standard%20C++%20Library")
	 ("'cygug"	"the Cygwin User's Guide at http://cygwin.com/cygwin-ug-net/cygwin-ug-net.html")
	 ("'cygwin"	"the Cygwin project page at http://www.cygwin.com/")
	 ("'gdbman"	"the GDB User Manual at http://www.gnu.org/software/gdb/documentation/")
	 ("'gnudist"	"the GNU software archive at http://www.gnu.org/prep/ftp.html")
	 ("'gnuman"	"the GNU manuals at http://www.gnu.org/manual/")
	 ("'goog"	"http://www.google.com/search?q=")
	 ("'grub"	"the GRUB project page at http://www.gnu.org/software/grub/")
	 ("'iptables"	"the iptables documentation page at http://www.netfilter.org/documentation/index.html")
	 ("'kernel"	"the Kernel HOWTO at http://www.tldp.org/HOWTO/Kernel-HOWTO.html")
	 ("'ldp"	"the Linux Documentation Project at http://www.tldp.org/")
	 ("'lkml"	"the Linux Kernel Mailing List archive at http://lkml.org/")
	 ("'nag"	"the Linux Network Administrator's Guide at http://www.oreilly.com/catalog/linag2/book/index.html")
	 ("'rp"		"Radio Paradise at http://www.radioparadise.com/")
	 ("'soma"	"Soma FM at http://www.somafm.com/")
	 ("'tcpguide"	"the TCP/IP Guide at http://www.tcpipguide.com/free/")
	 ("'toms"	"Tom's root/boot Linux on a floppy at http://www.toms.net/rb/"))))

  (if (boundp 'erc-mode-abbrev-table)
      (clear-abbrev-table erc-mode-abbrev-table))

  (dolist (info my-abbrev-defs)	   
    (apply 'define-abbrev erc-mode-abbrev-table info))
  "Redefined ERC Mode abbrevs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message Mode abbrevs.  The local abbrev table in Message Mode is actually
;; text-mode-abbrev-table, except in the To: and Cc: headers, where mail-abbrevs
;; is used instead (thanks to the fact that, in message buffers,
;; pre-abbrev-expand-hook is set to sendmail-pre-abbrev-expand-hook).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((my-abbrev-defs
       '(("gc"		"gcapalbo@backbone.com")
	 ("fl"		"flitterio@gmail.com"))))

  (when (boundp 'mail-abbrevs)
    (my-eval-silently (build-mail-abbrevs))
    (clear-abbrev-table mail-abbrevs))

  (dolist (info my-abbrev-defs)
    (apply 'define-abbrev mail-abbrevs info))
  "Redefined Message Mode abbrevs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ Mode abbrevs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((my-abbrev-defs
       '(("xret" "Returns SUCCESS if successful, FAILURE if an unexpected failure occurs."))))

  (if (boundp 'c++-mode-abbrev-table)
      (clear-abbrev-table c++-mode-abbrev-table))

  (dolist (info my-abbrev-defs)
    (apply 'define-abbrev erc-mode-abbrev-table info))
  "Redefined C++ Mode abbrevs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off abbrevs in the minibuffer.  You can still use "C-x '" to manually
;; expand abbrevs in the minibuffer, if necessary.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'minibuffer-setup-hook
	  (lambda () (if (= 32 (aref (buffer-name) 0)) ;; Sanity check.
			 (setq abbrev-mode nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable abbrevs that start with single-quote.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-advice abbrev--before-point (:around (origfun &rest args) my-around-abbrev--before-point)
  "Supports abbrevs that start with single-quote."
  (let ((my-tables (abbrev--active-tables))
        start end abbrev-string result)

    (while (and my-tables (not result))
      (let ((table (pop my-tables)))
	(setq my-tables (append (abbrev-table-get table :parents) my-tables))
	(setq result
	      (and (looking-back "\\('\\w+\\)\\W*"
				 (line-beginning-position))
		   (setq start (match-beginning 1))
		   (setq end (match-end 1))
		   (setq abbrev-string (buffer-substring start end))
		   (let ((my-abbrev-sym (abbrev-symbol abbrev-string table)))
		     (if my-abbrev-sym
			 (list my-abbrev-sym abbrev-string start end)))))))

    (or result (apply origfun args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-show-abbrevs ()
  "Pops up a new frame displaying all of my abbreviations."
  (interactive)
  (let (my-allabbrevs one-table-abbrevs abbrev abbrev-table-symbol)
    (dolist (abbrev-table-symbol '(global-abbrev-table erc-mode-abbrev-table))
      (setq one-table-abbrevs
	    (mapcar (lambda (sym)
		      (if (symbolp sym)
			  (cons (symbol-name sym)
				(concat (symbol-value sym)
					(if (and (string= "" (symbol-value sym))
						 (symbol-function sym))
					    (propertize (prin1-to-string (symbol-function sym))
							'face 'region))))))
		    (symbol-value abbrev-table-symbol)))
      (if one-table-abbrevs
	  (setq my-allabbrevs (append my-allabbrevs one-table-abbrevs))))

    (let ((inhibit-redisplay t)
	  (keymap (make-sparse-keymap)))
      (select-frame (make-frame '((height . 50) (width . 130))))
      (switch-to-buffer (get-buffer-create "*My Abbrevs*"))
      (erase-buffer)
      (setq truncate-lines t)

      (insert "ABBREV              EXPANSION\n")
      (insert "------              ---------\n")
      (dolist (abbrev my-allabbrevs)
	(when (stringp (car abbrev))
	  (insert (car abbrev))
	  (indent-to-column 20)
	  (insert (concat (cdr abbrev) "\n"))))
    
      (goto-char (point-min))
      (forward-line 2)
      (sort-lines nil (point) (point-max))
      (goto-char (point-min))
      (forward-line 1)

      (define-key keymap "q" (lambda () (interactive) (my-kill-buffer-window-and-frame)))
      (define-key keymap "Q" (lookup-key keymap "q"))
      (define-key keymap "n" 'next-line)
      (define-key keymap "p" 'previous-line)

      (add-text-properties (point-min) (point-max) `(keymap ,keymap read-only t))

      ;; This causes an "invalid face: default" error!
      ;;(set-foreground-color "pink")
      (set-frame-height (selected-frame) (+ 6 (count-lines (point-min) (point-max)))))
    (my-frame-center)
    ))
	

;; Local Variables:
;; truncate-lines: t
;; End:
