;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First, unset some global bindings to make room for long keypaths used below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<mouse-2>"))
(global-unset-key (kbd "<C-down-mouse-1>"))
(global-unset-key (kbd "<C-down-mouse-3>"))
(global-unset-key (kbd "C-x m"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings with no prefix.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "<C-M-f10>")	'font-lock-fontify-buffer)
(define-key global-map (kbd "<C-M-f5>")		'my-frame-make-all-frames-visible)
(define-key global-map (kbd "<C-f10>")		'font-lock-fontify-block)
(define-key global-map (kbd "<C-f5>")		'my-make-frame-delayed)
(define-key global-map (kbd "<C-f6>")		'my-insert-function-name)
(define-key global-map (kbd "<C-f8>")		'my-message-compose-emacs-bug-report)
(define-key global-map (kbd "<S-f12>")		'my-reset-font)
(define-key global-map (kbd "<S-f1>")		'previous-error)
(define-key global-map (kbd "<S-f8>")		'my-message-mail2news)
(define-key global-map (kbd "<S-f9>")		'iconify-frame)
(define-key global-map (kbd "C-.")		'xref-find-definitions)
(define-key global-map (kbd "C-'")		'xref-pop-marker-stack)
(define-key global-map (kbd "C-,")		(lambda () (interactive)
						  (if (null (get-buffer "*xref*"))
						      (error (my-highlight-string "There is no *xref* buffer!")))
						  (pop-to-buffer "*xref*")))
(define-key global-map (kbd "<f1>")		'next-error)
(define-key global-map (kbd "<f2>")		'my-switch-to-other-buffer)
(define-key global-map (kbd "<f3>")		'call-last-kbd-macro)
(define-key global-map (kbd "<f4>")		'my-frame-show-next-frame)
(define-key global-map (kbd "<f5>")		'make-frame)
(define-key global-map (kbd "<f6>")		'my-insert-buffer-name)
(define-key global-map (kbd "<f7>")		'my-cycle-eol-conventions)
(define-key global-map (kbd "<f8>")		'my-message-mail)
;;(define-key global-map (kbd "<f9>")		'my-frame-make-frame-invisible)
(define-key global-map (kbd "<f10>")		'my-toggle-font-lock-mode)
(define-key global-map (kbd "<f12>")		'my-rotate-font)
(define-key global-map (kbd "C-<f12>")		'my-revert-buffer-unconditionally)
(define-key global-map (kbd "C-M-<f12>")	'my-revert-log-file-unconditionally)

;; Mouse-1 bindings ...
(define-key global-map (kbd "<C-mouse-1>")	'browse-url-at-mouse)
(define-key global-map (kbd "<mode-line S-mouse-2>") 'my-mouse-split-window-vertically)
(define-key global-map (kbd "C-z <mouse-1>")	'mouse-set-font)

;; Mouse-3 bindings ...
(define-key global-map (kbd "<mouse-3>")	'my-mouse-buffer-menu)
(define-key global-map (kbd "<C-mouse-3>")	'my-mouse-buffer-menu)

(if (string= (or (getenv "TERM") "") "linux")  ;; Linux virtual console.
    (progn
      (define-key global-map (kbd "<kp-7>")	'beginning-of-line)
      (define-key global-map (kbd "<kp-1>")	'end-of-line)
      (define-key global-map (kbd "<insertchar>") (lambda () (interactive) (scroll-up 3)))
      (define-key global-map (kbd "<deletechar>") 'backward-delete-char)))

;; A note about binding to TAB.  Always bind to (kbd "TAB") instead of (kbd "<tab>"), because the
;; former affects both graphical and terminal Emacs, but the latter only affects graphical Emacs.
;; Also, (kbd "C-i") is exactly the same as (kbd "TAB"): both evaluate to a TAB character (ASCII 9).
(define-key global-map (kbd "TAB")		'tab-to-tab-stop)
(define-key global-map (kbd "<backspace>")	'delete-backward-char)
(define-key global-map (kbd "C-h")		'delete-backward-char)
(define-key global-map (kbd "<delete>")		'delete-char)
(define-key global-map (kbd "DEL")		'delete-char)
(define-key global-map (kbd "<prior>")		'scroll-down)
(define-key global-map (kbd "<next>")		'scroll-up)
(define-key global-map (kbd "<C-home>")		'beginning-of-buffer)
(define-key global-map (kbd "<C-end>")		'end-of-buffer)
(define-key global-map (kbd "<C-tab>")		'my-tab-to-replaceable-text)
(define-key global-map (kbd "<C-up>")		'my-scroll-down-one-line)
(define-key global-map (kbd "<C-down>")		'my-scroll-up-one-line)
(define-key global-map (kbd "<S-backspace>")	'backward-delete-char-untabify)
(define-key global-map (kbd "<S-return>")	'my-newline-basic)
(define-key global-map (kbd "<S-next>")		(lambda () (interactive) (scroll-up 16)))
(define-key global-map (kbd "<S-prior>")	(lambda () (interactive) (scroll-down 16)))
(define-key global-map (kbd "<S-left>")		(lambda () (interactive) (scroll-left 3)))
(define-key global-map (kbd "<S-right>")	(lambda () (interactive) (scroll-right 3)))
(define-key global-map (kbd "<M-left>")		'scroll-left)
(define-key global-map (kbd "<M-right>")	'scroll-right)
(define-key global-map (kbd "<M-backspace>")	'backward-kill-word)
(define-key global-map (kbd "<C-return>")	'my-newline-with-indent)
;;(define-key global-map (kbd "<C-left>")	'previous-buffer)
;;(define-key global-map (kbd "<C-right>")	'next-buffer)

(define-key global-map (kbd "C-/")		'my-toggle-highlight-word-at-point)
(define-key global-map (kbd "C-^")		'enlarge-window)
(define-key global-map (kbd "C-l")		'recenter)
(define-key global-map (kbd "C-o")		(lambda () (interactive)
						  (insert 10)
						  (backward-char 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings with prefix "C-x".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-x ESC")	'repeat-matching-complex-command)
(define-key global-map (kbd "C-x C-b")	'my-list-buffers)
(when (display-graphic-p)
  (define-key global-map (kbd "C-x C-c") 'my-delete-frame-or-save-buffers-kill-emacs)
  (define-key global-map (kbd "C-x C-/") 'save-buffers-kill-emacs))
(if (display-graphic-p)
    (define-key global-map (kbd "C-x C-k") 'my-kill-buffer-window-and-frame))
(define-key global-map (kbd "C-x C-y")	'my-yank-quoted)
(define-key global-map (kbd "C-x C-=")	'my-show-window-dimensions)
(if (null (display-graphic-p))
    (define-key global-map (kbd "C-x ,") 'set-mark-command))
(define-key global-map (kbd "C-x 2")	'my-split-window-vertically)
(define-key global-map (kbd "C-x a=")	'my-show-abbrevs)
(define-key global-map (kbd "C-x c")	'center-line)
(define-key global-map (kbd "C-x C")	'center-line)
(define-key global-map (kbd "C-x K")	'my-kill-buffer-and-window)
(define-key global-map (kbd "C-x l")	'my-count-lines-page)
(define-key global-map (kbd "C-x m m")	'my-message-mail)
(define-key global-map (kbd "C-x m p")	'my-gnus-post-news)
(define-key global-map (kbd "C-x t")	'my-toggle-truncate-lines)
(define-key global-map (kbd "C-x T")	'my-toggle-truncate-lines)
(define-key global-map (kbd "C-x x")	'hexl-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings with prefix "C-c".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-c C-m")	'my-deactivate-mark)
(define-key global-map (kbd "C-c C-w")	'my-message-signature)
(define-key global-map (kbd "C-c g")	'my-debug-on-entry)
(define-key global-map (kbd "C-c G")	'cancel-debug-on-entry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings with prefix "C-z".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-z SPC")	'my-delete-trailing-whitespace)
(define-key global-map (kbd "C-z [")	(lambda () (interactive) (insert #x2588)))
(define-key global-map (kbd "C-z .")	'my-remove-face-property)
(define-key global-map (kbd "C-z / p")	'my-insert-public-section-comment)
(define-key global-map (kbd "C-z / i")	'my-insert-internal-section-comment)
(define-key global-map (kbd "C-z / r")	'my-insert-protected-section-comment)
(define-key global-map (kbd "C-z / v")	'my-insert-private-section-comment)
(define-key global-map (kbd "C-z -")	(lambda () (interactive) (insert (make-string 80 ?-))))
(define-key global-map (kbd "C-z =")	'my-insert-separator)
(define-key global-map (kbd "C-z ;")	(lambda () (interactive) (insert (make-string 95 ?;))))
(define-key global-map (kbd "C-z #")	(lambda () (interactive) (insert (make-string 95 ?#))))
(when (display-graphic-p)
  (define-key global-map (kbd "C-z +")	'my-frame-center)
  (define-key global-map (kbd "C-z C-=") 'my-frame-goto-initial-position-and-size))
(define-key global-map (kbd "C-z <")	'align-regexp)
(define-key global-map (kbd "C-z >")	'my-indent-region-with-string)
(define-key global-map (kbd "C-z TAB")	'my-toggle-tabify-region)
(define-key global-map (kbd "C-z C-/")	'my-flip-slashes)
(define-key global-map (kbd "C-z C-c")	'calendar)
(define-key global-map (kbd "C-z C-b")	'my-shell)
(define-key global-map (kbd "C-z C-e")	'my-find-file-elevated)
(define-key global-map (kbd "C-z C-f")	'find-file-literally)
(define-key global-map (kbd "C-z g")	'my-grep)
(define-key global-map (kbd "C-z G")	'my-grep-recursive)
(define-key global-map (kbd "C-z C-M-g") 'my-grep-show-grep-buffer)
(define-key global-map (kbd "C-z C-h")	'my-highlight-region)
(define-key global-map (kbd "C-z C-m")	'my-remove-crs)
(define-key global-map (kbd "C-z C-r")	'replace-string)
(define-key global-map (kbd "C-z C-s")	'my-toggle-socksify-emacs)
(define-key global-map (kbd "C-z C-t")	'visit-tags-table)
(define-key global-map (kbd "C-z C-u")	'my-insert-ucs-character)
(define-key global-map (kbd "C-z C-w")	'my-make-file-writable)
(define-key global-map (kbd "C-z C-z")	'my-calc)
(define-key global-map (kbd "C-z 0")	(lambda () (interactive) (my-display-notice nil)))
(define-key global-map (kbd "C-z 1")	'my-insert-footnote)
(define-key global-map (kbd "C-z 2")	'my-insert-footnote)
(define-key global-map (kbd "C-z 3")	'my-launch-mp3tag)
(define-key global-map (kbd "C-z 6")	'my-base64-decode)
(define-key global-map (kbd "C-z C-6")	'base64-encode-region)
(define-key global-map (kbd "C-z a p")	(lambda () (interactive) (my-anon-compose-message 'news)))
(define-key global-map (kbd "C-z a m")	'my-anon-compose-message)
(define-key global-map (kbd "C-z B")	'my-find-non-ascii-char)
(define-key global-map (kbd "C-z C")	'my-compile-and-run)
(define-key global-map (kbd "C-z c b")	'my-crypt-encrypt-buffer)
(define-key global-map (kbd "C-z c d")	'my-crypt-decrypt-message)
(define-key global-map (kbd "C-z c f")	'my-crypt-forget-passphrase)
(define-key global-map (kbd "C-z c r")	'my-crypt-encrypt-region)
(define-key global-map (kbd "C-z c R")	'my-crypt-decrypt-region)
(define-key global-map (kbd "C-z c s")	'my-crypt-clearsign-region)
(define-key global-map (kbd "C-z e")    'my-explore-directory)
(define-key global-map (kbd "C-z E")    'my-reload-my-elisp-files)
(define-key global-map (kbd "C-z d")    'my-copy-current-directory-to-clipboard)
(define-key global-map (kbd "C-z f")    'my-show-faces-at)
(define-key global-map (kbd "C-z F")    'describe-face)
(define-key global-map (kbd "C-z i")    'my-erc-networkid-connect)
(define-key global-map (kbd "C-z k")    'my-turn-off-backup-this-buffer)
(define-key global-map (kbd "C-z C-l")  'linum-mode)
(define-key global-map (kbd "C-z m")    'man)
(define-key global-map (kbd "C-z M")    'fpl-moo-connect)
(define-key global-map (kbd "C-z U")    (lambda () (interactive)
					  (untabify (region-beginning) (region-end))
					  (message "Untabified region!")))
(if (display-graphic-p)
	(define-key global-map (kbd "C-z n") 'set-default-font))
(define-key global-map (kbd "C-z p")	'list-text-properties-at)
(define-key global-map (kbd "C-z q")	'my-random-quote)
(define-key global-map (kbd "C-z R")	'my-randomize-region-lines)
(define-key global-map (kbd "C-z r")	'revert-buffer)
(define-key global-map (kbd "C-z s")	'sort-lines)
(define-key global-map (kbd "C-z S")	'sort-numeric-fields)
(define-key global-map (kbd "C-z t")	'my-show-tags-files)
(define-key global-map (kbd "C-z T")    (lambda () (interactive)
					  (tabify (region-beginning) (region-end))
					  (message "Tabified region!")))
(define-key global-map (kbd "C-z v d")	'my-sc-diff)
(define-key global-map (kbd "C-z v e")	'my-sc-make-file-editable)
(define-key global-map (kbd "C-z v h")	'my-sc-history)
(define-key global-map (kbd "C-z v m")	'my-sc-three-way-merge)
(define-key global-map (kbd "C-z v g")	'my-sc-update)
(define-key global-map (kbd "C-z v i")	'my-sc-checkin)
(define-key global-map (kbd "C-z v o")	'my-sc-checkout)
(define-key global-map (kbd "C-z v u")	'my-sc-uncheckout)
(define-key global-map (kbd "C-z v s")	'my-sc-status)
(define-key global-map (kbd "C-z w")	'my-select-current-word)
(define-key global-map (kbd "C-z y")	'my-magnet-url)
(define-key global-map (kbd "C-z z")	'iconify-or-deiconify-frame)
(define-key global-map (kbd "C-z M-t")	'time-stamp)
(define-key global-map (kbd "C-z M-u")	'my-unadvise-function)
(define-key global-map (kbd "C-z M-z")	(lambda () (interactive)
					  (switch-to-buffer "*Async Shell Command*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings with prefixes "C-S-".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-S-b")	(lambda () (interactive)
					  (if (null (get-buffer "bash"))
					      (error "No buffer named 'bash'!"))
					  (switch-to-buffer "bash")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings with prefixes "M-C-".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "M-C-`")	'my-background-shell-command)
(define-key global-map (kbd "M-C-^")	'enlarge-window-horizontally)
(define-key global-map (kbd "M-C-0")	(lambda () (interactive) (recenter 0)))
(define-key global-map (kbd "M-C-d")	'my-kill-to-bottom)
(define-key global-map (kbd "M-C-e")	'my-find-error)
(define-key global-map (kbd "M-C-h")	'backward-kill-word)
(define-key global-map (kbd "M-C-o")	'my-squeeze-blank-lines)
(define-key global-map (kbd "M-C-u")	'undo)
(define-key global-map (kbd "M-C-w")	'write-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings with prefixes "M-".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "M-ESC")	'eval-expression)
(define-key global-map (kbd "M-RET")	'my-show-max-text)
(define-key global-map (kbd "M-`")	'shell-command)
(define-key global-map (kbd "M-#")	'ispell-buffer)
(define-key global-map (kbd "M-$")	'ispell-word)
(define-key global-map (kbd "M-/")	'hippie-expand)
(define-key global-map (kbd "M-&")	'query-replace-regexp)
(define-key global-map (kbd "M-)")	'my-print-toggle-onscreen-display)
(define-key global-map (kbd "M-_")	'my-print-region)
(define-key global-map (kbd "M-+")	'my-print-buffer)
(define-key global-map (kbd "M-a")	'my-scroll-up-one-line)
(define-key global-map (kbd "M-A")	(lambda () (interactive) (scroll-left 2)))
(define-key global-map (kbd "M-F")	'my-frame-set-width)
(define-key global-map (kbd "M-g")	'goto-line)
(define-key global-map (kbd "M-h")	'help-command)
(define-key global-map (kbd "M-h A")	'apropos-variable)
(define-key global-map (kbd "M-h t")	'tags-apropos)
(define-key global-map (kbd "M-h v")	'my-describe-variable)
(define-key global-map (kbd "M-h f")	'my-describe-function)
(define-key global-map (kbd "M-M 0")	'fundamental-mode)
(define-key global-map (kbd "M-M a")	'abbrev-mode)
(define-key global-map (kbd "M-M c")	'c++-mode)
(define-key global-map (kbd "M-M d")	'diff-mode)
(define-key global-map (kbd "M-M e")	'emacs-lisp-mode)
(define-key global-map (kbd "M-M f")	'auto-fill-mode)
(define-key global-map (kbd "M-M h")	'html-mode)
(define-key global-map (kbd "M-M i")	'indented-text-mode)
(define-key global-map (kbd "M-M j")	'java-mode)
(define-key global-map (kbd "M-M l")	'lisp-interaction-mode)
(define-key global-map (kbd "M-M m")	'message-mode)
(define-key global-map (kbd "M-M o")	'overwrite-mode)
(define-key global-map (kbd "M-M p")	'perl-mode)
(define-key global-map (kbd "M-M t")	'text-mode)
(define-key global-map (kbd "M-n")	'my-next16-line)
(define-key global-map (kbd "M-p")	'my-previous16-line)
(define-key global-map (kbd "M-Q")	'fill-region)
(define-key global-map (kbd "M-r")	'my-revert-all-buffers)
(define-key global-map (kbd "M-s")	'my-scroll-down-one-line)
(define-key global-map (kbd "M-S")	(lambda () (interactive) (scroll-right 2)))
(define-key global-map (kbd "M-t")	'my-n-column-tabs)
(define-key global-map (kbd "M-w")	'my-kill-ring-save)
(define-key global-map (kbd "M-Z")	'list-processes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minibuffer modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key minibuffer-local-completion-map (kbd "<SPC>") 'minibuffer-complete)
(define-key minibuffer-local-must-match-map (kbd "<SPC>") 'minibuffer-complete)
(define-key minibuffer-local-filename-completion-map (kbd "<SPC>") 'minibuffer-complete)
;; Variable minibuffer-local-filename-must-match-map is gone in 24.3.
;;(define-key minibuffer-local-filename-must-match-map (kbd "<SPC>") 'minibuffer-complete)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-menu mode.  Done here because there is no Buffer-menu-mode-hook.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key Buffer-menu-mode-map (kbd "<backspace>") 'Buffer-menu-backup-unmark)
(define-key Buffer-menu-mode-map (kbd "C-h") 'Buffer-menu-backup-unmark)
(define-key Buffer-menu-mode-map "p"	'previous-line)
(define-key Buffer-menu-mode-map "n"	'next-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Occur mode.  Done here because there is no occur-mode-hook.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key occur-mode-map "n"		'next-line)
(define-key occur-mode-map "p"		'previous-line)
(define-key occur-mode-map " "		'my-occur-mode-goto-occurrence)
