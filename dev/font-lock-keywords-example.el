George Bush
Yahoo Serious
Albert Einstein
Barrack Obama
Now is the time for all good men to come to the aid of their country

;; Earlier entries in font-lock-keywords prevent later entries from being
;; applied to the same text, unless the later entry has OVERRIDE set to t.

(progn
  (turn-on-font-lock)
  (my-kill-to-bottom)
  (setq font-lock-keywords nil)

  (font-lock-add-keywords
   nil '(;; Both entries have OVERRIDE == nil ...
	 ("Bush" 0 'font-lock-function-name-face nil nil)
	 ("George.*$" 0 'font-lock-warning-face nil nil)

	 ;; Both entries have OVERRIDE == t ...
	 ("Serious" 0 'font-lock-function-name-face t nil)
	 ("Yahoo.*$" 0 'font-lock-warning-face t nil)

	 ;; One entry has OVERRIDE == t and one has OVERRIDE == nil ...
	 ("Albert.*$" 0 'font-lock-warning-face nil nil)
	 ("Einstein" 0 'font-lock-function-name-face t nil)

	 ;; One entry has OVERRIDE == t and one has OVERRIDE == nil ...
	 ("Obama" 0 'font-lock-function-name-face t nil)
	 ("Barrack.*$" 0 'font-lock-warning-face nil nil)

	 ("time for all good men" 0 'font-lock-warning-face nil)
	 ("Now is .* country" 0 'font-lock-function-name-face nil)
	 ("all good" 0 'font-lock-comment-face t)
	 )
   'set)

  (font-lock-fontify-buffer)
  (goto-char (point-min)))