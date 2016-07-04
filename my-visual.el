;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure Emacs' appearance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'apropos)
(require 'comint)
(require 'compile)
(require 'faces)
(require 'font-lock)
(require 'info)
(require 'man)
(require 'paren)
(require 'tool-bar)
(require 'tooltip)
(require 'smerge-mode)
(require 'hexl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line highlighting duration for next-error, previous-error, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sharp end of highlight after 2 seconds.
(setq pulse-iterations 1
      pulse-delay 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOL stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq eol-mnemonic-unix			"/"
      eol-mnemonic-dos			"\\"
      eol-mnemonic-mac			"|")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default tab width is 4.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 8)
(setq tab-stop-list (number-sequence tab-width 150 tab-width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight the region when the mark is active.  Also let commands use the mark
;; even when it's inactive.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-mark-mode 1)
(setq mark-even-if-inactive t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off the menu bar.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode -1)    ;; Yes, there's a menu bar when Emacs runs in a terminal.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-menu mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq Buffer-menu-name-width 43
      Buffer-menu-size-width 8
      Buffer-menu-mode-width 14)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable resizable minibuffer windows.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq resize-mini-windows t
      max-mini-window-height 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off initial splash screen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (boundp 'inhibit-splash-screen)
    (setq inhibit-splash-screen t)
  (defun use-fancy-splash-screens-p () nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show matching parentheses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq show-paren-delay 0)
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-show-faces-at (point)
  "Displays the names of faces that affect the character at
POINT (interactively, the value of point).  This function doesn't support glyph
faces (yet)."
  (interactive "d")
  (let ((faces (delete 'nil (nreverse (mapcar (lambda (ov)
						(overlay-get ov 'face))
					      (overlays-at point)))))
	(text-property-faces (cadr (memq 'face (text-properties-at point)))))

    ;; text-property-faces can be: a face symbol, a face name string, a list of face
    ;; attributes (e.g., (:foreground "red")), or a list of the previous things.
    (add-to-list 'faces text-property-faces 'append)

    (if (null faces)
	(setq faces '(default)))

    (let ((fgcolor (foreground-color-at-point)))
      ;; If fgcolor is a color name instead of an RGB value, append its RGB value in parens.
      (if (/= (aref fgcolor 0) ?#)
	  (setq fgcolor (concat fgcolor " (" (apply 'color-rgb-to-hex (color-name-to-rgb fgcolor)) ")")))

      (message (concat fgcolor ", " (mapconcat 'prin1-to-string faces ", "))))))

(defun my-font-lock-buttonizer (limit)
  "A function called via my-font-lock-universal-keywords to buttonize URLs.
Because this function calls make-text-button, the button is created by adding text
properties to the text, which mean these buttons remain in the buffer even if Font Lock
mode is turned off!"
  (save-excursion
    (while (re-search-forward "\\(\\(ftp\\|file\\|https?\\)://[^ )\t\r\n'\"]+\\)[].,!;]?" limit t)
      (make-text-button (match-beginning 1) (match-end 1)
			'face 'my-url-face
			'mouse-face 'my-url-mouse-face
			'help-echo "mouse-2, RET: Fetch this URL"
			'action (lambda (button) (browse-url (button-label button))))))
  ;; Return nil to prevent font-lock-mode from fontifying the button I just created.
  nil)

;; Earlier entries in font-lock-keywords prevent later entries from being
;; applied to the same text, unless the later entry has OVERRIDE set to t.

(defvar my-font-lock-universal-keywords
  '((my-font-lock-buttonizer)
    ;; Why is this here if function my-font-lock-buttonizer (above) handles this?
    ;;("\\b\\(http\\|https\\|file\\|ftp\\)://[^ \t\r\n)'\"]+" 0 'my-url-face t)
    ("\\bmagnet:[^ \t\r\n'\"]+" 0 'my-url-face t)
    ("UNDER CONSTRUCTION" 0 'font-lock-warning-face t)
    ("ISSUE:" 0 'font-lock-warning-face t)
    ("FIXME:" 0 'font-lock-warning-face t)
    ("BUG:" 0 'font-lock-warning-face t)
    ("WARNING:" 0 'font-lock-warning-face t)
    ("NOTE:" 0 'font-lock-warning-face t)
    ("IMPORTANT:" 0 'font-lock-warning-face t)
    ("QUESTION:" 0 'font-lock-warning-face t)
    ("ATTENTION[:!]+" 0 'font-lock-warning-face t)
    ("![A-Z0-9][A-Z0-9-]+" 0 'my-replaceable-field-face nil)
    ("TODO:" 0 'font-lock-warning-face t)
    ("\\b[a-fA-F0-9]\\{40\\}\\b" 0 'my-sha1-hash-face))
  "These are Font-Lock keyword entries that are always appended to the value of
font-lock-keywords in every buffer (see function my-font-lock-post-command-hook).")

(defun my-font-lock-add-keywords-universal ()
  "Appends the Font Lock keywords from my-font-lock-universal-keywords to the Font Lock
keywords for the current buffer.  This will affect all buffers sharing this buffer's Font
Lock keywords."
  (font-lock-add-keywords nil my-font-lock-universal-keywords 'append))

(defun my-toggle-font-lock-mode (&optional state)
  "Toggles Font Lock mode.  If activating Font Lock mode, also calls
my-font-lock-add-keywords-universal.  Interactively, always toggles Font Lock mode.  With
argument STATE, an integer, turns Font Lock mode on if STATE is non-zero, and turns it off
if STATE is zero."
  (interactive)
  (if (and (not (integerp state))
	   (not (null state)))
      (error (format "my-toggle-font-lock-mode: Invalid parameter '%s'!  Must be nil or an integer!" state)))

  ;; Do nothing if not using a GUI.
  (if (display-graphic-p)
      (if (or (and (integerp state)
		   (= 0 state))
	      (and (null state)
		   font-lock-mode))
	  ;; Turn off Font Lock mode.
	  (font-lock-mode 0)

	;; Turn on Font Lock mode.
	(my-font-lock-add-keywords-universal)
	(font-lock-mode 1))))

(defvar-local my-font-lock-post-command-hook-done nil
   "This variable is used to optimize my-font-lock-post-command-hook so it doesn't have to
do anything if it has already run in a given buffer.  This variable becomes local to the
current buffer whenever it is set.")

(defvar my-font-lock-disabled-modes
  '(Buffer-menu-mode help-mode apropos-mode Man-mode shell-mode erc-mode erc-chanlist-mode
    xref--xref-buffer-mode gnus-server-mode gnus-group-mode gnus-summary-mode gnus-article-mode)
  "A list of major modes in which my-font-lock-post-command-hook should _not_ turn on
Font-Lock mode.  Put a mode in this list if it does its own highlighting that would be
disrupted by Font-Lock mode.")

(defvar my-font-lock-disabled-buffers
  '("*erc-protocol*" "*GPG Messages*")
  "A list of buffer names in which my-font-lock-post-command-hook should _not_ turn on
Font-Lock mode.")

(defun my-font-lock-post-command-hook ()
  "This hook is a hack to set font-lock-keywords in fundamental-mode buffers.
This also turns on Font-Lock mode in almost every buffer.  That can also be done using
Global Font-Lock mode, but I've seen Global Font-Lock mode fail to turn on Font-Lock mode
sometimes, and this always works.  The one thing this hack can't do is turn on Font Lock
mode when find-file loads a buffer from a file or when a buffer is first created.  For
example, (with-current-buffer (get-buffer-create \"xyzzy\") font-lock-mode) evaluates to
nil if buffer xyzzy doesn't exist.  To close that loophole for find-file, we turn on Font
Lock mode in my-find-file-hook for every file."
  (if (display-graphic-p)
      ;; Ignore errors, because this function runs from a hook, and we don't want this
      ;; function being removed from the hook because it signaled an error.
      (ignore-errors
	(when (and (null my-font-lock-post-command-hook-done)
		   ;; Keep Font-Lock mode off in some buffers.  If it goes on in combination with
		   ;; some major modes, then it undoes the major mode's own non-Font-Lock
		   ;; highlighting as well as my own hook-based highlighting for that major mode.
		   (not (string-match-p "\\.bat$" (buffer-name)))
		   (not (eq (minibuffer-window) (selected-window)))
		   (not (memq major-mode my-font-lock-disabled-modes))
		   (not (member (buffer-name) my-font-lock-disabled-buffers))
		   (not (string= "*Anonymous " (substring (buffer-name) 0 (min (length (buffer-name))
									       (length "*Anonymous "))))))
	  (my-toggle-font-lock-mode 1)
	  (setq my-font-lock-post-command-hook-done t)))))
      
(add-hook 'post-command-hook 'my-font-lock-post-command-hook)

(defun my-font-lock-add-keywords-programming ()
  "Adds Font Lock keywords common to many programming languages."
  ;; IMPORTANT: Don't put stuff in here that applies to just one or two languages.
  ;; This function is called from many hook functions in my-hooks.el.
  (font-lock-add-keywords
   nil
   '(;; Make the decimal point in numbers appear in font-lock-constant-face.
     ("\\([-+]?\\.[0-9]+[fFlL]?\\)\\b" 1 'font-lock-constant-face nil)

     ("\\b\\(hack\\|Hack\\|HACK\\)\\b" 0 'font-lock-warning-face nil))
   nil))

(defun my-remove-face-property (start end)
  "Removes the 'face text property from the text in the current buffer between
START and END."
  (interactive (list (region-beginning) (region-end)))
  (remove-text-properties start end '(face nil)))

(defun my-preferred-font ()
  "Returns the name of my preferred font on this system.  Must be defined outside of the
below '(when (display-graphic-p) ...)' form, because it needs to be defined when running
Emacs with -batch when byte-compiling all of my Elisp files."
  (if (and my-win32
	   (not (eq system-type 'cygwin)))
      ;; We are running under the native Windows GUI (i.e., not in X under Cygwin on
      ;; Windows).
      (if (file-exists-p (concat my-systemdrive "/windows/fonts/sixbyten.fon"))
	  "sixbyten"
	"Lucida Console 8")

    ;; We are running under X (on either UNIX or Windows), so use X's 6x10 font.
    ;; ISSUE: Why not use "6x10" here?
    "-Misc-Fixed-Medium-R-Normal--10-100-75-75-C-60-ISO8859-1"))

(when (display-graphic-p)

  ;; Turn off Font-Lock mode globally, except when I interactively byte-compile a file or reload my
  ;; Emacs configuration.
  (if (and (not my-load-current-elisp-file-running)
	   (not my-reload-my-elisp-files-running))
      (global-font-lock-mode 0))

  ;; Put the hostname in the frame titles.
  (let ((short-hostname (if (string-match "^[^.]+" (system-name))
			    (match-string 0 (system-name))
			  "UNKNOWN HOST")))
    (setq frame-title-format (concat "emacs - " short-hostname " - %b")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Simplify the GUI.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 3-D mode lines.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when (>= emacs-major-version 21)
    (set-face-attribute 'mode-line nil :box '(:line-width -1 :style released-button))
    (if (facep 'mode-line-inactive)
        (set-face-attribute 'mode-line-inactive nil :box '(:line-width -1 :style released-button))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Define my faces.  Keep these alphabetical by face name.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (set-face-foreground (make-face 'my-alert-face) "yellow")
  (set-face-background (make-face 'my-alert-face2) "darkblue")
  (set-face-foreground 'my-alert-face2 "yellow")

  (set-face-foreground (make-face 'my-button-face) "pink")	;; #cf8
  (set-face-foreground (make-face 'my-button-mouse-face) "red")
  (set-face-underline 'my-button-mouse-face nil)

  (set-face-foreground (make-face 'my-grey-face) "#ccc")

  (make-face 'my-highlight-line-face)
  (set-face-background 'my-highlight-line-face "#00a")
  (set-face-foreground 'my-highlight-line-face "yellow")

  (set-face-foreground (make-face 'my-sha1-hash-face) "#fac")

  (make-face 'my-shell-mode-input-face)
  (set-face-foreground 'my-shell-mode-input-face "#faf")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Adjust some Emacs faces.  Keep these alphabetical by face or variable name.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Ordering exception: this must come before any reference to face default
  ;; below.
  (set-face-foreground 'default "#ffffdd")	;; Matches FOREGROUND variable in ~/bin/rxvt.
  (set-face-background 'default "#000")

  (when (facep 'apropos-symbol)
    (set-face-foreground 'apropos-symbol "orange")
    (set-face-bold 'apropos-symbol nil))

  (if (facep 'Buffer-menu-buffer)
      (set-face-bold 'Buffer-menu-buffer nil))

  (if (facep 'Buffer-menu-buffer-face)
      (set-face-bold 'Buffer-menu-buffer-face nil))

  (when (facep 'button)
    (set-face-underline 'button nil)
    (set-face-foreground 'button "orange"))

  (when (facep 'comint-highlight-prompt)
    (set-face-foreground 'comint-highlight-prompt "black")
    (set-face-background 'comint-highlight-prompt (face-foreground 'default)))

  (if (facep 'compilation-error)
      (set-face-bold 'compilation-error nil))

  (if (facep 'completions-first-difference)
      (set-face-bold 'completions-first-difference nil))

  (if (facep 'fixed-pitch)
      (set-face-font 'fixed-pitch (my-preferred-font)))

  (if (facep 'compilation-info-file-name)
      (set-face-underline 'compilation-info-file-name nil))

  (when (facep 'compilation-warning)
    (set-face-bold 'compilation-warning nil)
    (set-face-underline 'compilation-warning nil))

  (when (facep 'compilation-info)
    (set-face-foreground 'compilation-info "orange")
    (set-face-bold 'compilation-info nil)
    (set-face-underline 'compilation-line-number nil)
    (set-face-underline 'compilation-info nil))

  (require 'diff-mode)
  (set-face-background 'diff-file-header "darkblue")
  (set-face-foreground 'diff-file-header "yellow")
  (set-face-bold 'diff-file-header nil)
  (set-face-background 'diff-header "darkblue")
  (set-face-background 'diff-refine-changed "#666")
  (set-face-foreground 'diff-refine-changed "yellow")

  ;; Font-Lock Mode faces.
  (set-face-foreground 'font-lock-builtin-face "lightblue")
  (set-face-foreground 'font-lock-comment-face "#ff9")
  (set-face-foreground 'font-lock-constant-face "#f88")
  (set-face-foreground 'font-lock-doc-face "#fcf")
  (set-face-foreground 'font-lock-function-name-face "#0cf")
  (set-face-foreground 'font-lock-keyword-face "orange")
  (set-face-foreground 'font-lock-string-face "green")	;; Was "wheat"
  (set-face-foreground 'font-lock-type-face "#aff")
  (if (facep 'font-lock-preprocessor-face)
    (set-face-foreground 'font-lock-preprocessor-face "yellow"))
  (set-face-foreground 'font-lock-variable-name-face "#f8f")
  (set-face-foreground 'font-lock-warning-face "#f6f")
  (set-face-bold 'font-lock-warning-face nil)
  (set-face-bold 'font-lock-regexp-grouping-backslash nil)
  (set-face-foreground 'font-lock-regexp-grouping-backslash "yellow")
  (set-face-bold 'font-lock-regexp-grouping-construct nil)
  (set-face-foreground 'font-lock-regexp-grouping-construct "yellow")

  (when (facep 'fringe)
    (set-face-foreground 'fringe "#0aa")
    (set-face-background 'fringe "black"))

  (set-face-background 'header-line (face-background 'default))
  (set-face-foreground 'header-line "#f0f")

  (set-face-underline 'help-argument-name nil)

  (set-face-foreground 'hexl-address-region "lightblue")
  (set-face-foreground 'hexl-ascii-region "lightblue")

  (set-face-foreground 'highlight "orange")
  (set-face-background 'highlight (face-background 'default))	;; nil doesn't work!

  (set-face-bold 'icomplete-first-match nil)

  ;; This face is used to highlight the current search text.
  (set-face-foreground 'isearch "#000")     ;; Was #fff
  (set-face-background 'isearch "orange")   ;; Was #06c

  (when (facep 'lazy-highlight)
    ;; This face is used to highlight occurrences of the current search text other
    ;; than the one the cursor is on.
    (set-face-foreground 'lazy-highlight "#fff")	;; Was #000
    (set-face-background 'lazy-highlight "#066"))	;; Was orange

  (set-face-foreground 'minibuffer-prompt "cyan")

  (set-face-foreground 'mode-line "black")
  (set-face-background 'mode-line "#ece")
  (set-face-inverse-video 'mode-line nil)	;; Need to force this to nil on some Windows systems.

  (when (facep 'mode-line-buffer-id)
    (set-face-bold 'mode-line-buffer-id nil))

  (when (facep 'mode-line-inactive)
    (set-face-foreground 'mode-line-inactive (face-foreground 'mode-line))
    (set-face-background 'mode-line-inactive (face-background 'mode-line)))

  (set-face-background 'mouse "yellow")

  (set-face-foreground 'region "yellow")	;; Was magenta
  (set-face-background 'region (face-background 'default))

  (set-face-background 'show-paren-match (face-background 'default))  ;; nil doesn't work!
  (set-face-foreground 'show-paren-match "#0ff")

  (set-face-background 'smerge-refined-change "#888")

  (when (facep 'tooltip)
    (set-face-font 'tooltip (my-preferred-font))
    (set-face-background 'tooltip (face-foreground 'default))	;; nil doesn't work!
    (set-face-foreground 'tooltip "black"))

  (if (facep 'warning)
      (set-face-bold 'warning nil))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Font-lock stuff.
  ;;
  ;; Earlier entries in font-lock-keywords prevent later entries from being
  ;; applied to the same text, unless the later entry has OVERRIDE set to t.
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (make-face 'my-replaceable-field-face)
  (set-face-foreground 'my-replaceable-field-face "#f6f")

  (make-face 'my-url-face)
  (set-face-foreground 'my-url-face "orange")
  (set-face-underline 'my-url-face nil)

  (make-face 'my-url-mouse-face)
  (set-face-foreground 'my-url-mouse-face "red")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Set up initial-frame-alist and default-frame-alist.  The initial value of
  ;; initial-frame-alist is nil, so this setq is non-destructive.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (my-set-initial-frame-alist)

  (my-set-default-frame-alist)
  
  (if (not my-reload-my-elisp-files-running)
      (ignore-errors (kill-buffer "*Shell Command Output*"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure the mode line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (boundp 'my-orig-mode-line-format))
    (setq my-orig-mode-line-format mode-line-format))

(defvar my-mode-line-notice nil
  "A string to appear in the mode line after all other info.  If nil, nothing appears.")
  
;; Disable the tooltip that displays when the mouse is over the mode line.
(setq mode-line-default-help-echo nil)

(setq-default mode-line-format
	      '(""
		mode-line-mule-info
		" " mode-line-modified
		(window-system " " " [%F] ")
		(:eval (if (memq major-mode
				 '(gnus-group-mode gnus-summary-mode gnus-article-mode
				   gnus-server-mode))
			   (concat (my-gnus-server-status) " ")))
		mode-line-buffer-identification " " global-mode-string
		" %[(" mode-name mode-line-process minor-mode-alist "%n" ")%] "
		(which-func-mode ("" which-func-format " "))
		(line-number-mode " %2l")
		(column-number-mode "/%c")
		;; Need 4 %'s here; 2 for format processing and 2 for mode-line-format processing!
		" " (:eval (format "%d%%%%"
				   (truncate (* (/ (float (line-number-at-pos))
						   (float (line-number-at-pos (point-max)))) 100))))
		" <" (:eval (int-to-string (window-width))) "> "
		"--"
		(my-socks-in-use (:eval (concat " socks (" (car-safe socks-server) ") ")) "")
		"--"
		;; This (:eval ...) form tricks Emacs into not stripping the 'face
		;; property from the text stored in my-mode-line-notice.
		(my-mode-line-notice (:eval my-mode-line-notice) "")
		"%-"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My variables that control appearance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local my-highlight-line-overlay nil
   "...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My functions that control appearance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-reset-font ()
  "Resets the current frame's font to the one specified by variable my-preferred-font."
  (interactive)
  (set-frame-font (my-preferred-font) 'keep-frame-size)
  (set-frame-parameter nil 'my-current-font-in-rotation 0))

(defun my-highlight-string (string)
  "Returns string STRING with its 'face property set to my-alert-face."
  (propertize string 'face 'my-alert-face))

(defun my-message-highlight (string &rest args)
  "Same as message but highlights the resulting message using my-highlight-string."
  (apply 'message (my-highlight-string string) args))

(defun my-highlight-line ()
  "Highlights the current line using an overlay.  The overlay is stored in buffer local
variable my-highlight-line-overlay.  Returns the overlay object.  You can change the
appearance of the highlighted line by modifying face my-highlight-line-face."
  (let* ((sol (save-excursion
		(beginning-of-line)
		(point)))
	 eol-column
	 (eol (save-excursion
		(end-of-line)
		(setq eol-column (current-column))
		(point)))
	 overlay)
    (setq overlay (make-overlay sol eol)
	  my-highlight-line-overlay overlay)

    (overlay-put overlay 'face 'my-highlight-line-face)

    (if (> (window-width) eol-column)
	(overlay-put overlay 'after-string
		     (propertize (make-string (- (window-width) eol-column) 32)
				 'face 'my-highlight-line-face)))))

(defun my-unhighlight-line ()
  "..."
  (if (memq 'my-unhighlight-line pre-command-hook)
      (remove-hook 'pre-command-hook 'my-unhighlight-line))
  (if (overlayp my-highlight-line-overlay)
      (delete-overlay my-highlight-line-overlay))
  (setq my-highlight-line-overlay nil))

(defun my-highlight-region (start end)
  "Highlights the region by setting the 'face' text-property to the symbol
'highlight'.  NOTE: This does nothing if Font-Lock mode is on in the current
buffer."
  (interactive "r")
  (if font-lock-mode
      (error "Cannot highlight region when Font-Lock mode is on!"))
  (put-text-property start end 'face 'highlight))

(defun my-unhighlight-region (start end)
  "Unhighlights the region by setting the 'face' text-property to the symbol
'default'."
  (interactive "r")
  (if font-lock-mode
      (error "Cannot un-highlight region when Font-Lock mode is on!"))
  (put-text-property start end 'face 'default))

(defun my-rotate-font (&optional prefix)
  "Rotates the current frame's font through a predefined set."
  (interactive "P")
  (if (null (display-graphic-p))
      (error "Not using a window system!"))

  (let ((fonts (if my-win32
		   '("sixbyten"
		     ;;"Lucida Console 8"
		     ;;"Lucida Console 9"
		     "Lucida Console 10"
		     "Lucida Console 12"
		     ;;"Lucida Console 14"
		     "Lucida Console 16"
		     )
		 '("-Misc-Fixed-Medium-R-Normal--10-100-75-75-C-60-ISO8859-1"  ;; 6x10
		   "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1"  ;; 9x15
		   ))))

    ;; Cycle through the font list using numeric indices.  Don't do this by
    ;; matching the current font string against the elements of the above list,
    ;; because after setting the font to one of the above strings, the value of
    ;; (frame-parameter nil 'font) won't exactly match the string.

    (if (null (frame-parameter nil 'my-current-font-in-rotation))
	(set-frame-parameter nil 'my-current-font-in-rotation 0))

    (set-frame-parameter nil 'my-current-font-in-rotation
			 (mod (if prefix
				  (1- (frame-parameter nil 'my-current-font-in-rotation))
				(1+ (frame-parameter nil 'my-current-font-in-rotation)))
			      (length fonts)))

    (set-frame-font (nth (frame-parameter nil 'my-current-font-in-rotation) fonts) 'keep-size)
    (my-frame-redisplay-hack)
    (message "Font set to %s" (nth (frame-parameter nil 'my-current-font-in-rotation) fonts))))

(defun my-choose-frame-font ()
  "Lets the user interactively choose a font and sets it as the font of the
current frame."
  (interactive)
  (let ((font (if (fboundp 'x-select-font)
		  (x-select-font)
		(mouse-select-font))))
    (set-frame-font font 'keep-size)
    (my-frame-redisplay-hack)
    (message "Font set to %s" font)))

(defun my-adaptive-fill-function ()
  "..."
  (if (looking-at "[ 	]*\\(o\\|[0-9]+[.:]\\) +")
      (save-excursion
	(forward-char (- (match-end 0) (match-beginning 0)))
	;;(message "my-adaptive-fill-function matched!")
	(make-string (current-column) 32))))

(setq adaptive-fill-function 'my-adaptive-fill-function)

(defun my-rgb (red green blue)
  "Creates an Emacs color-map value out of a red/green/blue 3-tuple.  Each argument
must be in the range 0..255."
  (+ (* blue 65536) (* green 256) red))

(make-face 'my-mode-line-notice-face)

(defvar my-mode-line-notice-blink-timer nil
  "...")

(defvar my-mode-line-notice-blink-counter 0
  "...")

(defun my-blink-notice ()
  "..."
  (setq my-mode-line-notice-blink-counter (1+ my-mode-line-notice-blink-counter))
  (if (> my-mode-line-notice-blink-counter 10)
      ;; Cancel the timer.
      (cancel-timer my-mode-line-notice-blink-timer)

    ;; Blink the notice.
    (let ((oldfg (face-foreground 'my-mode-line-notice-face))
	  (oldbg (face-background 'my-mode-line-notice-face)))
      (set-face-foreground 'my-mode-line-notice-face oldbg)
      (set-face-background 'my-mode-line-notice-face oldfg))))

(defun my-display-notice (text)
  "Displays TEXT in the mode line using a distinctive face.  If TEXT is nil, the
notice is removed."
  (if (null text)
      (progn
	(cancel-timer my-mode-line-notice-blink-timer)
	(setq my-mode-line-notice nil))

    ;; Initialize the notice face.
    (set-face-foreground 'my-mode-line-notice-face "blue")
    (set-face-background 'my-mode-line-notice-face (face-background 'mode-line))

    (setq my-mode-line-notice (propertize (concat " " text " ")
					  'face 'my-mode-line-notice-face))

    ;; Start blinking.
    (setq my-mode-line-notice-blink-counter 0)
    (setq my-mode-line-notice-blink-timer (run-at-time nil 1 'my-blink-notice))))
