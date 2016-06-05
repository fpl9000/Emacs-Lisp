;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register eval-after-load forms.  The general form of an eval-after-load call
;; is:
;;
;; (eval-after-load "libname"
;;  '(progn
;;     (message "whatever")))
;;
;; Note that the "libname" must match EXACTLY the argument to (load ...), including
;; any ".el" or ".elc" suffix and any leading path components!  Also, don't forget
;; to quote the form that is the second argument, because it's evaluated before
;; it's passed to eval-after-load.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cc-mode)
(require 'csharp-mode)	;; Update this from https://github.com/josteink/csharp-mode.git
(require 'sgml-mode)
(require 'ielm)
(require 'comint)
(require 'info)
(require 'shell)
(require 'python)
(require 'sh-script)

(eval-after-load "ange-ftp"
  '(progn
     ;; With Emacs 21.2.1 on W2K, Cygwin's ftp never displays a prompt unless we
     ;; invoke it with the "-p" option.
     (if my-win32
         (add-to-list 'ange-ftp-ftp-program-args "-p"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timer-driven code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-tramp-cleanup ()
  "Run from an idle timer.  Kills buffer *tramp output* if it exists and is
zero bytes long."
  ;; Keep the *tramp...* buffers buried.
  (let ((bufs (buffer-list))
        buf
        bufname)
    (while (setq buf (car bufs))
      ;; Be careful: substring raises an error is arg #1 isn't at least 6
      ;; characters long!
      (setq bufname (buffer-name buf))
      (when (and (>= (length bufname) 6)
                 (string= "*tramp" (substring bufname 0 6)))
        (bury-buffer buf))
      (setq bufs (cdr bufs))))

  ;; Kill empty *tramp output* buffers.
  (let ((buf (get-buffer "*tramp output*")))
    (if (and buf (= 0 (buffer-size buf)))
        (kill-buffer buf))))

(run-with-idle-timer 2 'repeat #'my-tramp-cleanup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define some hook functions (then install them).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'scheme-mode-hook 'fundamental-mode)

(defun my-before-save-hook ()
  "Added to hook before-save-hook.  Updates the file's time-stamp line by calling function
time-stamp."
  (time-stamp))

(add-hook 'before-save-hook 'my-before-save-hook)

(defun my-buffer-menu-fontify ()
  "Fontifies the Buffer Menu buffer, which must be the current buffer."
  (if (not (eq major-mode 'Buffer-menu-mode))
      (error "my-buffer-menu-fontify called in non-Buffer Menu buffer!"))

  (make-face 'my-buffer-menu-header-face)
  (set-face-foreground 'my-buffer-menu-header-face "#bcf")	;; #8f8

  (make-face 'my-buffer-menu-directory-face)
  (set-face-foreground 'my-buffer-menu-directory-face "#0ff")

  (make-face 'my-buffer-menu-mail-face)
  (set-face-foreground 'my-buffer-menu-mail-face "pink")

  (make-face 'my-buffer-menu-elisp-face)
  (set-face-foreground 'my-buffer-menu-elisp-face "#ff0")

  (make-face 'my-buffer-menu-pending-delete-face)
  (set-face-foreground 'my-buffer-menu-pending-delete-face "#f00")

  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "/$" nil t)
	(beginning-of-line)
	(let ((start (point)))
	  (end-of-line)
	  (add-text-properties start (point) '(face my-buffer-menu-directory-face)))))

    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "\\(\\.cs\\|\\.[ch]\\(pp\\)?\\)\\b" nil t)
	(beginning-of-line)
	(let ((start (point))
	      (face (if (looking-at-p "^.*\\.h\\(pp\\)?\\b")
			'my-buffer-menu-header-face
		      'my-dired-code-face)))
	  (end-of-line)
	  (add-text-properties start (point) `(face ,face)))))

    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "\\s-Emacs-Lisp\\s-" nil t)
	(beginning-of-line)
	(let ((start (point)))
	  (end-of-line)
	  (add-text-properties start (point) '(face my-buffer-menu-elisp-face)))))

    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^....\\*" nil t)
	(beginning-of-line)
	(let ((start (point)))
	  (end-of-line)
	  (add-text-properties start (point) '(face my-grey-face)))))

    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "\\(\\*smtptrace\\*\\|\\*sent mail \\|\\*mail\\*\\)" nil t)
	(beginning-of-line)
	(let ((start (point)))
	  (end-of-line)
	  (add-text-properties start (point) '(face my-buffer-menu-mail-face)))))
    
    ;; BUG: This doesn't work.  Consider using Font Lock mode in Buffer Menu buffers.
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^D" nil t)
	(beginning-of-line)
	(let ((start (point)))
	  (end-of-line)
	  (add-text-properties start (point) '(face my-buffer-menu-pending-delete-face)))))))

(defun my-buffer-menu-mode-hook ()
  "Added to buffer-menu-mode-hook."

  ;; I have to use run-at-time here to delay this code until after list-buffers finishes.
  ;; If we do it when this hook runs, a call to function tabulated-list-print loses all
  ;; the text properties (which is probably a bug in tabulated-list-print).
  (run-at-time 0 nil
	       (lambda ()
		 (let ((inhibit-read-only t))
		   (if (null my-show-all-buffers)
		       (save-excursion
			 (goto-char (point-min))
			 (while (search-forward-regexp "\\(\\bTAGS\\b\\|\\*Breadcrumb Bookmarks\\*\\|\\*Completions\\*\\|[0-9]\\s-+ERC\\|[0-9]\\s-+ERC\\s-\\|\\*Man [^*]+\\*\\|\\*Article\\*\\|\\*Summary \\|\\.newsrc-dribble\\|\\*info\\*\\|\\*Calendar\\*\\|/news/nnfolder/\\|\\*Calculator\\*\\|\\*Calc Trail\\*\\)" nil t)
			   (beginning-of-line)
			   (let ((start (point)))
			     (forward-line 1)
			     (delete-region start (point))))))

		   ;; For some reason, the filename for a Dired buffer doesn't use ~ for my home directory.
		   ;; This fixes that in Buffer Menu buffers.
		   (save-excursion
		     (let ((inhibit-read-only t))
		       (goto-char (point-min))
		       (while (search-forward "c:/franl" nil t)
			 (replace-match "~" nil t))))

		   (my-buffer-menu-fontify))))

  (setq goal-column 4)

  (cd "~")

  (local-set-key "q" (lambda () (interactive)
		       (if (> (length (window-list)) 1)
			   (delete-window)
			 (quit-window))))
  (local-set-key "=" 'delete-other-windows)
  (local-set-key "C" 'my-buffer-menu-cleanup)
  (local-set-key "E" 'my-erc-list-buffers)
  (local-set-key "g" 'my-list-buffers))

(add-hook 'Buffer-menu-mode-hook 'my-buffer-menu-mode-hook)

(defun my-c-and-c++-mode-hook ()
  "Added to c-mode-hook and c++-mode-hook (as defined by cc-mode.el).  NOTE:
This hook is designed to be used with both C and C++ files, test the value of
the symbol major-mode against 'c-mode or 'c++-mode."
  (auto-fill-mode 1)
  (setq fill-column 100
	font-lock-multiline t)

  (my-font-lock-add-keywords-programming)

  ;; Earlier entries in font-lock-keywords prevent later entries from being
  ;; applied to the same text, unless the later entry has OVERRIDE set to t.

  ;; Prepend these Font Lock keywords to the existing keywords.

  (font-lock-add-keywords
   nil
   '(;; If it looks like a macro, give it font-lock-constant-face.
     ("\\<\\([A-Z0-9_]+\\)\\>" 1 'font-lock-constant-face nil)

     ("\\<__leave\\>" 0 'font-lock-keyword-face nil)

     ;; Every word followed by a '(' gets font-lock-function-name-face,
     ;; except "if", "for", "while", etc.
     ("\\<\\(if\\|for\\|while\\)\\>" 1 'font-lock-keyword-face nil)
     ("\\([A-Za-z0-9_~]+\\)\\s-*\(" 1 'font-lock-function-name-face nil)

     ;; C++ Mode's fontification sometimes colors namespace and class names as
     ;; constants when they appear to the left of :: operators.  This forces
     ;; them to be colored as types (unless they consist entirely of uppercase
     ;; letters).
     ("\\([A-Za-z0-9_]*[a-z][A-Za-z0-9_]*\\)::" 1 'font-lock-type-face nil)

     ;; Some pragmas are colored wrong.
     ("#pragma\\s-+\\(.*\\)$" 1 'font-lock-constant-face nil)

     ;; Sometimes these words are not colored as types.
     ("u?\\(int\\|short\\|long\\)[0-9]+" 0 'font-lock-type-face nil)
     )
   nil)

  ;; Append these Font Lock keywords to the existing keywords.  Only put
  ;; entries in this set if:
  ;;
  ;; 1. They need to apply to unfontified text, or ...
  ;;
  ;; 2. If they need to override already-fontified text and putting them
  ;;    in the above set doesn't work.

  (font-lock-add-keywords
   nil
   '(;; Some additional keywords.
     ("\\<\\(__asm\\)\\>" 1 'font-lock-keyword-face t)
     
     ("\\<__asm\\>\\s-*\\(.*\\)$" 1 'font-lock-string-face t)

     ;; Some macros in #define statements are colored wrong.
     ("#\\s-*define\\s-+\\([A-Za-z0-9_]+\\)" 1 'font-lock-constant-face t)

     ;; Unfontified words following '(' and ',' are almost always variables.
     ("[,(]\\s-*\\([A-Za-z0-9_]+\\)" 1 'font-lock-variable-name-face nil)

     ;; Unfontified words to the right of "=", ".", and "->" are almost always
     ;; variables.
     ("\\(=\\|\.\\|->\\)\\s-*\\([A-Za-z0-9_]+\\)" 2 'font-lock-variable-name-face nil)

     ;; Color namespaces as types.
     ("\\<namespace\\s-+\\([A-Za-z0-9_]+\\)" 1 'font-lock-type-face t)

     ;; Make type names to the right of "::" in template references
     ;; appear in font-lock-type-face.
     ("<[^>]+::\\([A-Za-z0-9_]+\\)[^:>]*>" 1 'font-lock-type-face t)

     ;; Replace all existing highlighting in a comment with font-lock-comment-face.
     ("//.*$" 0 'font-lock-comment-face t)
     )
   t)

  (local-unset-key (kbd "M-a"))
  (local-unset-key (kbd "C-c C-n"))

  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key (kbd "C-z h") 'my-c++-switch-source-for-header)
  (local-set-key (kbd "C-z u") 'my-insert-under-construction-C)
  (local-set-key (kbd "M-i") 'indent-according-to-mode)
  (local-set-key (kbd "M-q") 'my-c-fill-paragraph)
  (local-set-key (kbd "<return>") 'my-auto-indent-newline)
  (local-set-key (kbd "C-{") 'my-insert-brace-block)

  (dolist (num '("1" "2" "3" "4" "5" "6" "7"))
    (local-set-key (concat "\C-z" num) 'my-insert-logger-call))

  (local-set-key (kbd "C-z l") 'my-insert-logger-call)

  ;; Tabs are 4 columns wide.  No tab characters allowed.
  (my-n-column-tabs 4)
  (setq indent-tabs-mode nil)

  (let ((my-style (assoc-string "franl" c-style-alist 'ignore-case)))
    (if my-style
        (setq c-style-alist (delete my-style c-style-alist)))

    (add-to-list 'c-style-alist
                 `("franl"
                   (c-basic-offset . ,tab-width)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-double-slash-is-comments-p . t)
                   (c-echo-syntactic-information-p . nil)
                   (c-electric-pound-behavior . (alignleft))
                   (c-indent-comments-syntactically-p . t)
                   (c-hanging-comment-ender-p . nil)
                   (c-recognize-knr-p . nil)
                   (c-offsets-alist . ((substatement-open . 0)
                                       (brace-list-open . 0)
                                       (inline-open . 0)
                                       ;;(c . 1)
                                       (arglist-close . ++)))))
    (c-set-style "franl"))
  )

(add-hook 'c++-mode-hook 'my-c-and-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c-and-c++-mode-hook)

(defun my-csharp-mode-hook ()
  "Added to csharp-mode-hook."
  (auto-fill-mode 1)
  (setq fill-column 100)

  (modify-syntax-entry ?_ "_" csharp-mode-syntax-table)

  (my-font-lock-add-keywords-programming)

  (set-face-foreground (make-face 'my-font-lock-comment-xml-face) "#ccc")
  (set-face-foreground (make-face 'my-font-lock-operators-face) "#ff0")

  ;; Earlier entries in font-lock-keywords prevent later entries from being
  ;; applied to the same text, unless the later entry has OVERRIDE set to t.

  ;; Prepend these Font Lock keywords to the existing keywords.

  (font-lock-add-keywords
   nil
   '(;; If it is all caps, give it font-lock-constant-face.
     ("\\<\\([A-Z0-9_]+\\)\\>" 1 'font-lock-constant-face nil)
     )
   nil)

  ;; The default Font Lock keywords appear between the above set and the below set.  This
  ;; is where language keywords etc. are fontified.

  ;; Append these Font Lock keywords to the existing keywords.  Only put entries in this
  ;; set if:
  ;;
  ;; 1. They need to apply to text that is unfontified after all other processing.
  ;;
  ;; 2. They need to override already-fontified text and putting them in the above set
  ;;    doesn't work.  In this case, set the override flag to t in the entry.

  (font-lock-add-keywords
   nil
   '(
     ;; Some unfontified keywords.
     ("\\<\\(else\\|unsafe\\|catch\\|fixed\\)\\>" 1 'font-lock-keyword-face t)

     ;; Unfontified words between two '.'s are almost always variables.
     ("\\.\\([a-zA-Z0-9_]+\\)\\." 1 'font-lock-variable-name-face nil)

     ;; Unfontified words to the left of '.' starting with a capital letter are
     ;; almost always type names.
     ("\\<\\([A-Z][a-zA-Z0-9_]*\\)\\." 1 'font-lock-type-face nil)
     
     ;; Unfontified words to the left of '.' starting with a lowercase letter
     ;; are almost always variables.
     ("\\<\\([a-z][a-zA-Z0-9_]*\\)\\." 1 'font-lock-variable-name-face nil)
     
     ;; Unfontified words immediately to the right of '.' and '!' are almost always
     ;; variables.
     ("[\\.!]\\([a-zA-Z0-9_]+\\)" 1 'font-lock-variable-name-face nil)
     
     ;; Unfontified words to the right of '=' and '*' and which are not followed
     ;; by '.'  are almost always variables.
     ("[=*]\\s-*\\([a-zA-Z0-9_]+\\)[^.]" 1 'font-lock-variable-name-face nil)
     
     ;; Unfontified words to the left of '[' are almost always variables.
     ("\\<\\([a-zA-Z0-9_]+\\)\\s-*\\[" 1 'font-lock-variable-name-face nil)
     
     ;; Unfontified words to the left of regexp "[-+*/%]?=" are almost always variables.
     ("\\<\\([a-zA-Z0-9_]+\\)\\s-*[-+*/%]?=" 1 'font-lock-variable-name-face nil)
     
     ;; An unfontified single word in "catch (...)" is always a type name.
     ("catch\\s-*(\\([a-zA-Z0-9_]+\\))" 1 'font-lock-type-face nil)

     ;; Unfontified words following "return" are almost always variables.
     ("\\<return\\>\\s-*\\([A-Za-z0-9_]+\\)" 1 'font-lock-variable-name-face nil)

     ;; Unfontified words following '(', ',', and '+' are almost always variables.
     ("[,(+]\\s-*\\([A-Za-z0-9_]+\\)" 1 'font-lock-variable-name-face nil)

     ;; Words in angle brackes are almost always types (see below for an entry to
     ;; handle XML comments.
     ("<[a-zA-Z0-9_]+>" 0 'font-lock-type-face t)

     ;; Words after keyword "ref" are almost always variables.
     ("\\<ref\\>\\s-*\\<\\([a-zA-Z0-9_]+\\)\\>" 1 'font-lock-variable-name-face t)
     
     ;; Fix a bug where "using System.IO;" fails to correctly fontify the word
     ;; "System".
     ("^using \\([^;]+\\);\\s-*$" 1 'font-lock-constant-face t)

     ;; Words followed immediately by "(" are colored as function names.
     ("\\([A-Za-z0-9_~]+\\)(" 1 'font-lock-function-name-face t)

     ;; For some reason, code like "if (" gets the keyword colored as a function name.
     ;; This fixes that.
     ("\\<\\(if\\|for\\|foreach\\|while\\|lock\\|catch\\)\\> " 1 'font-lock-keyword-face t)

     ;; Override the standard fontification within comments.  This prevents keywords and
     ;; the word following the word 'namespace' from being fontified in a comment.
     ;; IMPORTANT: Keep this entry and the next one near the end of the list, otherwise
     ;; keywords start to get fontified in comments.
     ("//.*$" 0 'font-lock-comment-face t)

     ;; XML tags in comments.  NOTE: This uses the obscure MATCH-ANCHORED syntax.
     ("///" (0 'font-lock-comment-face t) ("<[^>]+>" nil nil (0 'my-font-lock-comment-xml-face t)))

     ;; Make all unfontified words look like a variable.
     ("[a-zA-Z0-9_]+" 0 'font-lock-variable-name-face nil)
     )
   t)

  (local-unset-key (kbd "M-a"))

  (local-set-key (kbd "TAB")	'tab-to-tab-stop)
  (local-set-key (kbd "C-z u")	'my-insert-under-construction-C)
  (local-set-key (kbd "M-i")	'indent-according-to-mode)
  (local-set-key (kbd "M-q")	'my-c-fill-paragraph)
  (local-set-key (kbd "<return>") 'my-auto-indent-newline)
  (local-set-key (kbd "{")	'c-electric-brace)

  ;; Tabs are 4 columns wide.  No tab characters allowed.
  (my-n-column-tabs 4)
  (setq indent-tabs-mode nil)

  (let ((my-style (assoc-string "franl" c-style-alist 'ignore-case)))
    (if my-style
        (setq c-style-alist (delete my-style c-style-alist)))

    (add-to-list 'c-style-alist
                 `("franl" "C#"
                   (c-echo-syntactic-information-p . nil)
                   (c-basic-offset . ,tab-width)
                   ;;(c-comment-only-line-offset . (0 . 0))
                   ;;(c-double-slash-is-comments-p . t)
                   ;;(c-electric-pound-behavior . (alignleft))
                   ;;(c-indent-comments-syntactically-p . t)
                   ;;(c-hanging-comment-ender-p . nil)
                   ;;(c-recognize-knr-p . nil)
                   (c-offsets-alist . ((substatement-open . 0)
                                       (brace-list-open . 0)
                                       (inline-open . 0)
                                       (arglist-close . ++)))))
    (c-set-style "franl"))
  )

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(defun my-compilation-mode-hook ()
  "Added to compilation-mode-hook."
  (local-set-key "q" (lambda () (interactive)
		       (bury-buffer (current-buffer))
		       (delete-window))))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun my-docbook-xml-mode-hook ()
  "Aded to docbook-xml-mode-hook."
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key "\M-i" 'docbook-xml-indent-current-line))

(add-hook 'docbook-xml-mode-hook 'my-docbook-xml-mode-hook)

(defun my-diff-mode-hook ()
  "Added to diff-mode-hook."
  (set-face-bold 'diff-header nil)

  (local-set-key (kbd "<return>") 'diff-goto-source)
  (local-set-key "n" 'diff-hunk-next)
  (local-set-key "p" 'diff-hunk-prev)
  (local-set-key "\M-n" 'diff-file-next)
  (local-set-key "\M-p" 'diff-file-prev)
  (local-set-key "q" (lambda () (interactive)
		       (bury-buffer (current-buffer))
		       (if (= 1 (length (window-list)))
			   (switch-to-buffer nil)
			 (delete-window)))))

(add-hook 'diff-mode-hook 'my-diff-mode-hook)

(defun my-emacs-lisp-mode-hook ()
  "Added to emacs-lisp-mode-hook."
  (my-font-lock-add-keywords-programming)

  (local-set-key (kbd "C-z ;") (lambda () (interactive (insert (make-string 80 ?;)))))
  (local-set-key (kbd "C-z u") 'my-insert-under-construction-lisp)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key (kbd "M-i") 'indent-according-to-mode)
  (local-set-key (kbd "RET") 'my-auto-indent-newline)
  (local-set-key (kbd "M-C-b") 'my-byte-compile-current-file)
  (local-set-key (kbd "M-B") 'my-load-current-elisp-file)
  (local-set-key (kbd "M-C-f") 'compile-defun))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; TODO: Move this function and keymap out of this file.

;; Support paragraph filling in .ahk files.  Do not bind bind this function to variable
;; fill-paragraph-function in .ahk buffers: it will cause infinite recursion, because it
;; calls fill-paragraph.
(defun my-ahk-fill-paragraph-function ()
  (interactive)
  (let ((origin (point-marker)))
    (save-excursion
      (back-to-indentation)
      (if (not (looking-at-p ";"))
	  (error "Point not positioned inside a comment!")))

    ;; Find the first line of the current comment "paragraph".
    (let* ((paragraph-content "^\\s-*;; .+$")
	   (pstart (progn (while (progn
				  (beginning-of-line)
				  (if (looking-at-p "^\\s-*;[^;]")
				      (error "Malformed comment block: a line starts with just one ';'!"))
				  (and (not (bobp))
				       (looking-at-p paragraph-content)))
			   (forward-line -1))
			  (if (not (looking-at-p paragraph-content))
			      (forward-line 1))
			  (point)))
	   (pend (progn (while (looking-at-p paragraph-content)
			  (forward-line 1))
			(point))))
      (save-restriction
	(narrow-to-region pstart pend)
	(goto-char pstart)
	(fill-paragraph))
      
      (goto-char origin))))

(defvar my-ahk-local-map (make-sparse-keymap)
  "A keymap used in AutoHotkey script buffers.  See function my-find-file-hook.")

(defun my-find-file-hook ()
  "Added to find-file-hook.  Called after a buffer is loaded from a file."
  (if (member-ignore-case (buffer-name) '(".mtg" "journal.txt.asc" "passwords.txt.asc" "pw.txt"))
      (set (make-local-variable 'backup-inhibited) t))

  ;; Filename-based operations.
  (if (string-match-p "\\.pop$" (buffer-file-name))
      ;; Special handling of .pop (PopSel) files.
      ;; Earlier entries in font-lock-keywords prevent later entries from being
      ;; applied to the same text, unless the later entry has OVERRIDE set to t.
      (font-lock-add-keywords nil '(("^\\([^=]+\\)=.*$" 1 'font-lock-function-name-face nil)
				    ("^[^=]+=\\(.*\\)$" 1 'font-lock-comment-face t))
			      nil)

    (when (string-match-p "\\.ahk$" (buffer-file-name))
      ;; Special handling of .ahk (AutoHotkey) files.
      (use-local-map my-ahk-local-map)
      (local-set-key (kbd "M-q") 'my-ahk-fill-paragraph-function)

      ;; Earlier entries in font-lock-keywords prevent later entries from being
      ;; applied to the same text, unless the later entry has OVERRIDE set to t.
      (font-lock-add-keywords nil '(("\\([a-zA-Z_0-9]+\\)(" 1 'font-lock-function-name-face nil)
				    ("{[a-zA-Z0-9_]+}" 0 'font-lock-keyword-face nil)
				    ("\\b\\(true\\|false\\|ErrorLevel\\)\\b" 0 'font-lock-keyword-face nil)
				    ("ahk_[a-z]+" 0 'font-lock-keyword-face nil)
				    ("\\b\\([0-9]+\\)\\b" 1 'font-lock-constant-face nil)
				    ("\\b\\(0x[0-9A-Fa-f]+\\)\\b" 1 'font-lock-constant-face nil)
				    ("\\b\\(A_[A-Za-z0-9_]*\\)\\b" 1 'font-lock-variable-name-face nil)
				    ("\\b\\([A-Z_][A-Z_0-9_]*\\)\\b" 1 'font-lock-variable-name-face nil)
				    ("\\b\\(Else\\|If\\|ByRef\\)\\b" 0 'font-lock-keyword-face nil)
				    ("^\\s-*\\([A-Z][a-zA-Z]+\\)\\( \\|$\\)" 1 'font-lock-keyword-face nil)
				    ("::\\([A-Z][a-zA-Z_]+\\)" 1 'font-lock-keyword-face nil)
				    ("^\\(.*\\)::" 1 'font-lock-function-name-face t)
				    (":=\\|=" 0 'font-lock-keyword-face nil)
				    (" \\([.%]\\) " 1 'font-lock-keyword-face nil)
				    ("^[()]" 0 'font-lock-keyword-face nil)
				    ("#\\S-+" 0 'font-lock-keyword-face nil)
				    ("^\\S-+:" 0 'font-lock-function-name-face t)
				    ("%[^%\"]+%" 0 'font-lock-variable-name-face t)
				    ("[{}]" 0 'font-lock-keyword-face t)
				    ("::" 0 'font-lock-keyword-face t)
				    (";.*$" 0 'font-lock-comment-face t)
				    )
			      nil)))

  ;; Turn on Font Lock mode (with some exceptions).
  (my-font-lock-post-command-hook))

(add-hook 'find-file-hook 'my-find-file-hook)

(defun my-grep-mode-hook ()
  "Added to grep-mode-hook."
  (if (facep 'compilation-warning-face)
      (set-face-bold 'compilation-warning-face nil))
  (if (facep 'compilation-info-face)
      (set-face-bold 'compilation-info-face nil)))

(add-hook 'grep-mode-hook 'my-grep-mode-hook)

(defun my-help-mode-hook ()
  "Added to help-mode-hook."
  (set-face-italic 'help-argument-name nil)
  (set-face-foreground 'help-argument-name "orange"))

(add-hook 'help-mode-hook 'my-help-mode-hook)

(defun my-html-mode-hook ()
  "Added to html-mode-hook."
  (local-set-key (kbd "C-z <") "&lt;")
  (local-set-key (kbd "C-z >") "&gt;")
  (local-set-key (kbd "C-z &") "&amp;")
  (local-set-key (kbd "C-z SPC") "&nbsp;")
  (local-set-key (kbd "C-z l") 'html-href-anchor))

(add-hook 'html-mode-hook 'my-html-mode-hook)

(defun my-ielm-mode-hook ()
  "Added to ielm-mode-hook."
  (set-process-query-on-exit-flag (car (process-list)) nil)
  (setq ielm-noisy nil          ; No beeps on errors.
        ielm-prompt "\nelisp: "))

(add-hook 'ielm-mode-hook 'my-ielm-mode-hook)

(defun my-Info-mode-hook ()
  "Added to Info-mode-hook."
  ;; Adjust some Info faces.
  (set-face-foreground 'info-title-1 "#fc6")
  (set-face-foreground 'info-title-2 "#fc6")
  (set-face-foreground 'info-title-3 "#fc6")
  (set-face-foreground 'info-title-4 "#fc6")
  (set-face-foreground 'info-xref-visited "#6af")
  (set-face-bold 'info-menu-header nil)
  (set-face-bold 'info-title-4 nil)
  (set-face-foreground 'info-xref-visited "#f0c")
  (set-face-underline 'info-xref-visited nil)
  (ignore-errors
    (set-face-font 'info-menu-header
                   (if my-win32
                       "-outline-Arial-normal-r-normal-normal-11-82-96-96-p-*-iso10646-1"
                     "-outline-Helvetica-normal-r-normal-normal-13-97-96-96-p-*-iso10646-1")))
  (set-face-foreground 'info-menu-header "orange")
  (set-face-underline 'info-xref nil)

  (when (eq window-system 'x)
    (set-face-font 'info-node "6x10")
    (set-face-foreground 'info-node "yellow")
    (set-face-font 'info-xref "6x10")
    (set-face-foreground 'info-xref "cyan")
    (if (facep 'info-xref-visited)
        (set-face-foreground 'info-xref-visited "#0af")))
  (local-set-key " " 'scroll-up)
  (local-set-key "b" 'scroll-down)
  (local-set-key "B" 'scroll-down)
  (local-set-key "\M-n" 'my-next16-line)        ;; Replaces clone-buffer binding.
  (local-set-key "\M-s" 'my-scroll-down-one-line)
  (local-set-key "\C-h" 'scroll-down)
  (local-set-key [backspace] 'scroll-down)
  (local-set-key [delete] 'scroll-down))

(add-hook 'Info-mode-hook 'my-Info-mode-hook)

(defun my-isearch-mode-hook ()
  "Added to isearch-mode-hook."
  ;; IMPORTANT: Don't use local-set-key here or the local-map of the buffer where isearch is running
  ;; changes!
  (define-key isearch-mode-map [delete] 'isearch-delete-char)
  (define-key isearch-mode-map [backspace] 'isearch-other-control-char)
  (define-key isearch-mode-map "\C-h"   'isearch-other-control-char))

(add-hook 'isearch-mode-hook 'my-isearch-mode-hook)

(defun my-java-mode-hook ()
  "Added to java-mode-hook."
  (auto-fill-mode 1)    ;; Allows text in comments to wrap nicely.
  (setq fill-column 100)

  (my-font-lock-add-keywords-programming)

  (make-face 'my-javadoc-keyword-face)
  (set-face-foreground 'my-javadoc-keyword-face "pink")

  (font-lock-add-keywords nil '(("@see \\(<.*>\\)$"
                                 1 'font-lock-doc-face nil)
                                ("<[^> \n\r]*>"
                                 0 'font-lock-constant-face nil)
                                ("\\(@[a-z]+\\)\\s-"
                                 1 'my-javadoc-keyword-face nil))
                          nil)

  (local-unset-key "\C-c\C-n")
  (local-unset-key "\M-a")              ;; Expose my Fundamental mode binding.
  (local-set-key [f11] 'my-java-compile)
  (local-set-key [M-f11] (lambda () (interactive) (my-java-compile 'all)))
  (local-set-key [C-M-f11] 'my-java-clean)
  (local-set-key [f12] 'my-java-run)
  (local-set-key [M-f12] 'my-java-generate-documentation)
  (local-set-key "\r" 'my-auto-indent-newline)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key "\C-zu" 'my-insert-under-construction-java)
  (local-set-key "\M-i" 'indent-according-to-mode)

  (my-n-column-tabs 4)
  (setq indent-tabs-mode nil)

  (setq c-basic-offset                  4
        c-echo-syntactic-information-p  t
        c-offsets-alist                 `((substatement-open . 0)
                                          ,@c-offsets-alist)))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-js-mode-hook ()
  "My Javascript mode hook.  Bound to js-mode-hook."
  (setq fill-column 100
	indent-tabs-mode nil)

  (my-n-column-tabs 4)
  (my-font-lock-add-keywords-programming)

  (font-lock-add-keywords
   nil
   '(;; Every word followed by a '(' gets font-lock-function-name-face,
     ;; except "if", "for", "while", etc.
     ("\\<\\(if\\|for\\|each\\|while\\|catch\\)\\>" 1 'font-lock-keyword-face nil)
     ("\\([A-Za-z0-9_~]+\\)\\s-*\(" 1 'font-lock-function-name-face nil)
     
     ;;("\\([a-zA-Z0-9_]+\\)\\." 1 'font-lock-variable-name-face nil)
     ;;("\\([a-zA-Z0-9_]+\\)\\s-*=" 1 'font-lock-variable-name-face nil)
     ))

  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key "\M-i" 'indent-according-to-mode)
  (local-set-key "\r" 'my-auto-indent-newline))

(add-hook 'js-mode-hook 'my-js-mode-hook)

(defun my-kill-buffer-query-function ()
  "Added to kill-buffer-query-functions.  This is executed with the buffer to be killed
current.  If the buffer should not be killed, returns nil, which prevents it from being
killed."
  (let ((bufname (buffer-name)))
    (or (not (member bufname '("*scratch*" "*Messages*")))
	(yes-or-no-p (format "Are you sure you want to kill buffer %s? " bufname)))))

(add-hook 'kill-buffer-query-functions 'my-kill-buffer-query-function)

(defun my-lisp-interaction-mode-hook ()
  "Added to lisp-interaction-mode-hook."
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key "\M-i" 'indent-according-to-mode)
  (local-set-key "\r" 'my-auto-indent-newline))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)

(defun my-log-view-mode-hook ()
  "Added to log-view-mode-hook."
  (set-face-bold 'log-view-message nil)
  (set-face-foreground 'log-view-message "orange")
  (local-set-key "q" (lambda () (interactive)
		       (bury-buffer (current-buffer))
		       (delete-window))))

(add-hook 'log-view-mode-hook 'my-log-view-mode-hook)

(defun my-makefile-mode-hook ()
  "Added to makefile-mode-hook."

  ;; This isn't needed if I put "@" in front of the comment.
  ;;
  ;; Don't highlight tabs before comments in commands as a syntax error.
  ;;(font-lock-add-keywords nil '(("^	+#" 0 'default t)) t)

  (local-set-key "\M-p" 'my-previous16-line)
  (local-set-key "\M-n" 'my-next16-line)
  (set (make-local-variable 'paragraph-start) "#[ \t]*")
  (set (make-local-variable 'paragraph-separate) "#[ \t]*$"))

(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)

(defun my-Man-mode-hook ()
  "Added to Man-mode-hook."
  (set-face-bold 'Man-overstrike nil)
  (set-face-foreground 'Man-overstrike "cyan")

  (local-set-key "G" 'end-of-buffer)
  (local-set-key [backspace] 'scroll-down)
  (local-set-key "\C-h" 'scroll-down)
  (local-set-key "\C-x\C-k" 'kill-buffer)
  (local-set-key [return] 'my-scroll-up-one-line)
  (local-set-key "\r" 'my-scroll-up-one-line)
  (local-set-key "\M-n" 'my-next16-line)
  (local-set-key "\M-p" 'my-previous16-line)
  (local-set-key "q" 'my-delete-frame-or-save-buffers-kill-emacs)
  ;; Man-mode buffers are displayed in dedicated windows, so my "C-x C-k"
  ;; binding deletes the wrong frame.  This fixes that.
  (local-set-key "\C-x\C-k" 'my-delete-frame-or-save-buffers-kill-emacs))

(add-hook 'Man-mode-hook 'my-Man-mode-hook)

(defun my-perl-mode-hook ()
  "Added to perl-mode-hook."
  (my-font-lock-add-keywords-programming)
  (add-hook 'local-write-file-hooks #'my-delete-trailing-whitespace)

  (local-set-key "\C-zu"
                 (lambda () (interactive)
                   (let ((indent (make-string (current-column) 32)))
                   (save-excursion
                     (insert "####################################################\n"
                             indent
                             "#                UNDER CONSTRUCTION                #\n"
                             indent
                             "####################################################")))))
  (local-set-key "\r" 'my-auto-indent-newline)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key "\M-i" 'indent-according-to-mode))

(add-hook 'perl-mode-hook 'my-perl-mode-hook)

(defun my-python-mode-hook ()
  "Added to python-mode-hook."
  (my-n-column-tabs 4)
  (setq fill-column 85
	indent-tabs-mode t  ;; Allow tabs.
	python-indent-offset 4
	;; python-indent-levels '(0 4)  ;; Obsolete as of 24.5.
	python-fill-docstring-style 'pep-257-nn)

  (font-lock-add-keywords nil '(("\\([a-zA-Z0-9_]+\\)(" 1 'font-lock-function-name-face nil)) t)

  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key (kbd "M-i") 'indent-according-to-mode)
  (local-set-key (kbd "C-z u") 'my-insert-under-construction-shell))

(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun my-ruby-mode-hook ()
  "Added to ruby-mode-hook."
  (my-font-lock-add-keywords-programming)

  (setq paragraph-separate "\\s-*#\\(\\+\\+\\|\\s-*\\[.*\\]\\)?\\s-*$")

  (setq indent-tabs-mode t)	;; ruby-model.el sets this to nil!

  (local-set-key "\r" 'my-auto-indent-newline)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key "\M-i" 'indent-according-to-mode)
  (local-set-key "\C-zu" 'my-insert-under-construction-shell)
  (local-set-key "\C-zd" 'my-ruby-rdoc))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(defun my-sh-mode-hook ()
  "Added to sh-mode-hook."
  (font-lock-add-keywords nil '(("\\$([^)]+)" 0 'sh-quoted-exec t)
				("\\b\\(echo\\|if\\|then\\|else\\|elif\\|fi\\|while\\|do\\|done\\|continue\\|break\\|case\\|esac\\|shift\\|cd\\|for\\|in\\|unset\\|export\\|true\\|false\\|unexport\\|alias\\|unalias\\|eval\\|\\.\\|exec\\|exit\\|read\\|builtin\\|local\\|return\\|trap\\|declare\\)\\b" 1 'font-lock-keyword-face nil)
				("(:)\\s " 1 'font-lock-keyword-face nil)
				(";;\\|&&\\|||" 0 'font-lock-keyword-face nil)
				("[\\&]$" 0 'font-lock-keyword-face nil)
				("&?[<>]&?[0-9]?" 0 'font-lock-keyword-face nil)
				("^\\s *case\\b.*\\(in\\)\\s *$" 1 'font-lock-keyword-face nil)
				("\\b\\(unset\\|read\\)\\b\\s +\\([A-Za-z_0-9 \t]+\\)" 2 'font-lock-variable-name-face nil)
				("\\bunset\\s +-f\\s +\\([A-Za-z_0-9 \t]+\\)" 1 'font-lock-function-name-face nil)
				("\\s ?\\(\\[\\[?\\|\\]\\]?\\)\\(\\s \\|$\\)" 1 'my-sh-test-command-face nil)
				("((\\|))" 0 'my-sh-test-command-face nil)
				("\\(\\b[a-zA-Z_0-9]+\\)()" 1 'font-lock-function-name-face nil)
				("\"[^\"]+\"" 0 'font-lock-string-face nil)
				("\\${?\\([A-Z_0-9]+\\)" 1 'font-lock-variable-name-face nil)
				("\\belseif\\b" 0 'my-syntax-error-face nil)
				("\\balias\\s +\\(\\S +\\)=" 1 'font-lock-function-name-face nil)
				("\\([A-Za-z_0-9]+\\)\\+?=" 1 'font-lock-variable-name-face nil)
				("^\\s-*\\blocal\\b\\s +\\([A-Za-z_0-9 ]+\\)" 1 'font-lock-variable-name-face t)
				("\\bfor\\s-+\\([A-Za-z_0-9]+\\)\\s-+in\\b" 1 'font-lock-variable-name-face t)
				("\\bdeclare\\s-+\\(.*\\b\\([A-Z_0-9]+\\)\\b.*\\)+" 2 'font-lock-variable-name-face t)
				("\\[\\([^]]+\\)\\]=" 1 'font-lock-variable-name-face nil)
				("\\b\\([A-Za-z_0-9]+\\)\\[[^]]+\\]=" 1 'font-lock-variable-name-face nil)
				)
                          'set)

  (set-face-foreground (make-face 'my-sh-test-command-face) "cyan")
  (set-face-foreground (make-face 'my-syntax-error-face) "black")
  (set-face-foreground 'sh-heredoc "green")
  (set-face-background 'my-syntax-error-face "#f44")
  (set-face-foreground 'sh-quoted-exec "#ff0")

  (if (facep 'sh-heredoc)
      (set-face-bold 'sh-heredoc nil))

  (setq sh-indentation 8
	sh-basic-offset 8
	sh-indent-after-switch 0
	sh-indent-after-case 0
	sh-indent-for-case-label 0
	sh-indent-after-continuation 'always)

  (set-fill-column 100)

  (my-n-column-tabs 8)

  (local-unset-key (kbd "C-c +"))
  (local-set-key (kbd "C-c >") 'my-indent-region-with-string)
  ;;(local-set-key (kbd "RET") 'my-newline-basic)  ;; Not needed due to sh-indentation.
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key (kbd "M-i") 'indent-according-to-mode))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)

(defvar-local my-shell-mode-recompute-default-directory nil
  "...")

(defun my-shell-mode-post-command-hook ()
  "Highlights commands as they are typed."
  (ignore-errors
    (if (memq this-command '(self-insert-command comint-dynamic-complete yank))
	(let* ((proc (get-buffer-process (current-buffer)))
	       (cmdstart (and proc (marker-position (process-mark proc)))))
	  (if (>= (point) cmdstart)
	      (add-text-properties cmdstart (point-max) '(face my-shell-mode-input-face)))))))

(defun my-shell-mode-preoutput-filter-hook (string)
  "Highlights lines that look like my prompt.  Also highlights compiler errors in the
most recently output text."
  (when my-shell-mode-recompute-default-directory
    (setq my-shell-mode-recompute-default-directory nil)
    (if (string-match "^/.*$" string)
	(let ((new-cwd (match-string 0 string)))
	  (setq default-directory new-cwd)
	  (message new-cwd))))

  ;; Potential bug: This won't work if this function is passed only part of the prompt.
  (let ((last-line (car (last (split-string string "[\n\r\f\v]" nil)))))
    (if (and (not (string= "" last-line))
	     (string-match-p "^[a-z0-9-]+:[^:]+: $" last-line))
	(progn
	  ;; Highlight compiler errors that are near the bottom of the
	  ;; shell-mode buffer.
	  (run-at-time nil nil `(lambda () (with-current-buffer
					       ,(current-buffer)
					     (my-highlight-compiler-errors))))

	  ;; Complain if the buffer is too large.
	  (let ((buffer-line-count (count-lines (point-min) (point-max))))
	    (if (> buffer-line-count 3000)
		(run-at-time nil nil
			     `(lambda ()
				(message (propertize "There are now %d lines in this buffer!"
						     'face 'my-alert-face)
					 ,buffer-line-count)))))

	  (setq string
		(concat (substring string 0 (- (length string) (length last-line)))
			;; Highlight the prompt except for the trailing space.
			(propertize (substring last-line 0 -1)
				    'face 'comint-highlight-prompt
				    'read-only t
				    'rear-nonsticky '(read-only))
			" ")))))

  ;; Must return the string to insert into the shell buffer.
  string)

(defun my-shell-mode-special-cd-handler (string)
  "Handles commands of the form 'cd -N' by setting default-directory correctly so
that filename completion still works."
  (if (and shell-dirtrackp
	   (string-match-p "^[ \t]*cd[ \t]+-[0-9]+[ \t]*$" string))
      ;; The "cd -N" command hasn't executed yet, so we can't set default-directory
      ;; until after the command executes.  Set a flag that tells us to do that.
      (setq my-shell-mode-recompute-default-directory t)))

(defun my-shell-mode-input-filter (string)
  "Makes Shell mode buffers track the current directory of the shell when
I use my 'u' alias to move up one directory."
  (let ((cmd (car-safe (split-string string "[\n;|&]" t))))
    (if (string= cmd "u")
	(shell-process-cd ".."))))

(define-advice shell-process-cd (:filter-args (&rest args) my-ad-filter-args-shell-process-cd)
  "Changes the function so that Shell mode correctly tracks directory changes when I use
my 'u' alias to cd to a parent directory.  For this advice to work, the variable
shell-cd-regexp must be set to 'cd|u'."
  ;; Variable cmd is dynamically scoped from function shell-directory-tracker.
  ;; Use to silence a byte-compiler warning about a reference to a free
  ;; variable.  The byte-compiler still warns that variable cmd lacks a prefix.
  (if (with-no-warnings (string= cmd "u"))
      (cons ".." (cdr args))
    args))

(eval-when-compile
  (setq byte-compile-warnings t))

(defun my-shell-mode-hook ()
  "Added to shell-mode-hook."
  (if (string= "*Async Shell Command*" (buffer-name))
      ;; This undoes the call to shell-mode in function shell-command.
      (fundamental-mode)

    ;; Turn off Font-Lock mode.  I do my own highlighting.
    (font-lock-mode 0)

    ;; This change to shell-cd-regexp works with my advice on shell-process-cd
    ;; to make Shell mode track directory changes via my "u" alias.
    ;;(setq shell-cd-regexp (concat shell-cd-regexp "\\|u"))

    (if (file-exists-p "/bin/stty")
	(setq comint-input-sender
	      (lambda (proc string)
		"Prefixes all shell commands with an stty command to set the terminal's width
to match the width of the window displaying the Shell mode buffer."
		;; Only execute the stty command if input is at a shell prompt.
		(save-excursion
		  (forward-line -1)
		  (if (string-match-p "^[a-z0-9-]+:[^:]+: $"
				      (buffer-substring (point) comint-last-input-start))
		      (comint-simple-send proc
					  (concat (format "/bin/stty cols %d; " (window-width))
						  string))
		    (comint-simple-send proc string))))))

    ;; Make sure the input history is empty.  This shouldn't be necessary,
    ;; but there appears to be a bug in comint that leave crap in the input
    ;; history of brand new shell-mode buffers.
    (setq comint-input-ring
	  `(,comint-input-ring-size 0 . ,(make-vector comint-input-ring-size 'nil)))

    (setq comint-scroll-to-bottom-on-input 'this)

    (add-hook 'comint-input-filter-functions 'my-shell-mode-input-filter 'append 'local)
    (add-hook 'comint-input-filter-functions 'my-shell-mode-special-cd-handler 'append 'local)
    (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)
    (add-hook 'comint-preoutput-filter-functions 'my-shell-mode-preoutput-filter-hook
	      'append 'local)
    (add-hook 'post-command-hook 'my-shell-mode-post-command-hook 'append 'local)

    (local-set-key (kbd "C-a") 'my-comint-beginning-of-line)
    (local-set-key (kbd "<up>") 'my-comint-previous-line)
    (local-set-key (kbd "<down>") 'my-comint-next-line)
    (local-set-key (kbd "TAB") 'comint-dynamic-complete)
    (local-set-key (kbd "M-i") 'indent-according-to-mode)
    (local-set-key (kbd "C-p") 'comint-previous-input)
    (local-set-key (kbd "C-n") 'my-comint-next-line)
    (local-set-key (kbd "C-p") 'my-comint-previous-line)
    (local-set-key (kbd "M-p") 'my-previous16-line)
    (local-set-key (kbd "M-n") 'my-next16-line)
    (local-set-key (kbd "C-u") 'comint-kill-input)
    (local-set-key (kbd "C-c C-u") 'universal-argument)
    (local-set-key (kbd "C-z C-l") 'my-shell-clear-buffer)
    (local-set-key (kbd "C-l") 'my-comint-recenter)
    (local-set-key (kbd "C-z e") 'my-highlight-compiler-errors)
    (rename-buffer "bash" 'unique)))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(defun my-tcl-mode-hook ()
  "Added to tcl-mode-hook."
  (local-set-key (kbd "<return>") 'my-newline-with-indent)
  (local-set-key "\C-zu" 'my-insert-under-construction-shell)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key "\M-i" 'indent-according-to-mode))

(add-hook 'tcl-mode-hook 'my-tcl-mode-hook)

(defun my-text-mode-hook ()
  "Added to text-mode-hook."
  (auto-fill-mode 1)
  (modify-syntax-entry ?' "." text-mode-syntax-table)

  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (local-set-key "\M-i" 'indent-relative)
  (local-unset-key "\M-s")
  (local-unset-key "\M-S"))

(add-hook 'text-mode-hook 'my-text-mode-hook)
