;; My MOF mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the mode's keymap.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-mof-mode-map nil "Keymap for MOF mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create the mode's keymap.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (null my-mof-mode-map)
    (progn
      (setq my-mof-mode-map (make-sparse-keymap))
      ;;(define-key my-mof-mode-map "<" 'ding)
      ;;(define-key my-mof-mode-map ">" 'ding)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-face 'my-mof-identifier-face)
(set-face-foreground 'my-mof-identifier-face "#ccf")

(make-face 'my-mof-bracket-face)
(set-face-foreground 'my-mof-bracket-face "#f0f")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-mof-setup-font-lock ()
  "Set up Font Lock mode for MOF mode."
  (setq font-lock-multiline t)
  (font-lock-add-keywords
   nil
   '(
     ;; MOF comments.
     ("//.*$" 0 'font-lock-comment-face nil)
     ("\\s-*/\\*.*$" 0 'font-lock-comment-face nil)
     ("^\\s-*\\*.*$" 0 'font-lock-comment-face nil)

     ;; MOF keywords.
     ("\\b\\(class\\|IN\\|in\\|OUT\\|out\\|INOUT\\|inout\\|REF\\|ref\\|false\\|true\\)\\b" 1 'font-lock-keyword-face nil)

     ;; MOF class names.
     ;;("\\b\\([a-zA-Z_0-9]+\\)\\s-*[,{]" 1 'my-mof-attribute-name-face nil)
     ("\\bclass\\s-+\\([a-zA-Z_0-9]+\\)" 1 'font-lock-type-face nil)
     (":\\s-+\\([a-zA-Z_0-9]+\\)" 1 'font-lock-type-face nil)

     ;; MOF type names.
     ("\\b\\(uint16\\|uint32\\|datetime\\)" 1 'font-lock-type-face nil)

     ;; MOF identifiers.
     ("\\b[a-zA-Z_0-9]+\\b" 0 'my-mof-identifier-face nil)

     ;; MOF attribute block boundaries.
     ("\\[\\|\\]" 0 'my-mof-bracket-face t)
     )
   'set))

(defun my-mof-mode ()
  "Major mode used for editting MOF files.

Special commands:

\\{my-mof-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map my-mof-mode-map)
  (setq mode-name "MOF")
  (setq major-mode 'my-mof-mode)
  (my-mof-setup-font-lock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOF mode variable and function definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
