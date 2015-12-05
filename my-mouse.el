;; my-mouse.el
;; Some handy functions to be bound to mouse events.

(require 'button)

;; Disable sub-menu grouping in the menu displayed by mouse-buffer-menu.
(setq mouse-buffer-menu-mode-groups '(("." . "All")))

(defun my-mouse-buffer-menu (event)
  "Pop up a menu of buffers for selection with the mouse."
  (interactive "e")
  (let* ((menu))
    ;; Build the object that describes the menu to x-popup-menu.  Omit buffers
    ;; whose names begin with " ", "#" (e.g., ERC channel buffers) and the
    ;; some unimportant buffers.
    (setq menu (mapcar (lambda (buffer)
			 (let ((bufname (buffer-name buffer)))
			   (if (and (not (= 32 (aref bufname 0)))
				    (not (member-ignore-case bufname
							     '("*Completions*" "*Apropos*"
							       "*Calc Trail*")))
				    (or (memq 'shift (event-modifiers event))
					(not (with-current-buffer buffer
					       (eq major-mode 'erc-mode)))))
			       (cons bufname buffer))))
		       (buffer-list)))

    (setq menu (delq 'nil menu))
    (setq menu (list "Buffers" (append '("Buffers") menu)))

    (let ((buffer (x-popup-menu event menu))
	  (window (posn-window (event-start event))))
      (if buffer
	  (progn
	    (or (framep window) (select-window window))
	    (switch-to-buffer buffer))))))

(defun my-mouse-shortcuts (event)
  "Pop up a menu of commonly-performed actions."
  (interactive "e")
  (let ((action
	 (x-popup-menu
	  event
	  `("Shortcuts"
	    ("Misc"
	     ;;("Bash HOWTO" . (find-file "~/cvs/ldp/howto/docbook/Bash-Scripting-Introduction-HOWTO.xml"))
	     ("411data.txt" . (find-file "~/info/411data.txt"))
	     ("--")
	     ("Edit this menu" . (progn
				   (find-file-other-frame "~/elisp/my-mouse.el")
				   (modify-frame-parameters nil '((height . 50) (width . 160)))
				   (my-frame-redisplay-hack)
				   (my-frame-center)
				   (goto-char (point-min))
				   (search-forward "my-mouse-shortcuts" nil t)
				   (recenter 0)
				   (toggle-truncate-lines 1)
				   (beginning-of-line)))
	     )
	    ("Work"
	     ("meetings.txt" . (find-file "~/work/meetings.txt"))
	     )

	    ,(cons "Buffers"
		   (let (result)
		     (dolist (buffer (buffer-list))
		       (let ((bufname (buffer-name buffer)))
			 (if (and (/= 32 (aref bufname 0))
				  (not (member bufname '("*Completions*" "*Compile-Log*" "*Buffer List*")))
				  (not (member (with-current-buffer buffer major-mode)
					       '(erc-mode))))
			     (add-to-list 'result `(,bufname . (switch-to-buffer ,bufname))))))
		     (setq result (sort result (lambda (a b)
						 (string-lessp (downcase (car a)) (downcase (car b))))))
		     (add-to-list 'result '("--") 'append)
		     (add-to-list 'result '("Buffer List" . (my-list-buffers)) 'append)))

	    ("Directories"
	     ("~/audio" . (dired "~/audio"))
	     ("~/dist" . (dired "~/dist"))
	     ("~/src" . (dired "~/src"))
	     ("~/video" . (dired "~/video"))
	     ("~/work/clark" . (dired "~/work/clark"))
	     )

	    ("--" ())

	    ("TAGS Tables"
	     ("TBD" . (my-select-tags-file "TBD"))
	     ("TBD" . (my-select-tags-file "TBD"))
	     )
	    ))))
    (select-window (car (event-start event)))
    (sit-for 0)
    (eval action)))
