;; my-frame.el
;; Functions that deal with frames.

(defun my-set-initial-frame-alist ()
  "Sets up initial-frame-alist.  This is called from my-visual.el."
  ;; Must set the frame's default font here, so that when we call my-frame-initial-height and
  ;; my-frame-initial-width, they can compute the frame height/width correctly, because they
  ;; depend on the dimensions of the default font.
  (ignore-errors (set-frame-font (my-preferred-font)))
  (setq initial-frame-alist
	`((font . ,(my-preferred-font))
	  (background-color . ,(face-background 'default))
	  (foreground-color . ,(face-foreground 'default))
	  (horizontal-scroll-bars . nil)
	  (vertical-scroll-bars . nil)
	  (menu-bar-lines . 0)
	  ;;(icon-type . t)
	  (top . 50)		;; This is overridden by my-frame-center later.
	  (left . 50)		;; This is overridden by my-frame-center later.
	  (height . ,(my-frame-initial-height))
          (width . ,(my-frame-initial-width))
	  (cursor-color . "cyan")
	  (mouse-color . "yellow")
	  (tool-bar-lines . 0)
	  (menu-bar-lines . 0))))

(defun my-set-default-frame-alist ()
    "Sets default-frame-alist based on the value of initial-frame-alist, then tweaks some
of the parameters in it."
    ;; First, reset initial-frame-alist.
    (my-set-initial-frame-alist)

    ;; Don't use copy-sequence here -- it doesn't copy the list elements, just the list's
    ;; cons cells.  Use copy-alist instead.
    (setq default-frame-alist (copy-alist initial-frame-alist))
    (setcdr (assoc 'height default-frame-alist) (my-screen-percent-to-frame-height 75)))

(defun my-frame-goto-initial-position-and-size ()
  "..."
  (interactive)
  (my-reset-font)
  (set-frame-size (selected-frame)
		  (my-frame-initial-width)
		  (my-frame-initial-height))
  ;; Let the size change kick in before we recenter based on the new size.
  (sit-for 0)
  (my-frame-center))

(defun my-frame-initial-height ()
  "Returns the desired height of the initial frame in lines."
  (my-screen-percent-to-frame-height 97))

(defun my-frame-monitor-width (&optional frame)
  "Returns the width (in pixels) of the monitor dominating the current frame."
  (car (nthcdr 3 (assoc 'geometry (frame-monitor-attributes frame)))))

(defun my-frame-monitor-height (&optional frame)
  "Returns the height (in pixels) of the monitor dominating the current frame."
  (cadr (nthcdr 3 (assoc 'geometry (frame-monitor-attributes frame)))))

(defun my-frame-initial-width ()
  "Returns the desired width of the initial frame in characters."
  (if (> (my-frame-monitor-width) 1920)
      150
    (if (> (my-frame-monitor-width) 1366)
	135
      (if (> (my-frame-monitor-width) 1280)
	  110
	(if (> (my-frame-monitor-width) 1024)
	    100
	  95)))))

(defun my-frame-redisplay-hack (&optional frame try-hard)
  "Hack to redisplay the specified FRAME given Emacs' optimized rendering on
Windows.  If FRAME is nil, the selected frame is redisplayed.  This hack is
needed due to Win32 drawing changes made sometime in the spring or summer of
2008.  As of the 2010-04-16 development version of Emacs, it is not sufficent in
all cases to just call (sit-for 0).  If a frame is newly created, TRY-HARD
should be non-nil."
  (when my-win32
    (if try-hard
	(let ((target-frame (or frame (selected-frame))))
	  (make-frame-invisible target-frame 'force)
	  (make-frame-visible target-frame)
	  (redisplay 'force)))
    (sit-for 0)))

(defun my-frame-center (&optional frame bump-right)
  "Positions FRAME (or the selected frame if FRAME is nil) to a central location
on the screen and raises it."
  (interactive)
  (if (display-graphic-p)
      (let ((target-frame (or frame (selected-frame))))
	(raise-frame target-frame)
	(let ((top (my-frame-centered-top target-frame))
	      (left (my-frame-centered-left target-frame)))
	  (if (eq window-system 'x)
	      (setq top (+ top 0)))	;; For FVWM this should be 22 instead.
	  (if bump-right
	      (setq left (+ left (if (integerp bump-right)
				     bump-right
				   50))))
	  (set-frame-position target-frame left top)))))

(define-advice make-frame (:around (origfun &rest args) my-ad-around-make-frame)
  "Recomputes the value of default-frame-alist before calling make-frame.  This is needed
to cause make-frame to see a dynamically updated default-frame-alist when Emacs is
launched under a different screen resolution than is current, which can happen when using
RDP on Windows.  Also positions all new frames 50 pixels to the right of center, with some
exceptions."
  (my-set-default-frame-alist)

  (let ((new-frame (apply origfun args))
	(frame-params (car args)))
    (if (or (assoc 'my-center frame-params)	;; Frame parameter 'my-center overrides all.
	    (not (or (my-stack-match "^\\(my-\\)?erc-")
		     (my-stack-match "^gnus-")
		     (my-stack-match "^man$")
		     (my-stack-match "^manual-entry$")
		     (assoc 'top frame-params)
		     (assoc 'left frame-params))))
	(my-frame-center new-frame
			 (if (not (assoc 'my-center frame-params)) 'bump-right)))

    ;; Return the newly created frame object.
    new-frame))

(defun my-frame-show-next-frame ()
  "Raises and selects the next frame in the frame list, but only if the frame is visible."
  (interactive)
  (let ((frame (next-frame nil 'visible)))
    (raise-frame frame)
    (select-frame frame)))

(defun my-invisible-frames-menu ()
  "Pops up a list of invisible frames.  Selecting one makes it visible."
  (interactive)
  (let (items pane)
    (dolist (frame (frame-list))
      (if (not (frame-visible-p frame))
	  (setq items (append items
			      (list (cons (buffer-name (window-buffer (frame-first-window frame)))
					  frame))))))
    (setq pane (if (null items)
		   '("" "No invisible frames!")
		 `("" ,@(sort items (lambda (a b) (string-lessp (car a) (car b)))))))

    (let ((frame (x-popup-menu t `("Invisible Frames" ,pane))))
      (if frame
	  (make-frame-visible frame)))))

(defun my-frames-displaying-buffer (&optional buffer)
  "Returns a list of the frames that are displaying BUFFER, which is a buffer or
buffer name.  Returns nil if BUFFER is not displayed in any frame."
  (if (stringp buffer)
      (setq buffer (get-buffer buffer)))
  (if (null buffer)
      (setq buffer (current-buffer)))
  (if (not (bufferp buffer))
      (error "my-erc-frame-displaying-buffer: no such buffer!"))
  (let (result)
    (dolist (frame (frame-list))
      (dolist (window (window-list frame 'ignore-minibuf (frame-first-window frame)))
	(if (and (eq buffer (window-buffer window))
		 (not (memq frame result)))
	    (setq result (cons frame result)))))
    result))

(defun my-frame-make-frame-invisible (&optional frame)
  "Makes FRAME invisible.  FRAME defaults to the selected frame if nil"
  (interactive)
  (if (= 1 (length (frame-list)))
      (error "You don't want to make the only frame invisible!"))
  (let ((frame (or frame (selected-frame))))
    (make-frame-invisible frame)))

(defun my-frame-make-all-frames-visible ()
  "Makes all frames visible."
  (interactive)
  (dolist (frame (frame-list))
    (make-frame-visible frame))
  (message "All frames are now visible."))

(defun my-frame-call-in-new-frame (function &optional frame-params)
  "Calls FUNCTION interactively after making and selecting a new frame and
marking it as deletable using my-frame-mark-deletable.  Optional argument
FRAME-PARAMS is a alist of frame parameters passed to make-frame.  If
(display-graphic-p) is nil, FUNCTION is called without making a new frame."
  (interactive)
  (if (display-graphic-p)
      (my-frame-mark-deletable (select-frame (make-frame frame-params))))
  (call-interactively function))

(defun my-frame-mark-deletable (frame)
  "Sets my-delete-this-frame to t as a frame-local variable in the given frame.
Returns FRAME."
  (modify-frame-parameters frame '((my-delete-this-frame . t)))
  frame)

(defun my-frame-set-width (width)
  "Set the selected frame's width to WIDTH.  Interactively WIDTH is taken from the prefix
or (if the prefix is 1) is read from the minibuffer."
  (interactive (list (if (or (not (numberp current-prefix-arg))
			     (= 1 current-prefix-arg))
			 (read-from-minibuffer "New frame width: "
					       nil nil 'as-object nil)
		       (prefix-numeric-value current-prefix-arg))))
  (if (<= width 1)
      (error (format "Invalid frame width: %d" width)))
  (set-frame-width nil width))

(defun my-make-frame-delayed ()
  "Makes a new frame after a 2-second delay.  Returns the new frame object."
  (interactive)
  (sleep-for 2)
  (make-frame))

(defun my-make-named-frame (frame-name)
  "Makes a new frame with name NAME (prompting for NAME if necessary)."
  (interactive "@sName of new frame: ")
  (make-frame (list (cons 'name frame-name))))

(defun my-frame-name (&optional frame)
  "Given an optional frame (defaults to selected frame), returns a string which is the
name of the frame.  If the frame has no name, returns nil."
  (interactive)
  (let ((the-frame (or frame (selected-frame))))
    (cdr-safe (assoc 'name (frame-parameters the-frame)))))

(defun my-frame-change-name (newname &optional frame)
  "Changes the name of a frame.  The new name is NEWNAME.  The affected frame is either
optional argument FRAME or the selected frame if argument FRAME is not supplied."
  (interactive "sNew frame name: ")
  (modify-frame-parameters (or frame (selected-frame))
			   (list (cons 'name newname))))

(defun my-frame-important-p (frame)
  "Returns t if this frame is important, nil otherwise.  All frames are important, except
man page frames."
  (not (string-match-p "^man " (my-frame-name frame))))

(defun my-delete-frame-or-save-buffers-kill-emacs (&optional arg)
  "Deletes the current frame.  Kill Emacs if there's only one important frame left.  See
documentation for function my-frame-important-p for definition of important frames."
  (interactive "P")
  (let ((numframes (length (frame-list)))
	(frameinfo (mapcar 'my-frame-important-p (frame-list))))
    (if (and (= 1 (cl-count 't frameinfo))
	     (my-frame-important-p (selected-frame)))
	(save-buffers-kill-emacs)
      (delete-frame))))

(defun my-frame-left-right-border-width (&optional frame)
  "..."
  ;; Avoid calling frame-geometry when in batch mode.  It crashes Emacs on Windows.
  (if noninteractive
      0

    (if (null frame)
	(setq frame (selected-frame)))

    (let ((ext-border-sizes (cdr-safe (assoc 'external-border-size (frame-geometry frame)))))
      (if (consp ext-border-sizes)
	  (+ (car ext-border-sizes) (cdr ext-border-sizes))
	;; Make a guess ...
	6))))

(defun my-frame-top-bottom-border-width (&optional frame)
  "..."
  ;; Avoid calling frame-geometry when in batch mode.  It crashes Emacs on Windows.
  (if noninteractive
      0

    (if (null frame)
	(setq frame (selected-frame)))

    (let ((title-height (cdr-safe (assoc 'title-height (frame-geometry frame))))
	  (border (/ (my-frame-left-right-border-width frame) 2)))
      (+ (or title-height
	     ;; Make a guess ...
	     18)
	 (* 2 border)))))

(defun my-frame-width-to-pixels (width)
  "Converts WIDTH (a frame width in columns) into the pixel-width of a frame that has that
many columns.  The return value includes the window borders, which function
frame-pixel-width does not include!  Assumes the left and right fringes are 8 pixels wide.
TODO: Use variables fringe-mode and fringe-styles to compute the default fringe size."
  (let* ((pixels-per-column (default-font-width))
	 (total-fringe-pixels 16)
	 (window-border-pixels (my-frame-left-right-border-width)))
    (+ (* width pixels-per-column) total-fringe-pixels window-border-pixels)))

(defun my-frame-pixels-to-width (pixels)
  "Converts the width of a frame in PIXELS to a value for frame parameter 'width that
makes a frame as close to PIXELS wide as possible without going over.  Assumes the left
and right fringes are 8 pixels wide.  To set a frame's height to an exact number of pixels
use function set-frame-height or set-frame-size.  TODO: Use variables fringe-mode and
fringe-styles to compute the default fringe size."
  (let* ((pixels-per-column (default-font-width))
	 (total-fringe-pixels 16)
	 (window-border-pixels (my-frame-left-right-border-width)))
    (/ (- pixels window-border-pixels total-fringe-pixels) pixels-per-column)))

(defun my-frame-height-to-pixels (height)
  "Converts a frame HEIGHT (in rows) to the pixel-height of a frame that has that many
rows.  HEIGHT must include the minibuffer and mode-line.  The return value includes the
window borders, which function frame-pixel-height does not include!"
  (let* ((pixels-per-row (default-font-height))
	 (window-border-pixels (my-frame-top-bottom-border-width)))
    (+ (* height pixels-per-row) window-border-pixels)))

(defun my-frame-pixels-to-height (pixels)
  "Converts the height of a frame in PIXELS to a value for frame parameter 'height that
makes a frame as close to PIXELS high as possible without going over.  To set a frame's
height to an exact number of pixels use function set-frame-height or set-frame-size."
  (let* ((pixels-per-row (default-font-height))
	 (window-border-pixels (my-frame-top-bottom-border-width)))
    (/ (- pixels window-border-pixels) pixels-per-row)))

(defun my-screen-percent-to-horizontal-pixels (percent)
  "Converts a percentage of the width of the screen into pixels."
  (truncate (* (/ percent 100.0) (my-frame-monitor-width))))

(defun my-screen-percent-to-vertical-pixels (percent)
  "Converts a percentage of the height of the screen into pixels."
  (truncate (* (/ percent 100.0) (my-frame-monitor-height))))

(defun my-screen-percent-to-frame-width (percent)
  "Converts a percentage of the width of the screen into a frame width (in columns)."
  (my-frame-pixels-to-width (my-screen-percent-to-horizontal-pixels percent)))

(defun my-screen-percent-to-frame-height (percent)
  "Converts a percentage of the height of the screen into frame height (in rows)."
  (my-frame-pixels-to-height (my-screen-percent-to-vertical-pixels percent)))

(defun my-frame-centered-top (&optional frame)
  "Computes a new value of the 'top frame parameter for FRAME (or the selected
frame if FRAME is nil) such that FRAME is centered vertically on the screen."
  (let* ((frame (or frame (selected-frame)))
	 (newtop (- (/ (- (my-frame-monitor-height)
			  (frame-pixel-height frame))
		       2)
		    (if my-win32
			(if my-win7
			    15
			  14)
		      12))))
    newtop))

(defun my-frame-centered-left (&optional frame)
  "Computes a new value of the 'left frame parameter for FRAME (or the selected frame if
FRAME is nil) such that FRAME is centered horizontally on the monitor dominating FRAME."
  (let* ((frame (or frame (selected-frame)))
	 (newleft (/ (- (my-frame-monitor-width)
			(frame-pixel-width frame))
		     2)))
    (- newleft
       (if (eq window-system 'x)
	   3
	 (if my-win7
	     6
	   4)))))

(defun my-frame-pixel-width-to-centered-left (pixel-width)
  "Returns the value to use for the 'left frame parameter to center a frame that is
PIXEL-WIDTH pixels wide."
  (/ (- (my-frame-monitor-width) pixel-width) 2))

(defun my-frame-pixel-width-to-rightmost-left (pixel-width)
  "Returns the value to use for the 'left frame parameter to place a frame that is
PIXEL-WIDTH pixels wide against the right edge of the current monitor."
  (- (my-frame-monitor-width) pixel-width)
  ;;-1
  )

(defun my-frame-rightmost-left ()
  "..."
  (if my-win32 -1 -6))

(defun my-frame-bottommost-top ()
  "..."
  ;; TODO: Maybe use frame-geometry to get the value to use under X?
  (if my-win32 -1 -24))

(defun my-frame-center-vertically (&optional frame)
  "Centers FRAME (or the selected frame is FRAME is nil) vertically on the screen without
altering its horizontal position."
  (if (display-graphic-p)
      (let ((new-top (my-frame-centered-top frame)))
	(modify-frame-parameters frame `((top . ,new-top))))))

(defun my-frame-center-horizontally (&optional frame)
  "Centers FRAME (or the selected frame is FRAME is nil) horizontally on the screen without
altering its vertical position."
  (if (display-graphic-p)
      (let ((new-left (my-frame-centered-left frame)))
	(modify-frame-parameters frame `((left . ,new-left))))))
