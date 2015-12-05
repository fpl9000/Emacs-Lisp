;; my-crumbs.el
;; Fran Litterio <flitterio@gmail.com>
;;
;; This package implements a ring of buffer/file locations of interesting places (crumbs) where
;; point has been in the past.
;;
;; The following functions are available:
;;
;; UNDER CONSTRUCTION


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Things to do:
;;
;; o When my-bc-prev-crumb is used when positioned near crumb 0, go to crumb 1, but if point is
;;   sufficiently far from crumb 0, go to crumb 0 instead.
;;
;; o Delete (and skip) crumbs that don't exist when navigating.
;;
;; o Add command my-bc-goto-crumb-0 to move to front of ring.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-configurable variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-bc-max-crumbs 1000
  "The maximum number of crumb locations to store.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATTENTION: There are no user-configurable parameters below this line!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-bc-ring (make-ring my-bc-max-crumbs)
  "A list of property lists, each representing a crumb.  Each property list can
contain the following properties:

  :type		This property is always present.  This property's value is one of
		the following keywords denoting the type of this crumb:

                :buffer		This crumb specifies a location in a buffer.

		:info		This crumb specifies a location in the Info node
				specified by property :infonode.  This is necessary,
				because all Info nodes exist in a single buffer
				named *info*.

  :marker	This property is always present.  This property's value is a marker
		specifying the buffer location of this crumb.  If this crumb's
		buffer no longer exists, function marker-buffer returns nil for
		this marker, and this marker cannot be used.

  :infonode	If property :type is :info, this is a string of the form
		'(FILENAME)NODE' specifying the Info node referenced by this crumb.
		The value of property :marker specifies the location of the crumb
		within the Info node.")

(defvar my-bc-current-crumb-index 0
  "The user's current location in the crumb ring.  Index 0 is the front element of the
ring.  As the user moves backward through the crumb trail, this value goes up, wrapping
back to 0 after my-bc-max-crumbs.  If a new crumb is added, this becomes 0 again,
positioning the user back at the front of the ring.")

(defvar my-bc-suppress nil
  "This is dynamically bound to t when calling functions that might push a new crumb onto
the ring to tell them not to do so.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions.  Not for users to call.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-bc-current-crumb ()
  "Returns the current crumb referenced by index my-bc-current-crumb-index."
  (ring-ref my-bc-ring my-bc-current-crumb-index))

(defun my-bc-crumb-buffer (crumb)
  "Returns the buffer associated with CRUMB.  Returns nil is the buffer is dead."
  (marker-buffer (plist-get :marker crumb)))

(defun my-bc-crumb-position (crumb)
  "Returns the buffer position associated with CRUMB.  Returns nil if the marker points
nowhere."
  (marker-position (plist-get :marker crumb)))

(defun my-bc-goto-crumb (crumb)
  "Goes to the specified CRUMB."
  (let* ((buffer (my-bc-crumb-buffer crumb))  ;; TODO: Support Info nodes.
	 (my-bc-suppress t))

    (if (not (buffer-live-p buffer))
	(error "Crumb buffer (%s) doesn't exist!" buffer))

    (switch-to-buffer buffer)
    (goto-char (my-bc-crumb-position crumb))
    (recenter)))

(defun my-bc-check-newest-crumbs (crumb)
  "Signals an error if CRUMB is equal to one of the two newest crumbs."
  (let ((my-crumb0 (ring-ref my-bc-ring 0))
	(my-crumb1 (ring-ref my-bc-ring 1)))

    (if (or (equal my-crumb0 crumb)
	    (equal my-crumb1 crumb))
	(error "Attempt to duplicate one of the two newest crumbs!"))))

(defun my-bc-check-ring-empty ()
  "Signals an error if the crumb ring is empty."
  (if (ring-empty-p my-bc-ring)
      (error "No crumbs!")))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-visible functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-bc-prev-crumb ()
  "Moves one crumb back from the current crumb in the crumb ring.  If the location
specified by the crumb does not exist (e.g., a non-existent buffer), an error is
signalled."
  (interactive)
  (my-bc-check-ring-empty)
  (setq my-bc-current-crumb-index (% (1+ my-bc-current-crumb-index) (ring-length my-bc-ring)))
  (my-bc-goto-crumb (my-bc-current-crumb)))

(defun my-bc-next-crumb ()
  "Moves one crumb forward from the current crumb in the crumb ring.  If the location
specified by the crumb does not exist (e.g., a non-existent buffer), an error is
signalled."
  (interactive)
  (my-bc-check-ring-empty)
  (setq my-bc-current-crumb-index (if (= 0 my-bc-current-crumb-index)
				      (1- (ring-length my-bc-ring))
				    (1- my-bc-current-crumb-index)))
  (my-bc-goto-crumb (my-bc-current-crumb)))

(defun my-bc-goto-crumb-0 ()
  "Goes to the crumb at index 0 (the front of the ring)."
  (interactive)
  (my-bc-check-ring-empty)
  (setq my-bc-current-crumb-index 0)
  (my-bc-goto-crumb (my-bc-current-crumb)))

(defun my-bc-goto-current-crumb ()
  "Goes to the current crumb.  Useful if point has been moved off the crumb without having
adding a new crumb (which would make the new crumb current)."
  (interactive)
  (my-bc-check-ring-empty)
  (my-bc-goto-crumb (my-bc-current-crumb)))

(defun my-bc-show-current-crumb ()
  "Displays information about the current crumb."
  (interactive)
  (my-bc-check-ring-empty)

  (let* ((crumb (my-bc-current-crumb)))
    (message "Current crumb is %s:%d (of %d total crumbs)."
	     (my-bc-crumb-buffer crumb) (my-bc-crumb-position crumb) (ring-length my-bc-ring))))

(defun my-bc-show-all-crumbs ()
  (interactive)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   UNDER CONSTRUCTION                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (error "Not yet implemented!"))

(defun my-bc-add-crumb ()
  "Adds the current location as a new crumb at the front of the crumb ring."
  (interactive)
  (let ((my-crumb nil))
    ;; TODO: Handle Info nodes.
    (setq my-crumb (plist-put my-crumb :type :buffer))
    (setq my-crumb (plist-put my-crumb :marker (set-marker (make-marker) (point))))
    (ring-insert my-bc-ring my-crumb)
    (setq my-bc-current-crumb-index 0))

  (message "Added crumb at current location!"))

(defun my-bc-copy-current-crumb ()
  "Copies the current crumb to the front of the crumb ring.  If the current crumb is
already one of the two crumbs at the front of the of the ring, signals an error."
  (interactive)
  (my-bc-check-ring-empty)

  (if (<= my-bc-current-crumb-index 2)
      (error "Cannot copy the three newest crumbs!"))

  (my-bc-check-newest-crumbs (my-bc-current-crumb))

  (let ((my-crumb (my-bc-current-crumb)))
    (ring-insert my-bc-ring my-crumb)
    (setq my-bc-current-crumb-index 0)
    (message "Copied current crumb (%s:%d) to front of ring!" (my-bc-crumb-buffer my-crumb)
	     (my-bc-crumb-position my-crumb))))

(defun my-bc-delete-current-crumb ()
  "Removes the most recently visited crumb from the crumb ring."
  (interactive)
  (my-bc-check-ring-empty)

  (let* ((crumb (ring-remove my-bc-ring my-bc-current-crumb-index))
	 (marker (plist-get crumb :marker)))
    (message "Deleted current crumb (%s:%d)!"
	     (marker-buffer marker) (marker-position marker))))

(defun my-bc-delete-all-crumbs ()
  "Deletes all crumbs in the crumb ring."
  (interactive)
  (if (not (yes-or-no-p "Really delete all crumbs? "))
      (error "No crumbs deleted!"))

  (setq my-bc-ring (make-ring my-bc-max-crumbs)
	my-bc-current-crumb-index 0)
  (message "Deleted all crumbs!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings.  For testing only.  Move these to my-keys.el.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-z"))
(define-key global-map (kbd "C-<left>")		'my-bc-prev-crumb)
(define-key global-map (kbd "C-<right>")	'my-bc-next-crumb)
(define-key global-map (kbd "C-z SPC")		'my-bc-add-crumb)
(define-key global-map (kbd "C-z ,")		'my-bc-goto-crumb-0)
(define-key global-map (kbd "C-z b .")		'my-bc-goto-current-crumb)
(define-key global-map (kbd "C-z b d")		'my-bc-delete-current-crumb)
(define-key global-map (kbd "C-z b D")		'my-bc-delete-all-crumbs)
(define-key global-map (kbd "C-z b c")		'my-bc-copy-current-crumb)
(define-key global-map (kbd "C-z b s")		'my-bc-show-current-crumb)
(define-key global-map (kbd "C-z b /")		'my-bc-show-all-crumbs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice.  The following functions are advised to add a new crumb to the
;; front of the ring:
;;
;;   o find-file
;;   o find-alternate-file
;;   o switch-to-buffer
;;   o my-switch-to-other-buffer
;;   o kill-buffer
;;   o find-tag
;;   o beginning-of-buffer, end-of-buffer
;;   o scroll-down, scroll-up (but only after 3 or more in succession)
;;   o my-next16-line, my-previous16-line (but only after 3 or more in succession)
;;   o my-list-buffers
;;   o my-delete-frame-or-save-buffers-kill-emacs
;;   o info
;;
;; If a new crumb exactly matches either of the two newest crumbs in the ring, it is not added.
;; This is to prevent accumulation of groups of identical crumbs caused by, for instance, switching
;; back and forth between two buffers/files.
