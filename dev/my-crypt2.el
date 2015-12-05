;; my-crypt2.el
;; Support for manipulating encrypted files and doing encryption with Emacs.
;;
;; Author:
;;
;;	Fran Litterio
;;	franl@centerline.com
;;	PGP Public Key Id: 1270EA1D
;;	http://draco.centerline.com:8080/~franl/
;;
;; Commentary:
;;
;;	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	;;                   UNDER CONSTRUCTION                   ;;
;;	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Things to do:
;;
;; o Overhaul my-crypt-write-file-hook to use the encryption detector
;;   functions.
;;
;; o Figure out how to handle digital signatures and clearsigned text.
;;
;; o Figure out how to tell conventionally encrypted PGP ciphertext from
;;   public key encrypted PGP ciphertext (both in and out of ASCII armor).
;;
;; o Figure out a way to write a buffer in encrypted form when no
;;   decryption was ever done on the buffer.
;;
;; o Provide the function my-crypt-write-file-encrypted that can be
;;   invoked interactively.
;;
;; o Figure out how to handle PGP messages when decrypting.  We don't
;;   want the messages to go into the buffer with the plaintext or
;;   ciphertext.
;;
;; o Implement ciphertext detector function for PGP.
;;
;; o Implement a generic encryption detector function.  It should check the
;;   decryption history of the buffer, the pathname, and a portion of the
;;   buffer contents that is controllable by
;;   my-crypt-contents-search-region (a list of the form (START END) with
;;   the default value being (0 512)).
;;
;; o Figure out what non-nil values to give to my-crypt-buffer-was-decrypted.
;; 
;; o Figure out what to do when writing a buffer that had multiple regions
;;   of ciphertext decrypted when it was read from disk.
;;
;; o Implement my-crypt-revert-buffer.
;;
;; o Implement my-decrypt-buffer, my-encrypt-buffer, my-decrypt-region,
;;   my-encrypt-region, and my-decrypt-message-at-point.
;;
;; o Write commentary.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User-configurable options.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-crypt-decrypt-confirm t
  "*If this option is non-nil, the user is prompted to confirm decryption.
If nil, no confirmation is required.")

(defvar my-crypt-encrypt-confirm t
  "*If this option is non-nil, the user is prompted to confirm encryption.
If nil, no confirmation is required.")

(defvar my-crypt-disable-auto-save nil
  "*If this option is non-nil, auto-saving is shut off after decrypting so
as to minimize the amount of plaintext that is written to disk.  This only
affects the buffer where decryption occurred.  Other buffers continue to
auto-save normally.")

(defvar my-crypt-default-encrypt-method nil
  "*This option specifies the default encryption method to use when
my-crypt-write-file-encrypted is invoked.  If nil, you are prompted to
enter the symbol-name designating the encryption method.")

(defvar my-crypt-default-signature-method 'pgp-sign
  "*This option specifies the default method for creating a digital
signature.")

(defvar my-crypt-pgp-id (user-real-login-name)
  "*This option specifies a string that is the default userid passed to PGP
when encrypting plaintext using your own PGP public key.  For more
information, see the -e option to PGP.  It's default value is the string
returned by the function user-real-login-name.")

(defvar my-crypt-use-multiple-keys nil
  "*If this option is nil, multiple ciphertext regions within a newly-found
file are all decrypted using the same key.  If non-nil, a new key is
obtained for each ciphertext region.  Default value is nil.")

(defvar my-crypt-decryption-methods '()
  "*This option is a list of symbols specifying decryption methods that can
be applied to a newly-found file.  The default value is nil, so you must
set this option before you can benefit from automagic decryption of files.
See the documentation for the option my-crypt-use-multiple-methods for how to
set up cascading decryptions of a single file (to allow, for instance,
manipulating compressed ciphertext.")

(defvar my-crypt-use-multiple-methods nil
  "*If this option is nil, the search for ciphertext within a newly-found
file continues after the first group of ciphertext is found and decrypted.
So if the value of my-crypt-decryption-methods is (gzip des pgp), and the
\"gzip\" method's decryption detector function finds some ciphertext in the
file, then after decrypting it, the decryption detector functions for the
\"des\" method is called.  After that the \"pgp\" method's decryption
detector function is called.  This can be used to manipulate files of
compressed ciphertext by having a decompression method (e.g., gzip)
detected before detecting ciphertext.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User-visible function interface.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-crypt-define-method (method-id
			       encryption-detector decryption-detector
			       encryption-setup	decryption-setup
			       encryption-cleanup decryption-cleanup
			       encryption-message decryption-message)
  "Defines a new encryption/decryption method for the my-crypt package.  
You should understand how the entire my-crypt package works before trying to
define a new method with this function.  The arguments are:

METHOD-ID is a symbol used to uniquely identify this method.  This is the
  symbol to put in the list which is the value of the variable
  my-crypt-decryption-methods.  The print name of this symbol can be typed
  in response to any prompt asking which encryption or decryption method to
  use.

ENCRYPTION-DETECTOR is a function (taking arguments BUFFER START END) that
  scans BUFFER between START and END for plaintext to be encrypted by this
  encryption method.  It must return a list of the form ((START1 . END1)
  (START2 . END2) ...), where STARTx and ENDx are markers (not integers)
  denoting a region of plaintext to be encrypted.

DECRYPTION-DETECTOR is a function (taking arguments BUFFER START END) that
  scans BUFFER between START and END for ciphertext generated by this
  encryption method.  Returns a list of the form ((START1 . END1) (START2
  . END2) ...), where STARTx and ENDx are markers (not integers) denoting a
  region of ciphertext to be decrypted.

ENCRYPTION-SETUP is a function (taking arguments BUFFER START END) that is
  called to do any pre-encryption work for encrypting the portion of BUFFER
  between START and END.  For example, it might prompt for the names of the
  recipients of the encrypted message (if a public key cryptosystem is
  being used), or it might prompt for the encryption key (if a symmetric
  cryptosystem is being used).

  This function must return a list of strings where the first string is the
  full pathname to the encryption program and the remaining strings are
  command-line arugments to be provided to the program.  The encryption
  program must read plaintext from stdin and write ciphertext to stdout
  (this interface may be extended in the future (ideas are welcome)).

  In the case where there is no real work for the ENCRYPTION-SETUP function
  to do, it can be a list of strings instead of a function, in which case
  the list of strings is used as if it were returned by an actual function.

DECRYPTION-SETUP is a function (taking arguments BUFFER START END) that is
  called to do any pre-decryption work for decrypting the portion of BUFFER
  between START and END.  For example, it might prompt for the private key
  (if a public key cryptosystem is being used), or it might prompt for the
  decryption key (if a symmetric cryptosystem is being used).

  This function must return a list of strings where the first string is the
  full pathname to the decryption program and the remaining strings are
  command-line arugments to be provided to the program.  The decryption
  program must read ciphertext from stdin and write plaintext to stdout
  (this interface may be extended in the future (ideas are welcome)).

  In the case where there is no real work for the DECRYPTION-SETUP function
  to do, it can be a list of strings instead of a function, in which case
  the list of strings is used as if it were returned by an actual function.

ENCRYPTION-CLEANUP is a function (taking arguments BUFFER START END) that
  performs cleanup tasks after each encryption completes (such as removing
  temporary files or extracting informational messages from the buffer).
  This argument is nil if no post-encryption cleanup is required.  BUFFER
  is the buffer containing the ciphertext between START and END (which are
  markers _not_ integers).

DECRYPTION-CLEANUP is a function (taking arguments BUFFER START END) that
  performs cleanup tasks after each decryption completes (such as removing
  temporary files, extracting informational messages from the buffer, or
  formatting the plaintext).  This argument is nil if no post-decryption
  cleanup is required.  BUFFER is the buffer containing the plaintext
  between START and END (which are markers _not_ integers).

ENCRYPTION-MESSAGE is a string to be displayed in the echo area while the
  encryption program is running.  If this argument is nil, the standard
  message \"Encrypting...\" is used.

DECRYPTION-MESSAGE is a string to be displayed in the echo area while the
  decryption program is running.  If this argument is nil, the standard
  message \"Decrypting...\" is used."

  (if (or (not (symbolp method-id))
	  (not (my-crypt-functionp encryption-detector))
	  (not (my-crypt-functionp decryption-detector))
	  (and (not (stringp encryption-setup))
	       (not (my-crypt-functionp encryption-setup)))
	  (and (not (stringp decryption-setup))
	       (not (my-crypt-functionp decryption-setup)))
	  (and encryption-cleanup (not (my-crypt-functionp encryption-cleanup)))
	  (and decryption-cleanup (not (my-crypt-functionp decryption-cleanup)))
	  (and encryption-message (not (stringp encryption-message)))
	  (and decryption-message (not (stringp decryption-message))))
      (error "my-crypt-define-method: invalid argument!"))
  (setq my-crypt-method-info
	(append (list method-id encryption-detector decryption-detector
		      encryption-setup decryption-setup encryption-cleanup
		      decryption-cleanup encryption-message decryption-message)
		my-crypt-method-info)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ATTENTION!  All functions and variables defined from here on are not
;; for users to access or change.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Internal variables.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-crypt-buffer-was-decrypted nil
  "A variable local to all buffers that records whether this buffer was
decrypted.  A value of nil means it was not decrypted, non-nil means it
was.  It is used by my-crypt-write-file-hook to determine whether to
encrypt the buffer when it is written to disk.  The particular non-nil
value indicates the decryption method.")

(make-variable-buffer-local 'my-crypt-buffer-was-decrypted)
(put 'my-crypt-buffer-was-decrypted 'permanent-local t)

(defvar my-crypt-method-info nil
  "A list describing the available encryption/decryption methods.
Each element represents an encryption/decrytion method and is a list
of the form:

	(METHOD-ID
	 ENCRYPTION-DETECTOR	DECRYPTION-DETECTOR
	 ENCRYPTION-SETUP	DECRYPTION-SETUP
	 ENCRYPTION-CLEANUP	DECRYPTION-CLEANUP
	 ENCRYPTION-MESSAGE	DECRYPTION-MESSAGE)

See the documentation for the function my-crypt-define-method for the gory
details.")

(defconst my-crypt-method-lookup-table
  '((encryption-detector . 1) (decryption-detector . 2)
    (encryption-setup . 3) (decryption-setup . 4)
    (encryption-cleanup . 5) (decryption-cleanup . 6)
    (encryption-message . 7) (decryption-message . 7)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initial methods.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(my-crypt-define-method ('pgp-public-anyone
			 my-crypt-pgp-encryption-detector
			 my-crypt-pgp-decryption-detector
			 my-crypt-pgp-encryption-setup
			 my-crypt-pgp-decryption-setup
			 my-crypt-pgp-encryption-cleanup
			 my-crypt-pgp-decryption-cleanup
			 nil nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Internal functions specific to an encryption method.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method: pgp-public-anyone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-crypt-pgp-decryption-setup (buffer start end)
  "Obtains a PGP secret key passphrase, inserts it in BUFFER after START,
and returns the list '(\"pgp\" \"-f\")."
  (save-excursion
    (let ((passphrase (my-crypt-read-key "PGP secret key passphrase: ")))
      (set-buffer buffer)
      (goto-char start)
      (insert passphrase)))
  (garbage-collect)
  '("pgp" "-f"))

(defun my-crypt-pgp-decryption-cleanup (buffer start end)
  "..."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   UNDER CONSTRUCTION                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

(defun my-crypt-pgp-encryption-setup (buffer start end)
  "..."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   UNDER CONSTRUCTION                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

(defun my-crypt-pgp-encryption-cleanup (buffer start end)
  "..."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   UNDER CONSTRUCTION                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

(defun my-crypt-pgp-encryption-detector (buffer start end)
  "..."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   UNDER CONSTRUCTION                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

(defun my-crypt-pgp-decryption-detector (buffer start end)
  "..."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   UNDER CONSTRUCTION                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generic internal functions (i.e., not specific to any one encryption method).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-crypt-read-key (prompt)
  "Read a key from the user.  Echos a * for each character typed.  End with
RET, LFD, or ESC.  DEL or C-h rubs out.  ^U kills the line.  Returns the
key string or nil if an error or quit happened during the read."
  (unwind-protect
      (let ((key "")
	    (input-char 0)
	    (echo-keystrokes 0)
	    (cursor-in-echo-area t))
	(while (and (/= input-char ?\r)
		    (/= input-char ?\n)
		    (/= input-char ?\e))
	  (message "%s%s" prompt (make-string (length key) ?*))
	  (setq input-char (read-char))
	  (if (= input-char ?\C-u)
	      (setq key "")
	    (if (and (/= input-char ?\C-h)
		     (/= input-char ?\C-?))
		(setq key (concat key (char-to-string input-char)))
	      (if (> (length key) 0)
		  (setq key (substring key 0 -1))))))
	(substring key 0 -1))
    (garbage-collect)))

(defun my-crypt-get-method-parameter (method-id parameter)
  "Given a METHOD-ID, returns the associated PARAMETER stored in
my-crypt-method-info for that METHOD-ID.  This is the _only_ way to access
the data in my-crypt-method-info.  See the documentation for variable
my-crypt-method-info for more information."
  (let ((method-info-element (assq method-id my-crypt-method-info)))
    (if method-info-element
	(let ((index (cdr (assq parameter my-crypt-method-lookup-table))))
	  (if (null index)
	      (error "Unrecognized encryption parameter \"%s\""
		     (prin1-to-string parameter)))
	  (if (>= index (length method-info-element))
	      (error "my-crypt-get-method-parameter: internal error!"))
	  (nth index method-info-element)))))

(defun my-crypt-confirm (which)
  "Given an argument which is either 'decrypt or 'encrypt, asks the
user to confirm the action.  Returns t if the user confirms it, nil if
not.  See the documentation for the variables my-crypt-encrypt-confirm
and my-crypt-decrypt-confirm."
  (if (and (not (eq 'decrypt which))
	   (not (eq 'encrypt which)))
      (error (concat "Error: my-crypt-confirm: unrecognized argument: \""
		     (prin1-to-string which) "\"")))
  (prog1
      (or (not (if (eq which 'crypt)
		   my-crypt-encrypt-confirm
		 my-crypt-decrypt-confirm))
	  (y-or-n-p (if (eq which 'decrypt)
			"Decrypt buffer? "
		      (concat "Encrypt buffer before "
			      (if (eq ?\C-s last-input-char)
				  "saving? "
				"writing? ")))))
    (message "")))

(defun my-crypt-confirm-readonly-file (file)
  "If FILE is not writable by the user, ask user if he wants to save anyway.
Returns a non-nil value if the user answers \"yes\", nil if the user is not
asked.  If the user is asked and answers \"no\", signals an error."
  (and (not (file-writable-p file))
       (or (yes-or-no-p
	    (format "File %s is write-protected; try to save anyway? " file))
	   (error "Attempt to save to a file which you aren't allow to write"))))

(defun my-crypt-functionp (form)
  "Returns t if FORM is a symbol bound as a function, a lambda expression, or
an autoload form."
  (or (and (symbolp form)
	   (fboundp form))
      (and (listp form)
	   (or (eq 'lambda (car form))
	       (eq 'autoload (car form))))))

(defun my-crypt-encrypt/decrypt-region (start end which method key)
  "Encrypt or decrypt the region between START and END in the current buffer.  The
result of the encryption/decryption replaces the original text between START and
END.  If WHICH is 'encrypt do encryption.  If WHICH is 'decrypt, do decryption.
METHOD is a method-id symbol that specifies what kind of encryption/decryption to
do.  See the variable my-crypt-method-info.  KEY is the encryption/decryption key
or nil if no key is needed."
  (if (and (not (eq 'encrypt which))
	   (not (eq 'decrypt which)))
      (error "my-crypt-encrypt/decrypt-region: unrecognized WHICH argument: %s"
	     (prin1-to-string which)))
  (let* ((method (car (assq method my-crypt-method-info)))
	 (program (progn
		    (if (null method)
			(error "my-crypt-encrypt/decrypt-region: unrecognized \
METHOD argument!"))
		    (my-crypt-get-method-parameter method (if (eq 'encrypt which)
							      'encrypt-program
							    'decrypt-program))))
	 (decrypt-args (my-crypt-get-method-parameter method (if (eq 'encrypt which)
								 'decrypt-args
							       'encrypt-args)))
	 (arglist decrypt-args))

    ;; If there is a key, replace all occurrences of %s in each argument
    ;; string with it.
    ;;
    (if key
	(while arglist
	  (let ((argstring (car arglist)))
	    (while (string-match "%s" argstring)
	      (replace-match key 'fixedcase)))
	  (setq arglist (cdr arglist))))

    ;; Now do the encryption/decryption.
    ;;
    (let (process-connection-type 'pty)
      (apply 'call-process-region
	     (point-min) (point-max) program 'delete t nil args))))

(defun my-crypt-do-find-file-decryptions (method ciphertext-locations)
  "Do one set of decryptions during a find-file.  METHOD is a symbol indicating a
decryption method (see the variable my-crypt-method-info).  CIPHERTEXT-LOCATIONS
is a list of the form ((START1 . END1) (START2 . END2) ...), where each pair of
STARTx and ENDx are numbers or markers that indicate the start and end of a region
of ciphertext to be decrypted within the current buffer."
  (let ((region-list ciphertext-locations)
	(decryption-key nil)
	(process-environment process-environment)
	(decrypt-key-function
	 (my-crypt-get-method-parameter method 'decrypt-key-function))
	(decryption-message
	 (my-crypt-get-method-parameter method 'decryption-message)))
    (while region-list
      (let ((was-read-only nil))
	(if (and decrypt-key-function
		 (or (null key)
		     my-crypt-use-multiple-keys))
	    (setq decryption-key (funcall decrypt-key-function)))
	(if buffer-read-only
	    (progn
	      (toggle-read-only)
	      (setq was-read-only t)))
	(if (null decryption-message)
	    (setq decryption-message "Decrypting %s ..."))
	(message decryption-message (buffer-name))
	(let ((here (point))
	      (cursor-in-echo-area t))
	  (goto-char (point-max))
	  (my-crypt-encrypt/decrypt-region (point-min) (point-max)
					   'decrypt decryption-method
					   decryption-key)
	  (goto-char here))
	(if was-read-only
	    (toggle-read-only))
	(setq my-crypt-buffer-was-decrypted t)
	(set-buffer-modified-p nil)
	(message (concat decryption-message ".  Done") (buffer-name)
		 (if was-read-only
		     ".  Note: file is write protected"
		   ".")))

      ;; Do cleanup.
      ;;
      (if decrypt-cleanup-function
	  (funcall decrypt-cleanup-function))
      (if my-crypt-disable-auto-save
	  (auto-save-mode -1))
      (garbage-collect)

      ;; Next, please.
      ;;
      (setq region-list (cdr region-list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The hooks.  Functions that are bound to write-file-hooks, find-file-hooks,
;; and revert-buffer-function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-crypt-find-file-hook ()
  "Function that is appended to the value of find-file-hooks to implement
transparent decryption of files.  Scans the list of decryption methods.  For each
method, checks for any regions of ciphertext and decrypts them.  If
my-crypt-use-multiple-methods is non-nil, continues scanning the list after the first
set of ciphertext is decrypted.  This allows files of compressed ciphertext to be
decrypted all the way to the plaintext.  See the documentatoin for
my-crypt-use-multiple-methods for more info."
  (let ((method-list my-crypt-decryption-methods)
	(did-some-decrypting nil)
	(method nil))
    (while (and (setq method (car method-list))
		(or (null did-some-decrypting)
		    my-crypt-use-multiple-methods))
      (let* ((detector (my-crypt-get-method-parameter method 'decryption-detector))
	     (ciphertext-locations (funcall detector)))
	(if ciphertext-locations
	    (progn
	      (setq did-some-decrypting t)
	      (my-crypt-do-find-file-decryptions method ciphertext-locations)
	      (garbage-collect))))
      (setq method-list (cdr method-list)))))

(defun my-crypt-write-file-hook ()
  "Function added to write-file-hooks that writes buffers to disk in encrypted
form.  If encryption occurs, returns t to prevent other hook functions from
being called which might overwrite the encrypted file.  Because of this last
bit, be sure to append this function to write-file-hooks, and do _not_ append
any others after it (they will not be called for encrypted files)."
  (let ((buffername (buffer-name))
	(ciphertext-filename (buffer-file-name))
	(ciphertext-file-modes nil)
	copy-buffer
	modes-to-set
	(encryption-method (my-crypt-determine-encryption-method)))
    (if (and encryption-method
	     (my-crypt-confirm 'encrypt))
	 (progn
	   (if (my-crypt-confirm-readonly-file ciphertext-filename)
	       (setq ciphertext-file-modes (file-modes ciphertext-filename)))

	   ;; Warn here about double-encryption?
	   ;;
	   (let ((encrypt-key-function
		  (my-crypt-get-method-parameter encryption-method
						 'encrypt-key-function)))
	     (if encrypt-key-function
		 (setq encryption-key (funcall my-crypt-encrypt-obtain-key))))
	   (or buffer-backed-up
	       (setq modes-to-set (backup-buffer)))
	   (message "Encrypting %s ..." (buffer-name))
	   (setq copy-buffer (get-buffer-create " *encryption buffer*"))
	   (save-excursion	; In case something sets the mark.
	     (save-restriction
	       (widen)
	       ;;
	       ;; Cope with selective-display being non-nil.  See crypt++.el.
	       ;;
	       (copy-to-buffer copy-buffer (point-min) (point-max))
	       (narrow-to-region (point) (point))
	       (insert-buffer-substring copy-buffer)
	       (unwind-protect
		   (let ((kill-buffer-hook nil))
		     (kill-buffer copy-buffer)
		     (my-crypt-encrypt/decrypt-region (point-min) (point-max) 'encrypt
						      encryption-method encryption-key)
		     (if ciphertext-file-modes
			 (set-file-modes ciphertext-filename
					 (+ 128 ciphertext-file-modes)))
		     (write-region (point-min) (point-max) ciphertext-filename nil t))
		 (delete-region (point-min) (point-max)))))
	   (if ciphertext-file-modes
	       (set-file-modes ciphertext-filename ciphertext-file-modes))
	   (set-buffer-modified-p nil)
	   (message "Wrote %s (encrypted)" ciphertext-filename)
	   (if modes-to-set
	       (condition-case nil
		   (set-file-modes ciphertext-filename modes-to-set)
		 (error nil)))
	   t))))

(defun my-crypt-revert-buffer ()
  "..."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   UNDER CONSTRUCTION                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bind the hook functions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if emacs19
    (progn
      (add-hook 'find-file-hooks 'my-crypt-find-file-hook 'append)
      (add-hook 'write-file-hooks 'my-crypt-write-file-hook 'append))
  (setq find-file-hooks (append find-file-hooks (list 'my-crypt-find-file-hook)))
  (setq write-file-hooks (append write-file-hooks (list 'my-crypt-write-file-hook))))

(make-variable-buffer-local 'revert-buffer-function)
(put 'revert-buffer-function 'permanent-local t)
(setq revert-buffer-function 'my-crypt-revert-buffer-function)

(provide 'my-crypt2)


;;; Local Variables:
;;; eval: (set-fill-column 75)
;;; End:
