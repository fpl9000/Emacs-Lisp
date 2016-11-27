;; my-crypt.el
;; Author: Francis Litterio <franl@world.std.com>
;;
;; Enables some GPG operations on buffers and the region.  The supported
;; operations are encryption, encryption-and-signing, decryption, clearsigning,
;; and verification of clearsigned messages.  This package does not (yet)
;; support conventional encryption or creating or verifying detached signatures.
;;
;; This package does not alter how Emacs makes backup files, so be careful if
;; you decrypt the contents of a file, save it, encrypt its contents, then save
;; it again.  The backup may contain plaintext.
;;
;; This package requires a version of Bash that supports the "<<<" redirection
;; syntax.  If /bin/bash is not new enough, you will get an informative error,
;; and you should set the variable my-crypt-bash-path to the pathname of a
;; version of Bash that supports that syntax.
;;
;; To use this package, put this file in a directory listed in variable
;; load-path and add the following code in your .emacs startup file:
;;
;;	(require 'my-crypt)
;;
;; Then add code to your .emacs startup file to bind keys to the public
;; functions, such as:
;;
;;	(global-set-key "\C-c\C-e" 'my-crypt-encrypt-region)
;;	(global-set-key "\C-c\C-b" 'my-crypt-encrypt-buffer)
;;	(global-set-key "\C-c\C-d" 'my-crypt-decrypt-message)
;;	(global-set-key [f12] 'my-crypt-forget-passphrase)
;;
;; Optionally, add code to set the public variables, such as:
;;
;;	(setq my-crypt-program-path "/home/joe/bin/gpg")
;;	(setq my-crypt-default-signing-key "0x03DC6143F58E1BE3C7C63707FB2958B83307A1D9")
;;	(setq my-crypt-passphrase-save-duration 60)
;;
;; Here are the public functions defined by this package:
;;
;; Public Function		Purpose
;; --------------------------	-------------------------------------------------
;; my-crypt-encrypt-buffer	Encrypts, encrypts-and-signs, or clearsigns the
;;				contents of the current buffer.
;;
;; my-crypt-encrypt-region	Encrypts, encrypts-and-signs, or clearsigns the
;;				contents of the current region.
;;
;; my-crypt-clearsign-buffer	Clearsigns the contents of the current buffer.
;;
;; my-crypt-clearsign-region	Clearsigns the contents of the current region.
;;
;; my-crypt-decrypt-buffer	Decrypts (or verifies the signature on) the contents
;;				of the current buffer.
;;
;; my-crypt-decrypt-message	Decrypts (or verifies the signature on) the GPG
;;				message that point is located within.
;;
;; my-crypt-decrypt-region	Decrypts (or verifies the signature on) the contents
;;				of the current region.
;;
;; my-crypt-forget-passphrase	Forgets the cached passphrase (if it is cached).
;;				See variable my-crypt-passphrase-save-duration.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public variables.  You should examine the documentation strings for these
;; variables, and set them to appropriate values.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-crypt-program-path (if my-win32
				  (concat my-systemdrive "/apps/cygwin/bin/gpg.exe")
				(if (file-executable-p "/usr/local/bin/gpg")
				    "/usr/local/bin/gpg"
				  (if (file-executable-p "/usr/bin/gpg")
				      "/usr/bin/gpg"
				    nil)))
    "*Pathname of the GPG program used by functions in my-crypt.el.  Do not include
any quotes in this string!  They are added automatically for you.  WARNING: This
package requires the use of GPG, because it formats the command line using option
syntax that only GPG accepts.")

(defvar my-crypt-program-hashes (if my-win32
				  '("2f49d4abd57c529a3f5917f10e73b85b4115fbdc")
				  (if (file-executable-p "/usr/local/bin/gpg")
				      '("...")
				    (if (file-executable-p "/usr/bin/gpg")
					'("...")
				      '("..."))))
  "A list of the possible valid SHA-1 hashes of the program named in variable
my-crypt-program-path.  This is a list of possible hashes, because different
machines might have different versions of GPG installed.")

(defvar my-crypt-always-sign nil
  "*Non-nil means encryption is always accompanied by a digital signature.")

(defvar my-crypt-default-signing-key "0x834A25501371EFBA059559564EAC9EE186FD2201"
  "*If non-nil, this is a string containing the fingerprint, key-id, or username
specifying which key should be used for signing.  If nil, the user is prompted
for this information.")

(defvar my-crypt-always-in-place nil
  "*Non-nil means encryption, decryption, and signing always replace the
processed text (usually the region or buffer contents) with the results of the
operation.  If this is nil, you are asked whether you want the results to
replace the processed text.")

(defvar my-crypt-passphrase-save-duration 1800
  "*Number of seconds to cache a passphrase after its last use.  If 0, do not
cache passphrases.  This is dangerous if you leave your Emacs session
unattended, because your passphrase unlocks your private key.  Use function
my-crypt-forget-passphrase to cause the cached passphrase to be forgotten
prematurely.")

(defvar my-crypt-nicknames
  `(("me" . ,(concat "0xe94460ccd04ab265e237b62444cabdfaa6962a9a "	;; 0xA6962A9A Elgamal 4096
		     "0x834a25501371efba059559564eac9ee186fd2201 "	;; 0x86FD2201 Elgamal 1024
		     "0x0237df6c6643cd2c10c8b58b5734f321 "		;; 0xDDFA3E71 RSA 2048
		     "0x80A8DC53469C599DC3FFE5D95939D40F "		;; 0x1270EA1D RSA 1022
		     "0x77a49eaf40887140253a35d240a178b3e01f8351"))	;; 0xE01F8351 Elgamal 3072

    ("me-new" . "0xe94460ccd04ab265e237b62444cabdfaa6962a9a")		;; 0xA6962A9A Elgamal 4096

    ("George" . ,(concat "0x23694e17adb3d142a459dd637e9a923ed8c21342 "
			 "0x671baf2945232a03a1b54991ebe2799058ef7f72"))
     )
  "*An assoc list mapping nickname strings to key fingerprints, userids, or
key-ids.  The nicknames can be used at any prompt for recipient ids.  The car of
each element of the list should be a string specifying a nickname and the cdr of
each element should be a space-separated string of fingerprints, real userids,
or key-ids.")

(defvar my-crypt-bash-path (if my-win32
			       (concat my-systemdrive "/apps/cygwin/bin/bash")
			     "/bin/bash")
  "*Pathname of the Bash shell to use when spawning GPG commands.  This must be
Bash 2.x or newer to support the oddball I/O redirection syntax that this package
uses.  If this pathname contains no slashes, it is searched for in the value of
PATH.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables.  Not for user consumption.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-crypt-saved-passphrase nil
  "Holds the saved passphrase.")

(defvar my-crypt-recipients-history nil
  "Holds the input history used when reading recipients.")

(defvar my-crypt-signing-key-history nil
  "Holds the input history used when reading the signing key.")

(defvar my-crypt-messages-buffer-name "*GPG Messages*"
  "The name of the buffer that shows GPG informational messages.")

(defvar my-crypt-timer nil
  "Holds the timer object returned by run-at-time when we register the function
to forget the passphrase.")

(defvar my-crypt-bash-is-ok nil
  "Non-nil if Bash is sufficiently new enough for this code to work.  This is set
in function my-crypt-sanity.")

(make-face 'my-crypt-results-face)
(set-face-foreground 'my-crypt-results-face "yellow")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions.  Bind keys to these functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-crypt-encrypt-buffer (&optional operation)
  "Encrypts contents of current buffer.  Optional argument OPERATION is a symbol
specifying the operation to perform: nil or 'encrypt means encrypt to one or
more recipients, 'encrypt+sign means encrypt and sign, and 'clearsign means
clearsign (no encryption occurs).  Interactively, no prefix means just encrypt,
a prefix of C-u means encrypt and sign, and a prefix of C-u C-u means clearsign
without any encryption."
  (interactive (list (cond ((null current-prefix-arg) 'encrypt)
			   ((equal current-prefix-arg '(4)) 'encrypt+sign)
			   ((equal current-prefix-arg '(16)) 'clearsign)
			   (t (error (concat "Invalid prefix argument!  "
					     "Use only 'C-u' or 'C-u C-u'."))))))
  (my-crypt-encrypt-region (point-min) (point-max) operation))

(defun my-crypt-clearsign-region (&optional start end)
  "Clearsigns the region between START and END.  If omitted, arguments START and END
default to the start and end of the current region."
  (interactive)
  (let ((start (or start (region-beginning)))
	(end (or end (region-end))))
    (my-crypt-encrypt-region start end 'clearsign)))

(defun my-crypt-clearsign-buffer ()
  "Clearsigns the current buffer."
  (interactive)
  (my-crypt-encrypt-region (point-min) (point-max) 'clearsign))

(defun my-crypt-decrypt-buffer ()
  "Decrypts contents of current buffer."
  (interactive)
  (my-crypt-decrypt-region (point-min) (point-max)))

(defun my-crypt-decrypt-message ()
  "Decrypts the GPG-encrypted message that point is within.  This only works if the
message is ASCII armored."
  (interactive)
  (my-crypt-sanity)

  ;; TODO: Check for non-ASCII data near point.
  ;; Can non-ASCII data be clearsigned?

  (let (start
	end
	verify-clearsigned
	(here (point)))
    (save-excursion
      (end-of-line)	; Guarantee that we'll find BEGIN line.

      ;; Could be either "BEGIN PGP MESSAGE" or "BEGIN PGP SIGNED MESSAGE".
      (if (not (search-backward-regexp "^-----BEGIN PGP \\(.*\\)MESSAGE-----\\s-*$"
				       (point-min) t))
	  (error "The point is not on a GPG message!"))
      (setq start (point))
      
      ;; Is it a clearsigned message?
      (if (string= "SIGNED " (match-string 1))
	  (setq verify-clearsigned t))

      ;; Could be either "END PGP MESSAGE" or "END PGP SIGNATURE".
      (if (not (search-forward-regexp "^-----END PGP .*-----\\s-*$"
				      (point-max) t))
	  (error "The point is not on a GPG message!"))
      (setq end (point)))
    (if (> here end)
	(error "The point is not on a GPG message!"))

    (my-crypt-decrypt-region start end verify-clearsigned)))

(defun my-crypt-encrypt-region (start end &optional operation)
  "Encrypts and/or signs part of current buffer between START and END, which
must be integers or markers.  Optional argument OPERATION is a symbol specifying
the operation to perform: nil or 'encrypt means encrypt to one or more
recipients, 'encrypt+sign means encrypt and sign, and 'clearsign means
clearsign.  Interactively, no prefix means just encrypt, a prefix of C-u means
encrypt and sign, and a prefix of C-u C-u means clearsign.

After completion point is before the first character of ciphertext and mark is
after the last."
  (interactive (list (region-beginning) (region-end)
		     (cond ((null current-prefix-arg) 'encrypt)
			   ((equal current-prefix-arg '(4)) 'encrypt+sign)
			   ((equal current-prefix-arg '(16)) 'clearsign)
			   (t (error "Invalid prefix argument!")))))

  (my-crypt-sanity)
  (if (null operation)
      (setq operation 'encrypt))
  (if (not (integer-or-marker-p start))
      (error "my-crypt-encrypt-region: start is not an integer or marker!"))
  (if (not (integer-or-marker-p end))
      (error "my-crypt-encrypt-region: end is not an integer or marker!"))
  (if (not (memq operation '(encrypt encrypt+sign clearsign)))
      (error "my-crypt-encrypt-region: invalid operation!"))
  (if (equal start end)
      (error "Zero-length plaintext!"))

  (let ((in-place (or my-crypt-always-in-place
		      (y-or-n-p (concat "Replace plaintext with "
					(if (eq operation 'clearsign)
					    "clearsigned text"
					  "ciphertext")
					"? "))))
	(output-buffer-name "*Encrypt Output*")
	recipients
	passphrase
	command
	signing-key)

    (if (and in-place buffer-read-only)
	(error "Buffer is read only!"))

    (if (and (eq operation 'encrypt)
	     my-crypt-always-sign)
	(setq operation 'encrypt+sign))

    (if (not (eq operation 'clearsign))
	(setq recipients
	      (mapconcat (lambda (str)
			   (or (cdr (assoc str my-crypt-nicknames)) str))
			 (split-string
			  ;; TODO: Use completing-read-multiple instead?
			  (completing-read "Recipients: " my-crypt-nicknames nil nil nil
					   'my-crypt-recipients-history nil nil))
			 " ")))

    ;; For a digital signature, we need the signing key and its passphrase.
    (when (memq operation '(encrypt+sign clearsign))
      (setq signing-key (or my-crypt-default-signing-key
			    (read-from-minibuffer "Signing key: " nil nil nil
						  'my-crypt-signing-key-history)))
      (setq passphrase (my-crypt-get-passphrase "Enter passphrase: ")))

    ;; Do the encryption and/or signing.
    (setq command (concat "\"" my-crypt-program-path "\" --always-trust --batch"
			  (if passphrase
			      " --passphrase-fd 6")
			  (cond ((eq operation 'encrypt)
				 " --encrypt")
				((eq operation 'encrypt+sign)
				 (concat " --encrypt --sign --local-user " signing-key))
				((eq operation 'clearsign)
				 (concat " --clearsign --local-user " signing-key)))
			  (if (not (eq operation 'clearsign))
			      (mapconcat (lambda (recip)
					   (concat " --recipient " recip))
					 (split-string recipients " ") " "))
			  (if passphrase
			      (concat " 6<<<\"" passphrase "\""))))

    (my-crypt-erase-buffer my-crypt-messages-buffer-name)
    (my-crypt-erase-buffer output-buffer-name)

    (message (cond ((eq operation 'encrypt) "Encrypting ...")
		   ((eq operation 'encrypt+sign) "Encrypting and signing ...")
		   ((eq operation 'clearsign) "Clearsigning ...")))

    (let ((shell-file-name my-crypt-bash-path))
      (shell-command-on-region start end command output-buffer-name in-place
			       my-crypt-messages-buffer-name))
    (deactivate-mark)
    (save-excursion
      (when (not in-place)
	(set-buffer output-buffer-name)
	(goto-char (point-min)))
      (forward-line 1)
      (when (looking-at "^Version: .*\\( (Cygwin32)\\)")
	(replace-match "" nil 'literal nil 1)
	(forward-line 1))
      ;; This breaks clearsigned messages (GPG doesn't like comment headers on
      ;; clearsigned messages).
      ;;(insert "Comment: Created on " (time-stamp-yyyy-mm-dd) " at "
      ;;        (time-stamp-hh:mm:ss) "\n")
      )

    ;; Maybe show standard error buffer and output buffers.
    (my-crypt-display-results output-buffer-name)
    (message "Done.")))

(defun my-crypt-decrypt-region (start end &optional verify-clearsigned)
  "Decrypts or verifies part of current buffer between START and END, which must
be integers or markers.  If optional third argument, VERIFY-CLEARSIGNED, is
non-nil, this function does not request a passphrase, since it assumes it is
only verifying a clearsigned message."
  (interactive "r")

  (my-crypt-sanity)
  (if (not (integer-or-marker-p start))
      (error "my-crypt-decrypt-region: start is not an integer or marker!"))
  (if (not (integer-or-marker-p end))
      (error "my-crypt-decrypt-region: end is not an integer or marker!"))
  (if (equal start end)
      (error "Zero-length ciphertext!"))

  (let ((in-place (or my-crypt-always-in-place
		      (y-or-n-p (concat "Replace "
					(if verify-clearsigned
					    "clearsigned text"
					  "ciphertext")
					" with plaintext? "))))
	(output-buffer-name "*Decrypt Output*")
	passphrase
	command)

    (if (and in-place buffer-read-only)
	(error "Buffer is read only!"))

    (if (not verify-clearsigned)
	(setq passphrase (my-crypt-get-passphrase "Enter passphrase: ")))

    (setq command (concat "\"" my-crypt-program-path "\""
			  (if passphrase
			      " --passphrase-fd 6")
			  " --batch --decrypt"
			  (if passphrase
			      (concat " 6<<<\"" passphrase "\""))))
    (message "Processing ...")

    (my-crypt-erase-buffer my-crypt-messages-buffer-name)
    (my-crypt-erase-buffer output-buffer-name)

    (let ((shell-file-name my-crypt-bash-path))
      (shell-command-on-region start end command output-buffer-name in-place
			       my-crypt-messages-buffer-name))

    (deactivate-mark)

    ;; Maybe show standard error buffer and output buffers.
    (my-crypt-display-results output-buffer-name)
    (message "Done.")))

(defun my-crypt-forget-passphrase ()
  "Forget the saved passphrase."
  (interactive)
  (if (timerp my-crypt-timer)
      (cancel-timer my-crypt-timer))
  (if (stringp my-crypt-saved-passphrase)
      (my-crypt-wipe-string my-crypt-saved-passphrase))
  (setq my-crypt-saved-passphrase nil)
  (garbage-collect)
  (if (called-interactively-p 'any)
      (message "Forgot passphrase.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workhorse functions.  These are called from the high-level functions.
;; Don't bind keys to these functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-crypt-sanity ()
  "Perform sanity checks."
  (interactive)
  (message "Performing sanity checks ...")
  (if (not (file-directory-p "c:/temp"))
      (error "Directory c:/temp does not exist!"))
  (if (not (stringp my-crypt-program-path))
      (error "my-crypt-program-path is not set properly!"))
  (if (not (file-exists-p my-crypt-program-path))
      (error "%s does not exist!" my-crypt-program-path))
  (if (not (file-executable-p my-crypt-program-path))
      (error "%s is not executable!" my-crypt-program-path))
  (let ((program-hash (car (split-string (my-command (concat "sha1sum -b " my-crypt-program-path)
						     nil t)))))
    (dolist (hash my-crypt-program-hashes)
      (if (not (string= program-hash hash))
	  (error "%s failed hash check!" my-crypt-program-path))))
  (if (null my-crypt-bash-is-ok)
      (unwind-protect
	  (progn
	    (with-temp-message ""
	      (let ((shell-file-name my-crypt-bash-path))
		(shell-command-on-region (point) (point) "cat 6<<<foobar <&6"
					 " *My Crypt Sanity*" nil nil)))
	    (if (not (with-current-buffer " *My Crypt Sanity*"
		       (goto-char (point-min))
		       (looking-at-p "^foobar$")))
		(error "You need a version of Bash that supports '<<<' input redirection!")
	      (setq my-crypt-bash-is-ok t)))
	(ignore-errors (kill-buffer " *My Crypt Sanity*")))))

(defun my-crypt-get-passphrase (prompt)
  "Reads and returns a passphrase from the user without echoing it.  Prompts
with string PROMPT in the minibuffer.  If a cached passphrase is available,
returns it without reading a passphrase and restarts the passphrase forget
timer."
  (let ((cached-passphrase my-crypt-saved-passphrase))
    (if (stringp cached-passphrase)
	(progn
	  (my-crypt-start-timer)
	  cached-passphrase)
      (let* ((read-hide-char ?*)
	     (passphrase (read-passwd prompt)))
	(setq my-crypt-saved-passphrase passphrase)
	(if (> my-crypt-passphrase-save-duration 0)
	    (my-crypt-start-timer))
	passphrase))))

(defun my-crypt-start-timer ()
  "Starts (or restarts) the timer that will automatically forget the cached
passphrase."
  (if (timerp my-crypt-timer)
      (cancel-timer my-crypt-timer))
  (setq my-crypt-timer (run-at-time my-crypt-passphrase-save-duration
				    nil (function my-crypt-forget-passphrase))))

(defun my-crypt-wipe-string (string)
  "Replaces each character in a string with 'X' in place."
  (if (not (stringp string))
      (error "my-crypt-wipe-string: argument is not a string!"))
  (let ((index 0)
	(length (length string)))
    (while (< index length)
      (aset string index ?X)
      (setq index (1+ index))))
  string)

(defun my-crypt-erase-buffer (buffer)
  "Erases the contents of BUFFER, which is either a buffer name or a buffer
object.  If BUFFER does not exist, nothing happens."
  (let ((msgbuf (get-buffer buffer)))
    (if (bufferp msgbuf)
      (with-current-buffer msgbuf
	(if (eq msgbuf (current-buffer))
	    (erase-buffer))))))

(defun my-crypt-display-results (output-buffer-name)
  "..."
  (let ((outbuf (get-buffer output-buffer-name))
	(msgbuf (get-buffer my-crypt-messages-buffer-name))
	(msgwin nil)
	(outsize 0)
	(msgsize 0))
    (if (bufferp outbuf)
	(setq outsize (buffer-size outbuf)))
    (if (bufferp msgbuf)
	(setq msgsize (buffer-size msgbuf)))
    (delete-other-windows)

    ;; Display messages buffer first, so it appears bottommost.
    (if (> msgsize 0)
	(let ((origwin (selected-window)))
	  (pop-to-buffer msgbuf 'other-window 'norecord)
	  (setq msgwin (selected-window))
	  (goto-char (point-min))
	  (shrink-window-if-larger-than-buffer)

	  ;; This should not be necessary now that "*GPG Messages*" is listed in
	  ;; my-font-lock-disabled-buffers.
	  ;;
	  ;;(font-lock-mode 0)
	  (set-text-properties (point-min) (point-max) '(face my-crypt-results-face))

	  (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer)
	  (bury-buffer (current-buffer))
	  (set-buffer-modified-p nil)

	  (select-window origwin)))

    (if (> outsize 0)
	(switch-to-buffer outbuf))

    ;; Leave the message window selected (if there is one).
    (if (windowp msgwin)
	(select-window msgwin))

    ;; Delete empty buffers (which aren't displayed).
    (if (and outbuf (= 0 outsize))
	(kill-buffer outbuf))
    (if (and msgbuf (= 0 msgsize))
	(kill-buffer msgbuf))))
