;; my-constants.el
;; This file defines constants that are used in nearly every other file, so
;; this file must be loaded first.

(defconst my-win32 (eq system-type 'windows-nt)
  "Non-nil if running under Windows, nil otherwise.")

(defconst my-systemdrive (and my-win32 (downcase (getenv "SYSTEMDRIVE")))
  "Under Windows, this is a string containing the value of environment variable
SYSTEMDRIVE (in lowercase).  On all other OSes, this is nil.")

(defconst my-sysdl (and my-win32 (downcase (substring (getenv "SYSTEMDRIVE") 0 1)))
  "Under Windows, this is a string containing the drive letter of the system drive (in
lowercase).  On all other OSes, this is nil.")

(defconst my-progfilesx86 (let ((my-directory (concat my-systemdrive "/program files (x86)")))
			    (if (and my-win32 (file-exists-p my-directory))
				"program files (x86)"
			      "program files"))
  "Under 64-bit Windows, this is the string 'program files (x86)'.  Under 32-bit Windows,
this is the string 'program files'.  On all other OSes, this is nil.")

(defconst my-win7 (and my-win32 (file-directory-p (concat my-systemdrive "/users")))
  "Non-nil if running under Vista or above.  This is mostly used to cope with differing
window border sizes between XP and Vista+.")

(defconst my-solaris (and (not my-win32)
			  (string-match-p "sunos" (shell-command-to-string "/bin/uname -a")))
  "Non-nil if running under Solaris, nil otherwise.")

(defconst my-bsd (and (not my-win32)
		      (string-match-p "FreeBSD" (shell-command-to-string "/usr/bin/uname -a")))
  "Non-nil if running under BSD, nil otherwise.")
