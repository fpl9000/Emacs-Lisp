;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hacks to modify existing code to workaround bugs.  Remove these hacks when
;; the bugs are fixed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Work around Emacs bug #23759 (http://thread.gmane.org/gmane.emacs.bugs/119471)
;; that I filed where none of the files named in list gnutls-trustfiles exist, so
;; the %t in the strings in the list tls-program expands to "nil", which prevent
;; gnutls-cli from connecting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tls)

(setq tls-program '("gnutls-cli -p %p %h"
		    "gnutls-cli -p %p %h --protocols ssl3"
		    "openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))

