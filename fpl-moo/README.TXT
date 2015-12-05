Installation Instructions for fpl-moo.el
========================================

Here's how to install the package:

1. Download either the .tar.gz or .zip file from:

	http://world.std.com/~franl/dist/

   You want the one with the highest version number.

2. Unpack the .tar.gz or .zip file into some directory that you have created
   solely for that purpose (I'll call it /home/elisp in these instructions, but
   you can put them anywhere).

3. Edit your .emacs startup file to contain the following Emacs-Lisp
   code (change the "/home/elisp" path appropriately):

	(setq load-path (append load-path '("/home/lisp")))
	(require 'fpl-moo)
	(define-key global-map "\C-cm" 'fpl-moo-connect)

   You can change the keybinding to any key you want.

4. Type C-c m to connect to one of a list of predefined MOOs.  See
   the documentation for the variable fpl-moo-sites for how you can
   teach fpl-moo.el how to connect to MOOs it does not yet know
   about.  To see that documentation type "C-h v fpl-moo-sites RET".

If you have any questions or problems, contact the author at the below
address.
--
Francis Litterio
franl@world.std.com
http://world.std.com/~franl/
PGP public key fingerprints:
77A4 9EAF 4088 7140 253A 35D2 40A1 78B3 E01F 8351 (DH/DSS)
0237 DF6C 6643 CD2C 10C8 B58B 5734 F321 (RSA)
80A8 DC53 469C 599D C3FF E5D9 5939 D40F (RSA)
