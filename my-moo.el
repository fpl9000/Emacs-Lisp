(require 'fpl-moo)

(setq fpl-moo-default-connect-id "Karellan"
      fpl-moo-control-u-kills-line t
      fpl-moo-edit-new-frame (if (display-graphic-p)
				 (let ((newparams (copy-alist default-frame-alist)))
				   (setcdr (assoc 'height newparams) 30)
				   (setcdr (assoc 'top newparams) 300)
				   (setcdr (assoc 'left newparams) 0)
				   newparams))
      fpl-moo-sites '(("Number2" "localhost" 7777
		       player "Number2"
		       prompt "Number2: ")

		      ("Supervisor" "localhost" 7777
		       player "Supervisor"
		       prompt "Supervisor: ")

		      ("Local-2" "localhost" 9999
		       player "Number2"
		       prompt "(local) Number2: ")

		      ("Villagemoo" "localhost" 7777
		       prompt "VillageMOO: ")

		      ("Lambdamoo" "lambda.moo.mud.org" 8888
		       prompt "lambdamoo: ")

		      ("Karellan" "lambda.moo.mud.org" 8888
		       player "Karellan"
		       prompt "Karellan: ")

		      ("Karellan-local" "localhost" 8888
		       player "Karellan"
		       prompt "Karellan: ")

		      ("Lambdamoo-Guest" "lambda.moo.mud.org" 8888
		       connect "guest"
		       prompt "Guest: ")))


(defun my-fpl-moo-mode-hook ()
  "Added to fpl-moo-mode-hook."
  (set-face-foreground 'fpl-moo-default-highlight-face "#0ff")
  (fpl-react-register fpl-moo-id "^\\*BEEP\\*$" 'ding 'replace 'all))

(add-hook 'fpl-moo-mode-hook 'my-fpl-moo-mode-hook)


(defun my-fpl-moo-edit-mode-hook ()
  "Added to fpl-moo-edit-mode-hook."
  (define-key fpl-moo-edit-mode-map "\M-i" (lambda () (interactive) (ding)))
  (define-key fpl-moo-edit-mode-map "\C-i" "  ")
  (define-key fpl-moo-edit-mode-map "\C-c\C-d"
    "player:tell(\"        player: \", player);
player:tell(\"        caller: \", caller);
player:tell(\"caller_perms(): \", caller_perms());
player:tell(\"          verb: \", toliteral(verb));
player:tell(\"          args: \", toliteral(args));
player:tell(\"          dobj: \", dobj);
player:tell(\"       dobjstr: \", toliteral(dobjstr));
player:tell(\"       prepstr: \", toliteral(prepstr));
player:tell(\"          iobj: \", iobj);
player:tell(\"       iobjstr: \", toliteral(iobjstr));
player:tell(\"        argstr: \", toliteral(argstr));"))

(add-hook 'fpl-moo-edit-mode-hook 'my-fpl-moo-edit-mode-hook)

(fpl-react-register "villagemoo-2"
		    "^You say that\.$"
		    '(progn (message "You say that.")
			    (goto-char start)
			    (kill-line 1)
			    (run-at-time 2 nil #'message ""))
		    'replace 'all)

(fpl-react-register "lambdamoo-karellan"
		    "^You say that\.$"
		    '(progn (message "You say that.")
			    (goto-char start)
			    (kill-line 1)
			    (run-at-time 2 nil #'message ""))
		    'replace 'all)

(if my-win32
    ;; Windoze ...
    (progn
      (if (file-exists-p (concat my-systemdrive "/windows"))
	  (fpl-react-register "Number2"
			      "sense.*Number6.*connected"
			      '(progn (ding)
				      (sleep-for 0.3)
				      (ding)
				      (sleep-for 0.3)
				      (ding)
				      (message "Number 6 has connected!")
				      (raise-frame))
			      'replace 'all)))
  ;; UNIX ...
  (fpl-react-register "Number2"
		      "sense.*Number6.*connected"
		      '(progn (ding) (sleep-for 0.1) (ding) (sleep-for 0.5)
			      (ding) (sleep-for 0.1) (ding) (sleep-for 0.5)
			      (ding) (sleep-for 0.1) (ding) (sleep-for 0.5)
			      (ding) (sleep-for 0.1) (ding)
			      (message "Number 6 has connected!")
			      (raise-frame))
		      'replace 'all))
