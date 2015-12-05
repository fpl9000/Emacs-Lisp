;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code to implement a channel list window in ERC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'erc)
(require 'erc-backend)

;;(defvar my-erc-channels nil
;;  "A list of strings describing channel names and info for each server.  This
;;variable has a different value in each server buffer.  Its value outside of a
;;server buffer is undefined and should not be used.")
;;
;;(make-variable-buffer-local 'my-erc-channels)

(defvar my-erc-channel-list-frame nil
  "Holds the frame object for the frame showing the channel list.")

(defvar my-erc-channel-list-buffer-name "*ERC Channel List*"
  "The name of the channel list buffer.")

(defvar my-erc-channel-list-buffer nil
  "Holds the channel list buffer.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up server response handlers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We define our own version of this response handler, because the standard one
;; is to ignore the server message.

(define-erc-response-handler (323)
  "End of channel list." nil
  (erc-display-line (erc-make-notice "End of channel list."))
  (if (and (frame-live-p my-erc-channel-list-frame)
	   (buffer-live-p my-erc-channel-list-buffer))
      (save-window-excursion
	(select-frame my-erc-channel-list-frame)
	(goto-char (point-min))
	(forward-line 2)
	(let ((start (point)))
	  (goto-char (point-max))
	  (sort-lines nil start (point)))
	(goto-char (point-min)))))

(add-hook 'erc-server-323-functions 'erc-server-323)


;; Hook the 321 (LIST header) server response.

(defun my-erc-server-321-function (server-process parsed-response)
  "..."
  (if (buffer-live-p my-erc-channel-list-buffer)
      (with-current-buffer my-erc-channel-list-buffer
	(goto-char (point-max))
	(insert ">>HEADER: " (prin1-to-string parsed-response) "\n")))

  ;; Return t to stop the builtin processing of this server response.
  t)
  
(add-hook 'erc-server-321-functions 'my-erc-server-321-function)


;; Hook the 322 (LIST notice) server response.

(defun my-erc-server-322-function (server-process parsed-response)
  "..."

  ;; (erc-response.command resp)
  ;; "322"
  ;;
  ;; (erc-response.command-args resp)
  ;; ("franl" "#z0tk" "3" "[+ntr] z0TK's Chan")
  ;;
  ;; (erc-response.contents resp)
  ;; "[+ntr] z0TK's Chan"
  ;;
  ;; (erc-response.sender resp)
  ;; "chaos.us.p2p-network.net"
  ;;
  ;; (erc-response.unparsed resp)
  ;; ":chaos.us.p2p-network.net 322 franl #z0tk 3 :[+ntr] z0TK's Chan"

  (if (buffer-live-p my-erc-channel-list-buffer)
      (let ((buffer-name (nth 1 (erc-response.command-args parsed-response)))
	    (buffer-member-count (nth 2 (erc-response.command-args parsed-response)))
	    (buffer-topic (nth 3 (erc-response.command-args parsed-response))))
	(if (string= "*" buffer-name)
	    ;; Just return t to keep this line from being displayed.
	    t
	  (if (string-match "^\\[[^]\\]*\\s-+\\(.*\\)" buffer-topic)
	      (setq buffer-topic (match-string 1 buffer-topic)))

	  (with-current-buffer my-erc-channel-list-buffer
	    (goto-char (point-max))
	    (insert (format "%-23s  %5s  %s" buffer-name buffer-member-count buffer-topic) "\n"))
	  ;; Return t to stop the builtin processing of this server response.
	  t))))
  
(add-hook 'erc-server-322-functions 'my-erc-server-322-function)


;; The /list command.

(defun erc-cmd-LIST (&rest args)
  "Opens a window to display the channel names as they are returned by the server."
  (save-window-excursion
    (if (not (frame-live-p my-erc-channel-list-frame))
	(setq my-erc-channel-list-frame
	      (make-frame `((top . 100) (left . 100) (width . 100) (height . 30)
			    (title . ,(format "Channel List (%s)"
					      (buffer-name (erc-server-buffer))))))))

    (select-frame my-erc-channel-list-frame)
    (switch-to-buffer my-erc-channel-list-buffer-name)

    (if (not (string= (buffer-name) my-erc-channel-list-buffer-name))
	(error "erc-cmd-LIST: failed to switch to ERC channel list buffer!"))

    (setq my-erc-channel-list-buffer (current-buffer))
    (erase-buffer)
    (toggle-truncate-lines 1)
    (font-lock-mode 0)

    (insert "Channel Name             Users  Channel Info and Topic\n")
    (insert "-----------------------  -----  -----------------------------------------------\n"))

  (erc-display-line (erc-make-notice "Please give the server time to send the channel list."))

  ;; BUG: The cursor ends up before the next prompt.
  (erc-server-send "LIST"))


(provide 'my-erc-list)
