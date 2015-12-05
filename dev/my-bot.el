;; my-bot.el
;;
;; Author: Francis Litterio, Jr.
;;	   flitterio@gmail.com
;;         <franl> on the freenode IRC network.
;;
;; This is my IRC bot.  To configure it, search for the string "<CONFIGURE>"
;; below and read the comments nearby each occurance of that string.  You can
;; run this bot in a dedicated Emacs using this command in a Bourne-like shell:
;;
;;	emacs -q -nw -l /path/to/my-bot -f my-bot-start
;;
;; I haven't figured out how to make this work in Emacs batch mode, so the above
;; command devotes a terminal to the bot.  One way around this on UNIX systems
;; is to use the screen utility to allocate a pseudo-terminal and disconnect
;; from the screen session, like this:
;;
;;	screen -d -m emacs -q -nw -l /path/to/my-bot -f my-bot-start
;;
;; After executing the above command, type "screen -r" to reattach to the screen
;; session.  See the screen man page for how to use screen.
;;
;; This bot has Wiki page:
;;
;;	http://www.emacswiki.org/cgi-bin/wiki.pl?BirnyTheBot
;;
;; See the bottom of this file for a copy of the GNU General Public License.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2003 Francis Litterio, Jr.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; THINGS TO TEST:
;;
;; o Multi-network connectivity.  The bot has never been connected to multiple
;;   networks on a regular basis.
;;
;; BUGS TO FIX:
;;
;; o Command ".quit" says "Sorry, I'm not connected to that network" when quitting
;;   from a network to which it _is_ connected.
;;
;; o Command ".quit" doesn't delete the server buffer.
;;
;; o Commands ".connect" and ".join" cause the bot's Emacs to produce output as
;;   buffers are switched.
;;
;; o Command ".join" needs a NETWORK argument -- otherwise it's impossible to
;;   command the bot the join the first channel on a newly-connected network.
;;
;; o Command ".part" needs a NETWORK argument.  Maybe make it optional.
;;
;; o The bot should reconnect to a network when unexpectedly disconnected but
;;   still connected to another network.  Instead, it terminates and restarts.
;;
;; o This code assumes that a channel name (e.g., "#tcpip") is also the name
;;   of that channel's buffer.  But the buffer could be named "#tcpip<2>".  Thus,
;;   this bot cannot cope with joining two channels with the same name on
;;   different networks.
;;
;; FEATURES TO ADD:
;;
;; o Ignore excessive /MSGs from the same nick within too short a period of time.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <CONFIGURE>
;; Adjust load-path.
;;
;; Change the below pathname to refer to the directory that contains the ERC
;; source code.  This is necessary, because ERC is not yet part of Emacs.  To
;; download and learn about ERC, visit:
;;
;;	http://www.emacswiki.org/cgi-bin/wiki.pl?EmacsIRCClient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/cvs/erc")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <CONFIGURE>
;; Personalize keybindings.
;;
;; There are times that you may want to interact with the bot's Emacs session
;; directly.  Configure any keybindings you may want here.
;;
;; ISSUE: Should this section be after the package loading section that follows?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "M-h") 'help-command)
(define-key global-map (kbd "M-ESC") 'eval-expression)
(define-key global-map (kbd "C-x C-b") '(lambda () (interactive)
					  (delete-other-windows)
					  (list-buffers)
					  (ignore-errors (delete-window))))
(define-key global-map (kbd "C-x C-c") '(lambda () (interactive)
					  (if (yes-or-no-p "Really kill Emacs? ")
					      (save-buffers-kill-emacs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load required packages and check version numbers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (< emacs-major-version 21)
    (error "This package requires version 21 of Emacs or newer."))

(require 'erc)

(if (string-match "Revision:\\s-+[0-9]\\.\\([0-9]+\\)" erc-version-string)
    (if (< (string-to-int (match-string 1 erc-version-string)) 540)
	(error "This package requires revision 1.540 of ERC or newer!")))

(with-no-warnings
  (require 'cl))
(require 'time-stamp)
(require 'time-date)
(require 'erc-dcc)
(require 'erc-pcomplete)
(require 'erc-truncate)
(require 'erc-ring)
(require 'erc-list)
(require 'erc-fill)
(require 'erc-netsplit)
(require 'erc-track)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <CONFIGURE>
;; Configuration Variables.
;;
;; You _MUST_ change the values of some these variables before starting the bot,
;; especially those that identify the bot, those that specify which networks and
;; channel it should join, and the password hashes.  The default values are
;; already in use for a bot on the freenode network.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-bot-data-directory "~/.ebot"
  "The name of the directory in which all of the bot's data files are stored.")

(defvar my-bot-terminology-database (concat my-bot-data-directory "/termdb.el")
  "The pathname of the terminology database file.")

(defvar my-bot-help-database (concat my-bot-data-directory "/helpdb.el")
  "The pathname of the help database file.")

(defvar my-bot-seen-nicks-database (concat my-bot-data-directory "/seendb.el")
  "The pathname of the seen nicks database file.")

(defvar my-bot-other-database (concat my-bot-data-directory "/otherdb.el")
  "The pathname of the other databases file.")

(defvar my-bot-bin-directory "~/bin"
  "The name of the directory in which all of the bot's helper executables are stored.
The following executables are required for the commands shown in parentheses after each:

	bugtraq (.bugtraq)
	cidr (.netmask)
	google (.google)
	quote (.quote)
	slashnews (.slashnews)
	slashdot (.slashdot)
	rfcindex (.rfcindex)
	stdindex (.stdindex)")

(makunbound 'my-bot-master-password-hash)

;; <CONFIGURE>
(defvar my-bot-master-password-hash "cf418e8cc469cd3297889424fb862272"
  "The MD5 hash of the master password.  This password must be supplied as an
argument with the most dangerous commands.  To set your own master password, evaluate
an Elisp expression of this form:

	(md5 \"my password\")

and insert the result of that expression into the defvar for this variable in
the source code.")

(makunbound 'my-bot-master-password-downcased-hash)

;; <CONFIGURE>
(defvar my-bot-master-password-downcased-hash "cf418e8cc469cd3297889424fb862272"
  "The MD5 hash of the master password (after the password has been downcased).
This variable is used by function my-bot-scrub-password.")

(defvar my-bot-nick (if (getenv "MYBOTTEST") "birnytest" "birny")
  "The nick this bot will use when connecting to IRC.")

;; <CONFIGURE>
(defvar my-bot-fullname "Birny the Bot"
  "The full name this bot will use when connecting to IRC.")

;; <CONFIGURE>
(defvar my-bot-identification '(("freenode" "birny" "nickserv" "identify birny"))
  "A list that controls how the bot identifies itself on various networks.  Each
element of the list has the form:

	(NETWORK BOTNICK NICKSERV IDENTIFY-STRING)

where NETWORK is the name of a network, BOTNICK is the nick of the bot on that
network, NICKSERV is nick that the bot should /MSG with IDENTIFY-STRING in order
to identify itself.")

;; <CONFIGURE>
(defvar my-bot-autoconnections
  `((,(or (getenv "FREENODESERVER") "irc.freenode.net")
     ,(let ((portstring (getenv "FREENODEPORT")))
	(if portstring
	    (string-to-number portstring)
	  6667))
     ,my-bot-nick ,my-bot-fullname ("#tcpip" "#birny" "#emacs" "#sockets")))
  "A list of lists specifying the servers to which this bot should connect when
it starts and the channels on each server the bot should join.  Each element of
the list has the form:

	(SERVERNAME PORTNUM NICK FULLNAME (CHANNELNAME ...))")

(makunbound 'my-bot-admin-nicks)
;; <CONFIGURE>
(defvar my-bot-admin-nicks '("franl" "TheBonsai" "jamesd" "{xmb}" "xmb" "twkm" "usn")
  "A list of nicks allowed to use certain administrative commands.")

(defvar my-bot-auto-voice nil
  "A list of nicks that are auto-voiced when they join a channel that this bot is on.")

(defvar my-bot-log-filename (concat my-bot-data-directory
				    (if (getenv "TEST") "/botlog2" "/botlog"))
  "The name of the file to which log messages are appended.  This file is never
truncated.")

;; <CONFIGURE>
(defvar my-bot-log-channel-traffic '("#tcpip")
  "If non-nil, ordinary channel traffic (i.e., not directed at the bot) is logged in
addition to commands directed at the bot.  If this is a list of strings, each string
names a channel whose traffic will be logged, and traffic in other channels will not
be logged.")

(defvar my-bot-log-irc-traffic nil
  "If non-nil, IRC traffic is logged to the bot's logfile.  If the bot is in one or
two moderately busy channels, this can write a very large amount of data to the log
file.")

(defvar my-bot-catch-errors t
  "If non-nil, the bot will catch errors and say the string stored in
my-bot-caught-error-string into the channel specified by variable
my-bot-caught-error-announce-channel on the network specified by variable
my-bot-caught-error-network.  Otherwise, errors are not caught.  Not catching
errors can lead to the Lisp interpreter exceeding the maximum recursion depth.")

;; <CONFIGURE>
(defvar my-bot-caught-error-announce-channel "#tcpip"
  "The channel where the bot announces caught errors.  See variable my-bot-catch-errors.")

;; <CONFIGURE>
(defvar my-bot-caught-error-announce-network "freenode"
  "The network where the bot announces caught errors.  See variable my-bot-catch-errors.")

;; <CONFIGURE>
(defvar my-bot-caught-error-string "*burp*"
  "What the bot says in-channel when it catches an internal error.  See variable
my-bot-catch-errors.")

(defvar my-bot-gagged nil
  "A list of channel names in which the bot will never say anything in-channel.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <CONFIGURE>
;;
;; There's nothing to configure here.  I just wanted to point out that _nothing_
;; below this comment needs to be changed to configure the bot.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database variables.  Each of these variables is saved to disk.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-bot-terms
  '(("faq" "The TCP faq is at http://www.faqs.org/faqs/internet/tcp-ip/" "franl" (0 0 0)))
  "The bot's knowledge database.  This is initialized at start-up by function
my-bot-load-data.  This is a list where each element has the form:

	(TERM DEFINITION AUTHOR-NICK CREATION-TIME LOOKUP-COUNT)

TERM is a string: the term being defined.  DEFINITION is a string: the
definition of the term.  AUTHOR-NICK is a string: the nick of the person who
created or last changed this entry.  CREATION-TIME is a list of three integers:
the time of creation or last change as returned by function current-time.
LOOKUP-COUNT is an integer: the number of times the term has been looked up by
the bot.")

(put 'my-bot-terms 'needs-saving nil)

(defvar my-bot-help nil
  "An alist mapping help terms to their help strings.")

(put 'my-bot-help 'needs-saving nil)

(defvar my-bot-new-terms nil
  "A list of strings, each of which is a term recently added to the bot's terminology
database.")

(put 'my-bot-new-terms 'needs-saving nil)

(defvar my-bot-channel-topics nil
  "An alist where each element has the form (KEY . TOPIC-STRING), where KEY has the
form (NETWORK-NAME . CHANNEL-NAME).")

(put 'my-bot-channel-topics 'needs-saving nil)

(defvar my-bot-news nil
  "If non-nil, this is a string containing news to be displayed by the .news command.")

(put 'my-bot-news 'needs-saving nil)

(defvar my-bot-seen-nicks nil
  "A list describing nicks that the bot has seen along with info about each.  Each
element of this list has the form:

	(NICK ACTION LASTSEEN LASTSEENCHANNEL)

where NICK is a string, ACTION is a string describing the action the person was
last seen doing, LASTSEEN is the time the nick was last seen in any channel by
the bot, LASTSEENCHANNEL is a string specifying the channel in which the nick
was last seen, FIRSTSEEN is the time the nick was first seen in any channel by
the bot, and FIRSTSEENCHANNEL is a string specifying the channel in which the
nick was first seen.")

(put 'my-bot-seen-nicks 'needs-saving nil)

(defvar my-bot-slashnews-headlines nil
  "A list of the most recent Slashdot headlines.")

(put 'my-bot-slashnews-headlines 'needs-saving nil)

(defvar my-bot-away-messages nil
  "A list of away messages set by users.  Each element has the form:

	(NICK TIME CHANNEL MESSAGE REMINDERTIME)

where NICK is a string of the form \"nick@network-name\".")

(put 'my-bot-away-messages 'needs-saving nil)

(makunbound 'my-bot-hello-regexp)
(defvar my-bot-hello-regexp
  (format "^\\s-*\\(%s[,:]?\\s-+\\)?\\b\\(hello\\|hi\\|howdy\\|y[o0]\\)\\b\\([.!?]\\|,?\\s-+%s[.!?]?\\)?\\s-*$"
	  my-bot-nick my-bot-nick)
  "A regular expression matching channel text to which the bot should react by
saying one of the strings listed in the variable my-bot-hellos.")

(defvar my-bot-hellos '("Hi" "Howdy" "Hi there" "Yo" "Word" "Bonjour" "Guten Tag" "Cheers"
			"You da man" "Whaaaaazzzzzup" "Shalom" "Hallo" "Hola!" "Salut"
			"Hej" "Props" "Konichiwa")
  "Ways to say hello to people.")

(makunbound 'my-bot-goodbye-regexp)
(defvar my-bot-goodbye-regexp
  (format "^\\s-*\\(%s[,:]?\\s-+\\)?\\b\\(bye\\|goodbye\\|goodnight\\|g'night\\|g'nite\\|night\\|nite\\|later\\|l8r\\|so long\\|see ya\\)\\b\\([.!?]\\|,?\\s-+%s[.!?]?\\)?\\s-*$"
	  my-bot-nick my-bot-nick)
  "A regular expression matching channel text to which the bot should react by
saying one of the strings listed in the variable my-bot-goodbyes.")

(defvar my-bot-goodbyes '("Bye" "Goodbye" "Later" "See ya" "cul8r" "Adios" "Auf Wiedersehen"
			  "Cheers" "Shalom" "Au revoir" "Arrivederci" "Adeus")
  "Ways to say goodbye to people.")

(defvar my-bot-no-privilege-msgs
  '("Insufficient privilege, dude."
    "I'm sorry, Dave, but I'm afraid I can't do that."
    "Red Alert!  Red Alert!  Privilege violation!"
    "What?  Can you repeat that.  I was sleeping."
    "Now wait just a cotton-pickin' minute."
    "Sorry, your privilege level is too low."
    "Oh oh.  This will go down on your Permanent Record."
    "franl!  franl!  Someone's telling me what to do.  Make them stop!"
    "That's a doozie!"
    "Abort, Retry, Fail?"
    "I smell smoke.  Holy crap!  I'm on fire!"
    "wtf?"
    "Oh no you don't!"
    "Um, let's not and say we did.")
  "A list of strings, one of which is randomly chosen to tell someone that they lack
privilege to perform some action.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of database variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-bot-burp-count 0
  "The number of internal errors the bot has caught.")

(defvar my-bot-start-time '(0 0 0)
  "The time the bot was started.")

(defvar my-bot-last-say-time '(0 0 0)
  "The time that the bot last said anything (either in-channel or via /notice).")

(defvar my-bot-last-update-time nil
  "The time that the .update command was last used.  If nil, the .update command
has never been used.")

(defvar my-bot-quote-timer nil
  "The timer that makes the bot spout a pithy quote periodically.")

(defvar my-bot-save-databases-timer nil
  "The timer that makes the bot save its databases periodically.")

(defvar my-bot-rfc-update-timer nil
  "The times that makes the bot update the RFC index.")

(defvar my-bot-slashnews-timer nil
  "The timer that tracks Slashdot headlines.")

(defvar my-bot-ping-timer nil
  "The timer that makes the bot ping each IRC server periodically.")

(defvar my-bot-channel-traffic-times nil
  "...")

(make-variable-buffer-local 'my-bot-channel-traffic-times)

(defvar my-bot-log-buffer-name "*Bot-Log*"
  "The name of the log buffer.")

(defvar my-bot-inhibit-logging nil
  "If non-nil, my-bot-log does nothing.")

(defvar my-bot-commands nil
  "A list of strings, each of which is a command word to which the bot will react.  When
you write a function named my-bot-cmd-XXX, you must add the string \".XXX\" to this list,
or else the command will not be recognized.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Silence some byte-compiler warnings.
(defvar erc-enable-logging)
(defvar erc-echo-notices-in-minibuffer-flag)
(defvar erc-log-channels-directory)
(defvar erc-notify-interval)
(defvar erc-notify-list)

(setq erc-auto-query			nil
      erc-auto-reconnect		nil
      erc-button-buttonize-nicks	nil
      erc-common-server-suffixes	nil
      erc-dcc-get-default-directory	"/tmp/"
      erc-default-port			6667 ;; Windows doesn't grok the service name "ircd".
      erc-echo-notices-in-minibuffer-flag nil
      erc-enable-logging		nil
      erc-log-channels-directory	nil
      erc-email-userid			"Birny"
      erc-fill-function			#'erc-fill-variable
      erc-fill-prefix			"   "
      erc-flood-protect			nil
      erc-header-line-format		nil
      erc-interpret-controls-p		nil
      erc-join-buffer			nil
      erc-keywords			nil
      erc-kill-buffer-on-part		t
      erc-max-buffer-size		50000
      erc-minibuffer-notice		nil
      erc-notify-interval		30
      erc-notify-list			nil
      erc-paranoid			t
      erc-pcomplete-nick-postfix	", "
      erc-prompt			"ERC> "
      erc-reuse-buffers			t)

(when window-system
  (setcdr (assoc 'height initial-frame-alist) 30)
  (setcdr (assoc 'top initial-frame-alist) 0)
  (setcdr (assoc 'left initial-frame-alist) 16))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice erc-server-send (before my-ad-before-erc-server-sendactivate)
  "Logs to the bot's logfile all commands sent to the IRC server."
  (if my-bot-log-irc-traffic
      (my-bot-log "Sending to %s: \"%s\"" (buffer-name (erc-server-buffer)) (ad-get-arg 0))))

;; TODO: Fix this ...
(defvar my-bot-irc-input ""
  "...")

(defadvice erc-process-filter (before my-ad-before-erc-process-filter activate)
  "Logs to the bot's logfile all commands received from the IRC server."
  (when my-bot-log-irc-traffic
    (let ((string (ad-get-arg 1)))
      (setq my-bot-irc-input (concat my-bot-irc-input string))
      (while (string-match "^\\([^\n]*\n\\)" my-bot-irc-input)
	(let ((line (match-string 1 my-bot-irc-input)))
	  (if (> (length my-bot-irc-input) (length line))
	      (setq my-bot-irc-input (substring my-bot-irc-input (length line)))
	    (setq my-bot-irc-input ""))
	  (my-bot-log "Received from %s: \"%s\""
		      (buffer-name (process-buffer (ad-get-arg 0)))
		      (delete ?\n line)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-remove-crs ()
  "Removes all CR (^M) characters from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\C-m" nil t)
      (replace-match "" nil t))))

(defun my-command (shellcmd &optional inputstr)
  "Execute SHELLCMD with INPUTSTR as standard input.  If INPUTSTR is nil, it is
taken to be the empty string.  Returns the command's output as a string."
  (let (p1 p2)
    (save-excursion
      (set-buffer (get-buffer-create " *my-command output*"))
      (if (not (string= (buffer-name) " *my-command output*"))
	  (error "my-command: cannot create temporary buffer!"))
      (erase-buffer)
      (setq p1 (point))
      (insert (or inputstr ""))
      ;; Silence call to push-mark on line 1443 of simple.el.
      (cl-letf (((symbol-function 'push-mark) #'ignore))
	(condition-case nil
	    (shell-command-on-region p1
				     (setq p2 (point-max))
				     shellcmd
				     'insert	;; Insert output in this buffer.
				     'replace)	;; Replace region with output.
	  (error "")))
      (my-remove-crs)
      (prog1
	  (buffer-substring p2 (point-max))
	(kill-buffer (current-buffer))))))


(defun my-bot-log (&rest args)
  "Writes a message to the bot log.  ARGS are the same as for function format."
  (when (null my-bot-inhibit-logging)
    (let ((msg (apply 'format args))
	  (timestamp (concat (time-stamp-yyyy/mm/dd) " "
			     (time-stamp-hh:mm:ss)))
	  (origwin (selected-window)))

      ;; Maybe rotate the botlog.
      (let ((stats (file-attributes my-bot-log-filename))
	    (coding-system-for-write 'latin-1-unix))
	(when (> (nth 7 stats) 500000)
	  (write-region (concat timestamp " Rotating log file ...\n") nil my-bot-log-filename
				'append 'nomessage)
	  (rename-file my-bot-log-filename (concat my-bot-log-filename ".bak") 'overwrite)))

      ;; Remove face properties and trailing newlines.
      (remove-text-properties 0 (length msg) '(face nil) msg)
      (while (= ?\n (aref msg (1- (length msg))))
	(setq msg (substring msg 0 -1)))

      ;; Scrub out the master password.
      (setq msg (my-bot-scrub-password msg my-bot-master-password-downcased-hash))

      (setq msg (concat timestamp " " msg))

      (with-current-buffer (get-buffer-create my-bot-log-buffer-name)
	(goto-char (point-max))
	(insert msg "\n")
	(if (getenv "BATCH")
	    (message msg))
	(let ((coding-system-for-write 'latin-1-unix))
	  (write-region (concat msg "\n") nil my-bot-log-filename 'append 'nomessage))))))

(defun my-bot-show-log ()
  "Displays the log buffer."
  (switch-to-buffer (get-buffer-create my-bot-log-buffer-name)))

(defun my-bot-no-privilege-msg ()
  "Returns a random 'insufficient privilege' message."
  (my-bot-random my-bot-no-privilege-msgs))

(defun my-bot-voice-p (nick)
  "Returns non-nil if NICK has voice in the channel associated with the current
buffer, otherwise returns nil.  If the current buffer is not an ERC channel
buffer, returns nil."
  (if (or (not (eq major-mode 'erc-mode))
	  (null (erc-default-target))
	  (not (erc-channel-p (erc-default-target))))
      nil
    (let ((info (erc-get-channel-user nick)))
      (nth 2 info))))

(defun my-bot-op-p (nick channel-buffer)
  "Returns non-nil if NICK is an op in CHANNEL-BUFFER.  otherwise returns nil.
If CHANNEL-BUFFER is not an ERC channel buffer, returns nil."
  (if (or (not (bufferp channel-buffer))
	  (not (erc-channel-p (buffer-name channel-buffer))))
      nil
    (with-current-buffer channel-buffer
      (erc-channel-user-op-p nick))))

(defun my-bot-random (list)
  "Return a random element of LIST."
  (if list
      (nth (random (length list)) list)))

(defun my-bot-say (channel-buffer nick &rest args)
  "If CHANNEL-BUFFER is non-nil, say text into CHANNEL-BUFFER, otherwise /msg
NICK with text.  The text is computed by passing ARGS to function format.  Log
any action."
  (if (= 1 (length args))
      (setq args (list (my-bot-escape-% (car args)))))

  (let* ((text (apply 'format args))
	 (channel-name (if channel-buffer
			   (with-current-buffer channel-buffer
			     (erc-default-target)))))
    (if (string= text "")
	(setq text " "))

    (if (< (time-to-seconds (time-since my-bot-last-say-time)) 3)
	;; This keeps the bot from being kicked for flooding.
	(sleep-for 3))

    ;; If both channel-buffer and nick are nil, we say nothing to no one.

    (if channel-buffer
	(with-current-buffer channel-buffer
	  (my-bot-log "%s: %s" (format "-> %s" channel-name) text)
	  (if (not (member-ignore-case channel-name my-bot-gagged))
	      (erc-send-message text)))
      (if nick
	  (progn
	    (my-bot-log "%s: %s" (format "-> %s" nick) text)
	    (erc-cmd-NOTICE nick text))))

    (if (or channel-buffer nick)
	(setq my-bot-last-say-time (current-time)))))

(defun my-bot-save-terms ()
  "Saves the terminology database."
  (if (not (get 'my-bot-terms 'needs-saving))
      (my-bot-log "Terminology database does not need to be saved.")

    (put 'my-bot-terms 'needs-saving nil)
    (my-bot-log "Saving terminology database ...")
    (if (file-exists-p my-bot-terminology-database)
	(rename-file my-bot-terminology-database (concat my-bot-terminology-database ".bak")
		     'overwrite))
    (let ((standard-output '(lambda (str) nil))
	  (coding-system-for-write 'latin-1-unix))
      (write-region (concat (pp `(setq my-bot-terms ',my-bot-terms)) "\n\n"
			    (pp `(setq my-bot-new-terms ',my-bot-new-terms)))
		    nil my-bot-terminology-database nil 'nomessage))))

(defun my-bot-save-seen-nicks ()
  "Saves the seen nicks database."
  (if (not (get 'my-bot-seen-nicks 'needs-saving))
      (my-bot-log "Seen nicks database does not need to be saved.")

    (put 'my-bot-seen-nicks 'needs-saving nil)
    (my-bot-log "Saving seen nicks ...")
    (if (> (length my-bot-seen-nicks) 10000)
	(setq my-bot-seen-nicks (subseq my-bot-seen-nicks 0 10000)))
    (if (file-exists-p my-bot-seen-nicks-database)
	(rename-file my-bot-seen-nicks-database (concat my-bot-seen-nicks-database ".bak")
		     'overwrite))
    (let ((standard-output '(lambda (str) nil))
	  (coding-system-for-write 'latin-1-unix))
      (write-region (prin1-to-string `(setq my-bot-seen-nicks ',my-bot-seen-nicks))
		    nil my-bot-seen-nicks-database nil 'nomessage))
    (my-bot-log "Saved seen nicks.")))

(defun my-bot-save-help ()
  "Saves the help database."
  (if (not (get 'my-bot-help 'needs-saving))
      (my-bot-log "Help database does not need to be saved.")

    (put 'my-bot-help 'needs-saving nil)
    (my-bot-log "Saving help database ...")
    (if (file-exists-p my-bot-help-database)
	(rename-file my-bot-help-database (concat my-bot-help-database ".bak") 'overwrite))
    (let ((standard-output '(lambda (str) nil))
	  (coding-system-for-write 'latin-1-unix))
      (write-region (pp `(setq my-bot-help ',my-bot-help))
		    nil my-bot-help-database nil 'nomessage))))

(defun my-bot-save-other ()
  "Saves the other databases."
  (if (not (or (get 'my-bot-news 'needs-saving)
	       (get 'my-bot-channel-topics 'needs-saving)
	       (get 'my-bot-away-messages 'needs-saving)
	       (get 'my-bot-slashnews-headlines 'needs-saving)))
      (my-bot-log "Other databases do not need to be saved.")

    (put 'my-bot-news 'needs-saving nil)
    (put 'my-bot-channel-topics 'needs-saving nil)
    (put 'my-bot-away-messages 'needs-saving nil)
    (put 'my-bot-slashnews-headlines 'needs-saving nil)
    (if (file-exists-p my-bot-other-database)
	(rename-file my-bot-other-database (concat my-bot-other-database ".bak") 'overwrite))
    (let ((standard-output '(lambda (str) nil))
	  (coding-system-for-write 'latin-1-unix))
      (write-region (concat (pp `(setq my-bot-news ',my-bot-news)) "\n\n"
			    (pp `(setq my-bot-channel-topics ',my-bot-channel-topics)) "\n\n"
			    (pp `(setq my-bot-away-messages ',my-bot-away-messages)) "\n\n"
			    (pp `(setq my-bot-slashnews-headlines ',my-bot-slashnews-headlines)))
		    nil my-bot-other-database nil 'nomessage))))

(defun my-bot-save-all-databases ()
  "Saves all databases."
  (my-bot-log "Saving all databases ...")
  (my-bot-save-terms)
  (my-bot-save-help)
  (my-bot-save-seen-nicks)
  (my-bot-save-other))

(defun my-bot-load-databases ()
  "Loads the database from disk."
  (my-bot-log "Loading databases ...")
  (dolist (file (list my-bot-terminology-database
		      my-bot-help-database
		      my-bot-seen-nicks-database
		      my-bot-other-database))
    (if (not (file-readable-p file))
	(my-bot-log "ERROR: %s is unreadable (or not there)!" file)
      (load file 'noerror 'nomessage 'nosuffix)
      (my-bot-log "%s loaded." file))))

(defun my-bot-lookup (term &optional countit noalias)
  "Looks up TERM in the database.  If TERM is found, returns the database entry.
See documentation for variable my-bot-terms for the format of the entry.  If
optional argument COUNTIT is non-nil, the lookup count of the entry is
incremented."
  (let ((entry (assoc-ignore-case term my-bot-terms)))
    (if entry
	(if (and (null noalias)
		 (stringp (nth 1 entry))
		 (= ?% (aref (nth 1 entry) 0)))
	    (my-bot-lookup (substring (nth 1 entry) 1) nil 'noalias)
	  (if countit
	      (setf (nth 4 entry) (1+ (nth 4 entry))))
	  entry))))

(defun my-bot-lookup-help (term &optional noalias)
  "Returns the help string for TERM, which is either a command name minus the leading '.'
or a help topic in the help database (my-bot-help).  If no help is found, returns nil.
If optional argument NOALIAS is non-nil, aliases are disabled."
  (let* ((funcsym (intern-soft (concat "my-bot-cmd-" term)))
	 (alias (when (and (fboundp funcsym)
			   (symbolp (symbol-function funcsym)))
		  (setq funcsym (symbol-function funcsym))
		  (if (string-match "^.*-\\([^-]+\\)$" (symbol-name funcsym))
		      (match-string 1 (symbol-name funcsym)))))
	 (cmdhelp (get funcsym 'help))
	 (usage (my-bot-lookup-usage term)))
    (if cmdhelp
	(progn
	  (if alias
	      (setq cmdhelp (concat "." term " is an alias for '." alias "'.  " cmdhelp)))
	  (if usage
	      (concat cmdhelp "  " usage)
	    cmdhelp))

      ;; Lookup a help topic in my-bot-help.
      (let ((entry (assoc-ignore-case term my-bot-help)))
	(if entry
	    (if (and (null noalias)
		     (= ?% (aref (cdr entry) 0)))
		(my-bot-lookup-help (substring (cdr entry) 1) 'noalias)
	      (cdr entry)))))))

(defun my-bot-lookup-usage (command)
  "Returns the usage string for COMMAND, which is a string naming a command
without the leading dot.  Returns nil if COMMAND is not a valid bot command or
if it has no usage string defined."
  (let* ((funcsym (intern-soft (concat "my-bot-cmd-" command)))
	 (dummy (if (and (fboundp funcsym)
			 (symbolp (symbol-function funcsym)))
		    (setq funcsym (symbol-function funcsym))))
	 (usage (get funcsym 'usage)))
    (or usage
	"There is no usage string defined for this command.  Please tell franl.")))

(defun my-bot-scrubbed-data ()
  "Returns a copy of my-bot-terms with all entries whose cars start with \"%\" or
\".help\" removed."
  (let (result)
    (dolist (element my-bot-terms)
      (if (not (= ?% (aref (car element) 0)))
	  (add-to-list 'result element)))
    result))

(defun my-bot-escape-% (str)
  "Replaces all occurances of '%' with '%%' in STR.  STR must not contain any ^P
characters."
  (let ((result str))
    (while (string-match "%" result)
      (setq result (replace-match "\C-p" nil nil result)))
    (while (string-match "\C-p" result)
      (setq result (replace-match "%%" nil nil result)))
    result))

(defun my-bot-scrub-password (string password-hash)
  "Replaces all occurrances of words in STRING whose MD5 hash matches
PASSWORD-HASH with \"XXXX\".  When specifying PASSWORD-HASH, keep in mind that
Each word in STRING is downcased before being hashed."
  (let ((words (split-string string "[] \f\t\n\r\v'\"/?><.,:=[-]+")))
    (dolist (word words)
      (if (string= (md5 (downcase word) nil nil nil 'noerror) password-hash)
	  (if (string-match word string)
	      (setq string (replace-match "XXXX" nil nil string)))))
    string))

(defun my-bot-privcheck (nick channelbuf)
  "Returns t if NICK is a member of the list my-bot-admin-nicks, otherwise uses
my-bot-say to give the user an privilege violation message."
  (if (not (member-ignore-case nick my-bot-admin-nicks))
      (progn
	(my-bot-say channelbuf nick (my-bot-no-privilege-msg))
	nil)
    t))

(defun my-bot-check-password (password nick channelbuf)
  "Returns non-nil if the MD5 hash of PASSWORD matches the hash stored in
my-bot-master-password-hash.  Otherwise, outputs a privilege violation message
and returns nil."
  (if (string= (md5 password nil nil nil 'noerror) my-bot-master-password-hash)
      t
    (my-bot-say channelbuf nick (my-bot-no-privilege-msg))
    nil))

(defun my-bot-lookup-away-message (nick)
  "Returns an element from the list stored in variable my-bot-away-messages
corresponding to NICK.  You must use this function to lookup nicks in
my-bot-away-messages, because each server buffer has its own copy of this
variable."
  (assoc-ignore-case (concat nick "@" (erc-network-name)) my-bot-away-messages))

(defun my-bot-update-seen-nicks (nick channel action)
  "Updates my-bot-seen-nicks."
  (let ((entry (assoc-ignore-case nick my-bot-seen-nicks))
	found)
    (if (null entry)
	;; New nick.
	(progn
;;	  (if (and (string= channel "#tcpip")
;;		   (string= action "joining"))
;;	      (erc-cmd-MSG (format "franl %s has just joined #tcpip. (paging)" nick)))
	  (add-to-list 'my-bot-seen-nicks
		       (list nick action (current-time) channel)))
      (setcdr entry (list action (current-time) channel))))
  (put 'my-bot-seen-nicks 'needs-saving t))

(defun my-bot-duration-to-string (time)
  "Converts an Emacs time value (as returned by function time-since) into
English."
  (let (days hours minutes seconds format-args output)
    (setq seconds	(truncate (float-time time))
	  days		(/ seconds 86400)
	  seconds	(% seconds 86400)
	  hours		(/ seconds 3600)
	  seconds	(% seconds 3600)
	  minutes	(/ seconds 60)
	  seconds	(% seconds 60)
	  format-args	(if (> days 0)
			    `("%d days, %d hours, %d minutes, %d seconds"
			      ,days ,hours ,minutes ,seconds)
			  (if (> hours 0)
			      `("%d hours, %d minutes, %d seconds" ,hours ,minutes ,seconds)
			    (if (> minutes 0)
				`("%d minutes, %d seconds" ,minutes ,seconds)
			      `("%d seconds" ,seconds))))
	  output	(apply 'format format-args))
    ;; Change all "1 units" to "1 unit".
    (while (string-match "\\([^0-9]\\|^\\)1 \\S-+\\(s\\)" output)
      (setq output (replace-match "" nil nil output 2)))
    output))

(defun my-bot-get-erc-buffer (network channel)
  "Returns the ERC buffer object corresponding to CHANNEL on NETWORK (both
arguments are strings).  Returns nil, if the bot is not a member of the
specified channel.  If CHANNEL is the empty string, returns the server buffer
for the given network (or nil if the bot is not connected to that network)."
  (block nil
    (dolist (buffer (buffer-list) nil)
      (with-current-buffer buffer
	(if (and (eq major-mode 'erc-mode)
		 (string= network (erc-network-name))
		 (string= (downcase channel)
			  (downcase (or (erc-default-target) ""))))
	    (return buffer))))))

(defun my-bot-restart ()
  "Restarts the bot."
  (my-bot-save-all-databases)

  (my-bot-log "Spawning new bot ...")
  (shell-command (format "%s/ebot --daemonize" my-bot-bin-directory))

  (my-bot-log "*** TERMINATING IN 2 SECONDS ***")
  (run-at-time 2 nil 'kill-emacs))

(defun my-bot-log-input (input channel-buffer nick)
  "Logs input to which the bot is about to respond."
  (if (string-match "^\\.anon\\b" input)
      (my-bot-log "%s: %s" "<- ANON" input)
    (if channel-buffer
	;; The input came from a channel.
	(let ((channel-name (with-current-buffer channel-buffer (erc-default-target))))
	  (my-bot-log "%s: <%s> %s" (format "<- %s" channel-name) nick input))
      ;; The input came via /msg.
      (my-bot-log "%s: %s" (format "<- %s (%s)" nick (erc-network-name)) input))))

(defun my-bot-strip-first-word (string)
  "Returns a copy of STRING with the first word (and all surrounding whitespace)
removed.  If STRING contains only one word, returns the empty string.  A word is
considered to be a contiguous set of non-whitespace characters."
  (if (string-match "^\\s-*\\S-+\\s-+\\(\\S-.*\\)$" string)
      (match-string 1 string)
    ""))

(defun my-bot-set-network-name ()
  "If the network name is not set for the connection associated with the current
buffer, sets the network name based on the server name.  Returns nil if the current buffer
is not an ERC buffer or if the network name was already set, non-nil otherwise."
  (if (eq major-mode 'erc-mode)
      (with-current-buffer (erc-server-buffer)
	(if (null erc-network)
	    (if (string-match "\\bfreenode\\b" erc-session-server)
		(setq erc-network 'freenode))))))

(defun my-bot-identify ()
  "Identifies if necessary."
  (let ((identry (assoc-ignore-case (erc-network-name) my-bot-identification)))
    (when (and identry
	       (string= (downcase my-bot-nick) (downcase (nth 1 identry))))
      (my-bot-log "Identifying with %s ..." (nth 2 identry))
      (erc-cmd-MSG (concat (nth 2 identry) " " (nth 3 identry))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions called from timers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun my-bot-save-databases-timer-function ()
  "This function is called periodically by the timer stored in variable
my-bot-save-databases-timer."
  (my-bot-save-all-databases))

(defun my-bot-quote-timer-function ()
  "This function is called periodically by the timer stored in variable
my-bot-quote-timer."
  (let ((quotechannels '(("freenode" . "#tcpip")))
	chanbuf)
    (dolist (channelspec quotechannels)
      (setq chanbuf (my-bot-get-erc-buffer (car channelspec) (cdr channelspec)))
      (if chanbuf
	  (with-current-buffer chanbuf
	    ;; Only spout a quotation if there's been sufficient recent activity
	    ;; in this channel.
	    (if (and (>= (length my-bot-channel-traffic-times) 2)
		     (<= (float-time (time-since (nth 1 my-bot-channel-traffic-times)))
			 120))
		;; More than 4 in-channel PRIVMSGs in the last 2 minutes.
		(my-bot-say chanbuf "franl" (my-command "~/bin/quote"))))))))

;; ERC does this now.  See erc-send-ping-interval.
;;
;;(defun my-bot-ping-timer-function ()
;;  "This function is called periodically by the timer stored in variable
;;my-bot-ping-timer."
;;  (let ((serverbufs (erc-buffer-list 'erc-server-buffer-p)))
;;    (dolist (serverbuf serverbufs)
;;      (with-current-buffer serverbuf
;;	(erc-cmd-PING my-bot-nick)))))

(defun my-bot-slashnews-timer-function ()
  "This function is called periodically by the timer stored in variable
my-bot-slashnews-timer."
  (let ((newschannels nil)	;; '(("freenode" . "#tcpip"))
	(headlines (split-string (my-command (format "%s/slashnews 2>/dev/null"
						     my-bot-bin-directory))
				 "\n"))
	newheadlines count chanbuf)
    (when headlines
      (dolist (headline headlines)
	(when (not (member-ignore-case headline my-bot-slashnews-headlines))
	  (add-to-list 'my-bot-slashnews-headlines headline)
	  (add-to-list 'newheadlines headline 'append))))
    
    ;; Keep only the 30 most-recent headlines.
    (if (> (length my-bot-slashnews-headlines) 30)
	(setcdr (nthcdr 29 my-bot-slashnews-headlines) nil))

    (when newheadlines
      (my-bot-log "Processing %d new Slashdot headlines." (length newheadlines))
      (my-bot-save-other)
      (dolist (channelspec newschannels)
	(setq chanbuf (my-bot-get-erc-buffer (car channelspec) (cdr channelspec)))
	(if chanbuf
	    (with-current-buffer chanbuf
	      (my-bot-say chanbuf "franl" "This just in at Slashdot:")
	      (setq count 0)
	      (block nil
		(dolist (newheadline newheadlines)
		  (incf count)
		  (if (> count 5)
		      (return))
		  (my-bot-say chanbuf "franl" newheadline)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-bot-erc-server-PRIVMSG-function (proc parsed)
  "Added to erc-server-PRIVMSG-hook.  Makes private messages appear in the active
window.  This hook gets called for all PRIVMSGs!  If the message is addressed to a
channel, this function _must_ return nil so that erc-server-PRIVMSG-or-NOTICE runs,
which causes ordinary channel messages to appear."
  (eval
   `(condition-case my-bot-condition-var

      ;; This dflet prevents function message from writing to the echo area,
      ;; which helps keep data from piling up in screen's pty buffer when I'm
      ;; detached from the screen session.
      (dflet ((message (&rest args) (apply 'format args)))
	(let* ((activebuf (window-buffer (selected-window)))
	       (target (car (erc-response.command-args parsed)))
	       (nick (car (erc-parse-user (erc-response.sender parsed))))
	       (network (erc-network-name))
	       (text (erc-response.contents parsed))
	       channel-buffer)

	  ;; At this time, the current buffer is the server buffer.

	  (if (erc-channel-p target)
	      ;; It's a PRIVMSG to a channel that this bot is in.
	      (progn
		;; Need this to cope with the fact some some IRC servers reports
		;; channel names with uppercase letters in them
		;; (e.g. "#Sockets"), which can confuse birny's
		;; channel-name-to-buffer mapping code.
		(setq target (downcase target))

		(my-bot-update-seen-nicks nick target "talking in")

		(setq channel-buffer (my-bot-get-erc-buffer (erc-network-name) target))
						
		(if (and my-bot-log-channel-traffic
			 (or (not (listp my-bot-log-channel-traffic))
			     (member-ignore-case target my-bot-log-channel-traffic)))
		    (progn
		      (if (string-match "^\C-aACTION \\(.*\\)\C-a$" text)
			  (my-bot-log "%s: * %s %s" target nick (match-string 1 text))
			(my-bot-log "%s: <%s> %s" target nick text))))

		(with-current-buffer channel-buffer
		  (add-to-list 'my-bot-channel-traffic-times (current-time))
		  ;; Keep only the 30 most-recent elements on the list.
		  (if (> (length my-bot-channel-traffic-times) 30)
		      (setcdr (nthcdr 29 my-bot-channel-traffic-times) nil)))

		;; All channel PRIVMSGs are passed to my-bot-handle-input, even
		;; if they are not directed at this bot.
		(my-bot-handle-input channel-buffer target nick text)
		  
		;; Return nil to let other ERC hooks process it.
		nil)

	    ;; It's a PRIVMSG to the bot.  Do notices come through this code?
	    (if (string= nick my-bot-nick)
		;; Don't let the bot /MSG itself.
		t

	      (if (string-match "\C-a" text)
		  ;; It's a CTCP message.
		  (progn
		    (if (not (string-match "^\C-aPING " text))
			;; Don't log PINGs.
			(my-bot-log "CTCP: '%s'" text))

		    ;; Return nil to let other ERC hooks process it.
		    nil)

		;; It's a /MSG to the bot ...
		(my-bot-update-seen-nicks nick "" "/MSG'ing me")
		(my-bot-handle-input nil nil nick text)
		t)))))

    (,(if my-bot-catch-errors 'error 'my-bot-xyzzy-not-a-match)
     (ignore-errors
       (incf my-bot-burp-count)
       (my-bot-say (my-bot-get-erc-buffer my-bot-caught-error-announce-network
					  my-bot-caught-error-announce-channel)
		   ":nosuchnick:" my-bot-caught-error-string)
       ;; Should this be nil instead?
       t)))))

;; I append the above hook function to erc-server-PRIVMSG-hook so that the bot's
;; responses appear after the commands in the bot's own channel buffers.

(add-hook 'erc-server-PRIVMSG-functions 'my-bot-erc-server-PRIVMSG-function 'append)

(defun my-bot-erc-after-server-JOIN-function (proc parsed)
  "Added to the _end_ of erc-server-JOIN-hook.  When this function runs, the
channel buffer has been created and is the current buffer.  This hook must be
appended to the hook variable, so that it runs after the builtin hook.
Remember, this hook is called when any user joins a channel, not just me."
  (let* ((sndr (erc-parse-user (erc-response.sender parsed)))
	 (channel-name (erc-response.contents parsed))
	 (nick (car sndr)))
    (if (string= (erc-current-nick) nick)
	;; The bot just joined a channel, and all the builtin JOIN hooks have run.
	(run-at-time 0.5 nil '(lambda ()
				(delete-other-windows)
				(list-buffers)
				(ignore-errors (delete-window))))

      ;; Someone else just joined.
      (if (and my-bot-log-channel-traffic
	       (or (not (listp my-bot-log-channel-traffic))
		   (member-ignore-case channel-name my-bot-log-channel-traffic)))
	  (my-bot-log "%s has joined %s." nick channel-name))

      (my-bot-update-seen-nicks nick channel-name "joining")

      ;; If the person who just joined has set an away message, remind them
      ;; about it (but no more often than once every 3 hours (in case netsplits
      ;; make the person appear to be joining frequently).
      (let ((away-entry (my-bot-lookup-away-message nick)))
	(when (and away-entry
		   (nth 4 away-entry)
		   (> (float-time (time-since (nth 4 away-entry))) 10800))
	  (setcar (nthcdr 4 away-entry) (current-time))
	  (run-at-time 60 nil
		       `(lambda ()
			  (with-current-buffer ,(current-buffer)  ;; Selects the right network.
			    (if (my-bot-lookup-away-message ,nick)
				(my-bot-say nil ,nick "%s, your away message is set." ,nick)))))))

;;      ;; Welcome people to channel #tcpip on the freenode network.
;;      (if (and (string= channel-name "#tcpip")
;;	       (string= (erc-network-name) "freenode")
;;	       (not (string= nick "ChanServ")))
;;	  (run-at-time 10 nil
;;		       `(lambda ()
;;			  (with-current-buffer ,(current-buffer)  ;; Selects the right network.
;;			    (my-bot-say ,(my-bot-get-erc-buffer "freenode" "#tcpip") "franl"
;;			    	      (if (string= ,nick "TheBonsai")
;;			    		  "Willkommen, Herr Unteroffizier TheBonsai."
;;			    		"Welcome, %s.")
;;			      	      ,nick)))))

      (let (auto-voice-buffer)
	(if (and (string= "#tcpip" channel-name)
		 (string= "freenode" (downcase (erc-network-name)))
		 (my-bot-op-p my-bot-nick
			      (setq auto-voice-buffer (my-bot-get-erc-buffer "freenode" "#tcpip")))
		 (member-ignore-case nick my-bot-auto-voice))
	    (run-at-time 2 nil `(lambda ()
				  (my-bot-log "Voicing %s in %s." ,nick ,channel-name)
				  (with-current-buffer ,auto-voice-buffer
				    (erc-server-send (format "MODE %s +v %s"
							      (erc-default-target)
							      ,nick)))))))))
  nil)

(add-hook 'erc-server-JOIN-functions 'my-bot-erc-after-server-JOIN-function 'append)

(defun my-bot-erc-server-PART-function (proc parsed)
  "Added to erc-server-PART-hook."
  (let* ((channel-name (car (erc-response.command-args parsed)))
	 (reason (erc-response.contents parsed))
	 (sndr (erc-parse-user (erc-response.sender parsed)))
	 (nick (nth 0 sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr)))
    (my-bot-update-seen-nicks nick channel-name "leaving")

    (if (and my-bot-log-channel-traffic
	     (or (not (listp my-bot-log-channel-traffic))
		 (member-ignore-case channel-name my-bot-log-channel-traffic)))
	(my-bot-log "%s has left channel %s: %s" nick channel-name reason)))
  nil)

(add-hook 'erc-server-PART-functions 'my-bot-erc-server-PART-function)
	 
(defun my-bot-erc-server-QUIT-function (proc parsed)
  "Added to erc-server-QUIT-hook."
  (let* ((channel-name (car (erc-response.command-args parsed)))
	 (reason (erc-trim-string (erc-response.contents parsed)))
	 (sndr (erc-parse-user (erc-response.sender parsed)))
	 (nick (car sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr)))
    (my-bot-update-seen-nicks nick "" "quitting"))
  nil)

(add-hook 'erc-server-QUIT-functions 'my-bot-erc-server-QUIT-function)
	 
(defun my-bot-erc-server-NICK-function (proc parsed)
  "Added to erc-server-NICK-hook."
  (let* ((newnick (car (erc-response.command-args parsed)))
	 (sndr (erc-parse-user (erc-response.sender parsed)))
	 (nick (car sndr)))
    (my-bot-update-seen-nicks nick "" (format "changing nick to %s" newnick))
    (my-bot-update-seen-nicks newnick "" (format "changing nick from %s" nick)))
  nil)

(add-hook 'erc-server-NICK-functions 'my-bot-erc-server-NICK-function)

(defun my-bot-erc-server-TOPIC-function (proc parsed)
  "Added to erc-server-TOPIC-hook."
  (let* ((channel-name (car (erc-response.command-args parsed)))
	 (new-topic (erc-trim-string (erc-response.contents parsed)))
	 (sndr (erc-parse-user (erc-response.sender parsed)))
	 (nick (car sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr)))
    (my-bot-log "%s changes the topic in %s to \"%s\"." nick channel-name new-topic)))

(add-hook 'erc-server-TOPIC-functions 'my-bot-erc-server-TOPIC-function)

(defun my-bot-erc-disconnected-hook (nick ip reason)
  "Added to erc-disconnected-hook."
  (if quitting
      ;; The bot disconnected due to a /QUIT.
      (my-bot-log "Disconnected from %s due to /QUIT." (erc-network-name))

    ;; The bot disconnected unexpectedly!
    (my-bot-log "Disconnected from %s unexpectedly!" (erc-network-name))
    (if (memq t (mapcar '(lambda (buf) (with-current-buffer buf erc-connected))
			(erc-buffer-list 'erc-server-buffer-p)))
	;; BUG: The bot should reconnect.
	(my-bot-log "Not terminating because of remaining server connections.")
      (my-bot-restart))))

(add-hook 'erc-disconnected-hook 'my-bot-erc-disconnected-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-bot-handle-input (channel-buffer channel-name nick input)
  "Processes input to the bot, dispatching commands to the command functions
named my-bot-cmd-* (above) or looking up terms in the database.  CHANNEL-BUFFER
and CHANNEL-NAME are both nil if INPUT arrived via /msg, otherwise
CHANNEL-BUFFER is a buffer object and CHANNEL-NAME is a string, each of which
specify the channel in which INPUT appeared.  NICK is the nick of the user who
originated the INPUT."

  ;; Trim leading and trailing whitespace from INPUT.
  (setq input (erc-trim-string input))

  ;; For debugging.
  ;;(my-bot-log "RAW: '%s', channel: '%s', nick: '%s'" input channel-name nick)

  (when (not (string= nick "fsbot"))
    (setq input (erc-trim-string input))

    (cond
     ;; Say hello.
;;     ((and (string-match my-bot-hello-regexp input)
;;	   (not (string= channel-name "#sockets")))
;;      (my-bot-log-input input channel-buffer nick)
;;      (if (< (random 100) 20)
;;	  (my-bot-say channel-buffer nick
;;		      "%s, %s.  (BTW, I'm a bot.)"
;;		      (my-bot-random my-bot-hellos) nick)))

;;     ;; Say goodbye.
;;     ((and (string-match my-bot-goodbye-regexp input)
;;	   (not (assoc-ignore-case nick my-bot-seen-nicks))
;;	   (not (string= channel-name "#sockets")))
;;      (my-bot-log-input input channel-buffer nick)
;;      (my-bot-say channel-buffer nick "%s, %s." (my-bot-random my-bot-goodbyes) nick))

     ;; Dispatch commands to the command functions.
     ((or (string-match "^\\./\\.\\s-+" input)
	  (string-match "^\\.[^./].*$" input)
	  ;;(string= "help" input)
	  ;;(string-match "^help\\s-+.*$" input)
	  )

      (my-bot-log-input input channel-buffer nick)

      (let* ((words (split-string input))
	     (cmd (or (car words) ""))
	     (args (cdr words))
	     (argstring (erc-trim-string (if args (substring input (length cmd)) "")))
	     (argstring2 (my-bot-strip-first-word argstring)))

	(if (not (member-ignore-case cmd my-bot-commands))
	    (progn
	      (my-bot-log "Unrecognized input from %s: %s" (or channel-name nick) input)
	      (if (or (string= channel-name "#tcpip")
		      (null channel-buffer))
		  (my-bot-say channel-buffer nick "Sorry, I didn't understand that.  Type \"help\" to learn more about my capabilities.")))

	  (let* ((cmd-no-dot (if (= ?. (aref cmd 0)) (substring cmd 1) cmd))
		 (funcsym (intern-soft (format "my-bot-cmd-%s" cmd-no-dot))))
	    (if funcsym
		(funcall funcsym channel-buffer nick cmd args argstring argstring2)
	      (my-bot-say channel-buffer nick "*** Fatal internal error!  Please tell franl.")
	      (my-bot-log "*** ERROR: function my-bot-cmd-%s not defined!" cmd-no-dot))))))

     ;; Database lookup.
     ((string-match "^\\?\\s-*\\([^?!].*\\)$" input)
      (let* ((term (match-string 1 input))
	     (definition (my-bot-lookup term 'countit)))
	(my-bot-log-input input channel-buffer nick)
	(if definition
	    (progn
	      (setq definition (nth 1 definition))
	      (if (not (listp definition))
		  (setq definition (list definition)))
	      (dolist (line definition)
		(my-bot-say channel-buffer nick line)))
	  (let ((letter (aref term 0)))
	    (my-bot-say channel-buffer nick "I don't know that term.  Type \".index %c\" to see the terms in my database starting with '%c'." letter letter))))))))

(defun my-bot-start ()
  "Starts this bot."
  (interactive)
  
  ;;(setq debug-on-error t)

  (setq my-bot-start-time (current-time))

  (if (not (file-directory-p my-bot-data-directory))
      (make-directory my-bot-data-directory))

  (if (not (file-exists-p my-bot-log-filename))
      (shell-command (concat "touch " my-bot-log-filename)))

  (my-bot-show-log)
  (my-bot-log "=============================================")
  (my-bot-log "Bot started at %s %s." (time-stamp-yyyy-mm-dd) (time-stamp-hh:mm:ss))

  (cd my-bot-data-directory)
  (my-bot-load-databases)

  (if (null my-bot-autoconnections)
      (my-bot-log "No autoconnections defined!  See variable my-bot-autoconnections.")
  
    ;; We have autoconnections ...
    (my-bot-log "%d autoconnections defined." (length my-bot-autoconnections))

    (dolist (autoconnect my-bot-autoconnections)
      (let ((servername (car autoconnect))
	    (portnum (nth 1 autoconnect))
	    (nick (nth 2 autoconnect))
	    (fullname (nth 3 autoconnect))
	    (channels (nth 4 autoconnect))
	    serverbuf)
	(if (or (not (stringp servername))
		(not (integerp portnum))
		(not (stringp nick))
		(not (stringp fullname))
		(not (listp channels)))
	    (error "Bad value for my-bot-autoconnections."))
      
	(my-bot-log "Connecting to %s ..." servername)

	(setq serverbuf (erc servername portnum nick fullname 'connect))
      
	(if (not (bufferp serverbuf))
	    (my-bot-log "Connection to %s:%d failed!" servername portnum)
	
	  ;; We're connected ...
	  (my-bot-log "Connected!  Server buffer is %s." (buffer-name serverbuf))
	  (sit-for 3) ;; Is this needed?

	  ;; Cope with networks that don't identify themselves.
	  ;; BUG: This happens too soon!
	  (my-bot-set-network-name)

	  ;; Identify if necessary.
	  (my-bot-identify)

	  (dolist (channelname channels)
	    (my-bot-log "Joining %s ..." channelname)
	    ;; erc-cmd-JOIN only works when the current buffer is an ERC buffer.
	    (with-current-buffer serverbuf
	      (erc-cmd-JOIN channelname))
	    (sit-for 3))
	
	  (my-bot-log "Done joining channels."))))

    (my-bot-log "Done auto-connecting.")
	  
    (erc-track-mode 0)	;; Why is this necessary?  How does erc-track-mode get turned on?
    
    ;; Start a timer to refresh the RFC index once a day.
    (setq my-bot-rfc-update-timer
	  (run-at-time 3600 86400 'my-bot-cmd-rfcupdate nil nil ".rfcupdate" nil "" ""))

    ;; This is disabled to keep the bot quiet.
    ;; (setq my-bot-quote-timer (run-at-time 1200 1200 'my-bot-quote-timer-function))
    ;; (my-bot-log "Quotation timer started.")

    (setq my-bot-save-databases-timer
	  (run-at-time 600 600 'my-bot-save-databases-timer-function))
    (my-bot-log "Database saver timer started.")

    ;; Slashdot will ban us if we fetch headlines more often than once every 30 minutes.
    (setq my-bot-slashnews-timer (run-at-time 1900 1900 'my-bot-slashnews-timer-function))
    (my-bot-log "Slashdot headline tracking timer started.")

    (delete-other-windows)
    (list-buffers)
    (ignore-errors (delete-window))

    (my-bot-log "All done with startup tasks.  my-bot-start returning.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bot commands.
;;
;; NOTE: When invoked via "/msg birny ...", these functions run in the _server_
;; buffer not the channel buffer, otherwise they run in the channel buffer.
;;
;; IMPORTANT: Each of these functions _must_ leave the current buffer unchanged,
;; because erc-run-hooks (which is on the call stack for each of these functions)
;; cannot successfully call erc-server-buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-bot-cmd-== (channel-buffer nick cmd args argstring argstring2)
  "Implements the .== command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "=="))
    (let* ((expr (delete ?' argstring))
	   (output (delete ?\n
			   (my-command
			    (format "(ulimit -t 3; echo '%s' | /usr/bin/bc -l 2>/dev/null)"
				    expr)))))
      (if (= 0 (length output))
	  (my-bot-say channel-buffer nick "Syntax error (or you ran out of CPU time).")
	(setq output (delete ?\\ output))
	(if (> (length output) 400)
	    (my-bot-say channel-buffer nick "The result of that computation has %d digits, and that's too long for me to say."
			(1- (length output)))
	  (my-bot-say channel-buffer nick "%s == %s" expr output))))))

(put 'my-bot-cmd-== 'help "Performs arbitrary precision math.  Syntax of the expression is the same as accepted by the UNIX bc utility.  Example: .== 3 + 4.93 * 5 - (2 ^ 3) / 3.14159")
;; No usage string for this command.

(defun my-bot-cmd-admin (channel-buffer nick cmd args argstring argstring2)
  "Implements the .admin command."
  (my-bot-say channel-buffer nick
	      (concat "My administrators are: "
		      (mapconcat 'identity
				 (sort my-bot-admin-nicks 'string-lessp)
				 ", ")
		      ".")))

(put 'my-bot-cmd-admin 'help "Displays the nicks of my administrators.  These people have privileges to use certain commands that are not available to everyone.")
(put 'my-bot-cmd-admin 'usage "Usage: .admin")

(defun my-bot-cmd-anon (channel-buffer nick cmd args argstring argstring2)
  "Implements the .anon command."
  (my-bot-say channel-buffer nick
	      "While my author supports anonymity on principle, the .anon command is disabled due to the potential for abuse.")
;;  (if (< (length args) 2)
;;      (my-bot-say channel-buffer nick (my-bot-lookup-usage "anon"))
;;    (if channel-buffer
;;	(my-bot-say channel-buffer nick "Um, think about what you just did.")
;;      (let ((my-bot-inhibit-logging t)
;;	    (channel-name (car args))
;;	    (channelbuf (my-bot-get-erc-buffer (erc-network-name) (car args))))
;;	(if (null channelbuf)
;;	    (my-bot-say nil nick "Sorry, I'm not in channel '%s' on this network." channel-name)
;;	  (if (not (with-current-buffer channelbuf (erc-get-channel-user nick)))
;;	      (my-bot-say nil nick "You have to be in channel '%s' to speak anonymously there."
;;			  channel-name)
;;	    (my-bot-say channelbuf nil "Someone says: %s" argstrings))))))
  )

(put 'my-bot-cmd-anon 'help "Lets you say something anonymously in a channel.  You must /MSG me with this command, otherwise the whole channel sees your command (which isn't what you want).")
(put 'my-bot-cmde-anon 'usage "Usage: /msg birny .anon CHANNEL TEXT STRING")

(defun my-bot-cmd-away (channel-buffer nick cmd args argstring argstring2)
  "Implements the .away command.  Usage: .away [ -c | AWAY MESSAGE ].  Option '-c' clears
your away message.  When invoked with no arguments displays your current away message."
  (let ((entry (my-bot-lookup-away-message nick)))
    (if (null args)
	;; Display away message.
	(if (null entry)
	    (my-bot-say channel-buffer nick "Your away message isn't set.")
	  (my-bot-say channel-buffer nick "Your away message is: %s" (nth 3 entry)))
      (if (string= (car args) "-c")
	  ;; Clear away message.
	  (progn
	    (setq my-bot-away-messages (delete entry my-bot-away-messages))
	    (put 'my-bot-away-messages 'needs-saving t)
	    (my-bot-say channel-buffer nick "Cleared your away message."))
	;; Set away message.
	(if entry
	    (setcar (nthcdr 4 entry) argstring)
	  (add-to-list 'my-bot-away-messages
		       (list (concat nick "@" (erc-network-name)) (current-time) channel-buffer
			     argstring '(0 0 0)))
	  (put 'my-bot-away-messages 'needs-saving t))
	(my-bot-say channel-buffer nick "Set your away message.")))))

(put 'my-bot-cmd-away 'help "Sets or clears your away message.")
(put 'my-bot-cmd-away 'usage' "Usage: .away [ -c | MESSAGE ].  Option \"-c\" clears your away message.  When invoked with no arguments, displays your current away message.")

(defun my-bot-cmd-bugtraq (channel-buffer nick cmd args argstring argstring2)
  "Implements the .bugtraq command."
  (if nil
      (my-bot-say channel-buffer nick
		  "Sorry, the .bugtraq command is being serviced.  Try again later.")
    (let (lines output)
      (if (not (string= argstring "-n"))
	  (setq argstring ""))
      (setq lines (split-string (my-command (format "%s/bugtraq %s 2>/dev/null"
						    my-bot-bin-directory argstring))
				"\n"))
      (if (null lines)
	  (my-bot-say channel-buffer nick "Sorry, Bugtraq gave me nothing for that.")
	(dolist (line lines)
	  (my-bot-say channel-buffer nick line))
	(my-bot-say channel-buffer nick
		    "[See http://www.securityfocus.com/bid for the full list.]")))))

(put 'my-bot-cmd-bugtraq 'help
     "Displays a short list of the most recent Bugtraq security vulnerabilities.  With option \"-n\", restricts the output to vulnerabilities with the word \"network\" in their summaries.")

(put 'my-bot-cmd-bugtraq 'usage "Usage: .bugtraq [ -n ]")

(defun my-bot-cmd-c2f (channel-buffer nick cmd args argstring argstring2)
  "Implements the .c2f command."
  (if (/= (length args) 1)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "c2f"))
    (if (not (string-match "^[0-9.+-]+$" (car args)))
	(my-bot-say channel-buffer nick "Sorry, '%s' doesn't look like a number to me."
		    (car args))
      (my-bot-say channel-buffer nick "%s C is %f F."
		  (car args)
		  (+ (* (string-to-number (car args))
			1.8)
		     32)))))

(put 'my-bot-cmd-c2f 'help "Converts a temperature from Celsius to Fahrenheit.")
(put 'my-bot-cmd-c2f 'usage "Usage: .c2f NUMBER")

(defun my-bot-cmd-catch (channel-buffer nick cmd args argstring argstring2)
  "Implements the .catch command."
  (if (null args)
      (my-bot-say channel-buffer nick "Error catching mode is %s."
		  (if my-bot-catch-errors "on" "off"))
    (if (/= (length args) 2)
	(my-bot-say channel-buffer nick (my-bot-lookup-usage "catch"))
      (when (my-bot-check-password (car args) nick channel-buffer)
	(let ((which (or (nth 1 args) "")))
	  (if (not (member-ignore-case which '("on" "off")))
	      (my-bot-say channel-buffer nick "Usage: .catch [ PASSWORD { on | off } ]")
	    (cond
	     ((string= which "on")
	      (setq my-bot-catch-errors t
		    debug-on-error nil))
	     ((string= which "off")
	      (setq my-bot-catch-errors nil
		    debug-on-error t))
	     (t
	      (my-bot-log "INTERNAL ERROR in my-bot-cmd-catch!  Variable which = '%s'." which)))
	    (my-bot-say channel-buffer nick "Error catching mode is %s."
			(if my-bot-catch-errors "on" "off"))))))))

(put 'my-bot-cmd-catch 'help "Toggles my internal Emacs-Lisp error catching code.")
(put 'my-bot-cmd-catch 'usage "Usage: .catch [ PASSWORD { on | off } ]")

(defun my-bot-cmd-connect (channel-buffer nick cmd args argstring argstring2)
  "Implements the .connect command."
  (if (/= 3 (length args))
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "connect"))
    (when (my-bot-check-password (car args) nick channel-buffer)
      (my-bot-say channel-buffer nick "Connecting ... please wait ...")
      (let* ((orig-buffer (current-buffer))
	     (server (nth 1 args))
	     (portnum (string-to-int (nth 2 args)))
	     (serverbuf (erc server portnum my-bot-nick my-bot-fullname 'connect)))
	(if (not (bufferp serverbuf))
	    (progn
	      (my-bot-log "Connection to %s (port %) failed!" server portnum)
	      (my-bot-say channel-buffer nick "Connection to %s (port %d) failed!"
			  server portnum))

	  ;; The TCP connection exists now, but server is still sending us
	  ;; introductory data.
	  (my-bot-log "Connected to %s!" server)
	  (with-current-buffer orig-buffer
	    ;; We need to do this in the original buffer, because right now the current
	    ;; buffer is associated with a different network.  Without this buffer
	    ;; change, the wrong nick would be /MSG'ed.
	    (my-bot-say channel-buffer nick "Connected to %s!" server)))))

    ;; Do this regardless of whether the connection was successful.
    (run-at-time 1 nil '(lambda ()
			  (delete-other-windows)
			  (list-buffers)
			  (delete-window)))))

(put 'my-bot-cmd-connect 'help "Tells me to connect to an IRC server.")
(put 'my-bot-cmd-connect 'usage "Usage: .connect PASSWORD SERVER PORT")

(defun my-bot-cmd-cpan (channel-buffer nick cmd args argstring argstring2)
  "Implements the .cpan command."
  (if t
      ;; Disable this command until cpan is working on backboneaudio.com
      (my-bot-say channel-buffer nick "This command it currently unavailable.")
    (if (null args)
	(my-bot-say channel-buffer nick (my-bot-lookup-usage "cpan"))
      (let ((lines (split-string
		    (my-command (format "perl -MCPAN -e 'CPAN::Shell->m(\"/%s/\");' 2>/dev/null"
					(delete ?' (erc-trim-string argstring))))
		    "\n")))
	(if (or (< (length lines) 4)
		(string-match "^No objects.*found" (nth 3 lines)))
	    (my-bot-say channel-buffer nick "No CPAN module matches that regular expression.")
	  (dolist (line (subseq lines 0 (min 10 (length lines))))
	    (if (string-match "^Module\\s-+\\(\\S-+\\)" line)
		(my-bot-say channel-buffer nick (match-string 1 line)))))))))

(put 'my-bot-cmd-cpan 'help "Performs a CPAN search for a module name or regular expression.")
(put 'my-bot-cmd-cpan 'usage "Usage: .cpan { FULL-MODULE-NAME | /REGEX/ }")

(defun my-bot-cmd-defhelp (channel-buffer nick cmd args argstring argstring2)
  "Implements the .defhelp command.  Usage: .defhelp HELP TERM { is | = | == } HELP STRING"
  (if t
      (my-bot-say channel-buffer nick
		  "This command is temporarilly disabled while franl hacks on the help system.")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                   UNDER CONSTRUCTION                   ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (if (my-bot-privcheck nick channel-buffer)
	(if (or (< (length args) 3)
		(not (string-match "^\\(.*?\\)\\s-+\\(is\\|=\\|==\\)\\s-+\\(.*\\)$" argstring)))
	    (my-bot-say channel-buffer nick (my-bot-lookup-usage "defhelp"))
	  (let* ((helpterm (match-string 1 argstring))
		 (definition (match-string 3 argstring))
		 (olddef (my-bot-lookup-help helpterm)))
	    (if olddef
		(progn
		  (my-bot-say channel-buffer nick "Replacing: \"%s\"." (cdr olddef))
		  (setcdr olddef definition))
	      (add-to-list 'my-bot-help (cons helpterm definition)))
	    ;;(my-bot-save-help)
	    (put 'my-bot-help 'needs-saving t)
	    (my-bot-say channel-buffer nick "Defined help term \"%s\"." helpterm))))))

(put 'my-bot-cmd-defhelp 'help "Adds help to the help database.  Note: The help database is for help about the bot and his commands.  Use .learn to add technical terms to the terminology database.")
(put 'my-bot-cmd-defhelp 'usage "Usage: .defhelp HELP TERM { is | = | == } HELP STRING")

(defun my-bot-cmd-die (channel-buffer nick cmd args argstring argstring2)
  "Implements the .die command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "die"))
    (when (my-bot-check-password (car args) nick channel-buffer)
      (my-bot-save-all-databases)
      (my-bot-say channel-buffer nick "Shutting down!")
      (dolist (serverbuf (erc-buffer-list 'erc-server-buffer-p))
	(with-current-buffer serverbuf
	  (if erc-connected
	      (erc-cmd-QUIT (if (> (length args) 1)
				argstring2
			      "Shutting down!")))))

      (my-bot-log "*** TERMINATING IN 5 SECONDS ***")
      (run-at-time 5 nil 'kill-emacs))))

(put 'my-bot-cmd-die 'help "Tells me to terminate immediately.")
(put 'my-bot-cmd-die 'usage "Usage: .die PASSWORD [ QUIT REASON ]")

(defun my-bot-cmd-dns (channel-buffer nick cmd args argstring argstring2)
  "Implements the .dns command."
  (if (eq system-type 'windows-nt)
      (my-bot-say channel-buffer nick "This command is unavailable at the moment.")
    (if (or (null args)
	    (> (length args) 2))
	(my-bot-say channel-buffer nick (my-bot-lookup-usage "dns"))
      (let* ((recordtype (if (string-match "^-\\(mx\\|ns\\)$" (car args))
			     (match-string 1 (car args))))
	     (host (delete ?' (if (null recordtype) argstring argstring2)))
	     output lines)

	(block main
	  (when (not (memq ?. (string-to-list host)))
	    ;; host is really a nick.  Substitute the nick's host for the argument.
	    (when (null channel-buffer)
	      (my-bot-say channel-buffer nick "Sorry, you can't do nick DNS lookups via /MSG.")
	      (return-from main))

	    (let ((nickinfo (with-current-buffer channel-buffer
			      (erc-get-channel-user host))))
	      (if (null nickinfo)
		  (progn
		    (my-bot-say channel-buffer nick "I don't see %s in this channel." host)
		    (return-from main))
		(setq host (with-current-buffer channel-buffer
			     (erc-server-user-host (erc-get-server-user host)))))))

	  ;; Now, host is really a hostname.
	  (setq output (my-command (format "/usr/bin/dig '%s' %s 2>/dev/null | %s/prettydig"
					   host (or recordtype "") my-bot-bin-directory)))
	  (if (< (length output) 3)
	      (my-bot-say channel-buffer nick "Sorry, I found nothing for that query.")
	    (if (string-match "No such host" output)
		(cond
		 ((string= recordtype "mx")
		  (my-bot-say channel-buffer nick "No MX records for that host."))
		 ((string= recordtype "ns")
		  (my-bot-say channel-buffer nick "No NS records for that host."))
		 (t
		  (my-bot-say channel-buffer nick "No A records for that host.")))
	      (setq output (delete ?\n output))
	      (my-bot-say channel-buffer nick output))))))))

(put 'my-bot-cmd-dns 'help "Queries the Domain Name System for address info about a hostname.  With option \"-mx\", shows Mail Exchange information.  With option \"-ns\", shows nameserver information.")
(put 'my-bot-cmd-dns 'usage "Usage: .dns [ -mx | -ns ] { hostname | nick }")

(defalias 'my-bot-cmd-host 'my-bot-cmd-dns)

(defalias 'my-bot-cmd-domain 'my-bot-cmd-dns)

(defun my-bot-cmd-eval (channel-buffer nick cmd args argstring argstring2)
  "Implements the .eval command.  Usage: .eval PASSWORD LISP-EXPRESSION"
  (if (< (length args) 2)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "eval"))
    (if (my-bot-check-password (car args) nick channel-buffer)
	(condition-case nil
	    (let ((output (prin1-to-string (eval (read argstring2)))))
	      (if (> (length output) 400)
		  (setq output (concat (substring output 0 400)
				       " ... [truncated]")))
	      (my-bot-say channel-buffer nick output))
	  (error (my-bot-say channel-buffer nick
			     "An error occurred during evaluation of that Elisp code."))))))

(put 'my-bot-cmd-eval 'help "Evaluates an arbitrary Emacs-Lisp expression.")
(put 'my-bot-cmd-eval 'usage "Usage: .eval PASSWORD EXPRESSION.")

(defun my-bot-cmd-f2c (channel-buffer nick cmd args argstring argstring2)
  "Implements the .f2c command."
  (if (/= (length args) 1)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "f2c"))
    (if (not (string-match "^[0-9.+-]+$" (car args)))
	(my-bot-say channel-buffer nick "Sorry, '%s' doesn't look like a number to me."
		    (car args))
      (my-bot-say channel-buffer nick "%s F is %f C."
		  (car args)
		  (/ (- (string-to-number (car args))
			32)
		     1.8)))))

(put 'my-bot-cmd-f2c 'help "Converts a temperature from Fahrenheit to Celsius.")
(put 'my-bot-cmd-f2c 'usage "Usage: .f2c NUMBER")

(defun my-bot-cmd-forget (channel-buffer nick cmd args argstring argstring2)
  "Implements the .forget command.  Usage: .forget TERM"
  (if (my-bot-privcheck nick channel-buffer)
      (if (null args)
	  (my-bot-say channel-buffer nick (my-bot-lookup-usage "forget"))
	(let ((item (assoc-ignore-case (erc-trim-string argstring) my-bot-terms)))
	  (if (null item)
	      (my-bot-say channel-buffer nick "That's not in my database.")
	    (my-bot-say channel-buffer nick "Forgetting \"%s\"." argstring)
	    (setq my-bot-terms (delete item my-bot-terms))
	    (if (member-ignore-case argstring my-bot-new-terms)
		(setq my-bot-new-terms (delete argstring my-bot-new-terms)))
	    ;;(my-bot-save-terms)
	    (put 'my-bot-terms 'needs-saving t)
	    (put 'my-bot-new-terms 'needs-saving t)
	    )))))

(put 'my-bot-cmd-forget 'help "Makes me forget a term from my terminology database.")
(put 'my-bot-cmd-forget 'usage "Usage: .forget TERM")

(defun my-bot-cmd-google (channel-buffer nick cmd args argstring argstring2)
  "Implements the .google command."
  (if nil
      (my-bot-say channel-buffer nick
		  "Sorry, the .google command is being serviced.  Try again later.")
    (if (null args)
	(my-bot-say channel-buffer nick (my-bot-lookup-usage "google"))
      (let ((searchterm (delete ?' argstring))
	    lines output)
	(setq lines (split-string (my-command (format "%s/google '%s' 2>/dev/null"
						      my-bot-bin-directory searchterm))
				  "\n")
	      output (if (null lines)
			 "Sorry, Google gave me nothing for that."
		       (if (= (length lines) 2)
			   (format "%s.  Also: %s" (car lines) (nth 1 lines))
			 (mapconcat 'identity lines " -- "))))
	(my-bot-say channel-buffer nick output)))))

(put 'my-bot-cmd-google 'help "Performs a Google search for the specified TERM.")
(put 'my-bot-cmd-google 'usage "Usage: .google TERM")

(defun my-bot-cmd-help (channel-buffer nick cmd args argstring argstring2)
  "Implements the 'help' command.  Usage: help TOPIC"
  (if (or (null args)
	  (string= argstring "birny"))
      (my-bot-say channel-buffer nick
		  "I'm a bot that looks up terms in a knowledge database.  Type '? TERM' to see the definition of TERM.  Type '.help ?' to see my command set.  If you /MSG me, I'll /MSG you back, which helps keep channel clutter to a minimum.")

    (setq argstring (erc-trim-string argstring))
    (if (string-match "^[*?]$" argstring)
	;; List all help terms.  I use copy-list here because sort modifies the list being
	;; sorted by side effects, which can leave my-bot-help with only one entry.
	(let ((topichelp (concat "Help is available for these topics: "
				 (mapconcat 'car (sort (copy-list my-bot-help)
						       '(lambda (x y)
							  (string-lessp (car x) (car y))))
					    ", ")
				 "."))
	      (cmdhelp (concat "Help is available for these commands: "
			       (mapconcat 'identity
					  (sort (remove-duplicates
						 (mapcar '(lambda (cmd)
							    (if (= ?. (aref cmd 0))
								(substring cmd 1)
							      cmd))
							 my-bot-commands)
						 :test #'string=)
						#'string-lessp)
					  ", ")
			       ".")))
	  (my-bot-say channel-buffer nick topichelp)

	  (if (< (length cmdhelp) 200)
	      (my-bot-say channel-buffer nick cmdhelp)

	    ;; Break the cmdhelp string into multiple lines.
	    (while (string-match "^\\(.\\{1,425\\}[,.]\\)\\( \\|$\\)" cmdhelp)
	      (let ((piece (match-string 1 cmdhelp)))
		(my-bot-say channel-buffer nick (if (= 32 (aref piece 0))
						    (substring piece 1)
						  piece))
		(setq cmdhelp (substring cmdhelp (length piece)))))))

      ;; Give help for a specific command or term.
      (let* ((argstring (if (= ?. (aref argstring 0)) (substring argstring 1) argstring))
	     (helpstr (or (my-bot-lookup-help argstring)
			  (concat (format "No help available for '%s'.  Type '.help ?' for a list of help topics and commands."
					  argstring)))))
	(my-bot-say channel-buffer nick helpstr)))))

(put 'my-bot-cmd-help 'help "Displays help for a topic or command.")
(put 'my-bot-cmd-help 'usage "Usage: .help { TOPIC | COMMAND }")

(defun my-bot-cmd-http (channel-buffer nick cmd args argstring argstring2)
  "Implements the .http command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "http"))
    (let* ((code (or (delete ?' (car args)) ""))
	   (output (my-command (format "grep '^%s = ' %s/httpcodes 2>/dev/null"
				       code my-bot-data-directory))))
      (if (string-match "^[0-9]+ = \\(.*\\)$" output)
	  (my-bot-say channel-buffer nick "HTTP status code %s means \"%s\"."
		      code (match-string 1 output))
	(my-bot-say channel-buffer nick "That's not a valid HTTP status code.")))))

(put 'my-bot-cmd-http 'help "Displays the meaning of a numeric HTTP status code.")
(put 'my-bot-cmd-http 'usage "Usage: .http NNN (where NNN is a numeric HTTP status code).")

(defun my-bot-cmd-id (channel-buffer nick cmd args argstring argstring2)
  "Implements the .id command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "id"))
    (let* ((idregex (delete ?' argstring))
	   (output (my-command (format "egrep -i '%s' %s/idindex 2>/dev/null"
				       idregex my-bot-data-directory)))
	   (counter 1)
	   (lines (split-string output "\n")))
      (if (null lines)
	  (my-bot-say channel-buffer nick "Sorry, no ID matches that regular expression.")
	(if (<= (length lines) 5)
	    (my-bot-say channel-buffer nick "These %s IDs match:" (length lines))
	  (my-bot-say channel-buffer nick "Displaying 5 of %d matches (find all the IDs at http://www.ietf.org/ID.html)."
		      (length lines))
	  (setq lines (subseq lines 0 5)))
	(dolist (line lines)
	  (my-bot-say channel-buffer nick "%d. %s" counter line)
	  (incf counter))))))

(put 'my-bot-cmd-id 'help "The .id command searches an index of Internet Drafts and shows IDs that match.")
(put 'my-bot-cmd-id 'usage "Usage: .id REGEX (where REGEX is a valid egrep regular expression)")

(defun my-bot-cmd-index (channel-buffer nick cmd args argstring argstring2)
  "Implements the .index command."
  (if (or (/= (length args) 1)
	  (> (length argstring) 5)
	  (and (not (string= (car args) "-a"))
	       (> (length argstring) 1)
	       (not (string-match "^[^,]\\(,[^,]\\(,[^,]\\)?\\)?$" argstring))))
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "index"))

    (let (term-list output)
      (if (string= "-a" (car args))
	    (setq channel-buffer nil
		  term-list (sort (mapcar 'car (my-bot-scrubbed-data))
				  '(lambda (x y)
				     (string-lessp (downcase x) (downcase y))))
		  output (concat "I have the following terms in my database: "
				 (mapconcat 'identity term-list ", ")
				 "."))

	;; No -a option.  Just show a subset of the index.
	(let* ((letters (string-to-list (delete ?, (upcase argstring))))
	       (letters-verbose (concat "'"
					(if (> (length letters) 1)
					    (mapconcat 'char-to-string letters "' or '")
					  (char-to-string (car letters)))
					"'")))
	  (dolist (letter letters)
	    (dolist (term (sort (mapcar 'car (my-bot-scrubbed-data))
				'(lambda (x y)
				   (string-lessp (downcase x) (downcase y)))))
	      (if (= (downcase (aref term 0)) (downcase letter))
		  (add-to-list 'term-list term 'append))))

	  (if (null term-list)
	      (setq output (format "No terms in my database start with %s." letters-verbose))
	    (setq output (concat (format "These terms in my database start with %s: "
					 letters-verbose)
				 (mapconcat 'identity term-list ", ")
				 ".")))))

      ;; Emit the output (over several lines if necessary).
      (if (< (length output) 200)
	  (my-bot-say channel-buffer nick output)

	;; Break the output string into multiple lines.
	(while (string-match "^\\(.\\{1,435\\}[,.]\\)\\( \\|$\\)" output)
	  (let ((piece (match-string 1 output)))
	    (my-bot-say channel-buffer nick (if (= 32 (aref piece 0)) (substring piece 1) piece))
	    (setq output (substring output (length piece)))))))))

(put 'my-bot-cmd-index 'help "Displays some or all of the terms in the terminology database.")
(put 'my-bot-cmd-index 'usage "Usage: .index { -a | X | X,X | X,X,X } (where X is a single character).  Option '-a' means to /MSG you with the full index.")

(defun my-bot-cmd-info (channel-buffer nick cmd args argstring argstring2)
  "Implements the .info command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "info"))
    (let ((dbentry (my-bot-lookup argstring)))
      (if (null dbentry)
	  (my-bot-say channel-buffer nick "Sorry, that term isn't in my database.")
	(my-bot-say channel-buffer nick
		    "The term \"%s\" was last modified by <%s> at %s.  It has been looked up %d times."
		    argstring (nth 2 dbentry)
		    (format-time-string "%Y-%m-%d %H:%M:%S GMT" (nth 3 dbentry) 'gmt)
		    (nth 4 dbentry))))))

(put 'my-bot-cmd-info 'help "Shows administrative information about a database entry.")
(put 'my-bot-cmd-info 'usage "Usage: .info TERM")

(defun my-bot-cmd-irc (channel-buffer nick cmd args argstring argstring2)
  "Implements the .irc command."
  (if (my-bot-privcheck nick channel-buffer)
      (let ((orig-buffer (current-buffer)))
	(dolist (server (erc-buffer-list 'erc-server-buffer-p))
	  (with-current-buffer server
	    (let* ((chanbufs (delete nil (mapcar '(lambda (buf) (if (erc-channel-p buf) buf))
						 (erc-buffer-list nil erc-server-process))))
		   (server-and-port (concat erc-announced-server-name ":"
					    (int-to-string erc-session-port)))
		   (network (erc-network-name))
		   (channels (mapconcat '(lambda (chanbuf)
					   (with-current-buffer chanbuf (erc-default-target)))
					chanbufs ", ")))
	      (if (string-match "\\." network)
		  (setq network "UNKNOWN"))
	      (if (string= "" channels)
		  (setq channels "<no channels joined>"))
	      (with-current-buffer orig-buffer
		(my-bot-say channel-buffer nick
			    "Network %s (%s): %s" network server-and-port channels))))))))

(put 'my-bot-cmd-irc 'help "The .irc command displays the IRc networks and channels that I'm currently using.")
(put 'my-bot-cmd-irc 'usage "Usage: .irc")

(defun my-bot-cmd-join (channel-buffer nick cmd args argstring argstring2)
  "Implements the .join command."
  (if (/= (length args) 2)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "join"))
    (when (my-bot-check-password (car args) nick channel-buffer)
      (let* ((channel-to-join (nth 1 args))
	     (chanbuf (get-buffer channel-to-join)))
	(if (not (erc-channel-p channel-to-join))
	    (my-bot-say channel-buffer nick "That doesn't look like a channel name to me!"
			channel-to-join)
	  (if chanbuf
	      ;; Prevent joining two channels with the same name, even if they're on
	      ;; different networks.  This avoids a design flaw.
	      (my-bot-say channel-buffer nick "I'm already in channel %s on network %s!"
			  channel-to-join (with-current-buffer chanbuf (erc-network-name)))
	    (my-bot-say channel-buffer nick "Joining channel %s ..." channel-to-join)
	    (erc-cmd-JOIN channel-to-join)))))))

(put 'my-bot-cmd-join 'help "NO HELP AVAILABLE YET.")
(put 'my-bot-cmd-join 'usage "Usage: .join PASSWORD CHANNEL")

(defun my-bot-cmd-joke (channel-buffer nick cmd args argstring argstring2)
  "Implements the .joke command."
  (if (> (length args) 0)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "joke"))
    (my-bot-say channel-buffer nick (my-command "~/bin/joke"))))

(put 'my-bot-cmd-joke 'help "Displays a funny joke.")
(put 'my-bot-cmd-joke 'usage "Usage: .joke")

(defun my-bot-cmd-learn (channel-buffer nick cmd args argstring argstring2)
  "Implements the .learn command."
  (if (my-bot-privcheck nick channel-buffer)
      (let (term definition)
	(if (not (or (string-match "^\"?\\(\\S+\\)\"?\\s-+\\(is\\|=\\|==\\)\\s-+\"?\\(.*?\\)\"?\\s-*$"
				   argstring)
		     (string-match "^\"?\\(.+?\\)\"?\\s-+\\(is\\|=\\|==\\)\\s-+\"?\\(.*\\)\"?\\s-*$"
				   argstring)))
	    (my-bot-say channel-buffer nick (my-bot-lookup-usage "learn"))
	  (setq term (match-string 1 argstring)
		definition (match-string 3 argstring))

	  (if (> (length definition) 400)
	      (my-bot-say channel-buffer nick "WARNING: That definition is so long that you should verify that it wasn't truncated or otherwise modified by your IRC client."))

	  ;; Be sure to use my-bot-lookup to get the database entry for TERM, so that
	  ;; when you use .learn to replace the definition of an alias, the definition of the
	  ;; the real entry is changed, not the definition of the alias.

	  (let* ((dbentry (my-bot-lookup term)))
	    (if dbentry
		(setq my-bot-terms (delete dbentry my-bot-terms)))

	    (when (string-match "\\\\n" definition)
	      (setq definition
		    (split-string (replace-regexp-in-string "\\\\n" "\C-a" definition) "\C-a")))

	    (add-to-list 'my-bot-terms (list term definition nick (current-time) 0))

	    (if (null dbentry)
		(add-to-list 'my-bot-new-terms term))

	    (if (> (length my-bot-new-terms) 5)
		(setcdr (nthcdr 4 my-bot-new-terms) nil))

	    (put 'my-bot-terms 'needs-saving t)
	    (put 'my-bot-new-terms 'needs-saving t)
	    (my-bot-say channel-buffer nick "%s term \"%s\".  Total terms: %d."
			(if dbentry "Replaced" "Learned new") term (length my-bot-terms)))))))

(put 'my-bot-cmd-learn 'help "Adds a term to my knowledge database.")
(put 'my-bot-cmd-learn 'usage "Usage: .learn TERM { is | = | == } DEFINITION")

(defun my-bot-cmd-me (channel-buffer nick cmd args argstring argstring2)
  "Implements the .me command."
  (if (< (length args) 2)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "me"))
    (when (my-bot-check-password (car args) nick channel-buffer)
      (let ((actionchan (nth 1 args)))
	(if (not (erc-channel-p actionchan))
	    (my-bot-say channel-buffer nick "Dude, '%s' doesn't look like a channel name to me."
			actionchan)
	  (if (null (get-buffer actionchan))
	      (my-bot-say channel-buffer nick "Dude, I'm not in that channel on this network.")
	    (setq argstring2 (my-bot-strip-first-word argstring2))
	    (my-bot-log "%s: * %s %s" (format "-> %s" actionchan) my-bot-nick argstring2)
	    (with-current-buffer actionchan
	      (erc-cmd-ME (concat " " argstring2)))))))))

(put 'my-bot-cmd-me 'help "NO HELP AVAILABLE YET.")
(put 'my-bot-cmd-me 'usage "Usage: .me PASSWORD CHANNEL ACTION-PHRASE")

(defun my-bot-cmd-netmask (channel-buffer nick cmd args argstring argstring2)
  "Implements the .netmask command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "netmask"))
    (let ((address (car args))
	  baseaddr prefix octets)
      (if (not (string-match "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)/\\([0-9]+\\)$"
			     address))
	  (my-bot-say channel-buffer nick "Malformed argument.  Usage: .netmask IPADDRESS/XX, where XX is a CIDR network prefix.")
	(setq baseaddr (match-string 1 address)
	      prefix (string-to-int (match-string 2 address))
	      octets (progn
		       (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$"
				     baseaddr)
		       (list (string-to-int (match-string 1 baseaddr))
			     (string-to-int (match-string 2 baseaddr))
			     (string-to-int (match-string 3 baseaddr))
			     (string-to-int (match-string 4 baseaddr)))))
	
	(if (or (>= (car octets) 224)
		(= (car octets) 0))
	    (my-bot-say channel-buffer nick "The first octet of the IP address must be between 1 and 223 (inclusive).")
	  (if (or (> (nth 1 octets) 255)
		  (> (nth 2 octets) 255)
		  (> (nth 3 octets) 255))
	      (my-bot-say channel-buffer nick "Malformed IP address.")

	    (let ((output (my-command (format "%s/cidr %s 2>/dev/null" my-bot-bin-directory
					      address))))
	      (if (= (length output) 0)
		  (my-bot-say channel-buffer nick "Sorry, I encountered an internal error while executing that command.")
		(setq output (delete ?\n output))
		(my-bot-say channel-buffer nick output)))))))))

(put 'my-bot-cmd-netmask 'help "Displays the network address, netmask, and broadcast address for a given CIDR-style IP address.")
(put 'my-bot-cmd-netmask 'usage "Usage: .netmask IPADDRESS/XX (where XX is a CIDR network prefix).")
(defalias 'my-bot-cmd-cidr 'my-bot-cmd-netmask)

(defun my-bot-cmd-new (channel-buffer nick cmd args argstring argstring2)
  "Implements the .new command."
  (if (null my-bot-new-terms)
      (my-bot-say channel-buffer nick "No new terms.  This is probably a bug.  Please tell franl.")
    (my-bot-say channel-buffer nick (concat "Recently added/updated terms: \""
					    (mapconcat 'identity my-bot-new-terms "\", \"")
					    "\"."))))

(put 'my-bot-cmd-new 'help "Displays the most recently added terms in my terminology database.")
(put 'my-bot-cmd-new 'usage "Usage: .new")

(defun my-bot-cmd-news (channel-buffer nick cmd args argstring argstring2)
  "Implements the .news command."
  (if args
      (if (< (length args) 2)
	  (my-bot-say channel-buffer nick (my-bot-lookup-usage "news"))
	(when (my-bot-check-password (car args) nick channel-buffer)
	  (if (string= argstring2 "-")
	      (setq my-bot-news nil)
	    (if (/= ?+ (aref argstring2 0))
		(setq my-bot-news argstring2)
	      (setq argstring2 (substring argstring2 1)) ;; Remove the leading '+'.
	      (setq my-bot-news (if (listp my-bot-news)
				    (append my-bot-news `(,argstring2))
				  (list my-bot-news argstring2)))))
	  ;;(my-bot-save-other)
	  (put 'my-bot-news 'needs-saving t)
	  (my-bot-say channel-buffer nick "News updated.")))

    ;; No arguments.  Just show the news.
    (if (null my-bot-news)
	(my-bot-say channel-buffer nick "No news.")
      (if (listp my-bot-news)
	  (dolist (line my-bot-news)
	    (my-bot-say channel-buffer nick line))
	(my-bot-say channel-buffer nick my-bot-news)))))

(put 'my-bot-cmd-news 'help "Displays or changes the news.")
(put 'my-bot-cmd-news 'usage "Usage: .news [ PASSWORD { - | [+]NEWS STRING } ]")

(defun my-bot-cmd-part (channel-buffer nick cmd args argstring argstring2)
  "Implements the .part command."
  (if (< (length args) 2)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "part"))
    (if (my-bot-check-password (car args) nick channel-buffer)
	(let* ((channel-to-part (nth 1 args))
	       (channel-buffer-to-part (my-bot-get-erc-buffer (erc-network-name) channel-to-part)))
	  (if (null channel-buffer-to-part)
	      (my-bot-say channel-buffer nick "I'm not in channel %s on this network."
			  channel-to-part)
	    (my-bot-say channel-buffer nick "Parting from %s ..." channel-to-part)
	    (with-current-buffer channel-buffer-to-part
	      (erc-cmd-PART "")))))))

(put 'my-bot-cmd-part 'help "Tells me to part a channel.")
(put 'my-bot-cmd-part 'usage "Usage: .part PASSWORD CHANNEL")

(defun my-bot-cmd-ping (channel-buffer nick cmd args argstring argstring2)
  "Implements the .ping command."
  (if (eq system-type 'windows-nt)
      (my-bot-say channel-buffer nick "This command is unavailable at the moment.")
    (if (or (null args)
	    (> (length args) 1))
	(my-bot-say channel-buffer nick (my-bot-lookup-usage "ping"))
      (let* ((host (delete ?' argstring))
	     lines output)
	(block main
	  (when (not (memq ?. (string-to-list host)))
	    ;; host is really a nick.  Substitute the nick's host for the argument.
	    (when (null channel-buffer)
	      (my-bot-say channel-buffer nick "Sorry, you can't ping nicks via /MSG.")
	      (return-from main))

	    (let ((nickinfo (with-current-buffer channel-buffer
			      (erc-get-channel-user host))))
	      (if (null nickinfo)
		  (progn
		    (my-bot-say channel-buffer nick "I don't see %s in this channel." host)
		    (return-from main))
		(setq host (with-current-buffer channel-buffer
			     (erc-server-user-host (erc-get-server-user host)))))))
	  (setq lines
		;; BUG: Need a timeout in case this hangs during name resolution.
		(split-string (my-command (format "ping -q -c 3 -w 5 -- '%s' 2>/dev/null" host))
			      "\n"))
	  (if (null lines)
	      (my-bot-say channel-buffer nick "Sorry, ping produced no output.")
	    (if (string-match "unknown host" (car lines))
		(my-bot-say channel-buffer nick "Unknown host: %s." host)
	      (if (< (length lines) 4)
		  (my-bot-say channel-buffer nick "Sorry, I couldn't parse ping's output.")
		(setq output (nth 3 lines))
		(if (and (>= (length lines) 5)
			 (not (string= "" (nth 4 lines))))
		    (setq output (concat output " -- " (nth 4 lines))))
		(my-bot-say channel-buffer nick output)))))))))

(put 'my-bot-cmd-ping 'help "Pings a host and reports the summary info.")
(put 'my-bot-cmd-ping 'usage "Usage: .ping { hostname | ipaddress }")

(defun my-bot-cmd-port (channel-buffer nick cmd args argstring argstring2)
  "Implements the .port command."
  (if (/= (length args) 1)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "port"))
    (let ((service (delete ?' (car args)))
	  (port-numbers-file (concat my-bot-data-directory "/port-numbers"))
	  (count 0)
	  lines)
      (if (string-match "^[0-9]+$" service)
	  (setq lines
		(split-string
		 (my-command (format "egrep '\\b%s/' %s 2>/dev/null"
				     service port-numbers-file))
		 "\n"))
	(setq lines
	      (split-string
	       (my-command (format "egrep -v '^[ \t]*#' %s | egrep '%s' 2>/dev/null"
				   port-numbers-file service))
	       "\n")))
      (if (null lines)
	  (my-bot-say channel-buffer nick "Sorry, I found no match for that string.")
	(if (> (length lines) 5)
	    (my-bot-say channel-buffer nick "[Showing 5 of %d lines.]"
			(length lines)))
	(dolist (line (subseq lines 0 (min 5 (length lines))))
	  (my-bot-say channel-buffer nick (with-temp-buffer
					    (insert line)
					    (untabify (point-min) (point-max))
					    (buffer-string))))))))

(put 'my-bot-cmd-port 'help "Displays information about the specified port number or service.")
(put 'my-bot-cmd-port 'usage "Usage: .port { PORT-NUMBER | SERVICE-NAME }")

(defun my-bot-cmd-quit (channel-buffer nick cmd args argstring argstring2)
  "Implements the .quit command."
  (if (< (length args) 2)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "quit"))
    (if (my-bot-check-password (car args) nick channel-buffer)
	(let ((orig-buffer (current-buffer))
	      (network (nth 1 args))
	      (reason (my-bot-strip-first-word argstring2)))
	  (if (block nil
		(dolist (serverbuf (erc-buffer-list 'erc-server-buffer-p))
		  (with-current-buffer serverbuf
		    (when (and erc-connected
			       (string= (downcase (erc-network-name)) (downcase network)))
		      (with-current-buffer orig-buffer
			(my-bot-say channel-buffer nick "Quitting from network %s." network))
		      (my-bot-save-all-databases)
		      (erc-cmd-QUIT (or reason "I'm outta here!"))
		      (return nil))))
		t)
	      (my-bot-say channel-buffer nick "Sorry, I'm not connected to that network."))))))

(put 'my-bot-cmd-quit 'help "Tells me to quit from the specified IRC network.")
(put 'my-bot-cmd-quit 'usage "Usage: .quit PASSWORD NETWORK [ REASON STRING ]")

(defun my-bot-cmd-quote (channel-buffer nick cmd args argstring argstring2)
  "Implements the .quote command."
  (if (> (length args) 0)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "quote"))
    (my-bot-say channel-buffer nick (my-command "~/bin/quote"))))

(put 'my-bot-cmd-quote 'help "Displays a pithy quote.")
(put 'my-bot-cmd-quote 'usage "Usage: .quote")

(defalias 'my-bot-cmd-q 'my-bot-cmd-quote)

(defun my-bot-cmd-restart (channel-buffer nick cmd args argstring argstring2)
  "Implements the .restart command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "restart"))
    (when (my-bot-check-password (car args) nick channel-buffer)
      (my-bot-say channel-buffer nick "Restarting ...")

      (dolist (serverbuf (erc-buffer-list 'erc-server-buffer-p))
	(with-current-buffer serverbuf
	  (when erc-connected
	    (my-bot-log "Quitting from network %s." (erc-network-name))
	    (erc-cmd-QUIT (if (not (string= argstring2 ""))
			      argstring2
			    "Restarting ...")))))

      ;; Give the QUIT commands time to be processed, then restart.  Is this
      ;; enough time?
      (run-at-time 5 nil 'my-bot-restart))))

(put 'my-bot-cmd-restart 'help "Tells me to quit from all networks and restart myself.")
(put 'my-bot-cmd-restart 'usage "Usage: .restart PASSWORD [ QUIT REASON ]")

(defun my-bot-cmd-rfc (channel-buffer nick cmd args argstring argstring2)
  "Implements the .rfc command."
  (let ((rfcnum (downcase (delete ?' argstring)))
	(newest-synonyms '("newest" "last" "latest" "new" "recent" "highest")))
    (if (and (not (member-ignore-case rfcnum newest-synonyms))
	     (or (null args)
		 (not (string-match "^[0-9]+$" rfcnum))))
	(my-bot-say channel-buffer nick (my-bot-lookup-usage "rfc"))
      (let ((summary (if (member-ignore-case rfcnum newest-synonyms)
			 (my-command (format "tail -1 %s/rfcindex 2>/dev/null"
					     my-bot-data-directory))
		       (my-command (format "grep '^%s = ' %s/rfcindex 2>/dev/null"
					   rfcnum my-bot-data-directory)))))
	(if (not (string-match "^\\([0-9]+\\) = \\(.*\\)" summary))
	    (my-bot-say channel-buffer nick "Sorry, I couldn't find that RFC.")
	  (setq rfcnum (match-string 1 summary)
		summary (match-string 2 summary))
	  (my-bot-say channel-buffer nick "RFC %s is at http://www.ietf.org/rfc/rfc%s.txt -- %s"
		      rfcnum rfcnum summary))))))

(put 'my-bot-cmd-rfc 'help "Looks up an RFC by number and shows a summary.")
(put 'my-bot-cmd-rfc 'usage "Usage: .rfc NNNN (where NNNN is a valid RFC number or the word 'newest')")

(defun my-bot-cmd-rfcupdate (channel-buffer nick cmd args argstring argstring2)
  "Implements the .rfcupdate command."
  (if args
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "rfcupdate"))

    (let ((buf (get-buffer "*Async Shell Command*")))
      (if buf
	  (kill-buffer buf)))
    (shell-command "rfcindex > $HOME/.ebot/rfcindex &")
    (my-bot-say channel-buffer nick "Updating RFC index in the background.")))

(put 'my-bot-cmd-rfcupdate 'help "Launches a process to update birny's RFC index database in the background.")
(put 'my-bot-cmd-rfcupdate 'usage "Usage: .rfcupdate")

(defun my-bot-cmd-rfcgrep (channel-buffer nick cmd args argstring argstring2)
  "Implements the .rfcgrep command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "rfcgrep"))
    ;; Remove starting and ending '"' characters.
    (if (string-match "^\"\\(.*\\)\"$" argstring)
	(setq argstring (match-string 1 argstring)))
    (setq argstring (delete ?' argstring))
    (let ((lines (split-string (my-command (format "egrep -i '%s' %s/rfcindex 2>/dev/null"
						   argstring
						   my-bot-data-directory))
			       "\n"))
	  (index 1))
      (if (null lines)
	  (my-bot-say channel-buffer nick "Sorry, no RFCs matched that regular expression.")
	(my-bot-say channel-buffer nick "[Showing %d of %d matches.]"
		    (min 5 (length lines)) (length lines))
	(dolist (line (subseq lines 0 (min 5 (length lines))))
	  (my-bot-say channel-buffer nick "%d. %s" index line)
	  (incf index))))))

(put 'my-bot-cmd-rfcgrep 'help "Searches the RFC index for a regular expression.")
(put 'my-bot-cmd-rfcgrep 'usage "Usage: .rfcgrep REGULAR EXPRESSION (don't quote the regular expression, even if it contains spaces or '\\' characters)")

(defun my-bot-cmd-save (channel-buffer nick cmd args argstring argstring2)
  "Implements the .save command."
  (if (> (length args) 0)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "save"))
    (when (my-bot-privcheck nick channel-buffer)
      (let ((start (current-time))
	    duration)
	(my-bot-save-all-databases)
	(setq duration (my-bot-duration-to-string (time-since start)))
	(my-bot-say channel-buffer nick
		    "Saved databases (%d terms, %d help topics, %d nicks seen, %d away messages, %d channel topics).  Save time: %s."
		    (length my-bot-terms) (length my-bot-help) (length my-bot-seen-nicks)
		    (length my-bot-away-messages) (length my-bot-channel-topics)
		    duration)))))

(put 'my-bot-cmd-save 'help "Tells me to save all of my databases.")
(put 'my-bot-cmd-save 'usage "Usage: .save")

(defun my-bot-cmd-say (channel-buffer nick cmd args argstring argstring2)
  "Implements the .say command."
  (if (< (length args) 2)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "say"))
    (when (my-bot-check-password (car args) nick channel-buffer)
      (let ((saychan (nth 1 args)))
	(if (erc-channel-p saychan)
	    (if (null (get-buffer saychan))
		(my-bot-say channel-buffer nick "Dude, I'm not in that channel.")
	      (my-bot-say saychan nick (my-bot-strip-first-word argstring2)))
	  (my-bot-say channel-buffer nick "Dude, '%s' doesn't look like a channel name to me."
		      saychan))))))

(put 'my-bot-cmd-say 'help "Makes me say something in a channel.")
(put 'my-bot-cmd-say 'usage "Usage: .say PASSWORD [ CHANNEL ] TEXT")

(defun my-bot-cmd-seen (channel-buffer nick cmd args argstring argstring2)
  "Implements the .seen command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "seen"))
    (if (string= argstring my-bot-nick)
	(my-bot-say channel-buffer nick "Yeah, the last time I looked in a mirror.")
      (let ((entry (assoc-ignore-case argstring my-bot-seen-nicks))
	    (awayentry (my-bot-lookup-away-message argstring)))
	(if (null entry)
	    (my-bot-say channel-buffer nick
			(if awayentry
			    "I've never seen %s before, but ..."
			  "I've never seen %s before.")
			argstring)
	  ;; Each entry has this form:
	  ;; (NICK ACTION LASTSEEN LASTSEENCHANNEL)
	  (my-bot-say channel-buffer nick "%s was last seen %s at %s GMT (%s ago)."
		      (car entry)
		      (let ((seen-action (nth 1 entry))
			    (seen-channel (nth 3 entry)))
			(if (string= "" seen-channel)
			    seen-action
			  (concat seen-action " " seen-channel)))
		      (format-time-string "%Y-%m-%d %H:%M:%S" (nth 2 entry) 'gmt)
		      (my-bot-duration-to-string (time-since (nth 2 entry)))))
	(if awayentry
	    (my-bot-say channel-buffer nick "%s left this away message at %s (%s ago): \"%s\""
			(progn
			  (string-match "^\\([^@]+\\)@" (car awayentry))
			  (match-string 1 (car awayentry)))
			(format-time-string "%Y-%m-%d %H:%M:%S GMT" (nth 1 awayentry) 'gmt)
			(my-bot-duration-to-string (time-since (nth 1 awayentry)))
			(nth 3 awayentry)))))))

(put 'my-bot-cmd-seen 'help "Shows when and where I last saw someone.")
(put 'my-bot-cmd-seen 'usage "Usage: .seen NICK")

(defun my-bot-cmd-sh (channel-buffer nick cmd args argstring argstring2)
  "Runs ARGSTRING through a shell and /msg's the output lines to the user."
  (if (< (length args) 2)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "sh"))
    (if (my-bot-check-password (car args) nick channel-buffer)
	(let ((lines (split-string (my-command (erc-trim-string argstring2)) "\n")))
	  (when lines
	    (if (<= (length lines) 5)
		(if (> (length lines) 1)
		    (my-bot-say channel-buffer nick "[Showing %d lines.]" (length lines)))
	      (my-bot-say channel-buffer nick "[Showing 5 of %d lines.]" (length lines))
	      (setq lines (subseq lines 0 5)))

	    (dolist (line lines)
	      (my-bot-say channel-buffer nick line)))))))

(put 'my-bot-cmd-sh 'help "Runs a command using /bin/sh and displays the output.")
(put 'my-bot-cmd-sh 'usage "Usage: .sh PASSWORD COMMAND")

(defun my-bot-cmd-slashdot (channel-buffer nick cmd args argstring argstring2)
  "Implements the .slashdot command."
  (if nil
      (my-bot-say channel-buffer nick
		  "Sorry, the .slashdot command is being serviced.  Try again later.")
    (if (null args)
	(my-bot-say channel-buffer nick (my-bot-lookup-usage "slashdot"))
      (let ((searchterm (delete ?' argstring))
	    lines output)
	(setq lines (split-string (my-command (format "%s/slashdot '%s' 2>/dev/null"
						      my-bot-bin-directory searchterm))
				  "\n"))
	(if (null lines)
	    (my-bot-say channel-buffer nick "Sorry, Slashdot gave me nothing for that.")
	  (my-bot-say channel-buffer nick (car lines))
	  (if (= (length lines) 2)
	      (my-bot-say channel-buffer nick (format "Also: %s" (car (cdr lines))))))))))

(put 'my-bot-cmd-slashdot 'help "Searches Slashdot for a given term.")
(put 'my-bot-cmd-slashdot 'usage "Usage: .slashdot TERM")

(defalias (intern "my-bot-cmd-/.") 'my-bot-cmd-slashdot)

(defun my-bot-cmd-slashnews (channel-buffer nick cmd args argstring argstring2)
  "Implements the .slashnews command."
  (if (null my-bot-slashnews-headlines)
      (my-bot-say channel-buffer nick
		  "Sorry, there are no Slashdot headlines in my database right now.")
    (dolist (headline
	     (subseq my-bot-slashnews-headlines 0 (min 5 (length my-bot-slashnews-headlines))))
      (my-bot-say channel-buffer nick headline))))

(put 'my-bot-cmd-slashnews 'help "Displays recent Slashdot headlines.")
(put 'my-bot-cmd-slashnews 'usage "Usage: .slashnews")

(defun my-bot-cmd-stats (channel-buffer nick cmd args argstring argstring2)
  "Implements the .stats command."
  (let ((termcount (length my-bot-terms))
	(helpcount (+ (length my-bot-help) (length my-bot-commands)))
	(nickcount (length my-bot-seen-nicks))
	output)
    (setq output
	  (concat (format "Terminology database: %d.  Help database: %d.  Nick database: %d."
			  termcount helpcount nickcount)
		  (format-time-string "  Started: %Y-%m-%d %H:%M:%S EDT"
				      my-bot-start-time)
		  (format " (up for %s)."
			  (my-bot-duration-to-string (time-since my-bot-start-time)))
		  (if my-bot-last-update-time
		      (format-time-string "  Updated: %Y-%m-%d %H:%M:%S EDT."
					  my-bot-last-update-time))))
    (if (> my-bot-burp-count 0)
	(setq output (concat output (format "  Burps: %d." my-bot-burp-count))))
    (my-bot-say channel-buffer nick output)))

(put 'my-bot-cmd-stats 'help "Shows statistics about me and my databases.")
(put 'my-bot-cmd-stats 'usage "Usage: .stats")

(defun my-bot-cmd-std (channel-buffer nick cmd args argstring argstring2)
  "Implements the .std command."
  (let ((stdnum (car args)))
    (if (or (null args)
	    (not (string-match "^[0-9]+$" stdnum)))
	(my-bot-say channel-buffer nick (my-bot-lookup-usage "std"))
      (let ((results (my-command (format "grep '^%s = ' %s/stdindex 2>/dev/null"
					 stdnum my-bot-data-directory))))
	(if (string= "" results)
	    (my-bot-say channel-buffer nick "Sorry, I couldn't find that STD.")
	  (dolist (line (split-string results "\n"))
	    (if (string-match "^[0-9]+ = \\(.*\\)$" line)
		(my-bot-say channel-buffer nick "STD %s is %s" stdnum (match-string 1 line)))))))))

(put 'my-bot-cmd-std 'help "Looks up an STD by number and shows a summary.")
(put 'my-bot-cmd-std 'usage "Usage: .std NN (where NN is a valid STD number).")

(defun my-bot-cmd-stdupdate (channel-buffer nick cmd args argstring argstring2)
  "Implements the .stdupdate command."
  (if args
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "stdupdate"))
    (shell-command "stdindex > $HOME/.ebot/stdindex &")
    (my-bot-say channel-buffer nick "Updating STD index in the background.")))

(put 'my-bot-cmd-stdupdate 'help "Launches a process to update birny's STD index database in the background.")
(put 'my-bot-cmd-stdupdate 'usage "Usage: .stdupdate")

(defun my-bot-cmd-tell (channel-buffer nick cmd args argstring argstring2)
  "Implements the .tell command."
  (if (< (length args) 2)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "tell"))
    (let* ((tellee (car args))
	   (term (if (string-match "^about\\s-+\\(.*\\)$" argstring2)
		     (match-string 1 argstring2)
		   argstring2))
	   (dbentry (my-bot-lookup term 'countit)))
      (if (null dbentry)
	  (let ((letter (aref term 0)))
	    (my-bot-say channel-buffer nick "I don't know that term.  Type \".index %c\" to see the terms in my database starting with '%c'." letter letter))
	(my-bot-say nil tellee (format "(From %s): %s" nick (nth 1 dbentry)))
	(if (null channel-buffer)
	    (my-bot-say channel-buffer nick "Done."))))))

(put 'my-bot-cmd-tell 'help "Makes me tell someone the definition of a term in my terminology database.")
(put 'my-bot-cmd-tell 'usage "Usage: .tell NICK [about] TERM")


(defun my-bot-cmd-time (channel-buffer nick cmd args argstring argstring2)
  "Implements the .time command."
  (let (time location)
    (if args
	(setq location argstring)
      (setq location "London"
	    argstring "Greenwich, England,"))

    ;; Special case some locations.
    (if (member-ignore-case argstring '("New York" "New York City" "NYC" "Washington DC"
					"Washington D.C." "Washington, D.C." "Washington, DC"
					"Washington"))
	(setq location "Boston"))

    (setq location (delete ?' (delete 34 location))	;; 34 = '"'
	  location (mapconcat 'char-to-string
			      (substitute ?+ 32 (string-to-list location)) "")
	  time (my-command (format "wget -q -O - 'http://www.timeanddate.com/worldclock/results.html?query=\"%s\"' | grep 'Current time' | sed -e 's/<[^>]*>//g' -e 's/^.*Current time//' -e 's;UTC/GMT Offset.*;;'"
				     location))
	  time (delete ?\n time))

    (if (< (length time) 10)
	(my-bot-say channel-buffer nick "Sorry, I don't know the time in %s." argstring)

      ;; At this point, time has this format:
      ;;    Wednesday, December 31, 2003 at 6:30:54 AM EET.
      (if (string-match "^\\(.*\\) at \\(.*[AP]M\\)\\s-*\\(.*\\)$" time)
	  (let ((tz (match-string 3 time)))
	    (setq time (concat (match-string 2 time) ", " (match-string 1 time)
			       (if (not (string= "" tz))
				   (concat " (" tz ")"))))))
      
      ;; Collapse multiple spaces to a single space.
      (while (string-match "\\(  +\\)" time)
	(setq time (replace-match " " nil nil time 1)))

      (my-bot-say channel-buffer nick "The time in %s is %s."
		  (progn
		    (aset argstring 0 (upcase (aref argstring 0)))
		    argstring)
		  time))))
    
(put 'my-bot-cmd-time 'help "Displays the time in a given city (or in GMT if no city is given).")
(put 'my-bot-cmd-time 'usage "Usage: .time [ CITY ]")

;;(defun my-bot-cmd-time (channel-buffer nick cmd args argstring argstring2)
;;  "Implements the .time command."
;;  (if (and args
;;	   (not (string-match "^[+-][0-9]+$" argstring))
;;	   (not (assoc-ignore-case argstring my-bot-timezones)))
;;      (my-bot-say channel-buffer nick (my-bot-lookup-usage "time"))
;;
;;    (let (tzadjust
;;	  (time (my-command
;;		 "wget -q --timeout=10 -O - 'http://www.time.gov/timezone.cgi?Eastern/d/-5' | perl -e '@buffer = <STDIN>; print \"$1 $2\" if \"@buffer\" =~ /font size=\"7\" color=\"white\"><b>([0-9:]+?)<br>.*<font size=\"5\" color=\"white\">(.*?)<br>/s'"))
;;	  (tz (if args (assoc-ignore-case argstring my-bot-timezones))))
;;
;;      (setq tzadjust (+ 5 (if tz
;;			      (cdr tz)
;;			    (string-to-int (or (car args) "0")))))
;;
;;      ;; date --date='12:24:11 Tuesday, December 30, 2003' '+%H:%M:%S %A, %B %d, %Y'
;;      (setq time
;;	    (delete ?\n
;;		    (my-command (concat (format "/bin/date --date='%s + %d hours'"
;;						time tzadjust)
;;					" '+%H:%M:%S %A, %B %d, %Y'"))))
;;
;;      (my-bot-say channel-buffer nick "The USNO atomic clock says the time in timezone %s is %s."
;;		  (if tz
;;		      (upcase (car tz))
;;		    (format "GMT + %d hours" (- tzadjust 5)))
;;		  time))))

(defun my-bot-cmd-timers (channel-buffer nick cmd args argstring argstring2)
  "Implements the .timers command."
  (if args
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "timers"))
    (if (null timer-list)
	(my-bot-say channel-buffer nick "No timers are defined.")
      (my-bot-say channel-buffer nick
		  "The time is now %s." (format-time-string "%Y-%m-%d %H:%M:%S"))
      (dolist (timer timer-list)
	(let ((time (subseq timer 1 4))
	      (period (aref timer 4))
	      (function (aref timer 5)))
	  (if (symbolp function)
	      (my-bot-say channel-buffer nick
			  "At %s (%s): %s"
			  (format-time-string "%Y-%m-%d %H:%M:%S" (mapcar 'identity time))
			  (if period
			      (format "repeating every %d seconds" period)
			    "never to repeat")
			  (prin1-to-string function))))))))

(put 'my-bot-cmd-timers 'help "Displays the currently ticking timers and the functions they will execute.")
(put 'my-bot-cmd-timers 'usage "Usage: .timers")

(defun my-bot-cmd-tld (channel-buffer nick cmd args argstring argstring2)
  "Implements the .tld command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "tld"))
    (setq argstring (erc-trim-string argstring))
    (if (= ?. (aref argstring 0))
	(setq argstring (substring argstring 1)))
    (if (string= argstring "el")
	(my-bot-say channel-buffer nick "My homeland!  Emacsia.")
      (let ((answer (ignore-errors (what-domain argstring))))
	(if (null answer)
	    (my-bot-say channel-buffer nick "No such Top-Level Domain.")
	  (my-bot-say channel-buffer nick answer))))))

(put 'my-bot-cmd-tld 'help "Shows the country or domain class for a given Top-Level Domain.")
(put 'my-bot-cmd-tld 'usage "Usage: .tld TOP-LEVEL-DOMAIN")

(defalias 'my-bot-cmd-country 'my-bot-cmd-tld)

(defun my-bot-cmd-topic (channel-buffer nick cmd args argstring argstring2)
  "Implements the .topic command."
  (if (< (length args) 2)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "topic"))
    (when (my-bot-check-password (car args) nick channel-buffer)
      (let* ((network-name (erc-network-name))
	     (channel-name (nth 1 args))
	     (topic-channel-buffer (my-bot-get-erc-buffer network-name channel-name))
	     (topic-string (my-bot-strip-first-word argstring2))
	     (key (cons (downcase network-name) (downcase channel-name)))
	     (entry (assoc key my-bot-channel-topics)))
	(if (null topic-channel-buffer)
	    (my-bot-say channel-buffer nick "I'm not in channel %s." channel-name)
	  (if (not (my-bot-op-p my-bot-nick topic-channel-buffer))
	      (my-bot-say channel-buffer nick "I'm not an op in channel %s." channel-name)
	    (if (and (null entry)
		     (null topic-string))
		(my-bot-say channel-buffer nick
			    "Sorry, I have no record of the topic from that channel.")
	      (if topic-string
		  (progn
		    (my-bot-say channel-buffer nick "Setting topic in %s ..." channel-name)
		    (if (null entry)
			(add-to-list 'my-bot-channel-topics
				     `((,network-name . ,channel-name) . ,topic-string))
		      (setcdr entry topic-string))
		    ;;(my-bot-save-other)
		    (put 'my-bot-channel-topics 'needs-saving t))
		(my-bot-say channel-buffer nick "Resetting topic in %s ..." channel-name)
		(setq topic-string (cdr entry)))

	      (with-current-buffer topic-channel-buffer
		(erc-cmd-TOPIC topic-string)))))))))

(put 'my-bot-cmd-topic 'help "Makes me set the topic of the specified channel.")
(put 'my-bot-cmd-topic 'usage "Usage: .topic PASSWORD CHANNEL [ TOPIC-STRING ]")

(defun my-bot-cmd-undefhelp (channel-buffer nick cmd args argstring argstring2)
  "Implements the .undefhelp command."
  (if t
      (my-bot-say channel-buffer nick
		  "This command is temporarilly disabled while franl hacks on the help system.")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                   UNDER CONSTRUCTION                   ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (if (my-bot-privcheck nick channel-buffer)
	(if (null args)
	    (my-bot-say channel-buffer nick (my-bot-lookup-usage "undefhelp"))
	  (let* ((olddef (my-bot-lookup-help argstring))
		 (oldhelpterm (car olddef)))
	    (if (null olddef)
		(my-bot-say channel-buffer nick "Sorry, that term is not in my help database.")
	      (setq my-bot-help (delete olddef my-bot-help))
	      (my-bot-save-help)
	      (my-bot-say channel-buffer nick "Deleted \"%s\" from help database." oldhelpterm)))))))

(put 'my-bot-cmd-undefhelp 'help "Deletes help from the help database.")
(put 'my-bot-cmd-undefhelp 'usage "Usage: .undefhelp HELP-TERM")

(defun my-bot-cmd-update (channel-buffer nick cmd args argstring argstring2)
  "Implements the .update command."
  (if (null args)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "update"))
    (when (my-bot-check-password (car args) nick channel-buffer)
      (load "~/elisp/my-bot.el" 'noerror 'nomessage 'nosuffix)
      (setq my-bot-last-update-time (current-time))
      (my-bot-say channel-buffer nick "Code update complete."))))

(put 'my-bot-cmd-update 'help "Updates my source code to the latest version.")
(put 'my-bot-cmd-update 'usage "Usage: .update PASSWORD")

(defalias 'my-bot-cmd-u 'my-bot-cmd-update)

(defun my-bot-cmd-voice (channel-buffer nick cmd args argstring argstring2)
  "Implements the .voice and .unvoice commands."
  (if (< (length args) 3)
      (my-bot-say channel-buffer nick (my-bot-lookup-usage "voice"))
    (if (my-bot-check-password (car args) nick channel-buffer)
	(let* ((voice-channel-name (nth 1 args))
	       (nicks-to-voice (nthcdr 2 args))
	       (operation (if (string= cmd ".voice") "+v" "-v"))
	       (voice-channel-buffer (my-bot-get-erc-buffer (erc-network-name)
							    voice-channel-name)))
	  (if (null voice-channel-buffer)
	      (my-bot-say channel-buffer nick "I'm not in channel %s right now."
			  voice-channel-name)
	    (with-current-buffer voice-channel-buffer
	      (if (not (my-bot-op-p my-bot-nick voice-channel-buffer))
		  (my-bot-say channel-buffer nick "I'm not an op in channel %s!"
			      voice-channel-name)
		(if (string= (car nicks-to-voice) "*")
		    (setq nicks-to-voice (erc-get-channel-nickname-list)))
		(dolist (nick-to-voice nicks-to-voice)
		  (if (null (erc-get-channel-user nick-to-voice))
		      (my-bot-say channel-buffer nick "%s is not currently in channel %s."
				  nick-to-voice voice-channel-name)
		    (if (or (and (string= operation "+v")
				 (my-bot-voice-p nick-to-voice))
			    (and (string= operation "-v")
				 (not (my-bot-voice-p nick-to-voice))))
			(my-bot-say channel-buffer nick "%s %s voice in channel %s."
				    nick-to-voice (if (string= "+v" operation)
						      "already has"
						    "doesn't have")
				    voice-channel-name)
		      (my-bot-say channel-buffer nick "%s %s in %s ..."
				  (if (string= "+v" operation) "Voicing" "Unvoicing")
				  nick-to-voice voice-channel-name)
		      (erc-server-send (format "MODE %s %s %s" 
						voice-channel-name operation
						nick-to-voice))))))))))))

(put 'my-bot-cmd-voice 'help "Voices a nick in a given channel.")
(put 'my-bot-cmd-voice 'usage "Usage: .voice PASSWORD CHANNEL NICK [ NICK ... ]")

(defalias 'my-bot-cmd-unvoice 'my-bot-cmd-voice)

(defun my-bot-cmd-yow (channel-buffer nick cmd args argstring argstring2)
  "Implements the .yow command."
  (my-bot-say channel-buffer nick (replace-regexp-in-string "\n +" " " (yow))))

(put 'my-bot-cmd-yow 'help "Makes me say something that Zippy would say.")
(put 'my-bot-cmd-yow 'usage "Usage: .yow")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize my-bot-commands.  This has to be done here at the bottom of this
;; file so that all of the command functions have been defun'ed before we execute
;; this code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-bot-gencmds ()
  (let (commands)
    (mapatoms '(lambda (sym)
		 (let ((symname (symbol-name sym)))
		   (if (string-match "^my-bot-cmd-\\(.*\\)$" symname)
		       (setq commands (cons (concat "." (match-string 1 symname))
					    commands))))))

    ;; Add the "help" command specially.  We want both ".help" and "help" in the
    ;; command list.
    (setq commands (cons "help" commands))
    (setq my-bot-commands (sort commands #'string-lessp))))

(my-bot-gencmds)


(provide 'my-bot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 		    GNU GENERAL PUBLIC LICENSE
;; 		       Version 2, June 1991
;; 
;;  Copyright (C) 1989, 1991 Free Software Foundation, Inc.
;;                        59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;  Everyone is permitted to copy and distribute verbatim copies
;;  of this license document, but changing it is not allowed.
;; 
;; 			    Preamble
;; 
;;   The licenses for most software are designed to take away your
;; freedom to share and change it.  By contrast, the GNU General Public
;; License is intended to guarantee your freedom to share and change free
;; software--to make sure the software is free for all its users.  This
;; General Public License applies to most of the Free Software
;; Foundation's software and to any other program whose authors commit to
;; using it.  (Some other Free Software Foundation software is covered by
;; the GNU Library General Public License instead.)  You can apply it to
;; your programs, too.
;; 
;;   When we speak of free software, we are referring to freedom, not
;; price.  Our General Public Licenses are designed to make sure that you
;; have the freedom to distribute copies of free software (and charge for
;; this service if you wish), that you receive source code or can get it
;; if you want it, that you can change the software or use pieces of it
;; in new free programs; and that you know you can do these things.
;; 
;;   To protect your rights, we need to make restrictions that forbid
;; anyone to deny you these rights or to ask you to surrender the rights.
;; These restrictions translate to certain responsibilities for you if you
;; distribute copies of the software, or if you modify it.
;; 
;;   For example, if you distribute copies of such a program, whether
;; gratis or for a fee, you must give the recipients all the rights that
;; you have.  You must make sure that they, too, receive or can get the
;; source code.  And you must show them these terms so they know their
;; rights.
;; 
;;   We protect your rights with two steps: (1) copyright the software, and
;; (2) offer you this license which gives you legal permission to copy,
;; distribute and/or modify the software.
;; 
;;   Also, for each author's protection and ours, we want to make certain
;; that everyone understands that there is no warranty for this free
;; software.  If the software is modified by someone else and passed on, we
;; want its recipients to know that what they have is not the original, so
;; that any problems introduced by others will not reflect on the original
;; authors' reputations.
;; 
;;   Finally, any free program is threatened constantly by software
;; patents.  We wish to avoid the danger that redistributors of a free
;; program will individually obtain patent licenses, in effect making the
;; program proprietary.  To prevent this, we have made it clear that any
;; patent must be licensed for everyone's free use or not licensed at all.
;; 
;;   The precise terms and conditions for copying, distribution and
;; modification follow.
;; 
;; 		    GNU GENERAL PUBLIC LICENSE
;;    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;; 
;;   0. This License applies to any program or other work which contains
;; a notice placed by the copyright holder saying it may be distributed
;; under the terms of this General Public License.  The "Program", below,
;; refers to any such program or work, and a "work based on the Program"
;; means either the Program or any derivative work under copyright law:
;; that is to say, a work containing the Program or a portion of it,
;; either verbatim or with modifications and/or translated into another
;; language.  (Hereinafter, translation is included without limitation in
;; the term "modification".)  Each licensee is addressed as "you".
;; 
;; Activities other than copying, distribution and modification are not
;; covered by this License; they are outside its scope.  The act of
;; running the Program is not restricted, and the output from the Program
;; is covered only if its contents constitute a work based on the
;; Program (independent of having been made by running the Program).
;; Whether that is true depends on what the Program does.
;; 
;;   1. You may copy and distribute verbatim copies of the Program's
;; source code as you receive it, in any medium, provided that you
;; conspicuously and appropriately publish on each copy an appropriate
;; copyright notice and disclaimer of warranty; keep intact all the
;; notices that refer to this License and to the absence of any warranty;
;; and give any other recipients of the Program a copy of this License
;; along with the Program.
;; 
;; You may charge a fee for the physical act of transferring a copy, and
;; you may at your option offer warranty protection in exchange for a fee.
;; 
;;   2. You may modify your copy or copies of the Program or any portion
;; of it, thus forming a work based on the Program, and copy and
;; distribute such modifications or work under the terms of Section 1
;; above, provided that you also meet all of these conditions:
;; 
;;     a) You must cause the modified files to carry prominent notices
;;     stating that you changed the files and the date of any change.
;; 
;;     b) You must cause any work that you distribute or publish, that in
;;     whole or in part contains or is derived from the Program or any
;;     part thereof, to be licensed as a whole at no charge to all third
;;     parties under the terms of this License.
;; 
;;     c) If the modified program normally reads commands interactively
;;     when run, you must cause it, when started running for such
;;     interactive use in the most ordinary way, to print or display an
;;     announcement including an appropriate copyright notice and a
;;     notice that there is no warranty (or else, saying that you provide
;;     a warranty) and that users may redistribute the program under
;;     these conditions, and telling the user how to view a copy of this
;;     License.  (Exception: if the Program itself is interactive but
;;     does not normally print such an announcement, your work based on
;;     the Program is not required to print an announcement.)
;; 
;; These requirements apply to the modified work as a whole.  If
;; identifiable sections of that work are not derived from the Program,
;; and can be reasonably considered independent and separate works in
;; themselves, then this License, and its terms, do not apply to those
;; sections when you distribute them as separate works.  But when you
;; distribute the same sections as part of a whole which is a work based
;; on the Program, the distribution of the whole must be on the terms of
;; this License, whose permissions for other licensees extend to the
;; entire whole, and thus to each and every part regardless of who wrote it.
;; 
;; Thus, it is not the intent of this section to claim rights or contest
;; your rights to work written entirely by you; rather, the intent is to
;; exercise the right to control the distribution of derivative or
;; collective works based on the Program.
;; 
;; In addition, mere aggregation of another work not based on the Program
;; with the Program (or with a work based on the Program) on a volume of
;; a storage or distribution medium does not bring the other work under
;; the scope of this License.
;; 
;;   3. You may copy and distribute the Program (or a work based on it,
;; under Section 2) in object code or executable form under the terms of
;; Sections 1 and 2 above provided that you also do one of the following:
;; 
;;     a) Accompany it with the complete corresponding machine-readable
;;     source code, which must be distributed under the terms of Sections
;;     1 and 2 above on a medium customarily used for software interchange; or,
;; 
;;     b) Accompany it with a written offer, valid for at least three
;;     years, to give any third party, for a charge no more than your
;;     cost of physically performing source distribution, a complete
;;     machine-readable copy of the corresponding source code, to be
;;     distributed under the terms of Sections 1 and 2 above on a medium
;;     customarily used for software interchange; or,
;; 
;;     c) Accompany it with the information you received as to the offer
;;     to distribute corresponding source code.  (This alternative is
;;     allowed only for noncommercial distribution and only if you
;;     received the program in object code or executable form with such
;;     an offer, in accord with Subsection b above.)
;; 
;; The source code for a work means the preferred form of the work for
;; making modifications to it.  For an executable work, complete source
;; code means all the source code for all modules it contains, plus any
;; associated interface definition files, plus the scripts used to
;; control compilation and installation of the executable.  However, as a
;; special exception, the source code distributed need not include
;; anything that is normally distributed (in either source or binary
;; form) with the major components (compiler, kernel, and so on) of the
;; operating system on which the executable runs, unless that component
;; itself accompanies the executable.
;; 
;; If distribution of executable or object code is made by offering
;; access to copy from a designated place, then offering equivalent
;; access to copy the source code from the same place counts as
;; distribution of the source code, even though third parties are not
;; compelled to copy the source along with the object code.
;; 
;;   4. You may not copy, modify, sublicense, or distribute the Program
;; except as expressly provided under this License.  Any attempt
;; otherwise to copy, modify, sublicense or distribute the Program is
;; void, and will automatically terminate your rights under this License.
;; However, parties who have received copies, or rights, from you under
;; this License will not have their licenses terminated so long as such
;; parties remain in full compliance.
;; 
;;   5. You are not required to accept this License, since you have not
;; signed it.  However, nothing else grants you permission to modify or
;; distribute the Program or its derivative works.  These actions are
;; prohibited by law if you do not accept this License.  Therefore, by
;; modifying or distributing the Program (or any work based on the
;; Program), you indicate your acceptance of this License to do so, and
;; all its terms and conditions for copying, distributing or modifying
;; the Program or works based on it.
;; 
;;   6. Each time you redistribute the Program (or any work based on the
;; Program), the recipient automatically receives a license from the
;; original licensor to copy, distribute or modify the Program subject to
;; these terms and conditions.  You may not impose any further
;; restrictions on the recipients' exercise of the rights granted herein.
;; You are not responsible for enforcing compliance by third parties to
;; this License.
;; 
;;   7. If, as a consequence of a court judgment or allegation of patent
;; infringement or for any other reason (not limited to patent issues),
;; conditions are imposed on you (whether by court order, agreement or
;; otherwise) that contradict the conditions of this License, they do not
;; excuse you from the conditions of this License.  If you cannot
;; distribute so as to satisfy simultaneously your obligations under this
;; License and any other pertinent obligations, then as a consequence you
;; may not distribute the Program at all.  For example, if a patent
;; license would not permit royalty-free redistribution of the Program by
;; all those who receive copies directly or indirectly through you, then
;; the only way you could satisfy both it and this License would be to
;; refrain entirely from distribution of the Program.
;; 
;; If any portion of this section is held invalid or unenforceable under
;; any particular circumstance, the balance of the section is intended to
;; apply and the section as a whole is intended to apply in other
;; circumstances.
;; 
;; It is not the purpose of this section to induce you to infringe any
;; patents or other property right claims or to contest validity of any
;; such claims; this section has the sole purpose of protecting the
;; integrity of the free software distribution system, which is
;; implemented by public license practices.  Many people have made
;; generous contributions to the wide range of software distributed
;; through that system in reliance on consistent application of that
;; system; it is up to the author/donor to decide if he or she is willing
;; to distribute software through any other system and a licensee cannot
;; impose that choice.
;; 
;; This section is intended to make thoroughly clear what is believed to
;; be a consequence of the rest of this License.
;; 
;;   8. If the distribution and/or use of the Program is restricted in
;; certain countries either by patents or by copyrighted interfaces, the
;; original copyright holder who places the Program under this License
;; may add an explicit geographical distribution limitation excluding
;; those countries, so that distribution is permitted only in or among
;; countries not thus excluded.  In such case, this License incorporates
;; the limitation as if written in the body of this License.
;; 
;;   9. The Free Software Foundation may publish revised and/or new versions
;; of the General Public License from time to time.  Such new versions will
;; be similar in spirit to the present version, but may differ in detail to
;; address new problems or concerns.
;; 
;; Each version is given a distinguishing version number.  If the Program
;; specifies a version number of this License which applies to it and "any
;; later version", you have the option of following the terms and conditions
;; either of that version or of any later version published by the Free
;; Software Foundation.  If the Program does not specify a version number of
;; this License, you may choose any version ever published by the Free Software
;; Foundation.
;; 
;;   10. If you wish to incorporate parts of the Program into other free
;; programs whose distribution conditions are different, write to the author
;; to ask for permission.  For software which is copyrighted by the Free
;; Software Foundation, write to the Free Software Foundation; we sometimes
;; make exceptions for this.  Our decision will be guided by the two goals
;; of preserving the free status of all derivatives of our free software and
;; of promoting the sharing and reuse of software generally.
;; 
;; 			    NO WARRANTY
;; 
;;   11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
;; FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
;; OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
;; PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
;; OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
;; TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE
;; PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
;; REPAIR OR CORRECTION.
;; 
;;   12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
;; WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
;; REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
;; INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING
;; OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED
;; TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY
;; YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER
;; PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGES.
;; 
;; 		     END OF TERMS AND CONDITIONS
;; 
;; 	    How to Apply These Terms to Your New Programs
;; 
;;   If you develop a new program, and you want it to be of the greatest
;; possible use to the public, the best way to achieve this is to make it
;; free software which everyone can redistribute and change under these terms.
;; 
;;   To do so, attach the following notices to the program.  It is safest
;; to attach them to the start of each source file to most effectively
;; convey the exclusion of warranty; and each file should have at least
;; the "copyright" line and a pointer to where the full notice is found.
;; 
;;     <one line to give the program's name and a brief idea of what it does.>
;;     Copyright (C) <year>  <name of author>
;; 
;;     This program is free software; you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation; either version 2 of the License, or
;;     (at your option) any later version.
;; 
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;; 
;;     You should have received a copy of the GNU General Public License
;;     along with this program; if not, write to the Free Software
;;     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;; 
;; 
;; Also add information on how to contact you by electronic and paper mail.
;; 
;; If the program is interactive, make it output a short notice like this
;; when it starts in an interactive mode:
;; 
;;     Gnomovision version 69, Copyright (C) year name of author
;;     Gnomovision comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
;;     This is free software, and you are welcome to redistribute it
;;     under certain conditions; type `show c' for details.
;; 
;; The hypothetical commands `show w' and `show c' should show the appropriate
;; parts of the General Public License.  Of course, the commands you use may
;; be called something other than `show w' and `show c'; they could even be
;; mouse-clicks or menu items--whatever suits your program.
;; 
;; You should also get your employer (if you work as a programmer) or your
;; school, if any, to sign a "copyright disclaimer" for the program, if
;; necessary.  Here is a sample; alter the names:
;; 
;;   Yoyodyne, Inc., hereby disclaims all copyright interest in the program
;;   `Gnomovision' (which makes passes at compilers) written by James Hacker.
;; 
;;   <signature of Ty Coon>, 1 April 1989
;;   Ty Coon, President of Vice
;; 
;; This General Public License does not permit incorporating your program into
;; proprietary programs.  If your program is a subroutine library, you may
;; consider it more useful to permit linking proprietary applications with the
;; library.  If this is what you want to do, use the GNU Library General
;; Public License instead of this License.
