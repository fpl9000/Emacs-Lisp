;; My personal ERC configuration file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Issues/Questions:
;;
;; o Does killing a channel buffer with "C-x k RET" or from the Buffer Menu properly
;;   part the channel and close the devoted frame?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bugs:
;;
;; o /PART parts the channel but does not delete the channel buffer or close the
;;   devoted frame.
;;
;; o my-erc-make-frame-devoted needs to set the frame title.
;;
;; o Closing a devoted frame with the mouse leaves the ERC buffer in existence.  Is
;;   there a hook that runs when frames are closed?  If so, use it to kill the ERC
;;   buffer shown in that frame.  If not, how can this be implemented?
;;
;; o When icomplete increases the size of the minibuffer, the last line of channel
;;   buffer output can stop being positioned at the bottom of the window.  Check my
;;   use of function my-erc-position-prompt in this case.
;;
;; o During input of long lines, the last line of channel buffer output can stop
;;   being positioned at the bottom of the window.  Check my use of function
;;   my-erc-position-prompt in this case.
;;
;; o Typing 'q' in an ERC Keywords frame (showing messages after being away) doesn't
;;   close the frame.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ERC bugs:
;;
;; o erc-kill-channel doesn't check that the value returned by erc-default-target is
;;   non-nil, which can cause a "PART nil" command to be sent to the server.  Not yet
;;   reported.
;;
;; o ERC's standard nick completion doesn't cycle through available completions when
;;   pcomplete-cycle-completions is non-nil.
;;
;; o erc-kill-query-buffers can kill every channel buffer on every server.  Not yet
;;   reported.
;;
;; o erc-kill-query-buffers fails to check if parameter PROCESS is nil.  Reported as
;;   bug #21187.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Improvements:
;;
;; o Switch to erc-with-all-buffers-of-server instead of using dolist.
;;
;; o Always switch to " *ERC Pre-connect Buffer*" before connecting, not just when
;;   a disconnected server buffer already exists.  Put informative text in the buffer
;;   about the pending connection: date/time, network, nick, real name, ...
;;
;; o my-erc-join-channel offers completions for all channels from every network.
;;   Make it offer only channels from the current network.
;;
;; o Write my-erc-popup to wrap calls to my-popup (and not do them if
;;   my-erc-no-popups is non-nil).  Currently, this code is replicated in several
;;   places.
;;
;; o Make erc-cmd-NETWORK report an error if the new network name is already in use.
;;
;; o Channel list improvements:
;;
;;      o Bind 'q' and 'C-x C-c' to my-kill-buffer-window-and-frame (with confirmation).
;;      o Turn off Font Lock mode in the channel list buffer.
;;      o Hide Unicode characters that can't be rendered.
;;      o Set frame title to "NETWORK channel list"
;;      o Bind '?' to show help.
;;      o Bind 's' to sort by channel name.
;;      o Make closing the frame with mouse act like 'q' (above).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; IRC protocol to connect and join a channel:
;;
;;   USER franl2 0 * :Fran
;;   NICK franl2
;;   JOIN #channel
;;   PING <server-hostname>
;;   PONG franl2

(require 'time-stamp)
(require 'erc)
(require 'erc-button)
(require 'erc-dcc)
(require 'erc-log)
(require 'erc-match)
(require 'erc-pcomplete)
(require 'erc-truncate)
(require 'erc-ring)
(require 'erc-fill)
(require 'erc-notify)
(require 'erc-netsplit)
(require 'erc-sasl)     ;; See ~/elisp/erc-sasl.el.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patches.  Remove these if/when they are merged into the official sources.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Patched version of erc-login to work with erc-sasl.el.  See
;; https://www.emacswiki.org/emacs/ErcSASL for details.
(defun erc-login ()
  "Perform user authentication at the IRC server."
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
		   (erc-current-nick)
		   (user-login-name)
		   (or erc-system-name (system-name))
		   erc-session-server
		   erc-session-user-full-name))
  (if erc-session-password
      (erc-server-send (format "PASS %s" erc-session-password))
    (message "Logging in without password"))

  ;; If SASL is being used to authenticate, tell the server.  The response handlers
  ;; defined in erc-sasl.el handle it from here.
  (when (and (featurep 'erc-sasl)
             (erc-sasl-use-sasl-p))
    (erc-server-send "CAP REQ :sasl"))

  (erc-server-send (format "NICK %s" (erc-current-nick)))
  (erc-server-send
   (format "USER %s %s %s :%s"
	   ;; hacked - S.B.
	   (if erc-anonymous-login erc-email-userid (user-login-name))
	   "0" "*"
	   erc-session-user-full-name))
  (erc-update-mode-line))

;; ;; Patched version of erc-kill-channel to fix bug #23700 that I reported in this
;; ;; email to the Emacs bugs mailing list:
;; ;;
;; ;; http://thread.gmane.org/gmane.emacs.bugs/119119
;; 
;; (defun erc-kill-channel ()
;;   "Sends a PART command to the server when the channel buffer is killed.
;; This function should be on `erc-kill-channel-hook'."
;;   (when (erc-server-process-alive)
;;     (let ((my-target (erc-default-target)))
;;       (if my-target
;;           (erc-server-send (format "PART %s :%s" my-target
;;                                    (funcall erc-part-reason nil))
;;                            nil my-target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-configurable Variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(makunbound 'my-erc-default-networkid)
(defvar my-erc-default-networkid "Freenode SSL Any"
  "The default network ID that function my-erc-networkid-connect ('\\[my-erc-networkid-connect]')
should connect to.")

(makunbound 'my-erc-networks)
(defvar my-erc-networks
  '(("freenode"
     ;; Freenode servers listen on ports 6665-6667, 7000, 7070, and 8000-8002
     ;; SSL connections should be made to port 6697.
     ("localhost"       "localhost" 6667 nil nil)
     ("Any"             "chat.freenode.net" 8002 nil nil)
     ("SSL Any"         "ssl:chat.freenode.net" 6697 nil nil)
     ("US"              "chat.us.freenode.net" 6667 nil nil)
     ("Dallas TX"       "asimov.freenode.net" 6667 nil nil)
     ("Washington DC"   "card.freenode.net" 6667 nil nil)
     ("Ashburn VA"      "dickson.freenode.net" 6667 nil nil)
     ("Chicago IL"      "morgan.freenode.net" 6667 nil nil)
     ("SSL Chicago IL"  "ssl:morgan.freenode.net" 6697 nil nil)
     ("SSL Sweden"      "ssl:leguin.freenode.net" 6697 nil nil)
     ("San Jose CA"     "weber.freenode.net" 6667 nil nil)
     ("Ask ..."         "?" 6667 nil nil))

    ("OFTC"
     ;; SSL connections should be made to port 6697.
     ("localhost"       "localhost" 6669 nil nil)
     ("SSL Main"        "ssl:irc.oftc.net" 6697 nil nil)
     ("Main"            "irc.oftc.net" 6667 nil nil)
     ("IPv4"            "irc4.oftc.net" 6667 nil nil)
     ("SSL IPv4"        "irc4.oftc.net" 6667 nil nil))

    ("IRCnet"
     ("UTwente NL"      "irc.snt.utwente.nl" 6667 nil nil)
     ("Austria"         "vienna-ng.irc.at" 6667 nil nil)
     ("Germany"         "irc.man-da.de" 6667 nil nil)
     ("Prague"          "irc.felk.cvut.cz" 6667 nil nil)
     ("Italy"           "ircd.tiscali.it" 2001 nil nil))

    ("QuakeNet"
     ("Any"             "irc.quakenet.org" 6667 nil nil)
     ("US"              "blacklotus.ca.us.quakenet.org" 6667 nil nil)
     ("UK"              "b0rk.uk.quakenet.org" 6667 nil nil)
     ("Denmark"         "jubii2.dk.quakenet.org" 6667 nil nil)
     ("Norway"          "underworld1.no.quakenet.org" 6667 nil nil))

    ("EFNet"
     ("localhost"       "localhost" 6668 nil nil)
     ("Any"             "irc.efnet.net" 6667 nil nil)
     ("AZ Phoenix"      "irc.easynews.com" 6667 nil nil)
     ("CA San Francisco" "irc.prison.net" 6667 nil nil)
     ("Denmark"         "irc.inet.tele.dk" 6667 nil nil)
     ("FL Orlando"      "irc.colosolutions.net" 6667 nil nil)
     ("MI Ann Arbor"    "irc.umich.edu" 6667 nil nil)
     ("MN Minneapolis"  "irc.umn.edu" 6667 nil nil)
     ("Montreal"        "irc.choopa.ca" 6667 nil nil)
     ("NY New York"     "irc.choopa.net" 6667 nil nil)
     ("NY New York 2"   "irc.nac.net" 6667 nil nil)
     ("Toronto"         "irc.igs.ca" 6667 nil nil)
     ("TX Dallas"       "irc.blessed.net" 6667 nil nil)
     ("VA Reston"       "irc.wh.verio.net" 6667 nil nil))

    ("Undernet"
     ("Any"             "irc.undernet.org" 6667 nil nil)
     ("Atlanta, GA"     "atlanta.ga.us.undernet.org" 6667 nil nil)
     ("Fairfax, VA"     "fairfax.va.us.undernet.org" 6667 nil nil)
     ("Montreal"        "montreal.qu.ca.undernet.org" 6667 nil nil)
     ("Princeton, NJ"   "princeton.nj.us.undernet.org" 6667 nil nil))

    ("DALnet"
     ("Any"             "irc.dal.net" 7000 nil nil)
     ("US"              "irc.us.dal.net" 7000 nil nil)
     ("NYC"             "broadway.ny.us.dal.net" 7000 nil nil)
     ("UK"              "genesis-r.uk.eu.dal.net" 7000 nil nil)
     ("Virginia"        "jade.va.us.dal.net" 7000 nil nil)
     ("Georgia"         "acool.ga.us.dal.net" 7000 nil nil)
     ("EU"              "irc.eu.dal.net" 7000 nil nil))

    ("GalaxyNet"
     ("Any"             "galaxy.chattown.org" 6667 nil nil))

    ("P2PNet"
     ("Any"             "ssl:irc.p2p-network.net" 6697 nil nil))

    ("Mozilla"
     ("Any"             "irc.mozilla.org" 6667 nil nil)
     ("localhost"       "localhost" 6666 nil nil))

    ("AnonOps" ;; All connections _must_ be SSL.
     ("Main"            "ssl:irc.anonops.com" 6697 nil nil)
     ("Main Tor"        "ssl:127.0.0.1" 6697 nil nil))

    ("Xiph"
     ("Any"             "irc.xiph.org" 6667 nil nil)))
  "An alist mapping a network name to a list of information needed to connect to that
network.  The car of each element in the alist is the name of a network.  The cdr of each
element in the alist is a list of the form (LOCATION SERVER PORT NICK FULLNAME).  If
SERVER starts with 'ssl:', the connection is made using function erc-tls instead of erc.
See methods my-erc-connect.")

(makunbound 'my-erc-channel-sets)
(defvar my-erc-channel-sets
  '(("freenode"    . ("#emacs" "#cygwin" "#erc" "#bitcoin-dev" "#tcpip" "##c++" "#ahk" "#git"))
    ("EFNet"       . ("#physics"))
    ("Mozilla"     . ("#firefox"))
    ("OFTC"        . ("#tor"))
    ("P2PNET"      . ("#utorrent")))
  "An alist mapping a network name to a list of channels commonly joined on that network.
This variable is used by function my-erc-join-channel.")

(makunbound 'my-erc-channel-frame-locations)
(defvar my-erc-channel-frame-locations
  '(
    (("freenode" . "#emacs")         . (left   0 1/2 2))
    (("freenode" . "#cygwin")        . (right  0 1/2 1))
    (("freenode" . "#erc")           . (right  1 1/2 1))
    (("freenode" . "#bitcoin-dev")   . (left   2 1/3 1))
    (("freenode" . "##c++")          . (right  2 1/3 1))
    (("freenode" . "#tcpip")         . (center 2 1/3 1))
    (("freenode" . "#ahk")           . (right  3 1/2 1))
    (("freenode" . "#git")           . (right  4 1/2 1))
    (("OFTC" . "#tor")               . (left   3 1/2 2))
    )
  "An alist that maps a cons of the form (NETWORK . CHANNEL) to a list of location
metrics having the form (LEFT TOP WIDTH HEIGHT).  If the location metrics are nil, a
default position and size is used for the frame.  The location metrics specify where to
position the devoted frame for CHANNEL on NETWORK as follows:

  o LEFT is one of the symbols left, center, or right, meaning (respectively) the frame
    abuts the left edge of the screen, is centered between the left and right edges, or
    abuts the right edge.

  o TOP is an integer from 0 to 4 (inclusive) corresponding to 5 rows of equal-height
    frames.

  o WIDTH is one of the symbols 1/3, 1/2, 2/3, or full, meaning that the frame should
    be (respectively) one-third, one-half, two-thirds, or fully the width of the screen.

  o HEIGHT is an integer from 1 to 5 (inclusive), specifying the number of rows spanned by
    the frame.  Be careful not to make a frame extend beyond the bottom of the screen!

If the location metrics list has the form

    (:exact LEFT TOP WIDTH HEIGHT)

it specifies the exact values to be used for the corresponding frame parameters.")

;; Downcase all network and channel names in my-erc-channel-frame-locations.
(mapc (lambda (elt)
        (setcar (car elt) (downcase (car (car elt))))
        (setcdr (car elt) (downcase (cdr (car elt)))))
      my-erc-channel-frame-locations)

;; Freenode user modes (see https://freenode.net/using_the_network.shtml for details):
;;
;; D: This prevents you from receiving channel messages.  Only used by services.
;;
;; g: Caller-id.  Prevents you from receiving private messages from anyone not on a
;;    session-defined whitelist.  Manage the whitelist using the /ACCEPT command.
;;
;; i: Invisible.  Prevents you from appearing in global WHO/WHOIS by normal users,
;;    and hides which channels you are on.
;;
;; Q: Prevents you from being forwarded to another channel because of channel mode +f
;;    (see below) or by a ban (see +b below).
;;
;; R: Prevents users who are not identified to NickServ from sending you private
;;    messages.
;;
;; w: Lets you see WALLOPS announcements.
;;
;; Z: User is connected using SSL.
(defvar my-erc-user-modes '(("freenode" . "+Riw")
                            ("DALnet" . "+Ri"))
  "An alist mapping network names to user modes to set when connecting to that network.
It is best to include +i in all of these.")

(defvar my-erc-fullname "Fran"
  "The default full name to use if one is not supplied to my-erc-connect.")

(defvar my-erc-prompt-color "green"
  "The normal foreground color to set on face erc-prompt-face.")

(defvar my-erc-away-prompt-color "grey"
  "The foreground color to set on face erc-prompt-face when my nick ends with '_afk_'.")

(defvar my-erc-query-frame-parameters '((top . -85) (left . 85) (width . 100) (height . 15))
  "Default frame parameters for frame created by /QUERY.")

(defvar my-erc-hide-names-on-join t
  "Non-nil if nick names list should be hidden when joining a channel.  This variable
is buffer-local.")

(defvar my-erc-visible-join-channels nil
  "A list of strings naming channels in which joins should be visible.  If nil,
no joins are shown in any channel.  If t, joins are shown in all channels.
Currently, there is no way to identify which network the channel is on.")

(defvar my-erc-visible-part-channels t
  "A list of strings naming channels in which parts and quits should be visible.
If nil, no parts or quits are shown in any channel.  If t, parts and quits are shown in
all channels.  Currently, there is no way to identify which network the channel is on.")

(defvar my-erc-hidden-join-channels '("#ubuntu" "##linux" "##windows" "##c++")
  "A list of strings naming channels in which joins should not be visible.
This variable is consulted after variable my-erc-visible-part-channels, and thus
overrides it.  Currently, there is no way to identify which network the channel
is on.")

(defvar my-erc-hidden-part-channels '("#ubuntu" "##linux" "##windows" "##c++")
  "A list of strings naming channels in which parts and quits should not be visible.
This variable is consulted after variable my-erc-visible-part-channels, and thus
overrides it.  Currently, there is no way to identify which network the channel
is on.")

(defvar my-erc-channels-alert-on-join nil
  "A list of cons cells in which the car is a channel name and the cdr is a
network name.  A join in any of those channels on the given networks will cause
me to be alerted via my-popup.")

(defvar my-erc-channel-cleanup-interval 20
  "The number of seconds between timer-initiated calls to
my-erc-cleanup-channels.  This value should be less than the value of
my-erc-channel-cleanup-age.")

(defvar my-erc-channel-cleanup-age 25
  "The age threshold (in seconds) of non-discussion text before it is deleted by
my-erc-cleanup-channels.")

(defvar my-erc-frameless nil
  "If non-nil, never creates a new frame.")

(defvar my-erc-no-popups (not (null (member (system-name) '("izsystem023"))))
  "If non-nil, no dialog boxes will popup to announce significant events.")

(defvar my-erc-networks-alist nil
  "A list in the same format as erc-networks-alist that is logically prepended
to erc-networks-alist.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up SASL authentication.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'erc-sasl-server-regexp-list ".*\\.freenode\\.net")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change some messages in the English message catalog.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(erc-define-catalog
 'english
 '((disconnected . "Connection failed!  Re-establishing connection...")
   (disconnected-noreconnect . "Connection failed!  Not re-establishing connection.")
   (finished . "*** ERC finished ***")
   (notify_current . "People online: %l")
   (notify_list . "Current notify list: %l")
   (notify_on . "Detected %n on IRC network %m")
   (notify_off . "%n has left IRC network %m")
   ;;(s312   . "... HACK TO AVOID ERROR ...")
   (terminated . "*** ERC terminated: %e")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure various ERC variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-erc-nick "franl"
  "The default nick used by my-erc-connect if one is not provided.")

;; Make erc-cmd-LIST display the channel list in a new frame.
(add-to-list 'display-buffer-alist
             '("^\\*Channels of .*\\*$" .
               (display-buffer-pop-up-frame . ((pop-up-frame-parameters . ((top . 100)
                                                                           (left . 100)
                                                                           (height . 40)))))))

(setq erc-auto-query                    nil
      erc-autojoin-channels-alist       nil
      erc-button-buttonize-nicks        nil
      erc-button-face                   'my-button-face
      erc-button-mouse-face             'my-button-mouse-face
      ;;erc-chanlist-hide-modeline        t
      erc-common-server-suffixes        nil
      erc-dcc-get-default-directory     "~/"
      erc-default-port                  6667 ;; Windows doesn't grok the service name "ircd".
      erc-email-userid                  "nobody"
      erc-fill-function                 #'erc-fill-variable
      erc-fill-prefix                   "  "
      erc-fill-static-center            15
      erc-flood-protect                 nil
      erc-header-line-format            nil
      erc-interpret-controls-p          'remove
      erc-join-buffer                   nil
      erc-keywords                      `(,(concat "\\b" my-erc-nick "\\b") "\\bfran\\b" "\\bfran1\\b")
      erc-keyword-highlight-type        'all
      erc-max-buffer-size               75000
      erc-minibuffer-notice             nil
      erc-notify-interval               30
      erc-notify-list                   nil
      erc-paranoid                      t
      erc-part-reason                   #'my-erc-part-reason
      erc-pcomplete-nick-postfix        ","
      erc-prompt                        #'my-erc-prompt-function
      erc-query-display                 nil     ;; Needed by my advice on erc-cmd-QUERY.
      erc-reuse-buffers                 t
      erc-server-auto-reconnect         nil
      erc-timestamp-format              nil
      erc-quit-reason                   #'my-erc-quit-reason)

(setq-default erc-enable-logging        nil)

(make-variable-buffer-local 'erc-fill-column)
(set-default 'erc-fill-column 80)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activate/deactivate some ERC minor modes.  Don't try to do this by removing
;; modules from erc-modules.  That doesn't work well.
;;
;; QUESTION: Should these be done in my-erc-mode-hook?  That should only be
;; necessary if these have to execute more than once or only affect a single
;; buffer.  Is that the case?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(erc-autojoin-mode 0)   ;; Needed, because module erc-join is always loaded.
(erc-stamp-mode 0)      ;; Needed, because module erc-stamp is always loaded.
(erc-ring-mode 1)       ;; Input history ring.
(erc-button-mode 1)     ;; Clickable URLs.
(erc-match-mode 1)      ;; Needed for erc-keywords (see above).
(erc-netsplit-mode 1)   ;; Netsplit detection.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(copy-face 'erc-input-face 'erc-command-indicator-face)

(set-face-foreground (copy-face 'default 'erc-action-face) "green")
(set-face-foreground (copy-face 'default 'erc-direct-msg-face) "yellow")
(set-face-foreground (copy-face 'default 'erc-error-face) "orangered")
(set-face-foreground (copy-face 'default 'erc-input-face) "#e9a4ff")
(set-face-foreground (copy-face 'default 'erc-keyword-face) "yellow")
(set-face-foreground (copy-face 'default 'erc-nick-default-face) (face-foreground 'erc-input-face)) ;; #aff
(set-face-foreground (copy-face 'default 'erc-nick-msg-face) "yellow")
(set-face-foreground (copy-face 'default 'erc-notice-face) "orange")
(set-face-foreground (copy-face 'erc-input-face 'erc-prompt-face) my-erc-prompt-color)
(set-face-foreground 'erc-current-nick-face "yellow")

;; Emacs on Windows draws bold faces that use the sixbyten font strangely, so make all ERC faces
;; non-bold.
(mapatoms (lambda (sym)
            (if (and (facep sym)
                     (or (string-match-p "^erc-.*-face$" (symbol-name sym))
                         (string-match-p "^widget-.*-face$" (symbol-name sym))))
                (set-face-bold sym nil))))

(set-face-foreground (make-face 'my-erc-away-face) "yellow")
(set-face-foreground (make-face 'my-erc-joinpart-face) "darkgrey")
(set-face-foreground (make-face 'my-erc-nickchange-face) "darkgrey")
(set-face-foreground (make-face 'my-erc-modechange-face) "darkgrey")
(set-face-foreground (make-face 'my-erc-kick-face) "#cf6")
(set-face-foreground (make-face 'my-erc-quit-reason-face) "darkgrey")
(set-face-foreground (make-face 'my-erc-notify-face) "yellow")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables.  These variables are not for configuration purposes.
;; These are for internal use by the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-erc-no-warn-at-start
  ;; WARNING: Do _not_ manually edit this list -- your changes might be
  ;; overridden by the contents of ~/.erc-warn.  This list is just a fallback in
  ;; case the above file doesn't exist.  Even if the above file doesn't exist,
  ;; it is created automatically, rendering future use of this list irrelevant.
  '("(e.g." "(eg." "(ex:" "(i.e." "(ie." "..." "actually" "afternoon" "ah" "ahem" "ahh"
    "ahhh" "also" "although" "and" "anyway" "bah" "besides" "beware" "btw" "bullshit"
    "but" "bye" "certainly" "clearly" "congrats" "congratulations" "crap" "currently"
    "damn" "er" "especially" "evening" "ex" "example" "feh" "finally" "first" "folks"
    "g'day" "g'nite" "geez" "generally" "god" "good" "goodbye" "goodnight" "goto"
    "greetings" "heck" "heh" "hehe" "hell" "hello" "here" "hey" "hi" "hm" "hmm" "hmmm"
    "howdy" "however" "hrm" "huh" "iirc" "imho" "jeez" "jesus" "lastly" "later" "lol"
    "luckilly" "man" "maybe" "mm" "mmm" "mmmm" "mmmmm" "morning" "namely" "night" "nite"
    "no" "nope" "np" "odd" "oddly" "oh" "ok" "once" "ooh" "oops" "or" "otherwise"
    "personally" "phooey" "plus" "presently" "really" "remember" "right" "rotfl"
    "salutations" "second" "see" "shit" "similarly" "so" "sometimes" "sorry" "specifically"
    "still" "strange" "supposedly" "sure" "thankfully" "thanks" "then" "third" "this"
    "though" "try" "type" "ugh" "uh" "ultimately" "um" "umm" "unfortunately" "use"
    "versus" "wb" "welcome" "well" "what" "whatever" "wow" "wrong" "yeah" "yep" "yes"
    "yikes" "yo" "yow")
  "A list of words that commonly appear at the start of input before a comma or colon.  If
input starts with one of these words, the user will not be warned about the absence of a
nick by the same name in the current channel.")

;; This must happen after we defvar my-erc-no-warn-at-start.  TODO: Find a better place for
;; this.
(let ((my-nowarn-file "~/.erc-nowarn"))
  (if (file-exists-p my-nowarn-file)
      ;; Load the file.
      (load my-nowarn-file 'noerror 'nomessage 'nosuffix)
    ;; Create the file.
    (write-region (concat "(setq my-erc-no-warn-at-start '"
                          (prin1-to-string my-erc-no-warn-at-start)
                          ")")
                  nil my-nowarn-file)))

(defvar my-erc-hide-names my-erc-hide-names-on-join
  "Non-nil means to hide the names output by the /NAMES command.  Do not confuse
this with variable my-erc-hide-names-on-join, which is a configuration variable
that should not be altered by the below code.  This variable is initialized to
the value of my-erc-hide-names-on-join, so that if the user wants to hide names
when joining a channel, we achieve that by hiding names all the time (since the
names displayed on join arrive asynchronously after the join), but we set
my-erc-hide-names to nil in advice on erc-cmd-NAMES, so that command _will_ show
the names list.  Simple, no? ")

(defvar my-erc-cleanup-channels-timer nil
  "...")

(defvar my-erc-closing nil
  "Non-nil if the /CLOSE command has just been executed.  This is used by
my-erc-disconnected-hook to know when to kill ERC buffers for a disconnected server.")

(defvar my-erc-exiting nil
  "Non-nil if the /EXIT command has just been executed.  This is used by
my-erc-disconnected-hook to know when to kill ERC buffers for a disconnected server.")

(defvar-local my-erc-insertion-data '(nil . nil)
  "A cons that holds the values of point and erc-insert-marker when text
received from the server is inserted into an ERC buffer.  Set in
my-erc-insert-pre-hook and used in my-erc-insert-post-hook.  This variable is
needed because my-erc-insert-post-hook is called with the ERC buffer narrowed to
the just-inserted text, which hides the location of point prior to the
narrowing, and also because it's called after the text is inserted.")

(defvar-local my-erc-erased-text nil
  "Holds the text in an ERC buffer erased by my-erc-clear-screen.")

(defvar my-erc-last-privmsg-time '(0 0 0)
  "Time at which the last PRIVMSG was logged to buffer *privmsg*.")

(defvar-local my-erc-alert-time '(0 0 0)
   "The time (as returned by function current-time) when I was last alerted that
someone was talking to me in another channel.")

(defvar-local my-erc-join-alert-time '(0 0 0)
   "The time (as returned by function current-time) when I was last alerted that
someone joined one of the channels listed in my-erc-channels-alert-on-join.")

(defvar-local my-erc-last-command ""
   "The last /-prefixed command issued in each ERC buffer.  This variable is
buffer local.")

(defvar-local my-erc-last-command-time 0
   "The time (in seconds since the Epoch) that the last command /-prefixed
command was sent.  This variable is buffer local.")

(defvar-local my-erc-network-name nil
  "Holds the name of the current network.  This is only non-nil in server buffers.")

(defvar my-erc-join-no-new-frame nil
  "If non-nil, no new frame is created when joining a channel.  This is not for
user configuration -- don't set it to non-nil and expect channel buffers to stop
appearing in devoted frames.  Use variable my-erc-frameless instead.")

(defvar-local my-erc-last-msg-sender nil
   "If non-nil, this is the nick of the last user to /MSG me.  This variable is
local to all buffers, but you should only ever assign or reference the one that
is local to the server buffer for the current network.")

(defvar-local my-erc-size-warning-count 0
   "Used in my-erc-insert-modify-hook to space out warnings.")

(defvar my-erc-connect-history nil
  "History list used by my-erc-connect when reading a server name interactively.")

(defvar my-erc-join-channel-history nil
  "History list used by my-erc-join-channel when reading the channel name in the
minibuffer.")

(defvar my-erc-dcc-last-file nil
  "The filename of the last file received via /DCC GET.")

(defvar my-erc-netsplit-mitigation nil
  "If non-nil, change ERC behavior to cope with repeated netsplits.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup some ERC hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'erc-after-connect                 'my-erc-after-connect-hook)
(add-hook 'erc-channel-members-changed-hook  'my-erc-channel-members-changed-hook)
(add-hook 'erc-dcc-chat-mode-hook            'my-erc-dcc-chat-mode-hook)
(add-hook 'erc-disconnected-hook             'my-erc-disconnected-hook)
(add-hook 'erc-insert-modify-hook            'my-erc-insert-modify-hook)
(add-hook 'erc-insert-post-hook              'my-erc-insert-post-hook)
(add-hook 'erc-insert-pre-hook               'my-erc-insert-pre-hook)
(add-hook 'erc-kill-buffer-hook              'my-erc-kill-buffer-hook)
(add-hook 'erc-kill-channel-hook             'my-erc-kill-channel-hook)
(add-hook 'erc-kill-server-hook              'my-erc-kill-server-hook)
(add-hook 'erc-mode-hook                     'my-erc-mode-hook)
(add-hook 'erc-send-modify-hook              'my-erc-send-modify-hook)
(add-hook 'erc-send-post-hook                'my-erc-send-post-hook)
(add-hook 'erc-send-pre-hook                 'my-erc-send-pre-hook)
(add-hook 'erc-server-366-functions          'my-erc-server-366-function)
(add-hook 'erc-server-JOIN-functions         'my-erc-after-server-JOIN-function 'append)
(add-hook 'erc-server-JOIN-functions         'my-erc-server-JOIN-function)
(add-hook 'erc-server-MODE-functions         'my-erc-server-MODE-function)
(add-hook 'erc-server-NOTICE-functions       'my-erc-server-NOTICE-function)
(add-hook 'erc-server-PRIVMSG-functions      'my-erc-server-PRIVMSG-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-advice mouse-wheel-scroll-line (:after (&rest args) my-ad-after-mouse-wheel-scroll-line)
  "When the current buffer is an ERC buffer, arranges to keep the prompt on the
last line of the window (if the prompt is visible)."
  ;; This is needed so that (window-end) returns the correct value for the window
  ;; after the advised command performs the scroll.
  (sit-for 0)
  (if (and (eq major-mode 'erc-mode)
	   (memq (current-buffer) (erc-buffer-list))
	   (< erc-insert-marker (window-end)))
    (my-erc-end-of-buffer)))

(define-advice mwheel-scroll (:after (&rest args) my-ad-after-mwheel-scroll)
  "Makes sure the prompt is positioned at the bottom of the window when it scrolls
into view."
  (when (eq major-mode 'erc-mode)
    ;; This (sit-for 0) is needed (at least under Windows).  Without it, the value
    ;; returned by (window-end) is the value before the window is scrolled not after.
    (sit-for 0)
    (when (< erc-insert-marker (window-end))
      (goto-char (point-max))
      (my-erc-position-prompt))))

(define-advice erc-button-add-buttons (:around (origfun &rest args) my-ad-around-erc-button-add-buttons)
  "Advised so that it cannot raise errors."
  ;; TODO: Is this still needed???
  (ignore-errors
    (apply origfun args)))

(define-advice erc-cmd-JOIN (:around (origfun &rest args) my-ad-around-erc-cmd-JOIN)
  "Prevent double-joining the same channel on the same server.  erc-cmd-JOIN performs this
test too, but when joining an already-joined channel, it switches the current buffer to
that channel, which I don't like, so I catch the offending condition here."
  (let ((my-channel (car args))
        (my-joined-channels (mapcar (lambda (chanbuf)
                                      (with-current-buffer chanbuf
                                        (erc-default-target)))
                                    (erc-channel-list erc-server-process))))
    (if (erc-member-ignore-case my-channel my-joined-channels)
        (my-erc-alert "+++ You have already joined %s!" my-channel)
      (apply origfun args))))

(define-advice erc-cmd-NAMES (:before (&rest args) my-ad-before-erc-cmd-NAMES)
  "Sets my-erc-hide-names to nil in the current buffer, so that user names will be
displayed.  This is reset to be equal to my-erc-hide-names-on-join once the end of the
names list is received."
  (setq my-erc-hide-names nil))

(define-advice erc-cmd-NICK (:after (&rest args) my-ad-after-erc-cmd-NICK)
  "Sets the forceground face of erc-prompt-face according to the spelling of my nick."
  (if (string-match-p "_afk_?$" (or (car-safe args) ""))
      (set-face-foreground 'erc-prompt-face my-erc-away-prompt-color)
    (set-face-foreground 'erc-prompt-face my-erc-prompt-color)))

(define-advice erc-cmd-QUERY (:around (origfun &rest args) my-ad-around-erc-cmd-QUERY)
  "Displays query buffer in new frame."
  (let ((my-nick-to-query (car args))
        (my-orig-buffer (current-buffer))
        (my-orig-window (selected-window))
        (my-new-frame (make-frame my-erc-query-frame-parameters)))
    (if (and (null (assoc 'top my-erc-query-frame-parameters))
             (null (assoc 'left my-erc-query-frame-parameters)))
        (my-frame-center my-new-frame))

    (select-frame my-new-frame)
    (modify-frame-parameters my-new-frame `((name . ,(concat "Query " my-nick-to-query))))

    (let ((retval (apply origfun args)))
      (set-frame-parameter my-new-frame 'my-erc-devoted-frame-buffer (current-buffer))
      (my-erc-position-prompt my-orig-buffer)

      ;; This hack is necessary because the above call to my-erc-position-prompt doesn't
      ;; leave point after the newly inserted prompt in the original buffer where /query was
      ;; typed.  Debugging reveals it does move point to (point-max) but something deep
      ;; inside ERC moves it back before the prompt.  So I do this.
      (run-at-time 0 nil `(lambda ()
                            (let ((my-current-window (selected-window)))
                              (select-window ,my-orig-window)
                              (goto-char (point-max))
                              (select-window my-current-window))))

      ;; Return the value returned by the advised function.
      retval)))

(define-advice erc-dcc-chat-parse-output (:after (&rest args) my-ad-after-erc-dcc-chat-parse-output)
  "Always display the maximum amound of text by keeping the last line of the buffer at the
bottom."
  (if (eobp)
      (my-show-max-text)))

(define-advice erc-dcc-get-file (:after (&rest args) my-ad-after-erc-dcc-get-file)
  "Saves the name of the last file retrieved via /DCC GET in variable
my-erc-dcc-last-file."
  (setq my-erc-dcc-last-file (nth 1 args)))

(define-advice erc-default-server-handler (:override (&rest args) my-ad-override-erc-default-server-handler)
  "Now, always displays the message in the currently selected window."
  ;; Actual args: (proc parsed)
  (let ((my-buf (window-buffer (selected-window))))
    ;; If active buffer isn't an ERC buffer, display the server output in the server buffer.
    (if (not (eq (with-current-buffer my-buf major-mode) 'erc-mode))
        (setq my-buf (current-buffer)))

    (with-current-buffer my-buf
      ;; This lambda expression may result in multiple contiguous spaces in some server
      ;; messages.
      (my-erc-alert (concat "*** " (mapconcat (lambda (x) (if (stringp x) x))
                                              (nth 1 args) " "))))))

(define-advice erc-display-line (:before (&rest args) my-ad-before-erc-display-line)
  "Make all ERC output that would appear in the server buffer appear in the active buffer
instead."
  (let ((my-buffer (nth 1 args))
        (my-active-buffer (window-buffer (selected-window))))
    (if (and (or (null my-buffer)
                 (processp my-buffer)
                 (and (bufferp my-buffer)
                      (eq my-buffer (with-current-buffer my-buffer (erc-server-buffer)))))
             (memq (with-current-buffer my-active-buffer major-mode)
                   '(erc-mode erc-dcc-chat-mode)))
        (setf (nth 1 args) my-active-buffer))))

(define-advice erc-go-to-log-matches-buffer (:override (&rest args) my-ad-override-erc-go-to-log-matches-buffer)
  "Now, the log matches buffer appears in a new frame."
  (my-frame-center (select-frame (make-frame '((name . "ERC Keywords")
                                               (top . 0)
                                               (left . 3000) ;; Off-screen (for now :-)
                                               (width . 120)
                                               (height . 30)))))
  (switch-to-buffer "ERC Keywords"))

(define-advice erc-kill-input (:around (origfun &rest args) my-ad-around-erc-kill-input)
  "If point is before the prompt, this function acts like universal-argument,
otherwise it calls erc-kill-input and then resets the buffer-modified flag."
  (if (<= (point) erc-insert-marker)
      (call-interactively 'universal-argument)

    (let ((my-retval (apply origfun args)))
      (set-buffer-modified-p nil)

      ;; Return the value returned by the advised function.
      my-retval)))

(define-advice erc-open-network-stream (:override (name buffer host service)
                                                  my-ad-override-erc-open-network-stream)
  "Same as erc-open-network-stream but restricts the connection to use IPv4 only.  This is needed
because Emacs seems to have trouble connecting to IPv6 addresses, especially at home."
  (make-network-process :name name :buffer buffer :family 'ipv4
                        :host host :service service :nowait t))

(define-advice erc-pcomplete (:after (&rest args) my-ad-after-erc-pcomplete)
  "Removes the trailing space after a completed nick if the nick is not the first word on the
input line."
  (let ((my-input-line (buffer-substring (my-erc-start-of-input) (point))))
    (if (not (string-match-p "^\\S-+, $" my-input-line))
        (delete-horizontal-space))))

(define-advice erc-scroll-to-bottom (:around (origfun &rest args) my-ad-around-erc-scroll-to-bottom)
  "A total hack to workaround a bug in list-buffers-noselect that causes every buffer to
be displayed briefly."
  (my-with-advice ((sit-for ignore))
    (apply origfun args)))

(define-advice erc-send-current-line (:around (origfun &rest args) my-ad-around-erc-send-current-line)
  "Expands the abbrev before point before sending the line.  This works even if point is
not at the end of the line being sent."
  (if (and (/= (point-max) (my-erc-start-of-input))
           (= ?w (char-syntax (char-before (point)))))
      ;; We need the above char-syntax check, because expand-abbrev expands the word
      ;; before point, even if there are non-word-constituent characters between that
      ;; word and point.
      (expand-abbrev))

  (if (/= (my-erc-start-of-input) (point-max))
      (apply origfun args)))

(define-advice erc-toggle-debug-irc-protocol (:after (&rest args) my-ad-after-erc-toggle-debug-irc-protocol)
  "Turns off view-mode in the *irc-protocol* buffer."
  (view-mode 0))

(define-advice erc-update-mode-line-buffer (:override (&rest args) my-ad-override-erc-update-mode-line-buffer)
  "Disables this function."
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-erc-mode-hook ()
  "Added to erc-mode-hook.  ERC mode is the major mode in server and channel buffers (and
probably query buffers too)."
  ;; Delete all blank lines in the current buffer.  This happens for server, channel, and query
  ;; buffers.
  (run-at-time 3 nil `(lambda ()
                        (with-current-buffer ,(current-buffer)
                          (my-erc-delete-blank-lines))))

  (abbrev-mode 1)
  (modify-syntax-entry ?' "." erc-mode-syntax-table)

  (local-set-key (kbd "TAB")            'my-erc-tab)
  (local-set-key (kbd "<backspace>")    'my-erc-delete-backward-char)
  (local-set-key (kbd "<M-backspace>")  'my-erc-backward-kill-word)
  ;;(local-set-key (kbd "<down-mouse-3>") 'my-erc-channel-menu)
  (local-set-key (kbd "<f1>")           'my-erc-clear-screen)
  (local-set-key (kbd "<S-f1>")         'my-erc-clear-all-screens)
  (local-set-key (kbd "<C-f1>")         'my-erc-delete-unwanted-text)
  (local-set-key (kbd "<f2>")           'my-erc-toggle-server-frame-visibility)
  (local-set-key (kbd "<f4>")           'my-erc-toggle-big)
  (local-set-key (kbd "<f5>")           (lambda () (interactive)
                                          (select-frame (make-frame))
                                          (my-list-buffers)))
  (local-set-key (kbd "<S-f4>")         'my-erc-position-frame)
  (local-set-key (kbd "<M-S-f4>")       'my-erc-position-all-frames)
  (local-set-key (kbd "<f6>")           'my-erc-insert-channel-name)
  (local-set-key (kbd "<f12>")          'my-erc-refill-buffer)
  (local-set-key (kbd "<up>")           'my-erc-previous-command)
  (local-set-key (kbd "<down>")         'my-erc-next-command)
  (local-set-key (kbd "<left>")         'my-erc-backward-char)
  (local-set-key (kbd "<next>")         'my-erc-scroll-up)
  (local-set-key (kbd "<prior>")        'my-erc-scroll-down)
  (local-set-key (kbd "<home>")         'erc-bol)
  (local-set-key (kbd "C-b")            'my-erc-backward-char)
  (local-set-key (kbd "C-l")            (lambda ()
                                          (interactive)
                                          (if (> (point) erc-insert-marker)
                                              (recenter -1)
                                            (recenter))
                                          ;; Clear the echo area in all frames, because Emacs copies
                                          ;; the echo area text as the mouse moves from frame to frame.
                                          (mapc (lambda (frame)
                                                  (with-selected-frame frame (message "")))
                                                (frame-list))))
  (local-set-key (kbd "C-p")            'my-erc-previous-command)
  (local-set-key (kbd "C-n")            'my-erc-next-command)
  (local-set-key (kbd "C-u")            'erc-kill-input)
  (local-set-key (kbd "C-c C-u")        'universal-argument)    ;; "C-z C-u" is bound to browse-url
  (local-set-key (kbd "C-x C-c")        'my-erc-part-quit-endquery)
  (local-set-key (kbd "C-z C-d")        'my-erc-debug-irc-protocol)
  (local-set-key (kbd "C-z s")          (lambda ()
                                          (interactive)
                                          (setq my-erc-netsplit-mitigation
                                                (not my-erc-netsplit-mitigation ))
                                          (message "Netsplit mitigation is now %s!"
                                                   (if my-erc-netsplit-mitigation "on" "off"))))
  (local-set-key (kbd "C-z S")          (lambda ()
                                          (interactive)
                                          (message "Netsplit mitigation is now %s!"
                                                   (if my-erc-netsplit-mitigation "on" "off"))))
  (local-set-key (kbd "C-z C-p")        'my-erc-make-frame-devoted)
  (local-set-key (kbd "C-z C-j")        'my-erc-join-channel)
  (local-set-key (kbd "C-z C-k")        'my-erc-kill-all-buffers)
  (local-set-key (kbd "C-z C-l")        'erc-cmd-LIST)
  (local-set-key (kbd "C-z C-v")        'my-erc-version)
  (local-set-key (kbd "C-z C-q")        'my-erc-query-nick)
  (local-set-key (kbd "<return>")       'erc-send-current-line)
  (local-set-key (kbd "C-m")            (lambda () (interactive) (ding)))
  (local-set-key (kbd "<S-return>")     (lambda () (interactive) (insert "\n")))
  (local-set-key (kbd "C-v")            'my-erc-scroll-up)
  (local-set-key (kbd "C-x C-b")        'my-list-buffers)
  (local-set-key (kbd "M-p")            'my-previous16-line)
  (local-set-key (kbd "M-n")            'my-next16-line)
  (local-set-key (kbd "M-v")            'my-erc-scroll-down)
  (local-set-key (kbd "M->")            'my-erc-end-of-buffer)
  (local-set-key (kbd "M-w")            'my-erc-kill-ring-save)

  ;; Merge my-erc-networks-alist into erc-networks-alist.
  ;; QUESTION: Why is this done here?  Does erc-networks-alist get reset periodically?
  (dolist (netinfo my-erc-networks-alist)
    (if (not (assoc (car netinfo) erc-networks-alist))
        (setq erc-networks-alist (cons netinfo erc-networks-alist))))

  ;; Turn on Font-Lock mode.
  (my-toggle-font-lock-mode 1)

  ;; Disable undo.
  (buffer-disable-undo)

  ;; Turn off Auto Fill mode.
  (auto-fill-mode 0)

  ;; Configure TAB-completion of nicks.
  (setq pcomplete-cycle-completions t)
  (set (make-local-variable 'pcomplete-cycle-cutoff-length) 99999)
  (pcomplete-erc-setup)
  (erc-pcomplete-mode 1)

  ;; Turn off the mode line if we are using frames.
  (if (not my-erc-frameless)
      (setq mode-line-format nil))

  ;; Set up variable hooks that need to be local to ERC buffers.
  (add-hook 'window-size-change-functions 'my-erc-window-size-change-functions nil)

  (add-hook 'post-command-hook 'my-erc-post-command-hook nil 'local))

(defun my-erc-dcc-chat-mode-hook ()
  "Added to erc-dcc-chat-mode-hook."
  ;; This code is copied from my-erc-mode-hook.  Maybe it should be factored out.
  (local-set-key (kbd "M-w")            'my-erc-kill-ring-save)
  (local-set-key (kbd "C-b")            'my-erc-backward-char)
  (local-set-key (kbd "C-u")            'erc-kill-input)
  (local-set-key (kbd "C-c C-u")        'universal-argument)
  (local-set-key (kbd "C-c C-v")        'my-erc-version)
  (local-set-key (kbd "C-c C-q")        'my-erc-query-nick)
  (local-set-key (kbd "C-p")            'my-erc-previous-command)
  (local-set-key (kbd "C-n")            'my-erc-next-command)
  (local-set-key (kbd "<return>")       'erc-send-current-line)
  (local-set-key (kbd "C-m")            (lambda () (interactive) (ding)))
  (local-set-key (kbd "M-p")            'my-previous16-line)
  (local-set-key (kbd "M-n")            'my-next16-line)
  (local-set-key (kbd "M->")            'my-erc-end-of-buffer)
  (local-set-key (kbd "M-C-g")          'my-erc-show-last-dcc-get-file)

  (add-hook 'post-command-hook 'my-erc-post-command-hook nil 'local))

(defun my-erc-dcc-chat-connect-hook (proc)
  "Makes DCC chat windows appear in a new frame."
  (let ((my-channel-window (selected-window))
        (chat-buffer (current-buffer)))
    ;; Switch back to the channel buffer.
    (run-at-time 1 nil `(lambda ()
                          (select-window ,my-channel-window)
                          (switch-to-buffer nil)))
    (select-frame (make-frame '((my-center . t))))
    (switch-to-buffer chat-buffer)
    (goto-char (point-max))))

(add-hook 'erc-dcc-chat-connect-hook 'my-erc-dcc-chat-connect-hook)

(defun my-erc-window-size-change-functions (frame)
  "Added to window-size-change-functions locally in ERC buffers.  This re-fills the text
in ERC buffers when the window geometry changes.  From the docstring for the hook:

The buffer-local part is run once per window, with the relevant window selected; while the
global part is run only once for the modified frame, with the relevant frame selected."
  (when (and (eq major-mode 'erc-mode)
             (> (buffer-size) 0))
    (ignore-errors (my-erc-refill-buffer))
    (ignore-errors (my-erc-position-prompt))))

(defun my-erc-post-command-hook ()
  "Added to post-command-hook (locally) in ERC buffers."
  (if (> (point) erc-insert-marker)
      (let ((inhibit-read-only t)
            (my-start-of-input (my-erc-start-of-input)))
        ;; Make the space after the prompt intangible, like the prompt.
        (add-text-properties (1- my-start-of-input) my-start-of-input '(intangible t))

        ;; Fontify my typed input.
        (add-text-properties my-start-of-input (point-max) '(font-lock-face erc-input-face))

        ;; This check is an optimization that prevents a performance slowdown from the
        ;; below save-excursion/recenter code.
        (if (and (or (eq this-command 'self-insert-command)
                     (eq this-command 'yank))

                 (or (redisplay t) t)

                 ;; This extra check is an optimization that prevents a performance
                 ;; slowdown from the below save-excursion/recenter code.
                 ;;
                 ;; BUG: This condition is true when the input wraps at the right edge of
                 ;; the window even when point is still on the last line of the window.
                 ;; The value 2 that is subtracted from (window-height) needs to be
                 ;; incremented by one for each time the input line wraps.  Maybe use
                 ;; integer division to divide the column number by the window-width?
                 (or (< (- (line-number-at-pos (point-max))
                           (line-number-at-pos (window-start)))
                        (- (window-height) 2))

                     ;; This next term copes with the case where I've typed one character
                     ;; past the width of the current window and the line has wrapped
                     ;; causing the window to scroll (and thus leaving point no longer at
                     ;; the bottom line of the window).  The above condition isn't true in
                     ;; this case, probably because Emacs hasn't redisplayed by the time
                     ;; this code runs (but it knows the current column number).  Simple,
                     ;; no?
                     ;;(= 1 (% (current-column) (window-width)))
                     ))
            (save-excursion
              (goto-char (point-max))
              (recenter -1)))

        (if (= (point) my-start-of-input)
            (set-buffer-modified-p nil)))))

(defun my-erc-after-connect-hook (server nick)
  "Added to erc-after-connect.  This is called when the end of the MOTD has been
received.  The server buffer is current when this hook runs."
  ;; Set some variables to their initial values.  QUESTION: Do we need to set
  ;; these to nil here?  They're set to nil in my-erc-connect.
  (setq my-erc-closing nil
        my-erc-exiting nil)

  (if (timerp my-erc-cleanup-channels-timer)
      (cancel-timer my-erc-cleanup-channels-timer))

  (setq my-erc-network-name (erc-network-name))

  (setq my-erc-cleanup-channels-timer
        (run-at-time my-erc-channel-cleanup-interval my-erc-channel-cleanup-interval
                     'my-erc-cleanup-channels)))

(defun my-erc-kill-server-hook ()
  "Added to erc-kill-server-hook.  This hook runs whenever a server buffer is
killed."
  ;; Do not to change the current buffer in this function.  Other functions on this
  ;; hook expect to run with the current buffer unchanged.

  ;; Delete the devoted frame for this server buffer.
  (save-excursion
    (if (not my-erc-frameless)
        (let ((my-server-frame (my-erc-devoted-frame (current-buffer))))
          (if (frame-live-p my-server-frame)
              (delete-frame my-server-frame))))))

(defun my-erc-kill-channel-hook ()
  "Added to erc-kill-channel-hook.  This is called whenever an ERC channel buffer is
killed.  This closes the devoted frame for the channel.  This runs before the PART command
is sent by ERC for the channel."
  ;; Do not to change the current buffer in this function.  Other functions on this
  ;; hook expect to run with the current buffer unchanged.

  ;; Delete the devoted frame for this channel buffer.  
  (save-excursion
    (if (not my-erc-frameless)
        (let ((my-channel-frame (my-erc-devoted-frame (current-buffer))))
          (if (frame-live-p my-channel-frame)
              (delete-frame my-channel-frame)))))

  (my-erc-maybe-make-server-frame-visible))

(defun my-erc-kill-buffer-hook ()
  "Added to erc-kill-buffer-hook.  This is called whenever a non-server or non-channel
buffer is killed.  It close the devoted frame for query buffers."
  ;; Do not to change the current buffer in this function.  Other functions on this
  ;; hook expect to run with the current buffer unchanged.

  ;; Delete the devoted frame for this buffer.
  (save-excursion
    (if (not my-erc-frameless)
        (let ((my-devoted-frame (my-erc-devoted-frame (current-buffer))))
          (if (frame-live-p my-devoted-frame)
              (delete-frame my-devoted-frame)))))

  (my-erc-maybe-make-server-frame-visible))

(defun my-erc-disconnected-hook (nick ip reason)
  "Added to erc-disconnected-hook.  This is called whenever a server disconnects for any
reason.  The server buffer is current when this function executes."
  ;; Cancel the channel cleanup timer, if there are no servers connected.
  (if (and (timerp my-erc-cleanup-channels-timer)
           (null (erc-buffer-list (lambda () (and (erc-server-buffer-p) erc-server-connected)))))
      (cancel-timer my-erc-cleanup-channels-timer))

  ;; Was this an intentional or unintentional disconnection?
  (if (or my-erc-closing erc-server-quitting)
      ;; This disconnection was caused by /QUIT, /CLOSE, or /EXIT.  If it's /CLOSE or
      ;; /EXIT, kill all buffers associated with this server.  We have to delay this,
      ;; otherwise we kill the buffer before ERC is done using it.
      (if my-erc-closing
          ;; This disconnection was caused by /CLOSE, so kill all buffers for this server.
          (run-at-time 0.5 nil
                       `(lambda ()
                          ;; Kill the buffers.
                          (my-erc-kill-buffers-for-server 'all ,(current-buffer))

                          ;; If an /EXIT command is not in progress, or an /EXIT is in
                          ;; progress and no server buffers exist, reset my-erc-closing
                          ;; and my-erc-exiting to nil.  This sometimes sets
                          ;; my-erc-exiting to nil when it's already nil.
                          (if (or (not my-erc-exiting)
                                  (null (erc-buffer-list 'erc-server-buffer-p)))
                              (setq my-erc-closing nil
                                    my-erc-exiting nil))))
        
        ;; This disconnection was caused by /QUIT, so display a message in all
        ;; buffers of this server.
        (my-erc-alert-all-buffers-for-server "+++ Quit from network %s" (my-erc-network)))

    ;; This disconnection was unexpected.  Append a notice to each channel buffer for
    ;; this server.
    (my-erc-alert-all-buffers-for-server "+++ UNEXPECTED DISCONNECTION FROM NETWORK %s!"
                                         (my-erc-network))

    ;; Maybe show a popup notification.
    (if (not my-erc-no-popups)
        (my-popup "ERC Disconnection!" (format "ERC has disconnected from network %s!"
                                               (or (erc-network) "UNKNOWN"))
                  'traytip))))

(defun my-erc-send-pre-hook (string)
  "Added to erc-send-pre-hook."
  (my-erc-update-erc-fill-column)
  (if (and (not (string-match-p "^/msg" (downcase string)))
           (not (string-match-p "^\\(==\\|\\+\\+\\|--\\)" string))
           (or (if erc-channel-users
                   (> (hash-table-count erc-channel-users) 2))
               (not (and erc-channel-users
                         (erc-get-channel-user "birny"))))
           (let ((my-words (split-string string "[] \f\t\n\r\v'\"/?><.,:=[-]+")))
             (memq t (mapcar (lambda (s) (string= "cf418e8cc469cd3297889424fb862272"
                                                  (md5 (downcase s) nil nil nil 'noerror)))
                             my-words))))
      ;; Prevent me from issuing a secret bot command in any channel with more than 2
      ;; users where the users are not me and the bot.
      (progn
        (ding)
        (message "What you talkin' 'bout, Willis?")
        (setq erc-send-this nil))

    ;; Prompt for confirmation if my input contains newlines.
    (when (string-match-p "\n" string)
      (ding)
      (setq erc-send-this (y-or-n-p "Input contains newlines!  Really send? ")))

    ;; Prompt for confirmation if my input starts with whitespace.
    (when (and erc-send-this
               (string-match-p "^ +/" string))
      (ding)
      (setq erc-send-this (y-or-n-p "Command starts with whitespace!  Really send? ")))

    ;; Warn me if I'm about to do a /WHO or /WHOIS without the leading slash (for everyone to see).
    (when (and erc-send-this
               (string-match-p "^\\(who\\(is\\)?\\s-+\\S-+\\|msg\\s-+.*\\)$" string))
      (ding)
      (setq erc-send-this (y-or-n-p "Did you forget a '/'?  Really send? ")))

    (when (and erc-send-this
               erc-away
               (> (length string) 0)
               (/= ?/ (aref string 0)))
      (ding)
      (setq erc-send-this (y-or-n-p "You are away!  Really send? ")))

    ;; Prompt for confirmation if I'm addressing someone who isn't in the channel.
    (if (and erc-send-this
             (string-match (concat "^\\(\\S-+\\)[,:] ") string))
        ;; I may be addressing someone.
        (let ((my-nick (match-string 1 string)))
          (when (and (erc-channel-p (erc-default-target))
                     (not (erc-get-channel-user my-nick))
                     (not (member-ignore-case my-nick my-erc-no-warn-at-start)))
            (ding)
            (let* ((cursor-in-echo-area t)
                   (answer (read-char (format "\"%s\" isn't here!  Really send? (y/n/a)" my-nick))))
              (setq erc-send-this (memq answer '(?y ?a)))

              (if (= answer ?a)
                  (my-erc-add-to-no-warn-at-start my-nick))))))

    (if erc-send-this
          (message "")
      (message "Message not sent."))))

(defun my-erc-send-modify-hook ()
  "Added to erc-send-modify-hook."
  (save-excursion
    ;; Make the '<' and '>' characters around nicks have the same face as the nicks.
    ;; QUESTION: What is needed on erc-send-modify-hook?  Is this for my outgoing
    ;; channel messages?  If so, those don't appear anymore in ERC.
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "^<[^>]+>")
          (add-text-properties (point-min) (match-end 0) '(font-lock-face erc-nick-default-face))))

    (goto-char (point-min))

    (dolist (word (split-string (buffer-string) "[] \f\t\n\r\v'\"/?><.,:=[-]+"))
      (if (string= (md5 (downcase word) nil nil nil 'noerror)
                   "cf418e8cc469cd3297889424fb862272")
          (if (looking-at (concat "^.*\\(" word "\\)"))
              (replace-match (propertize "..." 'font-lock-face 'erc-input-face) nil nil nil 1)))))

  (erc-fill))

(defun my-erc-send-post-hook ()
  "Added to erc-send-post-hook.  Whent this hook runs the buffer is narrowed to just the
sent text."
  (let* ((prompt-and-input (buffer-string))
         (input (if (string-match "^#[^>]+> \\(.*\\)$" prompt-and-input)
                    (match-string 1 prompt-and-input))))
    (if (and (stringp input)
             (> (length input) 0)
             (= ?/ (aref input 0)))
        (setq my-erc-last-command input
              my-erc-last-command-time (erc-current-time))))
  
  (my-erc-position-prompt))

(defun my-erc-channel-members-changed-hook ()
  "Added to erc-channel-members-changed-hook."
  ;; The doc for erc-channel-members-changed-hook says "The buffer where the change happened is
  ;; current while this hook is called.", but for some reason, this hook is occassionally called in
  ;; a server buffer, so we protect against that case.
  (if (erc-channel-p (erc-default-target))
      ;; Update devoted frame titlebar if this is a channel buffer..
      (my-erc-update-titlebar)))

(defun my-erc-insert-pre-hook (string)
  "Added to erc-insert-pre-hook.  IMPORTANT: This hook cannot be used to modify
inserted text!  Instead use my-erc-insert-modify-hook."
  ;; Make sure my-erc-insert-modify-hook is always at the front of the list of
  ;; hooks on erc-insert-modify-hook.  This is necessary because if erc-fill
  ;; ends up in front of my-erc-insert-modify-hook, then my regular expressions
  ;; in that hook don't match correctly because of the newlines inserted by
  ;; erc-fill.
  (when (and (listp erc-insert-modify-hook)
             (memq 'my-erc-insert-modify-hook erc-insert-modify-hook)
             (not (eq 'my-erc-insert-modify-hook (car erc-insert-modify-hook))))
    (remove-hook 'erc-insert-modify-hook 'my-erc-insert-modify-hook)
    (add-hook 'erc-insert-modify-hook 'my-erc-insert-modify-hook))

  ;; Save the location of point for use later in my-erc-insert-post-hook.
  (setq my-erc-insertion-data (cons (point) (marker-position erc-insert-marker)))

  ;; Check for output to elide from ERC buffer.  Here my-target is a nick or ???.
  (let ((my-target (erc-default-target)))
    (if (not (stringp my-target))
        (setq my-target ""))

    (if (or (and (string-match-p "^\\*\\*\\* .* has joined" string)
                 (or my-erc-netsplit-mitigation
                     (null my-erc-visible-join-channels)
                     (and (listp my-erc-visible-join-channels)
                          (not (member-ignore-case my-target my-erc-visible-join-channels))
                          (listp my-erc-hidden-join-channels)
                          (member-ignore-case my-target my-erc-hidden-join-channels))))

            (and (string-match-p "^\\*\\*\\* .* has \\(left\\|quit\\)" string)
                 (or my-erc-netsplit-mitigation
                     (null my-erc-visible-part-channels)
                     (and (listp my-erc-visible-part-channels)
                          (not (member-ignore-case my-target my-erc-visible-part-channels))
                          (listp my-erc-hidden-part-channels)
                          (member-ignore-case my-target my-erc-hidden-part-channels))))

            (and (string-match-p "^\\*\\*\\* .* is now known as " string)
                 my-erc-netsplit-mitigation)

            (and (string-match-p "^\\*\\*\\* You have left channel " string)
                 (not (string-match-p "^/part\\b" my-erc-last-command)))

            (string-match-p "^\\*\\*\\* .* was created on " string)

            (string-match-p "^\\*\\*\\* .* topic set by " string)

            (and my-erc-hide-names
                 (string-match-p "^\\*\\*\\*.* Users on " string)))

        ;; Settting erc-insert-this to nil here tells ERC not to insert the current server output.
        (setq erc-insert-this nil))))

(defun my-erc-insert-modify-hook ()
  "Added to erc-insert-modify-hook.  This is called after erc-insert-pre-hook.  When this
hook is run, narrowing is in effect and the current buffer is the buffer where the text
got inserted."
  (my-erc-update-erc-fill-column)

  (save-excursion
    ;; Delete blank lines.
    (goto-char (point-min))
    (while (and (search-forward-regexp "^$" nil t)
                (not (eobp))
      (beginning-of-line)
      (let ((my-start (point)))
        (forward-line 1)
        (delete-region my-start (point)))))

    ;; Change Unicode elipses to "...".  This keeps the prompt near the echo area.
    ;; Without this, the height of the elipses glyph is greater than the height of my 6x10
    ;; font, so a gap appears between the prompt and the echo area.  See
    ;; http://www.unicode.org/charts/PDF/U2000.pdf.
    (goto-char (point-min))
    (while (search-forward (char-to-string ?\u2026) nil t)
      (delete-char -1)
      (insert "..."))

    ;; Change Unicode mdash to "--" for the same reason we replace Unicode elipses.
    (goto-char (point-min))
    (while (search-forward (char-to-string ?\u2014) nil t)
      (delete-char -1)
      (insert "--"))

    ;; Change Unicode dash to "--" for the same reason we replace Unicode elipses.
    (goto-char (point-min))
    (while (search-forward (char-to-string ?\u2013) nil t)
      (delete-char -1)
      (insert "--"))

    ;; Change Unicode Right Single Quotation Mark (0x2019) to "'".
    (goto-char (point-min))
    (while (search-forward (char-to-string ?\u2019) nil t)
      (delete-char -1)
      (insert "'"))

    ;; Change Unicode Right Double Quotation Mark (0x201d) to '"'.
    (goto-char (point-min))
    (while (search-forward (char-to-string ?\u201d) nil t)
      (delete-char -1)
      (insert 34))  ;; 34 = '"'

    ;; Change Unicode Left Double Quotation Mark (0x201d) to '"'.
    (goto-char (point-min))
    (while (search-forward (char-to-string ?\u201c) nil t)
      (delete-char -1)
      (insert 34))  ;; 34 = '"'

    ;; Cope with character codes in the range 128-160.  mIRC displays these as
    ;; valid glyphs, even though ISO-Latin-1 doesn't define them.  This converts
    ;; them to a '·' charcter.
    (goto-char (point-min))
    (when (looking-at-p "^.*[\200-\221\223-\237]")
      (goto-char (point-min))
      (while (search-forward-regexp "[\200-\221\223-\237]" nil t)
        (delete-char -1)
        (insert "·")))

    ;; Remember, the buffer is narrowed to the text that has been inserted, and the
    ;; current buffer is the buffer that contains the inserted text.
    (goto-char (point-min))

    (cond
     ;; Actions ...
     ((looking-at "^\\* \\(\\S-+\\) ")
      ;; My actions appear in erc-input-face; others' actions in erc-action-face.
      (if (string= (erc-current-nick) (match-string 1))
          (add-text-properties (point-min) (point-max) '(font-lock-face erc-input-face))
        (add-text-properties (point-min) (point-max) '(font-lock-face erc-action-face))))

     ;; Nick changes ...
     ((looking-at "^\\*\\*\\* .* \\((.*) \\)is now known as ")
      ;; Remove the users' fully-qualified identity from the message.
      (replace-match "" nil nil nil 1)
      (goto-char (point-min))
      (delete-char 4)
      (add-text-properties (point-min) (point-max)
                           `(font-lock-face my-erc-joinpart-face
                             my-erc-timestamp ,(float-time (current-time)))))

     ;; Parts ...
     ((looking-at "^\\(\\*\\*\\* \\).* \\((.*) has left channel\\)\\( \\S-+\\)\\(.*\\)")
      (let* ((partmsg (match-string 4))
             (cleanpartmsg (delete 34 (delete ?\\ partmsg))))   ;; 32 == '"'
        (replace-match cleanpartmsg nil nil nil 4)
        (replace-match (if (> (length partmsg) 0) ":" "") nil nil nil 3)
        (replace-match "has left" nil nil nil 2)
        (replace-match "" nil nil nil 1)
        (add-text-properties (point-min) (point-max)
                             `(font-lock-face my-erc-joinpart-face
                               my-erc-timestamp ,(float-time (current-time))))))

     ;; Quits ...
     ((looking-at "^\\(\\*\\*\\* \\).* \\((.*) \\)has quit: \\(.*\\)")
      (let* ((quitmsg (match-string 3))
             (cleanquitmsg (delete 34 (delete ?\\ quitmsg))))   ;; 34 == '"'
        (replace-match (propertize cleanquitmsg 'font-lock-face 'my-erc-quit-reason-face) nil nil nil 3)
        (replace-match "" nil nil nil 2)
        (replace-match "" nil nil nil 1)
        (add-text-properties (point-min) (point-max)
                             `(font-lock-face my-erc-joinpart-face
                               my-erc-timestamp ,(float-time (current-time))))))

     ;; Joins ...
     ((looking-at "^\\*\\*\\* .* \\((.*) has joined channel .*\\)")
      (replace-match "has joined" nil nil nil 1)
      (goto-char (point-min))
      (delete-char 4)
      (add-text-properties (point-min) (point-max)
                           `(font-lock-face my-erc-joinpart-face
                             my-erc-timestamp ,(float-time (current-time)))))


     ;; Mode changes ...
     ((looking-at "^\\*\\*\\* .* \\(has changed mode for \\(\\S-+\\) to\\) ")
      (let ((my-mode-target (match-string 2)))
        (save-match-data
          (if (erc-channel-p my-mode-target)
              (my-erc-update-titlebar))))
      (replace-match "sets mode" nil nil nil 1)
      (goto-char (point-min))
      (delete-char 4)
      (if (looking-at "^.* \\((.*) \\)")
          (replace-match "" nil nil nil 1))
      (add-text-properties (point-min) (point-max)
                           `(font-lock-face my-erc-modechange-face
                             my-erc-timestamp ,(float-time (current-time)))))

     ;; Post-join mode info ...
     ((looking-at-p ".* \\+\\S-*l [0-9]+")
      (my-erc-update-titlebar))

     ;; /NOTIFY output ...
     ((looking-at-p "^\\*\\*\\* \\(Detected .* on\\|left\\) IRC")
      (add-text-properties (point-min) (point-max) '(font-lock-face my-erc-notify-face)))

     ;; Kicks ...
     ((looking-at "^\\(\\*\\*\\* \\).* \\((.*) has kicked \\).*\\( off channel [^:]+\\):")
      (replace-match "" nil nil nil 3)
      (replace-match "kicks " nil nil nil 2)
      (replace-match "" nil nil nil 1)
      (add-text-properties (point-min) (point-max)
                           `(font-lock-face my-erc-kick-face
                             my-erc-timestamp ,(float-time (current-time)))))

     ;; /NAMES output ...
     ((looking-at "^\\(\\*\\*\\* Users on .*: \\)")
      (goto-char (point-min))
      (replace-match "  " nil nil nil 1))

     ;; Highlight the strings "trigger:" and "triggers:".
     ((looking-at "^.*\\(triggers?:\\)")
      (add-text-properties (match-beginning 1) (match-end 1) '(font-lock-face my-alert-face)))

     ;; Topic changes ...
     ;; *** bpt (~user@user86.net765.nc.sprint-hsd.net) has set the topic for #emacs: 
     ((looking-at "^\\(\\*\\*\\* \\).* \\(\\S-+ has set the topic for [^:]+:\\) \"\\(.*\\)\"")
      ;;((looking-at "^\\(\\*\\*\\* \\).* \\(has set the topic for [^:]+:\\) \"\\(.*\\)\"")
      (let ((my-newtopic (match-string 3)))
        (replace-match (propertize "sets topic to:" 'font-lock-face 'erc-notice-face) nil nil nil 2)
        (replace-match "" nil nil nil 1)
        (goto-char (point-min))
        (setq erc-channel-topic my-newtopic)
        (my-erc-update-titlebar))))

    ;; This must be done _after_ the above modifications.
    ;; QUESTION: Is this needed?  Variable erc-insert-modify-hook already contains erc-fill.
    (erc-fill)))

(defun my-erc-insert-post-hook ()
  "Added to erc-insert-post-hook.  When this function runs, the buffer is narrowed
to the just-inserted text."
  ;; This condition should always be true, but I saw this called in a non-ERC
  ;; buffer once.  Don't know how, so now I'm being paranoid.
  (when (eq major-mode 'erc-mode)

    (erc-truncate-buffer)

    ;; Keep the buffer modified flag on only if I've got partially-typed input.
    (save-restriction
      (widen)
      (if (= (point-max) (my-erc-start-of-input))
          (set-buffer-modified-p nil)))
      
    (my-erc-position-prompt)

    ;; Fontify nicks, and make the '<' and '>' characters around nicks have the same face
    ;; as the nicks.
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "^<\\([^>]+\\)>")
          ;;(add-text-properties (point-min) (match-end 0) '(font-lock-face erc-nick-default-face))
          (let* ((nick (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                 (nick-face (or (my-erc-nick-face nick) '(:forefound "red"))))
            (add-text-properties (point-min) (match-end 0) `(font-lock-face ,nick-face)))))

    (let ((channel-name (erc-default-target))
          (active-buffer (window-buffer (selected-window)))
          nick msg)

      ;; Alert me if someone is talking to me in this channel.
      (when (and (save-excursion
                   (goto-char (point-min))
                   (looking-at (concat "^<\\([^>]+\\)>.*\\b" (erc-current-nick) "\\b")))
                 (or my-erc-no-popups
                     (> (float-time (time-since my-erc-alert-time)) 300))
                 (not (eq active-buffer (current-buffer))))
        (setq nick (match-string 1))
        (when (not (member-ignore-case nick '("zzzbirny")))
          (when (not my-erc-no-popups)
            (setq my-erc-alert-time (current-time))
            (my-popup "ERC Alert!" (format "%s is talking to you in %s" nick channel-name) 'traytip)))))))

(defun my-focus-in-hook ()
  "Added to focus-in-hook.  Clears the echo area in all frames if I'm entering an ERC
devoted frame."
  (if (my-erc-is-frame-devoted)
      (mapc (lambda (frame)
              (with-selected-frame frame
                (message "")))
	    (frame-list))))

(add-hook 'focus-in-hook 'my-focus-in-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server output hooks.  These must return nil if remaining hooks are to run,
;; or t if remaining hooks are not to run.
;;
;; IMPORTANT: If these hooks are added to the front of their hook variables,
;; then they run _before_ the builtin ERC hook that process the server output,
;; otherwise they run after the builtin ERC hook!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-erc-server-366-function (proc parsed)
  "Added to erc-server-366-functions.  This indicates the end of a /NAMES list.  This runs
with the server buffer current."
  ;; Now that we have finished displaying the names list, set my-erc-hide-names
  ;; to once again equal my-erc-hide-names-on-join, as it did before the /NAMES
  ;; command was typed.
  (setq my-erc-hide-names my-erc-hide-names-on-join)

  ;; If the current channel has no devoted frame, emit a notice with the number of
  ;; users in this channel.  Otherwise, the frame title bar shows that value.
  (let ((target (second (erc-response.command-args parsed))))
    (with-current-buffer (erc-get-buffer target erc-server-process)
      (if (not (my-erc-devoted-frame (current-buffer)))
          (my-erc-display-notice (format "%d users in this channel."
                                         (hash-table-count erc-channel-users))
                                 (current-buffer)))))
  nil)

(defun my-erc-server-MODE-function (proc parsed)
  "Added to erc-server-MODE-functions."
  (let* ((buf (window-buffer (selected-window)))
         (tgt (first (erc-response.command-args parsed)))
         (mode (mapconcat 'identity (delete nil (erc-response.command-args parsed)) " "))
         (sndr (erc-parse-user (erc-response.sender parsed)))
         (nick (nth 0 sndr))
         (login (nth 1 sndr))
         (host (nth 2 sndr)))
    ;; Since function erc-server-MODE won't display the mode changes I make on myself in
    ;; the current buffer, I have to do it.
    (if (not (erc-channel-p tgt))
        (with-current-buffer buf
          (if (eq major-mode 'erc-mode)
              (erc-display-line (propertize (format "+++ %s sets mode %s on %s" nick mode tgt)
                                            'font-lock-face 'my-erc-modechange-face)
                                (current-buffer))))))
  nil)

(defun my-erc-server-JOIN-function (proc parsed)
  "Prepended to erc-server-JOIN-functions.  When this function runs due to
someone else joining, the channel buffer is current.  When this function runs
due to me joining a channel, the channel buffer has not been created yet."
  (let* ((channel-name (erc-response.contents parsed))
         (user (erc-parse-user (erc-response.sender parsed)))
         (nick (car user))
         (network (my-erc-network))
         (login (nth 1 user))
         (host (nth 2 user))
         (server-buffer (current-buffer))
         existing-frame)

    (if (string= (erc-current-nick) nick)
        ;; I just joined a channel, but the channel buffer isn't created yet.  After this
        ;; hook finishes, the builtin hook creates the channel buffer.

        ;; Reuse devoted frames and their buffers, but only if:
        ;;
        ;; o my-erc-join-no-new-frame is nil,
        ;; o erc-server-connected is nil in the buffer,
        ;; o the buffer was previously the buffer for the channel I'm joining,
        ;; o the buffer is on the same server as the channel I'm joining (???).

        (when (and (not my-erc-frameless)
                   (not my-erc-join-no-new-frame))
          (block existing-frame-search
            (dolist (frame (frame-list))
              (let ((channel-buffer (frame-parameter frame 'my-erc-devoted-frame-buffer)))
                (when (and channel-buffer
                           (buffer-live-p channel-buffer)
                           (string= (my-erc-buffer-name-noversion channel-buffer) channel-name)
                           (with-current-buffer channel-buffer
                             (and (null erc-server-connected)
                                  (string= (downcase (my-erc-network)) (downcase network)))))
                  (setq existing-frame frame)
                  (return-from existing-frame-search)))))

          (let ((newframe (or existing-frame
                              ;; We make the frame off the right edge of the display here,
                              ;; and we resize and reposition it later in
                              ;; my-erc-after-server-JOIN-function.
                              (make-frame `((top . 0)
                                            (left . ,(+ (my-frame-monitor-width) 3000)))))))
            ;; Initially, display the server buffer in the new frame, in case there's an error
            ;; during later join processing.
            (select-frame newframe)

            ;; This sit-for call makes the above select-frame stick under X in
            ;; certain circumstances.
            (if (not my-win32)
                (sit-for 0))

            (switch-to-buffer (erc-server-buffer))
            (my-erc-position-prompt)))

      ;; Someone else joined a channel I'm on.  Maybe popup a notification.  The
      ;; time limit is to make this less annoying when a netsplit ends.
      (let ((item (assoc-string channel-name my-erc-channels-alert-on-join 'ignore-case)))
        (if (and item
                 (not (string= "chanserv" (downcase nick)))
                 (string= (downcase (cdr item)) (downcase network))
                 (not my-erc-no-popups)
                 (> (float-time (time-since my-erc-alert-time)) 45))
            (my-popup "ERC Alert!"
                      (format "%s has just joined %s on network %s" nick channel-name
                              network) 'traytip)))))
  nil)

(defun my-erc-after-server-JOIN-function (proc parsed)
  "Added to the _end_ of erc-server-JOIN-functions.  When this function runs,
the channel buffer has been created, the current buffer is the server buffer,
and the selected frame is the new channel frame (if frames are being used).
This hook must be appended to the hook variable, so that it runs after the
builtin hook.  This hook is called when _any_ user joins a channel, not just
me."
  (let* ((user (erc-parse-user (erc-response.sender parsed)))
         (channel-buffer (erc-get-buffer (erc-response.contents parsed)))
         (nick (car user)))

    (when (string= (erc-current-nick) nick)
      ;; I just joined a channel, and all the builtin JOIN hooks have run.
      (set-buffer channel-buffer)

      (when (not my-erc-frameless)
        (if my-erc-join-no-new-frame
            (setq my-erc-join-no-new-frame nil)

          ;; A devoted frame is displaying the channel buffer.
          (set-frame-parameter nil 'my-erc-devoted-frame-buffer channel-buffer)

          ;; We have to call my-erc-position-frame in this hook instead of in
          ;; my-erc-server-JOIN-function, because it needs to know the channel
          ;; and network name of the buffer displayed in the frame, and that
          ;; buffer does not exist until this hook runs.
          (my-erc-position-frame (selected-frame)))

        ;; Hide the server frame.
        (my-erc-iconify-server-frame))

      (set-buffer-modified-p nil)))
  nil)

(defun my-erc-server-PRIVMSG-function (proc parsed)
  "Added to erc-server-PRIVMSG-functions.  Makes private messages appear in the active
window.  This hook gets called for all PRIVMSGs!  If the message is addressed to a
channel, this function _must_ return nil so that erc-server-PRIVMSG-or-NOTICE runs,
which causes ordinary channel messages to appear."
  (let* ((activebuf (window-buffer (selected-window)))
         (target (first (erc-response.command-args parsed)))
         (sender (car (erc-parse-user (erc-response.sender parsed))))
         (network (my-erc-network))
         (text (erc-response.contents parsed)))

    (if (erc-channel-p target)
        ;; It's a PRIVMSG to a channel.  Return nil to let other hooks process it.
        nil

      ;; It's a PRIVMSG to me personally ...
      (if (string-match-p "\C-a" text)
          ;; It's a CTCP message.  Let ERC handle it.
          nil

        ;; It's a /MSG to me ...

        ;; Should I ignore it?  I do this because ERC's /ignore support only applies to in-channel
        ;; PRIVMSGs.  This handles both kinds of PRIVMSGs.
        (if (erc-ignored-user-p sender)
            t

          ;; Remember the nick of the most recent person to /MSG to me.
          (with-current-buffer (erc-server-buffer)
            (setq my-erc-last-msg-sender sender))

          ;; Check if I have a query buffer open to the sender.  If so, display
          ;; it there, otherwise, continue my special handling.

          (if (block query-buffer-scan
                (dolist (query-buffer (erc-buffer-list 'erc-query-buffer-p))
                  (with-current-buffer query-buffer
                    (if (string= (erc-default-target) sender)
                        (return-from query-buffer-scan 'fall-back-to-erc-processing))))
                nil)

              ;; Let ERC display it in the proper query buffer.
              nil

            (if (eq (with-current-buffer activebuf major-mode) 'erc-mode)
                ;; Display the PRIVMSG in the currently active channel or server
                ;; buffer.  Although, ERC does that by default (now), this code
                ;; prepends "[network]" to the message.
                (progn
                  (with-current-buffer activebuf
                    (setq text
                          (let ((default-target (erc-default-target)))
                            (if (or (null default-target)
                                    (erc-channel-p default-target))
                                (propertize (concat "[" network "] *" sender "* " text)
                                            'font-lock-face 'erc-direct-msg-face)
                              (concat "<" (propertize sender 'font-lock-face 'erc-nick-default-face) "> "
                                      text))))
                    (erc-display-line text activebuf))
                  t)

              (if my-erc-frameless
                  ;; Put a message in the echo area.
                  (progn
                    (message (propertize (format "PRIVMSG from %s on %s" sender network)
                                         'font-lock-face 'my-alert-face))
                    nil)

                ;; The currently active buffer is not an ERC buffer, so save the /MSG
                ;; in buffer *privmsg* and maybe pop up a window on it.
                (save-excursion
                  (setq my-erc-last-privmsg-time (current-time))
                  (let ((orig-window (selected-window)))
                    (pop-to-buffer "*privmsg*")
                    (set-window-text-height nil 6)
                    (goto-char (point-max))
                    (insert (my-erc-add-timestamp (propertize (concat "[" network "] *" sender "* "
                                                                      (erc-controls-interpret text))
                                                              'font-lock-face 'erc-direct-msg-face))
                            "\n")
                    (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer)
                    (when (eq (get-buffer "*privmsg*") (window-buffer (selected-window)))
                      (forward-line -1)
                      (recenter -1))
                    (select-window orig-window)))
                t))))))))

(defun my-erc-server-NOTICE-function (proc parsed)
  "Added to erc-server-NOTICE-functions.  Makes notices appear in the active
window."
  (let* ((activebuf (window-buffer (selected-window)))
         (target (first (erc-response.command-args parsed)))
         (sender (car (erc-parse-user (erc-response.sender parsed))))
         (network (my-erc-network))
         (text (propertize (concat "[" network "] -" sender "- " (erc-response.contents parsed))
                           'font-lock-face 'erc-direct-msg-face))
         chanbuf)

    (if (erc-channel-p target)
        ;; It's a NOTICE to a channel.  Return nil to let other hooks process it.
        nil

      ;; It's a NOTICE to me personally.
      ;; Should I ignore it?
      (if (erc-ignored-user-p sender)
          t

        (if (eq (with-current-buffer activebuf major-mode) 'erc-mode)
            ;; Display the NOTICE in the current buffer.
            (progn
              (erc-display-line text activebuf)
              t)

          (if my-erc-frameless
              ;; Put a message in the echo area.
              (progn
                (message (propertize (format "NOTICE from %s on %s" sender network)
                                     'font-lock-face 'my-alert-face))
                nil)

            ;; Pop up a buffer to display the NOTICE.
            (save-excursion
              (let ((orig-window (selected-window)))
                (pop-to-buffer "*notice*")
                (set-window-text-height nil 6)
                (goto-char (point-max))
                (insert (my-erc-add-timestamp (propertize (erc-controls-interpret text)
                                                          'font-lock-face 'erc-direct-msg-face))
                        "\n")
                (local-set-key (kbd "q") 'my-delete-window-and-bury-buffer)
                (forward-line -1)
                (recenter -1)
                (select-window orig-window)))
            t))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC Commands.  These functions must return t or else a "bad syntax" error
;; is reported to the user.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erc-cmd-QUIET (nick)
  "..."
  (erc-server-send (format "MODE %s +q %s!*@*" (erc-default-target) nick)))

(defun erc-cmd-UNQUIET (nick)
  "..."
  (erc-server-send (format "MODE %s -q %s!*@*" (erc-default-target) nick)))

(defun erc-cmd-BAN (nick &optional test)
  "Bans nick from the current channel.  If nick contains either '!' or '@', then it
is taken as a literal banmask to use (not as a nick)."
  (let ((banspec "")
        (nickinfo (car (erc-get-channel-user nick)))
        host)
    (if (null nickinfo)
        (if (string-match-p "[@!]" nick)
            (if test
                (my-erc-alert (format "MODE %s +b %s" (erc-default-target) nick))
              (erc-server-send (format "MODE %s +b %s" (erc-default-target) nick)))

          (my-erc-alert "%s is not in this channel!" nick))

      (setq host (erc-server-user-host nickinfo))

      (if (null host)
          (progn
            ;; ISSUE: Can this ever happen?
            (my-erc-alert "+++ Please issue the /BAN command again.")
            (erc-cmd-WHOIS nick))

        (if (string-match "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\.\\)[0-9]+\$" host)
            (setq host (concat (match-string 1 host) "*"))
          (if (or (string-match "\\([^.]+\\.[^.]+\\.[^.]+\\)$" host)
                  (string-match "\\([^.]+\\.[^.]+\\)$" host))
              (setq host (concat "*" (match-string 1 host)))))

        (if test
            (my-erc-alert (format "MODE %s +b *!*@%s" (erc-default-target) host))
          (erc-server-send (format "MODE %s +b *!*@%s" (erc-default-target) host))))))
  t)

(defun pcomplete/erc-mode/BAN ()
  (while (pcomplete-here (pcomplete-erc-nicks)))) 

(defun erc-cmd-BK (nick)
  "Bans then kicks NICK from the current channel."
  (erc-cmd-BAN nick)
  (erc-cmd-KICK nick "Yow!")
  t)

(defun pcomplete/erc-mode/BK ()
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun erc-cmd-CLOSE (&optional reason no-prompt)
  "Quits from the current server with optional reason REASON (defaults to the value
returned by my-erc-quit-reason), and arranges to for my-erc-disconnected-hook to kill all
of this server's buffers after disconnection.  If optional parameter NO-PROMPT is non-nil,
no confirmation prompt is displayed."
  (let* ((reason (if (stringp reason)
                     (erc-trim-string reason)
                   ""))
         (my-network-name (my-erc-network))
         (my-prompt (if (not no-prompt)
                        (if (string= "UNKNOWN" my-network-name)
                            (format "Really kill all buffers for server buffer <%s>? " (erc-server-buffer))
                          (format "Really quit network %s and kill all its buffers? " (my-erc-network))))))

    (if (and (not no-prompt)
             (not (yes-or-no-p my-prompt)))
        (my-erc-alert "/CLOSE aborted!")

      (if (not erc-server-connected)
          ;; This server is not connected, so there's no need to QUIT.  Just kill all
          ;; of this server's buffers.
          (my-erc-kill-buffers-for-server 'all (erc-server-buffer))

        (setq my-erc-closing t)
        (erc-cmd-QUIT reason)))))

(put 'erc-cmd-CLOSE 'do-not-parse-args t)

(defun erc-cmd-COUNT ()
  "Reports the number of users in the current channel."
  (if (erc-channel-p (erc-default-target))
      (erc-display-line (propertize (format "+++ %d users in this channel."
                                            (hash-table-count erc-channel-users))
                                    'font-lock-face '(:foreground "green"))
                        (current-buffer))))
        
(defalias 'erc-cmd-C 'erc-cmd-COUNT)

(defun erc-cmd-DEOPME ()
  "Ops me using ChanServ."
  (erc-cmd-MSG (format "ChanServ op %s -%s" (erc-default-target) (erc-current-nick))))

(defun erc-cmd-DH ()
  "Shows usage for /DCC command."
  (erc-display-line
   (propertize "/dcc chat NICK                  - Accept/offer chat w/ NICK
/dcc close TYPE [NICK]          - Close connection TYPE (send/get/chat) w/ NICK
/dcc get NICK [FILE]            - Accept file-send offer from NICK
/dcc list                       - List all offers and connections
/dcc send NICK FILE             - Offer to send FILE to NICK\n"
               'font-lock-face 'my-alert-face)
   'active)
  t)

(defun erc-cmd-EXEC (command)
  "Executes the arguments as a Bash command and sends the output to the current channel."
  (let* ((output (my-command (erc-trim-string command)))
         (lines (split-string output "\n" 'omit-nulls))
         line)
    (if (or (= 1 (length lines))
            (y-or-n-p "Output contains newlines, send anyway? "))
        (if (or (<= (length lines) 3)
                (y-or-n-p "Output contains more than 3 lines, send anyway? "))
            (while (prog1
                       (setq line (car lines))
                     (setq lines (cdr lines)))
              (erc-send-message (if (string= "" line) " " line))
              (if lines
                  (sit-for 2.0))))))
  (message "Done.")
  t)

(put 'erc-cmd-EXEC 'do-not-parse-args t)

(defun erc-cmd-EXIT (reason)
  "Quits from all servers and kills all ERC buffers and frames.  Parameter REASON is a
string to be passed to erc-cmd-CLOSE (and ultimately to erc-cmd-QUIT)."
  (setq reason (erc-trim-string reason)
        my-erc-exiting t)

  (when (eq major-mode 'erc-mode)
    ;; This function can be called in the *ERC Buffer List* buffer, so only
    ;; reposition the prompt if this is an ERC buffer.
    (my-erc-position-prompt))

  (if (not (yes-or-no-p "Really quit all servers and kill all ERC buffers? "))
      (my-erc-alert "/EXIT aborted!")

    (let ((erc-flood-protect nil))
      (dolist (serverbuf (erc-buffer-list 'erc-server-buffer-p))
        (with-current-buffer serverbuf
          (if (and erc-server-connected
                   (not (string= reason "/f")))

              ;; Close this server.
              (erc-cmd-CLOSE reason 'no-prompt)

            ;; This server buffer is not connected (or we don't care that it is
            ;; due to the /f option), so kill the server buffer first and then
            ;; all associated channel and query buffers.
            (my-erc-kill-buffers-for-server 'all)))
        
        ;; TODO: Remove this eventually.
        ;; This seems to prevent the problem where one channel buffer is left in
        ;; existence after all other ERC buffers are killed.
        ;;(sleep-for 2)
        )))
  t)

(put 'erc-cmd-EXIT 'do-not-parse-args t)

(defun erc-cmd-ID (password)
  "Identifies me to NickServ."
  (let ((network (downcase (my-erc-network)))
        (mynick (erc-current-nick)))
    (cond
     ((or (string= network "freenode")
          (string= network "oftc"))
      (erc-cmd-MSG (format "nickserv identify %s" password)))

     ((string= network "undernet")
      (erc-cmd-MSG (format "X@channels.undernet.org login %s %s" mynick password)))

     ((string= network "quakenet")
      (erc-cmd-MSG (format "q@cserve.quakenet.org auth %s %s" mynick password)))

     ((string= network "dalnet")
      (erc-cmd-MSG (format "nickserv@services.dal.net identify %s" password)))

     ((string= network "galaxynet")
      (erc-cmd-MSG (format "NS@services.galaxynet.org AUTH %s %s" mynick password)))

     (t (error "Network (%s) not supported!" network))))
  t)

(defun erc-cmd-IGNOREFOR (seconds nick)
  "Ignores NICK for SECONDS seconds, after which time NICK is no longer ignored."
  (erc-cmd-IGNORE nick)
  (run-at-time seconds nil
               `(lambda ()
                  (let* ((chanbuf ,(current-buffer))
                         (serverbuf ,(erc-server-buffer)))
                    (when (buffer-live-p serverbuf)
                      (with-current-buffer serverbuf
                        (erc-cmd-UNIGNORE ,nick))
                      (if (buffer-live-p chanbuf)
                          (erc-display-line (erc-make-notice (format "Auto unignoring %s" ,nick))
                                            chanbuf))))))
  t)

(defun pcomplete/erc-mode/IGNOREFOR ()
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun erc-cmd-JB (channel)
  "Same as erc-cmd-JOIN, but does not create a new frame."
  (setq my-erc-join-no-new-frame t)
  (erc-cmd-JOIN channel))

(defalias 'erc-cmd-KB 'erc-cmd-BK)

(defun pcomplete/erc-mode/KB ()
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun erc-cmd-LIMIT (&optional limit)
  "Sets the channel limit for the current channel."
  (if limit
      (erc-server-send (format "MODE %s +l %s" (erc-default-target) limit))
    (erc-server-send (format "MODE %s -l" (erc-default-target)))))

(defun erc-cmd-NETWORK (name)
  "Sets the current network name to NAME."
  (setq name (erc-string-no-properties name))

  (erc-with-server-buffer
    (let ((my-networkcons (assoc "NETWORK" erc-server-parameters))
          (my-orig-network-name (erc-network-name)))
      (if my-networkcons
          (setcdr my-networkcons name)
        (setq erc-server-parameters (cons (cons "NETWORK" name) erc-server-parameters)))

      (setq erc-network (intern name)
            my-erc-network-name name)

      (erc-display-line (propertize (format "+++ Set network name to \"%s\" (was \"%s\")." name
                                            my-orig-network-name)
                                    'font-lock-face 'my-alert-face)
                        'active)))
  t)

(defun erc-cmd-NICKALL (new-nick)
  "Sets my nick to NEW-NICK on all connected networks."
  (let ((erc-flood-protect nil)
        (origbuf (current-buffer)))
    (dolist (serverbuf (erc-buffer-list 'erc-server-buffer-p))
      (with-current-buffer serverbuf
        (if erc-server-connected
            (with-current-buffer serverbuf
              ;; This is a no-op if my nick is already NEW-NICK.
              (erc-cmd-NICK new-nick)))))))

(defun erc-cmd-NOWARN (string)
  "Adds STRING to my-erc-no-warn-at-start.  Also modifies the file .erc-nowarn."
  (if (my-erc-add-to-no-warn-at-start string)
      (my-erc-alert "+++ Added '%s' no-warn list." string)
    (my-erc-alert "+++ '%s' is already on the no-warn list." string))
  t)

(defun erc-cmd-OPME ()
  "Ops me using ChanServ."
  (erc-cmd-MSG (format "ChanServ op %s %s" (erc-default-target) (erc-current-nick))))

(defun erc-cmd-RAW (&rest command)
  "Sends raw IRC protocol COMMAND to the server."
  (if (not (erc-server-process-alive))
      (my-erc-alert "This buffer has no server connection!")

    (if (= 0 (length command))
        (my-erc-alert "Usage: /RAW <raw-irc-protocol-data>")

      (let ((string (mapconcat 'identity command " ")))
        (erc-display-line (concat (erc-make-notice "Sending: \"")
                                  (propertize string 'font-lock-face 'erc-input-face)
                                  "\"")
                          (current-buffer))
        (process-send-string erc-server-process (concat string "\n")))))
  t)

(defun erc-cmd-RECOVER (nick password)
  "Makes NickServ recover and release NICK."
  (if (string= (erc-current-nick) nick)
      (my-erc-alert "+++ You're nick is already %s!" nick)

    ;; Assume I'm on Freenode: use the ghost command.
    (erc-cmd-MSG (format "NickServ ghost %s %s" nick password))))

(defun erc-cmd-THINK (&rest words)
  "Makes it appear you are thinking WORDS."
  (erc-send-message (propertize (format ". o O ( %s )"
                                        (mapconcat 'identity words " "))
                                'font-lock-face 'erc-input-face))
  t)

(add-to-list 'erc-noncommands-list 'erc-cmd-THINK)

(defun erc-cmd-VOICE (&rest nicks)
  "Sets mode +v on each nick in NICKS."
  (if (null nicks)
      (my-erc-alert "+++ usage: /VOICE nick [nick ...]")
    (dolist (nick nicks)
      (erc-server-send (format "MODE %s +v %s" (erc-default-target) nick))
      (sleep-for 0.5)))
  t)

(defun pcomplete/erc-mode/VOICE ()
  (while (pcomplete-here (pcomplete-erc-nicks))))

(defun erc-cmd-UNBAN (mask)
  "Unbans the specified MASK."
  (erc-server-send (format "MODE %s -b %s" (erc-default-target) mask))
  t)

(defun pcomplete/erc-mode/UNBAN ()
  ;; As of Emacs 24, channel-banlist is named erc-channel-banlist.
  (while (pcomplete-here (mapcar 'cdr erc-channel-banlist))))

(defun erc-cmd-UNVOICE (&rest nicks)
  "Sets mode -v on each nick in NICKS."
  (if (null nicks)
      (my-erc-alert "+++ usage: /UNVOICE nick [nick ...]")
    (dolist (nick nicks)
      (erc-server-send (format "MODE %s -v %s" (erc-default-target) nick))
      (sleep-for 0.5)))
  t)

(defun pcomplete/erc-mode/UNVOICE ()
  (while (pcomplete-here (pcomplete-erc-nicks))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-erc-nick-master-faces
  (let ((my-erc-nick-face-alist '((orange       . "orange")     ;; #ffa500
                                  (cyan         . "cyan")       ;; #00ffff
                                  (green        . "green")      ;; #00ff00
                                  (lightgreen   . "lightgreen") ;; #90ee90
                                  (pink         . "pink")       ;; #ffc0cb
                                  (skyblue      . "#55aaff")
                                  (lightcyan    . "#66dddd") ;; Looks too much like cyan.
                                  (yellowgreen  . "#c9ef4d")
                                  (brightblue   . "#8888ff")
                                  (tan          . "#eab671")
                                  (lightmagenta . "#f02288")
                                  (darkred      . "#ff0030")
                                  (lightred     . "#ff8072")
                                  (redishorange . "#ff5544") ;; Looks too much like darkred.
                                  (lightpurple  . "#ff88b0")
                                  (paleyellow   . "#fff278"))))
    (mapcar (lambda (faceinfo)
              (let ((newface (make-face (intern (concat "my-erc-nick-face-"
                                                        (symbol-name (car faceinfo)))))))
                (set-face-foreground newface (cdr faceinfo))
                newface))
            my-erc-nick-face-alist))
  "A list of face symbols used by function my-erc-nick-face when associating a face with a
nick.  The buffer-local variable my-erc-nick-channel-faces is initialized to a copy of
this list.")

(defvar my-erc-nick-faces-alist nil
  "An alist (local to each channel buffer) mapping nick's to face symbols.  This alist is
initially empty in each channel buffer and grows as faces are allocated to nicks.")

(defvar my-erc-nick-channel-faces nil
  "A list (local to each channel buffer) of face symbols from which faces are allocated by
function my-erc-nick-face.  As each face is allocated from this list it is removed from
this list until this list is empty.  This list is initialized to a copy of
my-erc-nick-master-faces when the first face is allocated to a nick in each channel
buffer.")

(defun my-erc-nick-face (nick)
  "Maps a nick to a face.  The mapping is consistent for the last 30 nicks to speak in a
given channel.  When all faces have been allocated and a new nick needs a color, the
oldest nick removed from the mapping and its face is reused."
  (ignore-errors
    (when (null my-erc-nick-faces-alist)
      ;; First use of this function in this buffer.  Initialize local value of
      ;; my-erc-nick-channel-faces.  IMPORTANT: Must use copy-sequence here, otherwise the
      ;; below calls to delete on my-erc-nick-channel-faces modify
      ;; my-erc-nick-master-faces!
      (make-local-variable 'my-erc-nick-faces-alist)
      (make-local-variable 'my-erc-nick-channel-faces)
      (setq my-erc-nick-channel-faces (copy-sequence my-erc-nick-master-faces)))

    (let ((mapping (assoc-string nick my-erc-nick-faces-alist 'casefold)))
      (if mapping
          (progn
            ;; Promote this nick's mapping to the front of the alist.
            (setq my-erc-nick-faces-alist (delete mapping my-erc-nick-faces-alist))
            (setq my-erc-nick-faces-alist (cons mapping my-erc-nick-faces-alist))
            (cdr mapping))

        ;; No mapping found.
        (if my-erc-nick-channel-faces
            ;; Allocate an unused face.
            (let ((my-face (nth (random (length my-erc-nick-channel-faces)) my-erc-nick-channel-faces)))
              (setq my-erc-nick-faces-alist (cons (cons nick my-face) my-erc-nick-faces-alist))
              (setq my-erc-nick-channel-faces (delete my-face my-erc-nick-channel-faces))
              my-face)

          ;; Eject the oldest mapping from the alist.
          (let ((reclaimed-face (cdr (car (last my-erc-nick-faces-alist)))))
            (setq my-erc-nick-faces-alist (delete (car (last my-erc-nick-faces-alist))
                                                  my-erc-nick-faces-alist))
            (setq my-erc-nick-faces-alist (cons (cons nick reclaimed-face) my-erc-nick-faces-alist))
            reclaimed-face))))))

(defun my-erc-delete-blank-lines ()
  "Deletes all blank lines in the current buffer."
  (ignore-errors
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (and (search-forward-regexp "^$" nil t)
                    (not (eobp)))
          (beginning-of-line)
          (let ((start (point)))
            (forward-line 1)
            (delete-region start (point))))))))

(defun my-erc-kill-buffers-for-server (&optional which server-buffer)
  "Kills some or all buffers associated with SERVER-BUFFER.  Parameter WHICH is one of the
following: 'all, 'server, 'non-server.  If SERVER-BUFFER is nil, it defaults to the
current buffer.  If WHICH is nil, it defaults to 'all."
  (if (and (null server-buffer)
           (not (erc-server-buffer-p (current-buffer))))
      (error "INTERNAL ERROR: my-erc-kill-buffers-for-server: server-buffer is nil in a non-server buffer!"))

  (let ((server-buffer (or server-buffer (current-buffer)))
        (which (or which 'all)))

    ;; This with-current-buffer form is necessary, because the code below references
    ;; buffer-local variable erc-server-process, and this function may not have been
    ;; invoked with server-buffer as the current buffer.
    (with-current-buffer server-buffer

      (if (memq which '(all server))
          ;; Prevent Emacs from asking if it's OK to exit because of this server
          ;; connection.  QUESTION: Is this really needed?  We're about to kill the
          ;; server buffer, which terminates the connection if it still exists.
          (set-process-query-on-exit-flag (with-current-buffer server-buffer
                                            erc-server-process)
                                          nil))

      ;; We have to compute the list of channel and query buffers before we kill
      ;; the server buffer, because erc-server-process is nil after the server
      ;; buffer is killed.
      (let ((channel-and-query-buffers (erc-buffer-list (lambda () (not (erc-server-buffer-p)))
                                                        erc-server-process)))
        ;; Maybe kill the channel and query buffers.
        (if (memq which '(all non-server))
            (dolist (buf channel-and-query-buffers)
              (kill-buffer buf)))

        ;; Maybe kill the server buffer.
        (if (memq which '(all server))
            (kill-buffer server-buffer))))))

(defun my-erc-part-quit-endquery ()
  "Quits the current server, parts the current channel, or ends the current query after
asking the user to confirm the action."
  (interactive)
  (if (not (my-erc-is-frame-devoted))
      ;; In non-devoted frames, behave just like my global binding for "C-x C-c".
      (call-interactively (lookup-key global-map (kbd "C-x C-c")))

    ;; The current frame is a devoted ERC frame.
    (if (erc-server-buffer-p)
        ;; The current buffer is a server buffer.  Call erc-cmd-CLOSE.
        (erc-cmd-CLOSE)

      ;; The current buffer is a non-server ERC buffer: channel, query, ... ???
      (if (my-erc-channel-buffer-p)
          ;; It's a channel buffer.
          (if (not (y-or-n-p "Really kill this channel buffer? "))
              (error "Aborted!")

            ;; ERC sends a PART command when a channel buffer is killed.  The devoted
            ;; frame is closed by my-erc-kill-channel-hook.
            (kill-buffer))

        (if (my-erc-query-buffer-p)
            ;; It's a query buffer.
            (if (not (y-or-n-p "Really kill this query buffer? "))
                (error "Aborted!")
              (kill-buffer (current-buffer)))

          ;; This should never happen.  ISSUE: What about DCC buffers?
          (error "INTERNAL ERROR in my-erc-part-quit-endquery!"))))))

(defun my-erc-channel-buffer-p (&optional buffer)
  "Returns t if BUFFER (or current buffer if BUFFER is nil) is an ERC channel buffer,
nil otherwise."
  (let ((buffer (or buffer (current-buffer))))
    (and (eq major-mode 'erc-mode)
         (not (erc-server-buffer-p buffer))
         (= ?# (aref (buffer-name buffer) 0)))))

(defun my-erc-query-buffer-p (&optional buffer)
  "Returns t if BUFFER (or current buffer if BUFFER is nil) is an ERC query buffer,
nil otherwise."
  (let ((buffer (or buffer (current-buffer))))
    (and (eq major-mode 'erc-mode)
         (not (erc-server-buffer-p buffer))
         (not (my-erc-channel-buffer-p buffer)))))

(defun my-erc-send-input (text &optional channel network)
  "Sends TEXT as if it were typed in CHANNEL on NETWORK.  CHANNEL and NETWORK are strings.
If CHANNEL is nil, it defaults to the current channel buffer.  If NETWORK is nil, it
defaults to the current network."
  (let ((channel (or channel (and (my-erc-channel-buffer-p)
                                  (erc-default-target))
                     (error "Current buffer (%s) is not a channel buffer!" (buffer-name))))
        (network (or network (my-erc-network)
                     (error "Current buffer (%s) is not an ERC buffer!" (buffer-name)))))

    ;; Get a one-element list containing the desired channel buffer.
    (let ((channel-buffers (erc-buffer-list (lambda ()
                                              (and (string= (my-erc-network) network)
                                                   (string= (erc-default-target) channel))))))
      ;; If channel-buffers has more than one element, something is very wrong.
      (if (> (length channel-buffers) 1)
          (error "INTERNAL ERROR in my-erc-send-input!"))

      ;; If channel-buffers is nil, the supplied channel or network names were wrong.
      (if (null channel-buffers)
          (error "my-erc-send-input: No channel named %s on network %s!" channel network))
      
      (with-current-buffer (car channel-buffers)
        (goto-char (point-max))
        (insert text)
        (erc-send-current-line)))))

(defun my-erc-display-notice (string &optional buffer)
  "..."
  (erc-display-line (my-erc-make-notice string) buffer))

(defun my-erc-make-notice (string)
  "Just like erc-make-notice, except the notice starts with '+++' instead of '***'."
  (let ((erc-notice-prefix "+++ "))
    (erc-make-notice string)))

(defun my-erc-cleanup-channels ()
  "..."
  (let ((now (float-time (current-time))))
    (dolist (chanbuf (erc-channel-list nil))
      (with-current-buffer chanbuf
        ;; Make sure the mode line is gone when using frames.
        (if (not my-erc-frameless)
            (setq mode-line-format nil))

        ;; Delete all blank lines.
        (my-erc-delete-blank-lines)

        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp "^[^< #]" nil t)
            (let ((timestamp (get-text-property (point) 'my-erc-timestamp)))
              (if (and (numberp timestamp)
                       (> (- now timestamp) my-erc-channel-cleanup-age))
                  (progn
                    (beginning-of-line)
                    (let ((start (point)))
                      (end-of-line)

                      ;; This search-forward-regexp skips over intangible text (i.e., the
                      ;; channel prompt), which sometimes leaves point after the prompt.
                      ;; Cope with that.

                      (if (search-forward-regexp "^[^ ]" nil t)
                          (let ((inhibit-read-only t))
                            (if (= (1+ (length (my-erc-prompt-function)))
                                   (- (point) erc-insert-marker))
                                ;; We are positioned immediately after the prompt.  Move back past it.
                                (forward-char -1) ;; Moves point back past channel prompt.
                              ;; This works everywhere except on the line containing the prompt.
                              (beginning-of-line))
                            (delete-region start (point))))))
                (end-of-line)
                ))))

        (my-erc-position-prompt)))))

(defun my-erc-kill-all-disconnected-buffers ()
  "Kills all disconnected ERC buffers and their devoted frames without interacting with
the server."
  (interactive)
  (if (not (yes-or-no-p "Really kill all disconnected ERC buffers? "))
      (error "Aborted!"))

  (let ((count 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'erc-mode)
                   (not erc-server-connected))
          (kill-buffer buffer)
          (setq count (1+ count)))))

    (message "Killed %d disconnected buffers." count)))

(defun my-erc-kill-all-buffers ()
  "Kills all ERC buffers and their devoted frames.  Unlike the /EXIT and /QUIT commands,
this does not issue a quit command before killing any of that server's buffers.  If a
server buffer is connected, ERC will react to these buffers being killed by sending QUIT
or PART commands to the server."
  (interactive)
  (if (not (yes-or-no-p "Really kill all ERC buffers without quitting any server? "))
      (error "Aborted!"))

  (dolist (server-buffer (erc-buffer-list 'erc-server-buffer-p))
    (my-erc-kill-buffers-for-server 'all server-buffer)))

(defun my-erc-delete-unwanted-text ()
  "Deletes all non-discussion text from the current ERC buffer, leaving other
text untouched.  This correctly handles server notices that have wrapped onto
additional lines."
  (interactive)
  (dolist (chanbuf (erc-buffer-list))
    (with-current-buffer chanbuf
      ;; Make sure the mode line is gone when using frames.
      (if (not my-erc-frameless)
          (setq mode-line-format nil))

      ;; Delete all blank lines.
      (my-erc-delete-blank-lines)

      (when (erc-channel-p (current-buffer))
        (while (progn
                 (goto-char (point-min))
                 (or (search-forward-regexp "^\\(\\*\\*\\*\\|\\+\\+\\+\\|-[^-]+-\\|-ChanServ-\\|\\[[^]]+\\] -[^-]+-\\|\\S-+ has quit:\\|\\S-+ sets mode\\|\\S-+ is now known as\\) " nil t)
                     (search-forward-regexp "^\\S-+ has joined$" nil t)
                     (search-forward-regexp "^\\S-+ has left\\(: .*\\)?$" nil t)))
          (beginning-of-line)
          (let ((start (point)))
            (forward-line 1)
            (while (looking-at-p erc-fill-prefix)
              (forward-line 1))
            (let ((inhibit-read-only t))
              (delete-region start (point)))))
        (goto-char (point-max))
        (my-erc-position-prompt)))))

(defun my-erc-insert-channel-name ()
  "Inserts the name of the IRC channel associated with the current buffer."
  (interactive)
  (if (erc-server-buffer-p)
      (error "This is not a channel buffer!")
    (insert (erc-default-target))))

(defun my-erc-add-to-no-warn-at-start (string)
  "Adds STRING to my-erc-no-warn-at-start and to ~/.erc-nowarn.  Returns t if successful,
nil if STRING is already there."
  (if (not (member-ignore-case string my-erc-no-warn-at-start))
      (let ((nowarnfile "~/.erc-nowarn")
            (standard-output (lambda (str) nil)))
        (add-to-list 'my-erc-no-warn-at-start
                     (downcase (erc-string-no-properties string)))
        (if (file-exists-p nowarnfile)
            (rename-file nowarnfile (concat nowarnfile ".bak") 'overwrite))
        (write-region (pp `(setq my-erc-no-warn-at-start ',my-erc-no-warn-at-start))
                      nil nowarnfile nil 'nomessage)
        t)))

(defun my-erc-read-one-char (prompt chars)
  "Prompts the user with PROMPT (which should end with a space), then reads one
character of input that must be one of the characters in the list CHARS.  Does
not return until a valid character is typed.  Returns the character that was
typed."
  (let ((done nil)
        (cursor-in-echo-area t)
        input)
    (while (not done)
      (setq input (read-char-exclusive prompt))
      (if (memq input chars)
          (setq done t)
        (ding)
        (let ((msg "Please type '"))
          (setq msg (concat msg
                            (mapconcat 'char-to-string (cl-subseq chars 0 -1) "', '")
                            "', or '"
                            (char-to-string (car (last chars)))
                            "'."))
          (message msg)
          (sit-for 2))))
    input))

(defun my-erc-make-frame-devoted ()
  "Makes the selected frame devoted to the current buffer."
  (interactive)
  (if (not (eq major-mode 'erc-mode))
      (error "my-erc-make-frame-devoted: Buffer '%s' is not an ERC buffer!" (buffer-name)))
  (set-frame-parameter nil 'my-erc-devoted-frame-buffer (current-buffer))
  (message "This frame is now devoted to buffer %s." (buffer-name)))

(defun my-erc-is-frame-devoted (&optional frame)
  "Returns non-nil if FRAME is a devoted frame.  FRAME defaults to the selected frame."
  (frame-parameter (or frame (selected-frame)) 'my-erc-devoted-frame-buffer))

(defun my-erc-show-last-dcc-get-file ()
  "Displays the last file to be retrieved using /DCC GET in a new frame."
  (interactive)
  (select-frame (make-frame '((my-center . t))))
  (find-file my-erc-dcc-last-file))

(defun my-erc-refill-buffer ()
  "Re-fills each paragraph in the buffer.  Does nothing in server buffers."
  (interactive)
  (if (not (erc-channel-p (current-buffer)))
      (error "Cannot re-fill non-channel buffers!"))

  (my-erc-update-erc-fill-column)
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t) ;; Inhibit the 'intangible text property.
        (fill-column erc-fill-column)
        (fill-prefix erc-fill-prefix)
        ;;(para-start-regexp "^[<#[*+-]")
        (para-start-regexp "^[^ ]")
        start)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp para-start-regexp nil t)
        (beginning-of-line)
        (setq start (point))
        (end-of-line)
        (if (search-forward-regexp para-start-regexp nil 'move-to-eob-if-not-found)
            (beginning-of-line))

        ;; Don't fill the input line, otherwise the line breaks will cause multiple lines
        ;; to be sent to the server.
        (if (< (point) (my-erc-start-of-input))
            ;; Must use fill-region-as-paragraph instead of fill-region, because the
            ;; latter fails to fill lines of input I have entered that start with '/'!
            (fill-region-as-paragraph start (point) nil t nil))))))

(defun my-erc-server-connected-networks ()
  "Returns a list of names of the networks to which we are currently connected."
  (let (result)
    (dolist (serverbuf (erc-buffer-list (lambda ()
                                          (and (erc-server-buffer-p)
                                               erc-server-connected))))
      (setq result (cons (with-current-buffer serverbuf (my-erc-network))
                         result)))
    result))

(defun my-erc-network-channels (&optional network)
  "Returns a list of the names of all joined channels on NETWORK (defaults to
current network if nil).  Copes correctly with multiple server connections."
  (let (result
        (network (or network (my-erc-network))))
    (dolist (buf (erc-buffer-list))
      (with-current-buffer buf
        (if (and (erc-channel-p (current-buffer))
                 (string= (downcase network) (downcase (my-erc-network))))
            (add-to-list 'result (erc-default-target)))))
    result))

(defun my-erc-buffer-name-noversion (&optional buffer)
  "Returns the name of BUFFER (or the current buffer if BUFFER is nil) minus any trailing
version suffix (e.g., \"<2>\").  BUFFER can also be a string."
  (let* ((buf (or buffer (current-buffer)))
         (bufname (if (stringp buffer) buffer (buffer-name buf))))
    (if (string-match "^\\(.*\\)<[0-9]+>$" bufname)
        (match-string 1 bufname)
      bufname)))

(defun my-erc-add-timestamp (text)
  "Prefixes TEXT with a timestamp.  Returns the timestamped text."
  (concat (propertize (concat "[" (time-stamp-string "%02H:%02M:%02S") "]")
                      'font-lock-face 'erc-timestamp-face)
          " " text))

(defun my-erc-debug-irc-protocol ()
  "Enables logging of IRC protocol traffic.  Displays buffer *erc-protocol* in
a new frame.  Kill the *erc-protocol* buffer to stop logging."
  (interactive)
  (if erc-debug-irc-protocol
      (error "IRC protocol debugging is already enabled!"))
  (when (null (get-buffer-window "*erc-protocol*" t))
    (my-frame-center (select-frame (make-frame '((name . "*erc-protocol*")
                                                 (width . 170)
                                                 (height . 20)))))
    (switch-to-buffer "*erc-protocol*"))
  (erc-toggle-debug-irc-protocol)
  (setq truncate-lines t)
  (local-set-key (kbd "RET") 'erc-toggle-debug-irc-protocol))

(defun my-erc-version ()
  "Displays the ERC version in the minibuffer."
  (interactive)
  (message erc-version-string))

(defun my-erc-alert (string &rest args)
  "Displays (apply #'format STRING ARGS) in face 'my-alert-face in the current buffer
if it is an ERC buffer, otherwise displays it in the minibuffer."
  (let ((msg (propertize (apply #'format string args) 'font-lock-face 'my-alert-face)))
    (if (eq major-mode 'erc-mode)
        (progn
          (goto-char (point-max))
          ;; ISSUE: erc-display-line does not always display text in a disconnected
          ;; server buffer.  This may have something to do with variable
          ;; erc-active-buffer.
          (erc-display-line msg (current-buffer)))

      ;; The current buffer is not an ERC buffer, so display the alert in the
      ;; minibuffer.
      (message msg)
      (sit-for 2))))

(defun my-erc-alert-all-buffers-for-server (string &rest args)
  "Display an alert in every ERC buffer associated with the current server."
  (let ((msg (propertize (apply #'format string args) 'font-lock-face 'my-alert-face)))
    (erc-display-line msg 'all)))

(defun my-erc-alert-all-buffers (string &rest args)
  "Display an alert in every ERC buffer associated with every server."
  (let ((msg (propertize (apply #'format string args) 'font-lock-face 'my-alert-face)))
    (dolist (server-buffer (erc-buffer-list 'erc-server-buffer-p))
      (with-current-buffer server-buffer
        (erc-display-line msg 'all)))))

(defun my-erc-tab ()
  "Bound to TAB in ERC buffers.  If invoked in an empty input line, this inserts \"/msg
xxx \", where xxx is the last nick that messaged me, otherwise returns nil, so that the
normal ERC TAB processing can proceed."
  (interactive)
  (if (save-excursion (erc-bol) (eobp))
      (progn
        (insert (propertize (format "/msg %s " (or (with-current-buffer (erc-server-buffer)
                                                     my-erc-last-msg-sender)
                                                   (error "No nick available!")))
                            'font-lock-face 'erc-input-face))
        t)

    ;; ERC's standard nick completion doesn't cycle through available completions when
    ;; pcomplete-cycle-completions is non-nil, so this makes nick-cycling work.  Let
    ;; pcomplete complete the nick at point.  Need to fake out the value of last-command
    ;; to get pcomplete to cycle nicks on subsequent TAB presses.  If last-command is
    ;; 'my-erc-tab, this is a 2nd or subsequent press of TAB, so bind last-command to
    ;; 'pcomplete, to make it do cycling of available completions.
    (let ((last-command (if (eq last-command 'my-erc-tab)
                            'pcomplete
                          last-command)))
      (pcomplete t))))

(defun my-erc-part-reason (&optional reason)
  "Called by ERC to generate a part reason string.  Optional parameter REASON should be a
string.  If REASON is nil, a default reason is returned."
  (if (and (stringp reason)
           (setq reason (erc-trim-string reason))  ;; Always true.
           (not (string= "" reason))
           (not (string= "\n" reason)))
      ;; Use the reason supplied by the user to the /PART command.
      reason

    ;; Compute the part reason.
    (let ((my-network-name (my-erc-network))
          (my-channel-name (if (erc-channel-p (buffer-name))
                               (buffer-name)
                             "")))
      (cond
       ((string= my-network-name "...")
        "...")
       ((string= my-channel-name "#...")
        "...")
       (t
        "O Elbereth! Gilthoniel! We still remember ...")))))

(defun my-erc-quit-reason (&optional reason)
  "Called by ERC to generate a quit reason string.  Optional parameter REASON should be a
string.  If REASON is nil, a default reason is returned."
  ;; For now, use the same algorithm as for /PART reasons.
  (my-erc-part-reason reason))

(defun my-erc-update-erc-fill-column ()
  "Sets a new value for erc-fill-column based on the width of the windows displaying
the current buffer.  If no window is displaying the current buffer, erc-fill-column is
unchanged."
  (let ((windows (get-buffer-window-list (current-buffer) nil t))
        (width 99999))
    (when windows
      (dolist (win windows)
        (if (< (window-width win) width)
            (setq width (window-width win))))
      (setq erc-fill-column (- width 2)))))

(defun my-erc-kill-ring-save ()
  "Like kill-ring-save, but removes all text properties from the saved text."
  (interactive)
  (kill-new (buffer-substring-no-properties (point) (mark t)))
  (deactivate-mark))

(defun my-erc-query-nick (nick)
  "Uses erc-cmd-QUERY to start a /QUERY to NICK."
  (interactive (list (completing-read "Query nick: " (erc-get-channel-nickname-alist))))
  (if (or (not (stringp nick))
          (string= "" nick))
      (error "Invalid nick!"))
  (erc-cmd-QUERY nick))

(defun my-erc-scroll-down ()
  "Like scroll-down but doesn't (ding) when the top of the buffer is reached."
  (interactive)
  (if (= 1 (window-start))
      (message "Beginning of buffer.")
    (scroll-down)))

(defun my-erc-scroll-up ()
  "Like scroll-up but always leaves prompt positioned on last line of the window."
  (interactive)
  (when (>= erc-insert-marker (window-end))
    (scroll-up)

    ;; IMPORTANT: Do _not_ remove this (sit-for 0), otherwise (window-end)
    ;; returns the pre-scrolled window end instead of the post-scrolled window
    ;; end!
    (sit-for 0) 

    (when (< erc-insert-marker (window-end))
      (goto-char (point-max))
      (my-erc-position-prompt))))

(defun my-erc-devoted-frame (buffer)
  "Returns the frame devoted to BUFFER (a string or buffer) if one exists, nil
otherwise."
  (let (result
        (bufferobj (if (stringp buffer)
                       (get-buffer buffer)
                     buffer)))
    (if (null bufferobj)
        ;; If BUFFER is the name of a non-existant buffer, just return nil.
        nil

      (if (not (bufferp bufferobj))
          (error "my-erc-devoted-frame: invalid argument!"))

      (dolist (frame (frame-list))
        (if (and (null result)
                 (frame-live-p frame)
                 (eq bufferobj (frame-parameter frame 'my-erc-devoted-frame-buffer)))
            (setq result frame)))

      result)))

(defun my-erc-topic (&optional buffer)
  "Returns the topic string for BUFFER, which defaults to the current buffer if nil."
  (if (not (bufferp buffer))
      (setq buffer (current-buffer)))
  (let ((topic (with-current-buffer buffer (erc-controls-strip erc-channel-topic))))
    (if (or (not (stringp topic))
            (= 0 (length topic)))
        "[no topic set]"
      (car (split-string topic "")))))

(defun my-erc-update-titlebar (&optional buffer)
  "Updates the titlebar of the frame devoted to BUFFER, which must be an ERC
buffer or the name of one.  If BUFFER-OR-FRAME is nil, it defaults to the
current buffer."
  (if (null buffer)
      (setq buffer (current-buffer))

    (if (stringp buffer)
        (setq buffer
              (or (get-buffer buffer)
                  (error "my-erc-update-titlebar: No buffer named '%s'!" buffer)))))

  (let ((frame (my-erc-devoted-frame buffer)))
    (if frame
        (let* ((target (or (erc-default-target)
                           (my-erc-buffer-name-noversion)))
               (network (my-erc-network))
               (topic (my-erc-topic buffer))
               (titlebar (if (erc-channel-p target)
                             ;; Channel frame titlebar.
                             (format "%s (%s) %d%s  %s" target network
                                     (hash-table-count erc-channel-users)
                                     (if (numberp erc-channel-user-limit)
                                         (format "/%d" erc-channel-user-limit)
                                       "")
                                     topic)
                           ;; Server and query frame titlebar.
                           (format "%s (%s)" network target))))
          (set-frame-parameter frame 'name titlebar)))))

(defun my-erc-format-titlebar (channel-or-server network nickcount nicklimit topic)
  "Returns a string suitable for setting as the titlebar in an ERC buffer frame.
Arguments are CHANNEL-OR-SERVER (a string), NETWORK (a string), NICKCOUNT (an
integer), and TOPIC (a string)."
  (if (erc-channel-p channel-or-server)
      (format "%s (%s) %d%s  %s" channel-or-server network nickcount
              (if nicklimit
                  (or (ignore-errors (format "/%d" nicklimit))
                      "/inf")
                "")
              topic)
    (format "%s (%s)" network channel-or-server)))

(defun my-erc-position-prompt (&optional buffer)
  "If BUFFER is an ERC buffer, then in every window displaying BUFFER, if the
prompt is visible, positions the line containing the prompt at the bottom of the
window.  If BUFFER is nil, it defaults to the current buffer.  The entire input
area is made visible, so the prompt may not end up on the bottom line of the
window if the input area contains enough text to wrap onto a new line."
  (if (null buffer)
      (setq buffer (current-buffer)))
  (if (not (bufferp buffer))
      (error "my-erc-position-prompt: argument #1 is not a buffer!"))

  (if (with-current-buffer buffer
        (not (eq major-mode 'erc-mode)))
      (error "my-erc-position-prompt: argument #1 is not an ERC buffer: %s" (buffer-name)))

  (save-restriction
    (widen)

    ;; When this function runs, the selected window might not be the window that is showing
    ;; the current buffer, so we have to preserve each independantly.  IMPORTANT: Don't use
    ;; save-window-excursion here, because it undoes the effect of the below call to
    ;; recenter.
    (let ((orig-window (selected-window))
          (orig-buffer (current-buffer)))

      (dolist (window (get-buffer-window-list buffer nil t))

        ;; Select the window displaying BUFFER.
        (select-window window)

        ;; Only do something if the prompt is visible.  Be sure to use < and not <= when
        ;; comparing erc-insert-marker to the end of the window.  If the prompt is on the
        ;; line immediately below the last visible line, <= returns true, and we think the
        ;; prompt is visible when it isn't.

        (if (and (or (< erc-insert-marker (window-end window))
                     ;; This save-excursion form is a hack to make this function work when
                     ;; called from an erc-cmd-XXX function.  When those functions execute,
                     ;; the prompt has been deleted and erc-insert-marker _equals_ the end
                     ;; of the window.
                     (save-excursion
                       (goto-char erc-insert-marker)
                       (and (eobp)
                            (= erc-insert-marker (window-end window)))))
                 (>= erc-insert-marker (window-start window)))
            ;; The prompt is visible.
            (save-excursion
              ;; By going to (point-max) here, the entire input line is made visible (even if
              ;; it's wider than the current window.
              (goto-char (point-max))
              (recenter -1))))

      (select-window orig-window)
      (set-buffer orig-buffer))))

(defun my-erc-delete-backward-char (N)
  "Just like delete-backward-char, but doesn't display \"Text is read only\" when
executed at start of input area."
  (interactive "p")
  (if (/= (point) (my-erc-start-of-input))
      (call-interactively #'delete-backward-char)))

(defun my-erc-backward-char (N)
  "Just like backward-char, but doesn't move past the prompt."
  (interactive "p")
  (if (/= (point) (my-erc-start-of-input))
      (call-interactively #'backward-char)))

(defun my-erc-backward-kill-word (N)
  "Just like backward-kill-word, but doesn't display \"Test is read only\" when
executed at start of input area.  Also doesn't leave point before the prompt."
  (interactive "p")
  (if (/= (point) (my-erc-start-of-input))
      (call-interactively #'backward-kill-word)))

(defun my-erc-position-all-frames ()
  "Positions and sizes all devoted frames according to each frame's ERC buffer
channel and network name."
  (interactive)
  (dolist (erc-buffer (erc-buffer-list))
    (let ((devoted-frame (my-erc-devoted-frame erc-buffer)))
      (if devoted-frame
          (my-erc-position-frame devoted-frame)))))

(defun my-erc-position-frame (frame)
  "Positions and sizes FRAME according to its channel and network name."
  (interactive (list (selected-frame)))

  (if (null (frame-parameter frame 'my-erc-devoted-frame-buffer))
      (error "This is not a devoted frame."))

  (if (erc-server-buffer-p (frame-parameter frame 'my-erc-devoted-frame-buffer))
      ;; It's displaying a server buffer.
      (modify-frame-parameters frame erc-frame-alist)

    ;; It's displaying a non-server buffer (channel, query, etc.), find the location
    ;; metrics for this channel and network.
    (let* ((channelname (with-current-buffer (window-buffer (frame-first-window frame))
                          (erc-default-target)))
           (networkname (with-current-buffer (window-buffer (frame-first-window frame))
                          (my-erc-network)))
           (location-metrics (if (and channelname networkname)
                                 (cdr (assoc (cons (downcase networkname) (downcase channelname))
                                             my-erc-channel-frame-locations))))
           (left-metric (nth 0 location-metrics))
           (top-metric (nth 1 location-metrics))
           (width-metric (nth 2 location-metrics))
           (height-metric (nth 3 location-metrics)))

      ;; Map the location metrics to frame parameters.
      (if (null location-metrics)
          ;; We have no location metrics for this frame, so use our default
          ;; position.
          (progn
            (modify-frame-parameters frame '((width . 120) (height . 20)))
            (my-frame-redisplay-hack nil 'try-hard)
            (my-frame-center frame))

        (if (and (= 5 (length location-metrics))
                 (eq ':exact (car location-metrics)))
            ;; We have the exact left/top/width/height frame parameters for this frame.
            (modify-frame-parameters frame `((left . ,(nth 1 location-metrics))
                                             (top . ,(nth 2 location-metrics))
                                             (width . ,(nth 3 location-metrics))
                                             (height . ,(nth 4 location-metrics))))

          ;; We have location metrics.  Convert them to frame parameters and pixel
          ;; dimensions.
          (let* ((frame-info (my-erc-metrics-to-frame-info left-metric top-metric
                                                           width-metric height-metric))
                 (left (nth 0 frame-info))
                 (top (nth 1 frame-info))
                 (width (nth 2 frame-info))
                 (height (nth 3 frame-info)))

            ;; First, set the frame's position.
            (set-frame-position frame left top)

            ;; Next, set the frame's width/height in pixels.
            (set-frame-size frame width height t)))))))

(defun my-erc-metrics-to-frame-info (left-metric top-metric width-metric height-metric)
  "Maps a devoted frame's location metrics to a list of frame parameters.  The return
value is a list of the form (LEFT TOP WIDTH HEIGHT), where LEFT and TOP are left/top frame
parameter values and WIDTH and HEIGHT are in pixels."
  (if (not (memq left-metric '(left center right)))
      (error "my-erc-metrics-to-frame-info: Invalid left-metric parameter (%s)!" left-metric))

  (if (or (not (integerp top-metric))
          (> top-metric 4)
          (< top-metric 0))
      (error "my-erc-metrics-to-frame-info: Invalid top-metric parameter (%s)!" top-metric))

  (if (not (memq width-metric '(1/3 1/2 2/3 full)))
      (error "my-erc-metrics-to-frame-info: Invalid width-metric parameter (%s)!" width-metric))

  (if (or (not (integerp height-metric))
          (< height-metric 1)
          (> height-metric 5))
      (error "my-erc-metrics-to-frame-info: Invalid height-metric parameter (%s)!" height-metric))

  (let* ((monitor-pixel-width (my-frame-monitor-width))
         (monitor-pixel-height (my-frame-monitor-height))
         (totalrows 5)

         ;; Padding-related values.
         (screen-bottom-padding 16)
         (horizontal-frame-shrinkage 1)
         (vertical-frame-padding 1)

         ;; Compute the pixel width of the frame.  So that this value does not create
         ;; frames with zero space between them, we reduce this value by
         ;; horizontal-frame-shrinkage.
         (frame-pixel-width (- (cond ((eq width-metric '1/3)
                                      (/ monitor-pixel-width 3))

                                     ((eq width-metric '1/2)
                                      (/ monitor-pixel-width 2))

                                     ((eq width-metric '2/3)
                                      (truncate (* monitor-pixel-width 0.6666)))

                                     ((eq width-metric 'full)
                                      monitor-pixel-width))
                               horizontal-frame-shrinkage))

         ;; Compute the pixel height of a one-row-high frame.
         (min-frame-pixel-height (/ (- monitor-pixel-height screen-bottom-padding)
                                    totalrows))

         (frame-left (cond ((eq 'left left-metric)
                            0)

                           ((eq 'center left-metric)
                            (my-frame-pixel-width-to-centered-left frame-pixel-width))

                           ((eq 'right left-metric)
                            (my-frame-pixel-width-to-rightmost-left frame-pixel-width))))

         (frame-top (* top-metric (+ min-frame-pixel-height vertical-frame-padding)))

         ;; This is in pixels not character columns.
         (frame-width (- frame-pixel-width 16 (my-frame-left-right-border-width)))

         ;; This is in pixels not chracters rows.
         (frame-height (- (+ (* height-metric min-frame-pixel-height)
                             (* (1- height-metric) vertical-frame-padding))
                          (my-frame-top-bottom-border-width))))

    (list frame-left frame-top frame-width frame-height)))

(defun my-erc-network ()
  "Returns the name of the IRC network for the current ERC buffer, or the string \"UNKNOWN\"
if the network name cannot be determined, or nil if the current buffer is not an ERC buffer."
  (if (eq major-mode 'erc-mode)
      ;; First, ask ERC.
      (let ((my-network-name (erc-network-name)))
        (if (and (stringp my-network-name)
                 (not (string= "nil" my-network-name))) ;; Yes, this can happen.
            my-network-name

          ;; Next, try server buffer local variable my-erc-network-name.
          (or (erc-with-server-buffer
                (if (stringp my-erc-network-name)
                    my-erc-network-name))

              ;; Lastly, we don't know.
              "UNKNOWN")))))

(defun my-erc-join-channel (channel &optional prefix)
  "Like erc-join-channel, but allows completion of channel names.  Interactively, a prefix
argument means to pause a short time before joining."
  (interactive (list (completing-read "Join channel: "
                                      (mapcar (lambda (elt) (cons (cdr (car elt)) nil))
                                              my-erc-channel-frame-locations)
                                      nil nil "#" 'my-erc-join-channel-history
                                      nil nil)
                     current-prefix-arg))
  (if (string-match-p "^\\s-*$" channel)
      (error "No channel specified!"))

  ;; If C-u prefix was given, sleep 3 seconds before joining the channel.  This
  ;; gives me time to switch to a different virtual desktop.
  (if (equal '(4) prefix)
      (sleep-for 3))

  (if (string= "#" channel)
      ;; I'm auto-joining a set of channels.  Get the list of channels to auto-join.
      (let ((channels (cdr (assoc-string (my-erc-network) my-erc-channel-sets 'case-fold)))
            (erc-flood-protect nil))
        (if (null channels)
            (error "No channels to auto-join on this network!"))

        (dolist (chan channels)
          (when (and (my-erc-join-channel chan)
                     (not (string= chan (car (last channels)))))
            ;; Sleep 3 seconds before joining the next channel.  This helps avoid
            ;; flood protection on some networks.  This redisplay call helps update
            ;; the content of channel windows when many channels are being joined.
            (sleep-for 4)
            (redisplay))))

    ;; I'm joining a specific channel.
    (let (key)
      (if (string-match "^\\([^,]*\\),\\(.*\\)$" channel)
          (setq key (match-string 2 channel)
                channel (match-string 1 channel)))

      (if (not (erc-channel-p channel))
          (error "Invalid channel name: %s" channel))

      (if (and key (string-match-p "^\\s-*$" key))
          (error "Invalid key: \"%s\"" key))

      (erc-cmd-JOIN channel key)
      t)))

(defun my-erc-toggle-big ()
  "Toggles a channel frame between small-and-docked and large-and-centered."
  (interactive)
  (if (null (frame-parameter nil 'my-erc-devoted-frame-buffer))
      (error "This is not a devoted ERC frame."))

  (if (frame-parameter nil 'my-erc-channel-frame-is-big)
      ;; It's big.  Make it normal.
      (progn
        (modify-frame-parameters nil '((my-erc-channel-frame-is-big . nil)))
        (my-erc-position-frame (selected-frame))
        (my-show-max-text))

    ;; It's not big.  Make it big.
    (modify-frame-parameters nil '((my-erc-channel-frame-is-big . t)))
    (modify-frame-parameters nil '((height . 32) (width . 130)))
    (my-frame-redisplay-hack)
    (my-frame-center))
  
  (set-mouse-pixel-position (selected-frame) 3 3))

(defun my-erc-make-server-frame-visible (&optional frame)
  "Makes the server frame for the current ERC buffer (or FRAME if given) visible.  Also
raises it to the top of the Z-order."
  (if my-erc-frameless
      (error "Sorry, my-erc-frameless is non-nil!"))

  (let* ((serverbuf (erc-server-buffer))
         (frame (or frame
                    (if serverbuf (my-erc-devoted-frame serverbuf)))))
    (when (frame-live-p frame)
      (make-frame-visible frame)
      (raise-frame frame))))

(defun my-erc-iconify-server-frame ()
  "Minimizes the server frame for the current ERC buffer."
  (if my-erc-frameless
      (error "There is no server frame!"))
  (let* ((serverbuf (erc-server-buffer))
         (frame (if serverbuf (my-erc-devoted-frame serverbuf))))
    (if (frame-live-p frame)
        (iconify-frame frame))))

(defun my-erc-maybe-make-server-frame-visible ()
  "Makes visible the frame that is devoted to the server buffer if there are no remaining
channel buffers for this server."
  (when (not my-erc-frameless)
    (let ((channels (erc-channel-list erc-server-process)))
      (if (<= (length channels) 1)
          (my-erc-make-server-frame-visible)))))

(defun my-erc-toggle-server-frame-visibility ()
  "Toggles the visibility of the frame showing the ERC server buffer for the connection
associated with the current buffer."
  (interactive)
  (if my-erc-frameless
      (error "There is no server frame!"))

  (let* ((serverbuf (erc-server-buffer))
         (frame (if serverbuf (my-erc-devoted-frame serverbuf))))
    (if (and (called-interactively-p 'any)
             (null frame))
        (error "The server buffer has no devoted frame!"))

    (if (frame-live-p frame)
        (if (eq t (frame-visible-p frame))
            ;; The server buffer devoted frame is visible.
            (iconify-frame frame)

          ;; The server buffer devoted frame is iconified or invisible.
          (make-frame-visible frame)
          (my-erc-position-frame frame)
          (raise-frame frame)))))

(defun my-erc-start-of-input ()
  "Returns the location of the first character of user input after the prompt."
  (save-excursion
    (goto-char erc-insert-marker)
    (end-of-line)
    (erc-bol)
    (point)))

(defun my-erc-end-of-buffer ()
  "Moves point to end of buffer, then displays maximum text."
  (interactive)
  (goto-char (point-max))
  (recenter -1))

(defun my-erc-insert-at-eob ()
  "Moves point to end of buffer and inserts the character that invoked this command."
  (interactive)
  (goto-char (point-max))
  (my-erc-position-prompt)
  ;; Don't insert anything if user pressed ENTER.
  (if (/= 13 (aref (this-command-keys) 0))
      (call-interactively #'self-insert-command)))

(defun my-erc-previous-command (prefix)
  "If point is after end of prompt, recalls previous line from input history,
otherwise moves to previous line."
  (interactive "p")
  (let ((start-of-input (my-erc-start-of-input)))
    (if (or (< (point) start-of-input)
            (string-match-p "\n" (buffer-substring-no-properties start-of-input (point))))
        (with-no-warnings
          (previous-line prefix))
      (erc-previous-command))))

(defun my-erc-next-command (prefix)
  "If point is after end of prompt, recalls next line from input history,
otherwise moves to next line."
  (interactive "p")
  (let ((start-of-input (my-erc-start-of-input)))
    (if (or (< (point) start-of-input)
            (string-match-p "\n" (buffer-substring-no-properties (point) (point-max))))
        (with-no-warnings
          (next-line prefix))
      (erc-next-command))))

(defun my-erc-clear-screen ()
  "Clears the current ERC buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (input-text (buffer-substring (my-erc-start-of-input) (point-max)))
        (erased-text (buffer-substring (point-min) erc-insert-marker)))
    (if (> (length erased-text) 0)
        (setq my-erc-erased-text erased-text))
    (erase-buffer)
    (erc-display-prompt)
    (goto-char (point-max))
    (insert input-text)
    (set-buffer-modified-p nil)))

(defun my-erc-clear-all-screens ()
  "Clears all ERC buffers."
  (interactive)
  (let (devoted-frame
        (orig-frame (selected-frame)))
    (dolist (buf (erc-buffer-list))
      (when (erc-channel-p buf)
        (setq devoted-frame (my-erc-devoted-frame buf))
        (if devoted-frame
            (select-frame devoted-frame))
        (with-current-buffer buf
          (my-erc-clear-screen))))
    (select-frame orig-frame)))

(defun my-erc-prompt-function ()
  "Bound to erc-prompt.  Be careful -- this function is called with buffers current that
are not displayed in the currently selected window or frame (and my not be displayed in
any window)!"
  (let* (titlebar
         (prompt (if (erc-server-buffer-p)
                     ;; It's a server buffer.
                     (if (stringp erc-session-server)
                         erc-session-server
                       ;; Sometimes erc-session-server is null, so we use the buffer name.
                       (let ((bufname (my-erc-buffer-name-noversion)))
                         (if (string-match "^\\([^:]+\\):" bufname)
                             (match-string 1 bufname)
                           bufname)))
                   ;; It's a channel buffer.
                   (erc-default-target)))

         (propertized-prompt (erc-propertize (concat prompt ">")
                                             'read-only t
                                             'intangible t
                                             'front-nonsticky t)))

    (if (eq (current-buffer)
            (frame-parameter nil 'my-erc-devoted-frame-buffer))
        (my-erc-update-titlebar))

    propertized-prompt))

(defun my-erc-connect (server &optional port nick fullname network)
  "Connects to PORT on SERVER on NETWORK as NICK with fullname FULLNAME.  PORT defaults to
erc-default-port.  NICK defaults to value of variable my-erc-nick.  FULLNAME defaults to
value of variable my-erc-fullname.  If USE-TLS is non-nil, connects using function erc-tls
instead of erc.  Returns the server buffer object if the connection was successful,
otherwise returns nil."
  (interactive (progn
                 (if (/= (minibuffer-depth) 0)
                     (error "Cannot connect to server while minibuffer is active!"))
                 (if (null (display-graphic-p))
                     (error "ERC cannot be used without a graphical display!"))
                 (list (read-from-minibuffer "Server hostname (or IP): ")
                       (read-from-minibuffer "Port: " (int-to-string erc-default-port))
                       (read-from-minibuffer "Nick: " my-erc-nick)
                       (read-from-minibuffer "Fullname: " my-erc-fullname)
                       nil)))

  (if (not (stringp server))
      (error "my-erc-connect: argument #1 is not a string!"))

  (when (string= "?" server)
    (setq server (read-string "Enter server hostname: "))
    (sleep-for 3))

  (if (stringp port)
      (setq port (string-to-number port)))
  (if (or (null port)
          (= 0 port))
      (setq port (int-to-string erc-default-port)))
  (if (null nick)
      (setq nick my-erc-nick))
  (if (null fullname)
      (setq fullname my-erc-fullname))
  (if (null network)
      (setq network "UNKNOWN"))

  ;; Sanity check the arguments.
  (if (not (integerp port))
      (error "my-erc-connect: argument #2 is not an integer!"))
  (if (not (stringp nick))
      (error "my-erc-connect: argument #3 is not a string!"))
  (if (not (stringp fullname))
      (error "my-erc-connect: argument #4 is not a string!"))
  (if (not (stringp network))
      (error "my-erc-connect: argument #5 is not a string!"))

  ;; If network name was given, check for double connect to same network.
  (dolist (buf (erc-buffer-list))
    (with-current-buffer buf
      (let ((serverbuf (erc-server-buffer)))
        (if (buffer-live-p serverbuf)
            (with-current-buffer serverbuf
              (if (and erc-server-connected
                       (string= (downcase (my-erc-network)) (downcase network)))
                  (error "You are already connected to network %s!" network)))))))

  (let (use-tls serverframe serverbuffer password)

    ;; If SERVER starts with "ssl:", use function erc-tls instead of erc to connect
    ;; to the server.
    (if (string= "ssl:" (substring server 0 4))
        (setq use-tls t
              server (substring server 4 nil)))

    ;; Reuse server buffers and their devoted frames, but only if the server buffer
    ;; is disconnected.
    (setq serverbuffer (get-buffer (concat server ":" (int-to-string port))))
    
    (if serverbuffer
        (with-current-buffer serverbuffer
          (if erc-server-connected
              ;; I'm already connected to this server.  Set serverbuffer to nil so that function
              ;; erc creates a new server buffer.  BUG: ERC reuses _connected_ server buffers!!!
              (setq serverbuffer nil)

            ;; Erase the old contents of the server buffer.  This fixes a bug where a previous
            ;; message about failing to connect remains at the bottom of the server buffer even
            ;; after the next successful connection.
            (let ((inhibit-read-only t))
              (erase-buffer)))))

    (when (not my-erc-frameless)
      (if serverbuffer
          (setq serverframe (my-erc-devoted-frame serverbuffer)))

      (if serverframe
          (my-erc-make-server-frame-visible serverframe))

      (when (null serverframe)
        (sleep-for 2)

        ;; Recompute erc-frame-alist on every connection, because the screen
        ;; resolution may have changed (e.g., due to launching Emacs in an RDP
        ;; session, etc.).
        (setq erc-frame-alist `((top . 0)
                                (left . ,(my-frame-pixel-width-to-rightmost-left
                                          (my-frame-width-to-pixels 90)))
                                (width . 90)
                                (height . 14)))

        (setq serverframe (make-frame erc-frame-alist))
        (raise-frame (select-frame serverframe))

        ;; QUESTION: Why not always switch to the ERC pre-connect buffer?  And maybe
        ;; display useful info about the pending connection?
        (if (null serverbuffer)
            (switch-to-buffer " *ERC Pre-connect Buffer*")

          ;; A disconnected server buffer already exists, so switch to it.
          (switch-to-buffer serverbuffer)
          (set-frame-parameter serverframe 'my-erc-devoted-frame-buffer serverbuffer))))

    ;; I'm invisible by default.
    (setq erc-user-mode (cdr (assoc-string network my-erc-user-modes t))
          my-erc-closing nil
          my-erc-exiting nil)

    ;; If using SASL to authenticate to this server, read the password.  Do this
    ;; after the above call to make-frame and select-frame, so the prompt appears in
    ;; the devoted frame for the server buffer.
    (setq password
          (block my-read-sasl-password
            (dolist (regexp erc-sasl-server-regexp-list)
              (if (string-match-p regexp server)
                  (return-from my-read-sasl-password
                    (read-passwd (format "Enter password for server %s: " server)))))))

    ;; Connect to the IRC server.  Temporarilly bind function erc-get-buffer-create
    ;; to return the existing server buffer (if there is one), otherwise to create a
    ;; server buffer as it normally would.
    (setq serverbuffer (my-with-advice ((erc-get-buffer-create
                                         (lambda (origfun &rest args)
                                           (or serverbuffer
                                               (apply origfun args)))))

                         ;; Function erc throws an error if the connect fails.
                         ;; ignore-errors returns nil if an error is thrown, otherwise
                         ;; the value returned by the form, which is the server
                         ;; buffer.
                         (ignore-errors (funcall (if use-tls #'erc-tls #'erc)
                                                 :server server
                                                 :port port
                                                 :nick nick
                                                 :password password
                                                 :full-name fullname))))
    (if (null serverbuffer)
        ;; Connection failed.
        (message "my-erc-connect: Connection to %s:%d failed!" server port)

      ;; Successfully connected.  If we made a new frame, make it devoted to the server buffer.
      (if (and (not my-erc-frameless)
                 serverframe)
          (modify-frame-parameters serverframe
                                   `((my-erc-devoted-frame-buffer . ,(current-buffer))
                                     (name . ,(format "%s (%s:%d)" network server port))))))

    serverbuffer))

(defun my-erc-networkids-for-completing-read (network-specs)
  "Helper function that generates the TABLE argument to be passed to
completing-read in my-erc-networkid-connect.  See that function for more info."
  (let (result
        entry)
    (dolist (network network-specs)
      (dolist (item (cdr network))
        (setq entry (concat (car network) " " (car item))
              result (append result (list (cons entry entry))))))
    result))

(defun my-erc-networkid-connect (&optional networkid)
  "Connects to NETWORKID.  Looks up NETWORKID's server, port, etc. using
variable my-erc-networks."
  (interactive (progn
                 ;; Don't connect if the minibuffer is active.  ERC doesn't behave well in that case.
                 (if (minibuffer-window-active-p (minibuffer-window))
                     (error "Can't connect to IRC when minibuffer is active!"))
                 (list (completing-read "IRC Network: "
                                        (my-erc-networkids-for-completing-read my-erc-networks)
                                        nil t `(,my-erc-default-networkid . 0) 'my-erc-connect-history
                                        nil nil))))
  (if (null networkid)
      (setq networkid my-erc-default-networkid))

  (if (not (stringp networkid))
      (error "my-erc-networkid-connect: argument #1 is not a string!"))

  (if (not (string-match "^\\(\\S-+\\)\\s-+\\(.*\\)$" networkid))
      (error (format "Malformed network-id: %s" networkid)))

  (let* ((network (match-string 1 networkid))
         (serverid (match-string 2 networkid))
         (serverinfo (cdr (assoc-string serverid
                                        (cdr (assoc-string network my-erc-networks 'casefold))
                                        'casefold))))
    (if (null serverinfo)
        (error (format "my-erc-networkid-connect: Unrecognized network ID: %s" networkid)))

    (add-to-list 'serverinfo network 'append)

    (apply 'my-erc-connect serverinfo)))
