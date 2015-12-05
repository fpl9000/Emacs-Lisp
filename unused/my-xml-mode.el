;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My personal XML editting mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Things to do:
;;
;; o Don't set the buffer-modified flag if indentation didn't change.
;; o Make my-xml-electric-open-tag handle end tags too.
;; o Write my-xml-goto-prev-level and my-xml-goto-prev-element.

(defvar my-xml-indent-level 2
  "Number of columns to indent new element blocks beyond the enclosing block.")

;; (makunbound 'my-xml-docbook-tags)
(defvar my-xml-docbook-tags
  '(("abbrev") ("abstract") ("accel") ("ackno") ("acronym") ("action")
    ("address") ("affiliation") ("alt") ("anchor") ("answer") ("appendix")
    ("appendixinfo") ("application") ("area") ("areaset") ("areaspec")
    ("arg") ("article" multiline) ("articleinfo") ("artpagenums") ("attribution")
    ("audiodata") ("audioobject") ("author" multiline) ("authorblurb") ("authorgroup")
    ("authorinitials") ("beginpage") ("bibliocoverage") ("bibliodiv")
    ("biblioentry") ("bibliography") ("bibliographyinfo") ("biblioid")
    ("bibliomisc") ("bibliomixed") ("bibliomset") ("bibliorelation")
    ("biblioset") ("bibliosource") ("blockinfo") ("blockquote") ("book")
    ("bookinfo") ("bridgehead") ("callout") ("calloutlist") ("caption")
    ("caution") ("chapter") ("chapterinfo") ("citation") ("citebiblioid")
    ("citerefentry") ("citetitle") ("city") ("classname") ("classsynopsis")
    ("classsynopsisinfo") ("cmdsynopsis") ("co") ("collab") ("collabname")
    ("colophon") ("colspec") ("command") ("computeroutput") ("confdates")
    ("confgroup") ("confnum") ("confsponsor") ("conftitle") ("constant")
    ("constraint") ("constraintdef") ("constructorsynopsis") ("contractnum")
    ("contractsponsor") ("contrib") ("copyright") ("coref") ("corpauthor")
    ("corpname") ("country") ("database") ("date") ("dedication") ("destructorsynopsis")
    ("edition") ("editor") ("email") ("emphasis") ("entry") ("entrytbl")
    ("envar") ("epigraph") ("equation") ("errorcode") ("errorname") ("errortext")
    ("errortype") ("example" multiline) ("exceptionname") ("fax") ("fieldsynopsis")
    ("figure") ("filename") ("firstname") ("firstterm") ("footnote")
    ("footnoteref") ("foreignphrase") ("formalpara") ("funcdef") ("funcparams")
    ("funcprototype") ("funcsynopsis") ("funcsynopsisinfo") ("function")
    ("glossary") ("glossaryinfo") ("glossdef") ("glossdiv") ("glossentry")
    ("glosslist") ("glosssee") ("glossseealso") ("glossterm") ("graphic")
    ("graphicco") ("group") ("guibutton") ("guiicon") ("guilabel") ("guimenu")
    ("guimenuitem") ("guisubmenu") ("hardware") ("highlights") ("holder")
    ("honorific") ("html:form") ("imagedata") ("imageobject") ("imageobjectco")
    ("important") ("index") ("indexdiv") ("indexentry") ("indexinfo")
    ("indexterm") ("informalequation") ("informalexample") ("informalfigure")
    ("informaltable") ("initializer") ("inlineequation") ("inlinegraphic")
    ("inlinemediaobject") ("interface") ("interfacename") ("invpartnumber")
    ("isbn") ("issn") ("issuenum") ("itemizedlist") ("itermset") ("jobtitle")
    ("keycap") ("keycode") ("keycombo") ("keysym") ("keyword") ("keywordset")
    ("label") ("legalnotice") ("lhs") ("lineage") ("lineannotation")
    ("link") ("listitem") ("literal") ("literallayout") ("lot") ("lotentry")
    ("manvolnum") ("markup") ("medialabel") ("mediaobject") ("mediaobjectco")
    ("member") ("menuchoice") ("methodname") ("methodparam") ("methodsynopsis")
    ("mml:math") ("modespec") ("modifier") ("mousebutton") ("msg") ("msgaud")
    ("msgentry") ("msgexplan") ("msginfo") ("msglevel") ("msgmain") ("msgorig")
    ("msgrel") ("msgset") ("msgsub") ("msgtext") ("nonterminal") ("note" multiline)
    ("objectinfo") ("olink") ("ooclass") ("ooexception") ("oointerface")
    ("option") ("optional") ("orderedlist") ("orgdiv") ("orgname") ("otheraddr")
    ("othercredit") ("othername") ("pagenums") ("para" multiline) ("paramdef")
    ("parameter") ("part") ("partinfo") ("partintro") ("personblurb")
    ("personname") ("phone") ("phrase") ("pob") ("postcode") ("preface")
    ("prefaceinfo") ("primary") ("primaryie") ("printhistory") ("procedure")
    ("production") ("productionrecap") ("productionset") ("productname")
    ("productnumber") ("programlisting" multiline) ("programlistingco") ("prompt")
    ("property") ("pubdate") ("publisher") ("publishername") ("pubsnumber")
    ("qandadiv") ("qandaentry") ("qandaset") ("question") ("quote") ("refclass")
    ("refdescriptor") ("refentry") ("refentryinfo") ("refentrytitle")
    ("reference") ("referenceinfo") ("refmeta") ("refmiscinfo") ("refname")
    ("refnamediv") ("refpurpose") ("refsect1") ("refsect1info") ("refsect2")
    ("refsect2info") ("refsect3") ("refsect3info") ("refsection") ("refsectioninfo")
    ("refsynopsisdiv") ("refsynopsisdivinfo") ("releaseinfo") ("remark")
    ("replaceable") ("returnvalue") ("revdescription") ("revhistory")
    ("revision") ("revnumber") ("revremark") ("rhs") ("row") ("sbr")
    ("screen") ("screenco") ("screeninfo") ("screenshot") ("secondary")
    ("secondaryie") ("sect1" multiline) ("sect1info") ("sect2" multiline)
    ("sect2info") ("sect3" multiline) ("sect3info") ("sect4" multiline)
    ("sect4info") ("sect5" multiline) ("sect5info") ("section")
    ("sectioninfo") ("see") ("seealso") ("seealsoie") ("seeie") ("seg")
    ("seglistitem") ("segmentedlist") ("segtitle") ("seriesvolnums")
    ("set") ("setindex") ("setindexinfo") ("setinfo") ("sgmltag") ("shortaffil")
    ("shortcut") ("sidebar") ("sidebarinfo") ("simpara") ("simplelist")
    ("simplemsgentry") ("simplesect") ("spanspec") ("state") ("step")
    ("street") ("structfield") ("structname") ("subject") ("subjectset")
    ("subjectterm") ("subscript") ("substeps") ("subtitle") ("superscript")
    ("surname") ("svg:svg") ("symbol") ("synopfragment") ("synopfragmentref")
    ("synopsis") ("systemitem") ("table") ("tbody") ("term") ("tertiary")
    ("tertiaryie") ("textdata") ("textobject") ("tfoot") ("tgroup") ("thead")
    ("tip") ("title") ("titleabbrev") ("toc") ("tocback") ("tocchap")
    ("tocentry") ("tocfront") ("toclevel1") ("toclevel2") ("toclevel3")
    ("toclevel4") ("toclevel5") ("tocpart") ("token") ("trademark") ("type")
    ("ulink") ("userinput") ("varargs") ("variablelist") ("varlistentry")
    ("varname") ("videodata") ("videoobject") ("void") ("volumenum")
    ("warning" multiline) ("wordasword") ("xref" empty) ("year"))
  "DocBook tags for use with variable my-xml-tags.")

;; (makunbound 'my-xml-tags)
(defvar my-xml-tags my-xml-docbook-tags
  "An assoc list of tags used by my-xml-insert-element to do a completing read
from the minibuffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No user configurable variables below this point!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-xml-mode-map nil "Keymap for my-xml-mode.")

(progn
  (setq my-xml-mode-map (make-sparse-keymap))
  (define-key my-xml-mode-map "\C-i"		'tab-to-tab-stop)
  (define-key my-xml-mode-map "\M-i"		'my-xml-indent-current-line)
  (define-key my-xml-mode-map [return]		'my-xml-newline)
  (define-key my-xml-mode-map "\C-j"		'my-xml-newline)
  ;; For some reason, "\C-," isn't accepted here ...
  (define-key my-xml-mode-map [(control ?,)]	'(lambda () (interactive) (insert "<")))
  (define-key my-xml-mode-map [(control ?\")]	'my-xml-insert-quote)
  (define-key my-xml-mode-map [(control ?!)]	'my-xml-insert-comment)
  (define-key my-xml-mode-map "<"		'my-xml-electric-open-tag)
  (define-key my-xml-mode-map ">"		'my-xml-electric-close-tag)
  (define-key my-xml-mode-map " "		'my-xml-electric-space)
  (define-key my-xml-mode-map "\M-e"		'my-xml-insert-emphasis)
  (define-key my-xml-mode-map "\M-r"		'my-xml-render)
  (define-key my-xml-mode-map "\C-\M-p"		'my-xml-insert-para)
  (define-key my-xml-mode-map "\C-\M-c"		'my-xml-insert-computeroutput)
  (define-key my-xml-mode-map "\C-cu"		'my-xml-insert-under-construction))

(defvar my-xml-insert-element-history nil
  "Minibuffer history variable used by my-xml-insert-element.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-xml-mode ()
  "Major mode used for editing XML documents.

Special commands:

\\{my-xml-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'indent-line-function)
  (use-local-map my-xml-mode-map)
  (setq mode-name "XML"
	major-mode 'my-xml-mode
	indent-line-function #'my-xml-indent-current-line
	fill-paragraph-function #'my-xml-fill-paragraph))

(defun my-xml-indent-current-line ()
  "Indents the current line appropriately for my-xml-mode."
  (interactive)
  (catch 'mismatch
    (let ((origin (set-marker (make-marker) (point)))
	  (origin-point (point))
	  (origin-bol (progn (beginning-of-line)
			     (set-marker (make-marker) (point))))
	  (tagname-regexp "[^?! \t<>]+")
	  (current-line-number (save-excursion
				 (beginning-of-line)
				 (1+ (count-lines (point-min) (point)))))
	  (orig-indent-column (current-indentation))
	  (prev-nonblank-text "")
	  prev-nonblank-indent-column
	  taginfo tagtype tagname tagstart tagend tagstartcol
	  is-empty-element
	  stack)

      ;; First, build a stack representing the indentation of elements leading
      ;; up to the current line.
      (goto-char (point-min))
      (catch 'done
	;; Loop until we find no more tags or until something throws 'done or
	;; 'mismatch.
	(while (setq taginfo (my-xml-find-next-tag))

	  (setq tagtype (nth 0 taginfo)
		tagname (nth 1 taginfo)
		tagstart (nth 2 taginfo)
		tagend (nth 3 taginfo)
		tagstartcol (nth 4 taginfo)
		is-empty-element (memq tagtype '(comment empty)))

	  ;; If we've scanned past origin-bol, we're done building the stack.
	  ;; Don't process this tag.
	  (if (> tagend origin-bol)
	      (throw 'done nil))

	  ;; Ignore empty elements.
	  (if (not is-empty-element)
	    ;; If it's a start tag, push its column on the stack and keep looping.
	    (if (eq tagtype 'start)
		(setq stack (cons (cons tagname tagstartcol) stack))
	      ;; It's an end tag.  If it doesn't match the top of the stack, abort.
	      (when (not (string= tagname (my-xml-stack-top-tagname stack)))
		(my-xml-message "Mismatched <%s> tag on line %d!"
				(concat (if (eq tagtype 'end) "/") tagname)
				current-line-number)
		(goto-char origin)
		(throw 'mismatch nil))
	      ;; It matches the top of the stack, so pop the top tag off the stack and
	      ;; go keep looping.
	      (setq stack (cdr stack))))))
    
      (setq taginfo nil
	    tagtype nil
	    tagname nil
	    tagstart nil
	    tagend nil
	    tagstartcol nil)

      ;; Second, get the text from the previous non-blank line.  If
      ;; prev-nonblank-text is nil, it means there is no such line.
      (goto-char origin-bol)
      (when (my-xml-goto-prev-nonblank-line)
	(setq prev-nonblank-indent-column (current-column)
	      prev-nonblank-text (buffer-substring (point) (progn (end-of-line)
								  (point)))))

      ;; Now indent the current line based on the current line, the previous
      ;; non-blank line, and the stack that we just built.  Here's the logic:
      ;;
      ;; If the current line ...
      ;; ... starts with a non-tag, keep indentation the same as the previous
      ;;     non-blank line.
      ;; ... starts with an end tag, indent same as top-of-stack.
      ;; ... starts with start tag or is empty, then
      ;;     if the previous non-blank line ...
      ;;     ... does not exist, indent to column 0.
      ;;     ... starts with a tag, indent 2 + top-of-stack indent.
      ;;     ... starts with a non-tag, keep indentation the same as the previous
      ;;         non-blank line. (???)
      ;;
      ;; NOTE: The stack might be empty, so always use the my-xml-stack-top-*
      ;; functions to access the top of the stack.

      (goto-char origin-bol)
      (delete-horizontal-space)

      ;; Special case <programlisting>.  Leave line at left margin if it
      ;; follows <programlisting>.
      (if (not (string= prev-nonblank-text "<programlisting>"))
	  ;; Otherwise, compute the indentation.
	  (if (and (not (looking-at-p "^$"))
		   (looking-at-p "[^<]"))
	      ;; Current line starts with a non-tag.
	      (indent-to-column prev-nonblank-indent-column)
	    ;; Current line starts with a tag.
	    (if (looking-at (concat "</\\(" tagname-regexp "\\)"))
		;; End tag.
		(progn
		  (setq tagname (match-string 1))
		  (if (string= tagname (my-xml-stack-top-tagname stack))
		      ;; Matches the tag on the top-of-stack.
		      (indent-to-column (my-xml-stack-top-indent stack))
		    ;; Mismatched tag!
		    (my-xml-message "Mismatched </%s> tag at line %d!"
				    tagname current-line-number)
		    (indent-to-column orig-indent-column)
		    (goto-char origin-point)
		    (throw 'mismatch nil)))
	      ;; Start tag or empty line.
	      (if (string= "" prev-nonblank-text)
		  ;; No previous non-blank line.
		  (indent-to-column 0)
		;; There is a previous non-blank line.
		(if (string-match-p "^<[^!?]" prev-nonblank-text)
		    ;; Previous non-blank line starts with a tag.
		    (indent-to-column (+ (my-xml-stack-top-indent stack)
					 ;; Special case lines following <para> tags.
					 (if (string= "para"
						      (my-xml-stack-top-tagname stack))
					     0
					   my-xml-indent-level)))
		  ;; Previous non-blank line starts with a non-tag.
		  (indent-to-column prev-nonblank-indent-column))))))

      ;; Leave point in a reasonable location.
      (goto-char origin)
      (if (bolp)
	  (back-to-indentation)))))

(defun my-xml-newline ()
  "Inserts a newline and indents the new line appropriately."
  (interactive)
  (newline)
  (my-xml-indent-current-line))

(defun my-xml-electric-close-tag ()
  "Inserts '>', re-indents the current line, and bounces cursor to the matching
'<' if it's nearby."
  (interactive)
  (insert ">")
  (my-xml-indent-current-line)
  (save-excursion
    (backward-char 2)
    (while (not (looking-at-p "[<>]"))
      (backward-char 1))
    (if (looking-at-p "<")
	(sit-for 0.7))))

(defun my-xml-electric-space ()
  "Inserts a space, and, if the current column is greater than fill-column, fills
the line."
  (interactive)
  (catch 'return
    (let ((origin (set-marker (make-marker) (point))))
      (when (and (integerp fill-column)
		 (> (current-column) fill-column))
	(move-to-column (+ fill-column 5))
	(while (not (looking-at-p " "))
	  (backward-char))
	(delete-char 1)
	;; Handle the case where the line ends with spaces that span the
	;; fill-column.
	(if (looking-at-p "\\s-+")
	    (progn
	      (my-xml-newline)
	      (throw 'return nil))
	  (my-xml-newline)
	  (goto-char origin))))
    (insert " ")))

(defun my-xml-electric-open-tag ()
  "Prompts the user for a tag name, then inserts an element using that tag."
  (interactive)
  (insert "<")
  (call-interactively #'my-xml-insert-element))

(defun my-xml-insert-element (tagname)
  "Inserts a new XML element.  Leaves point inside the element."
  (interactive (list (completing-read "Element: " my-xml-tags nil nil nil
				'my-xml-insert-element-history "para" nil)))
  (let ((taginfo (cdr (assoc (downcase tagname) my-xml-tags)))
	(tagname (downcase tagname))
	(indent (current-indentation)))
    (if (memq 'multiline taginfo)
	(progn
	  (insert tagname ">\n")
	  (indent-to-column indent)
	  (save-excursion (insert "</" tagname ">")))
      (if (memq 'empty taginfo)
	  (progn
	    (insert tagname "/>")
	    (when (string= tagname "xref")
	      (backward-char 2)
	      (insert " linkend=\"\"")
	      (backward-char 1)))
	(insert tagname ">")
	(save-excursion (insert "</" tagname ">"))))))

(defun my-xml-insert-para ()
  "Inserts a new PARA element.  Leaves point inside the element."
  (interactive)
  (insert "<para>")
  (my-xml-indent-current-line)
  (my-xml-newline)
  (insert "</para>")
  (beginning-of-line)
  (open-line 1)
  (indent-relative))

(defun my-xml-insert-quote ()
  "Inserts a new quote element.  Leaves point inside the element."
  (interactive)
  (insert "<quote></quote>")
  (backward-char 8))

(defun my-xml-insert-emphasis ()
  "Inserts a new emphasis element.  Leaves point inside the element."
  (interactive)
  (insert "<emphasis></emphasis>")
  (backward-char 11))

(defun my-xml-insert-comment ()
  "Inserts a new comment element.  Leaves point inside the element."
  (interactive)
  (insert "<!--  -->")
  (backward-char 4))

(defun my-xml-insert-computeroutput ()
  "Inserts a new computerouput element.  Leaves point inside the element."
  (interactive)
  (insert "<computeroutput></computeroutput>")
  (backward-char 17))

(defun my-xml-insert-under-construction ()
  "Inserts an UNDER CONSTRUCTION comment."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (let ((indent (current-indentation))
	  (lines '("<para>*** UNDER CONSTRUCTION ***</para>"
		   "<!--"
		   "*************************************************************"
		   "*                     UNDER CONSTRUCTION                    *"
		   "*************************************************************"
		   "-->"))
	  line)
      (while (setq line (car lines))
	(setq lines (cdr lines))
	(insert (concat line (if lines "\n")))
	(indent-to-column indent)))))

(defun my-xml-fill-paragraph (arg)
  "Fills the current paragraph in my-xml-mode."
  (catch 'return
    (let ((origin (point))
	  (origin-marker (set-marker (make-marker) (point)))
	  (fill-prefix fill-prefix)
	  (case-fold-search t)
	  para-start
	  para-end)
      ;; If inside a tag, do nothing.
      ;; What about text in comments?
      (if (save-excursion (and (search-forward-regexp "[<>]" nil t)
			       (not (backward-char)) ;; Always t.  Need side-effect.
			       (looking-at-p ">")))
	  (throw 'return nil))

      ;; Narrow the buffer to the paragraph to be filled.  The paragraph is bounded
      ;; above by one of:
      ;;
      ;;   o A <para> tag.
      ;;   o A line containing exactly one start tag (e.g., <listitem>).
      ;;   o A blank line.
      ;;   o A line containing only whitespace.
      ;;   o The beginning of the buffer.
      ;;
      ;; The paragraph is bounded below by one of:
      ;;
      ;;   o A </para> tag.
      ;;   o A line containing exactly one start tag (e.g., <listitem>).
      ;;   o A blank line.
      ;;   o A line containing only whitespace.
      ;;   o The end of the buffer.

      (if (not (search-backward-regexp "\\(<para\\|^\\s-*<[^/>]+>\\s-*$\\|^\\s-*$\\)"
				       nil t))
	  (setq para-start (point-min))
	(if (looking-at "<para[^>]+")
	    (forward-char (length (match-string 0)))
	  (forward-line))
	(setq para-start (point)))

      (goto-char origin)
      (if (not (search-forward-regexp "\\(</para\\|^\\s-*<[^/>]+>\\s-*$\\|^\\s-*$\\)"
				      nil t))
	  (setq para-end (point-max))
	(beginning-of-line)
	(if (looking-at "\\(.*\\)</para")
	    (forward-char (length (match-string 1)))
	  (forward-line))
	(setq para-end (point)))
	  
      ;; Compute the fill-prefix.  This has to be done before we narrow the buffer
      ;; below.
      (when (null fill-prefix)
	(goto-char para-start)
	(beginning-of-line)
	(if (looking-at "\\(\\s-+\\)")
	    (setq fill-prefix (match-string 1))))

      ;; Now fill the paragraph.
      (let ((fill-paragraph-function nil)
	    added-newline)
	(goto-char origin)
	(save-restriction
	  (narrow-to-region para-start para-end)
	  (save-excursion
	    (goto-char (point-max))
	    (when (not (bolp))
	      (insert "\n")
	      (setq added-newline t)))
	  ;;(debug)
	  (fill-paragraph nil)
	  (when added-newline
	    (goto-char (point-max))
	    (backward-delete-char 1))))

      ;; Leave point somewhere sane.
      (goto-char origin-marker))))

(defun my-xml-render ()
  "Renders the current XML document into HTML."
  (interactive)
  (if (buffer-modified-p)
      (error "Buffer has been modified.  Save your changes first!"))
  (message "Rendering document into HTML ...")
  (shell-command (format "db2html %s" (buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-xml-find-next-tag ()
  "Searches forward from point in the current buffer for an XML tag.  This
function returns nil if a complete tag cannot be found, otherwise it returns a
list of the form

	TAGTYPE TAGNAME START END STARTCOL

TAGTYPE is one of the symbols: comment, empty, start, end.  TAGNAME is a string:
\"\" for comments and some other tags.  START and END are integers denoting
buffer positions.  STARTCOL is an integer denoting the column in which the tag's
'<' appears."
  (catch 'return
    (let ((tagname "")
	  (case-fold-search t)
	  tagtype start end startcol)
      (when (search-forward "<" nil t)
	(setq start (1- (point))
	      startcol (1- (current-column)))
    
	;; Get the tag type.
	(if (looking-at-p "\\?\\|!\\(doctype\\|entity\\)")
	    (setq tagtype 'empty)
	  (if (looking-at-p "!--")
	      (setq tagtype 'comment)
	    (if (not (looking-at-p "/"))
		;; This might change to 'empty below.
		(setq tagtype 'start)
	      (setq tagtype 'end)
	      (forward-char))))

	;; Get the tag name.
	(let ((namestart (point)))
	  (skip-chars-forward "a-zA-Z0-9:_-")
	  (setq tagname (buffer-substring namestart (point))))

	;; Find the end of the tag.
	(if (eq tagtype 'comment)
	    (progn
	      (if (null (my-xml-find-end-of-comment))
		  (throw 'return nil))
	      (setq end (point)))
	  (if (null (search-forward ">"))
	      (throw 'return nil)
	    (setq end (point))
	    (backward-char 2)
	    (when (looking-at-p "/>")
	      (setq tagtype 'empty))
	    (goto-char end)))
	
	;; Compute result.
	(if (and tagtype start end)
	    (list tagtype tagname start end startcol))))))

(defun my-xml-find-end-of-comment ()
  "Moves point forward in current buffer to just after the closing '>' in a
comment, taking into consideration that comments can contain nested tags.
Returns nil if end of the comment cannot be found, non-nil if it is found."
  ;; Assume we are already past the "<" which starts the comment.  Start out
  ;; with nesting-level set to 1.  Increment it for each "<" we find.  Decrement
  ;; for each ">" we find.  When it reaches 0, we're at the end of the comment.
  (let ((nesting-level 1)
	(origin (point))
	(comment-start-line (count-lines (point-min) (point))))
    (while (and (> nesting-level 0)
		(search-forward-regexp "[<>]" nil t))
	(if (string= "<" (buffer-substring (1- (point)) (point)))
	    (setq nesting-level (1+ nesting-level))
	  (setq nesting-level (1- nesting-level))))
    (= 0 nesting-level)))

(defun my-xml-message (string &rest args)
  "Like message but propertizes the message string to have a face of
my-alert-face2."
  (apply #'message (propertize string 'face 'my-alert-face2) args))

(defun my-xml-stack-top-indent (stack)
  "Returns the cdr of the car of STACK, or 0 if there is none."
  (if stack
      (let ((indent (cdr (car stack))))
	(if (integerp indent)
	    indent
	  0))
    0))

(defun my-xml-stack-top-tagname (stack)
  "Returns the car of the car of STACK, or "" if there is none."
  (if stack
      (let ((tag (car (car stack))))
	(if (stringp tag)
	    tag
	  ""))
    ""))

(defun my-xml-goto-prev-nonblank-line ()
  "Moves cursor to the first non-whitespace character on the closest preceding
non-blank line.  Returns t if such a line exists, nil if not (in which case
point is not moved."
  (let ((origin (point))
	(result (catch 'result
		  (while t
		    (beginning-of-line)
		    (if (bobp)
			(throw 'result nil))
		    (forward-line -1)
		    (while (looking-at-p "[\t ]")
		      (forward-char 1))
		    (if (not (looking-at-p "$"))
			(throw 'result t))))))
    (if (null result)
	(goto-char origin))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frob auto-mode-alist.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(let ((entry (assoc "\\.xml\\'" auto-mode-alist)))
;;  (if entry
;;      (setcdr entry 'my-xml-mode)
;;    (add-to-list 'auto-mode-alist '("\\.xml\\'" . my-xml-mode))))
