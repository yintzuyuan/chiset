;;; char-db-turtle.el --- Character Database utility -*- coding: utf-8-er; -*-

;; Copyright (C) 2017 MORIOKA Tomohiko.

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: CHISE, Character Database, RDF, Turtle, ISO/IEC 10646, UCS, Unicode, MULE.

;; This file is part of CHISET (CHISE/Turtle).

;; XEmacs CHISE is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; XEmacs CHISE is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs CHISE; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'char-db-util)
(require 'cwiki-common)
(require 'isd-turtle)
(require 'ideograph-util)

(setq est-coded-charset-priority-list
  '(; =ucs
    =mj
    =adobe-japan1-0
    =adobe-japan1-1
    =adobe-japan1-2
    =adobe-japan1-3
    =adobe-japan1-4
    =adobe-japan1-5
    =adobe-japan1-6
    =ucs@iso
    =jis-x0208 =jis-x0208@1990
    =jis-x0213-1
    =jis-x0213-1@2000 =jis-x0213-1@2004
    =jis-x0213-2
    =jis-x0212
    =gt
    =hanyo-denshi/ks
    =hanyo-denshi/tk
    =ucs-itaiji-001
    =ucs-itaiji-002
    =ucs-itaiji-003
    =ucs-itaiji-004
    =ucs-itaiji-005
    =ucs-itaiji-006
    =ucs-itaiji-084
    =ucs-var-001
    =ucs-var-002
    =ucs-var-004
    =cns11643-1 =cns11643-2 =cns11643-3
    =cns11643-4 =cns11643-5 =cns11643-6 =cns11643-7
    =gb2312
    =big5-cdp
    =ks-x1001
    =gt-k
    =ucs@unicode
    =ucs@JP/hanazono
    =gb12345
    =ucs@cns
    =zinbun-oracle =>zinbun-oracle
    =daikanwa
    =ruimoku-v6
    =cbeta =jef-china3
    =+>ucs@iso =+>ucs@unicode
    =+>ucs@jis
    =+>ucs@cns
    =+>ucs@ks
    =>mj
    =>jis-x0208 =>jis-x0213-1
    =>jis-x0208@1997
    =>ucs@iwds-1
    =>ucs@component
    =>iwds-1
    =>ucs@iso
    =>ucs@unicode
    =>ucs@jis =>ucs@cns =>ucs@ks
    =>gt
    =>gt-k
    =>>ucs@iso =>>ucs@unicode
    =>>ucs@jis =>>ucs@cns =>>ucs@ks
    =>>gt-k
    ==mj
    ==ucs@iso
    ==ucs@unicode
    ==adobe-japan1-0
    ==adobe-japan1-1
    ==adobe-japan1-2
    ==adobe-japan1-3
    ==adobe-japan1-4
    ==adobe-japan1-5
    ==adobe-japan1-6
    ==ks-x1001
    ==hanyo-denshi/ks
    ==hanyo-denshi/tk
    ==ucs@jis
    ==gt
    ==cns11643-1 ==cns11643-2 ==cns11643-3
    ==cns11643-4 ==cns11643-5 ==cns11643-6 ==cns11643-7
    ==jis-x0212
    ==ucs@cns
    ==koseki
    ==daikanwa
    ==gt-k
    ==ucs@JP/hanazono
    =>>jis-x0208 =>>jis-x0213-1 =>>jis-x0213-2
    =+>jis-x0208 =+>jis-x0213-1 =+>jis-x0213-2
    =+>hanyo-denshi/jt
    =+>jis-x0208@1978
    =>>gt
    =+>adobe-japan1
    =>>adobe-japan1
    =jis-x0208@1983 =jis-x0208@1978
    =>ucs-itaiji-001
    =>ucs-itaiji-002
    =>ucs-itaiji-005
    ==>ucs@bucs
    =big5
    =>cbeta
    ===mj
    ===ucs@iso
    ===ucs@unicode
    ===hanyo-denshi/ks
    ===ks-x1001
    ===gt
    ===gt-k
    ===ucs@ks
    ===ucs@gb
    =shinjigen
    =shinjigen@1ed
    =shinjigen/+p@rev
    ==shinjigen
    ==shinjigen@rev
    ==daikanwa/+p
    ===daikanwa/+p
    =>daikanwa/ho
    ===daikanwa/ho
    ))

(defvar chise-turtle-ccs-prefix-alist nil)

(defun charset-code-point-format-spec (ccs)
  (cond ((memq ccs '(=ucs))
	 "0x%04X")
	(t
	 (let ((ccs-name (symbol-name ccs)))
	   (cond
	    ((string-match
	      "\\(shinjigen\\|daikanwa/ho\\|=>iwds-1\\)"
	      ccs-name)
	     "%04d")
	    ((string-match
	      "\\(gt\\|daikanwa\\|adobe-japan1\\|cbeta\\|zinbun-oracle\\|hng\\)"
	      ccs-name)
	     "%05d")
	    ((string-match "\\(hanyo-denshi/ks\\|koseki\\|mj\\)" ccs-name)
	     "%06d")
	    ((string-match "hanyo-denshi/tk" ccs-name)
	     "%08d")
	    (t
	     "0x%X"))))))

(defun chise-turtle-uri-decode-feature-name (uri-feature)
  (cond ((string= "a.ucs" uri-feature)
	 '=ucs)
	((string= "a.big5" uri-feature)
	 '=big5)
	(t
	 (www-uri-decode-feature-name uri-feature))))

(defun chise-turtle-uri-encode-ccs-name (feature-name)
  (cond
   ((eq '=ucs feature-name)
    "a.ucs")
   ((eq '=big5 feature-name)
    "a.big5")
   ((eq '==>ucs@bucs feature-name)
    "bucs")
   (t
    (mapconcat (lambda (c)
		 (cond
		  ((eq c ?@)
		   "_")
		  ((eq c ?+)
		   "._.")
		  ((eq c ?=)
		   ".:.")
		  (t
		   (char-to-string c))))
	       (www-uri-encode-feature-name feature-name)
	       ""))))

(defun chise-turtle-uri-encode-feature-name (feature-name)
  (cond
   ((eq '->subsumptive feature-name)
    ":subsume")
   ((eq '<-denotational feature-name)
    ":denotation-of")
   ((eq '<-formed feature-name)
    ":form-of")
   ((eq 'hanyu-dazidian feature-name)
    "ideo:hanyu-dazidian")
   (t
    (concat ":" (chise-turtle-uri-encode-ccs-name feature-name)))))

(defun chise-turtle-format-ccs-code-point (ccs code-point)
  (let ((ccs-uri (chise-turtle-uri-encode-ccs-name ccs)))
    (unless (assoc ccs-uri chise-turtle-ccs-prefix-alist)
      (setq chise-turtle-ccs-prefix-alist
	    (cons (cons ccs-uri ccs)
		  chise-turtle-ccs-prefix-alist)))
    (format "%s:%s"
	    ccs-uri
	    (format (charset-code-point-format-spec ccs)
		    code-point))))

(defun chise-turtle-encode-char (object)
  (let ((ccs-list est-coded-charset-priority-list)
	ccs ret ret2)
    (if (setq ret (encode-char object '=ucs))
	(chise-turtle-format-ccs-code-point '=ucs ret)
      (while (and ccs-list
		  (setq ccs (pop ccs-list))
		  (not (setq ret (encode-char object ccs 'defined-only)))))
      (cond (ret
	     (chise-turtle-format-ccs-code-point ccs ret)
	     )
	    ((and (setq ccs (car (split-char object)))
		  (setq ret (encode-char object ccs)))
	     (chise-turtle-format-ccs-code-point ccs ret)
	     )
	    ((setq ret (get-char-attribute object 'ideographic-combination))
	     (format "ideocomb:%s"
		     (mapconcat (lambda (cell)
				  (cond ((characterp cell)
					 (char-to-string cell)
					 )
					((setq ret2 (find-char cell))
					 (char-to-string ret2)
					 )
					(t
					 (format "%S" cell)
					 )))
				ret ""))
	     )
	    (t
	     (format (if est-hide-cgi-mode
			 "system-char-id=0x%X"
		       "system-char-id:0x%X")
		     (encode-char object 'system-char-id))
	     )))))

(defun chise-split-feature-name (feature-name)
  (let (base domain number meta)
    (setq feature-name (symbol-name feature-name))
    (if (string-match ".\\*." feature-name)
	(setq meta (intern
		    (format ":%s" (substring feature-name (1- (match-end 0)))))
	      feature-name (substring feature-name 0 (1+ (match-beginning 0)))))
    (if (string-match "\\$_\\([0-9]+\\)$" feature-name)
	(setq number (car (read-from-string (match-string 1 feature-name)))
	      feature-name (substring feature-name 0 (match-beginning 0))))
    (if (string-match "@" feature-name)
	(setq domain (car (read-from-string (substring feature-name (match-end 0))))
	      base (intern (substring feature-name 0 (match-beginning 0))))
      (setq base (intern feature-name)))
    (list base domain number meta)))

(defun chise-compose-feature-name (base domain number meta)
  (let ((name (if domain
		  (format "%s@%s" base domain)
		(symbol-name base))))
    (if number
	(setq name (format "%s$_%d" name number)))
    (if meta
	(setq name (format "%s*%s" name
			   (substring (symbol-name meta) 1))))
    (intern name)))

(defvar chise-feature-name-base-metadata-alist nil)

(defun chise-update-feature-name-base-metadata-alist ()
  (interactive)
  (let (base domain number metadata
	     bcell dcell ncell ret)
    (setq chise-feature-name-base-metadata-alist nil)
    (dolist (fname (sort (char-attribute-list)
			 #'char-attribute-name<))
      (setq ret (chise-split-feature-name fname)
	    base (car ret)
	    domain (nth 1 ret)
	    number (nth 2 ret)
	    metadata (nth 3 ret))
      (when metadata
	(if (setq bcell (assq base chise-feature-name-base-metadata-alist))
	    (if (setq dcell (assq domain (cdr bcell)))
		(if (setq ncell (assq number (cdr dcell)))
		    (unless (memq metadata (cdr ncell))
		      (setcdr ncell (nconc (cdr ncell)
					   (list metadata))))
		  (setcdr dcell (cons (list number metadata)
				      (cdr dcell))))
	      (setcdr bcell (cons (list domain (list number metadata))
				  (cdr bcell))))
	  (setq chise-feature-name-base-metadata-alist
		(cons (list base (list domain (list number metadata)))
		      chise-feature-name-base-metadata-alist))
	  )))))

(chise-update-feature-name-base-metadata-alist)

(defun chise-get-char-attribute-with-metadata (character feature-name-base domain)
  (let ((value (get-char-attribute
		character
		(chise-compose-feature-name feature-name-base domain nil nil)
		'*feature-value-is-empty*))
	dcell
	base-metadata metadata
	ret m i rest dest)
    (unless (eq value '*feature-value-is-empty*)
      (cond
       ((and (setq ret (assq feature-name-base
			     chise-feature-name-base-metadata-alist))
	     (setq dcell (assq domain (cdr ret))))
	(if (setq ret (assq nil (cdr dcell)))
	    (dolist (bmn (cdr ret))
	      (when (setq m (get-char-attribute
			     character
			     (chise-compose-feature-name
			      feature-name-base domain nil bmn)))
		(setq base-metadata
		      (list* bmn m base-metadata)))))
	(setq i 1
	      rest value)
	(while (consp rest)
	  (setq dest
		(cons (cond
		       ((setq ret (assq i (cdr dcell)))
			(setq metadata nil)
			(dolist (mn (cdr ret))
			  (when (setq m (get-char-attribute
					 character
					 (chise-compose-feature-name
					  feature-name-base domain i mn)))
			    (setq metadata (list* mn m metadata))))
			(if metadata
			    (list* :target (car rest) metadata)
			  (car rest))
			)
		       (t (car rest)))
		      dest))
	  (setq i (1+ i)
		rest (cdr rest)))
	(list (nconc (nreverse dest) rest)
	      base-metadata)
	)
       (t (list value nil)))
      )))
	  
(defun chise-split-ccs-name (ccs)
  (cond ((eq ccs '=ucs)
	 '(ucs abstract-character nil)
	 )
	((eq ccs '=big5)
	 '(big5 abstract-character nil)
	 )
	(t
	 (setq ccs (symbol-name ccs))
	 (let (ret)
	   (if (string-match "^\\(=[=+>]*\\)\\([^=>@*]+\\)@?" ccs)
	       (list (intern (match-string 2 ccs))
		     (chise-decode-ccs-prefix (match-string 1 ccs))
		     (if (string= (setq ret (substring ccs (match-end 0))) "")
			 nil
		       (intern ret))))
	   ))))

(defun chise-decode-ccs-prefix (ccs)
  (or (cdr (assoc ccs '(("==>" . super-abstract-character)
			("=>"  . abstract-character)
			("=+>" . unified-glyph)
			("="   . abstract-glyph)
			("=>>" . detailed-glyph)
			("=="  . abstract-glyph-form)
			("===" . glyph-image))))
      'character))

(defun chise-turtle-uri-split-ccs (uri-ccs)
  (cond
   ((string-match "^a2\\." uri-ccs)
    (cons ":super-abstract-character"
	  (substring uri-ccs (match-end 0)))
    )
   ((string-match "^a\\." uri-ccs)
    (cons ":abstract-character"
	  (substring uri-ccs (match-end 0)))
    )
   ((string-match "^o\\." uri-ccs)
    (cons ":unified-glyph"
	  (substring uri-ccs (match-end 0)))
    )
   ((string-match "^rep\\." uri-ccs)
    (cons ":abstract-glyph"
	  (substring uri-ccs (match-end 0)))
    )
   ((string-match "^g\\." uri-ccs)
    (cons ":detailed-glyph"
	  (substring uri-ccs (match-end 0)))
    )
   ((string-match "^g2\\." uri-ccs)
    (cons ":abstract-glyph-form"
	  (substring uri-ccs (match-end 0)))
    )
   ((string-match "^gi\\." uri-ccs)
    (cons ":abstract-glyph-form"
	  (substring uri-ccs (match-end 0)))
    )
   ((string-match "^repi\\." uri-ccs)
    (cons ":glyph-image"
	  (substring uri-ccs (match-end 0)))
    )
   (t (cons ":character" uri-ccs))))

(defun char-db-turtle-insert-relation-feature (char name value line-breaking
						    ccss readable)
  (insert (format "    %s%s        "
		  (chise-turtle-uri-encode-feature-name name)
		  line-breaking))
  (char-db-turtle-insert-relations value readable)
  (insert " ;")
  )

(defun char-db-turtle-insert-metadata (name value)
  (let (col indent)
    (insert (format "%-7s " name))
    (cond
     ((or (eq name :sources)
	  (eq name :denied))
      (setq col (current-column))
      (setq indent (make-string col ?\ ))
      (insert (format "chisebib:%s"
		      (chise-turtle-uri-encode-ccs-name (car value))))
      (dolist (source (cdr value))
	(insert (format " ,\n%schisebib:%s" indent
			(chise-turtle-uri-encode-ccs-name source))))
      nil)
     (t
      (insert (format "%S" value))
      nil))))

(defun char-db-turtle-insert-radical (radical-number)
  (insert (format "        %3d ; # %c"
		  radical-number
		  (ideographic-radical radical-number)))
  'with-separator)

(defun char-db-turtle-insert-list (value &optional readable)
  (let (lbs separator rest cell al cal key ret)
    (insert "( ")
    (setq lbs (concat "\n" (make-string (current-column) ?\ ))
	  separator nil)
    (while (consp value)
      (setq cell (car value))
      (if (and (consp cell)
	       (consp (car cell))
	       (setq ret (condition-case nil
			     (find-char cell)
			   (error nil))))
	  (progn
	    (setq rest cell
		  al nil
		  cal nil)
	    (while rest
	      (setq key (car (car rest)))
	      (if (find-charset key)
		  (setq cal (cons key cal))
		(setq al (cons key al)))
	      (setq rest (cdr rest)))
	    (if separator
		(insert lbs))
	    (char-db-turtle-insert-char-features ret
						 readable
						 al
						 nil 'for-sub-node)
	    (setq separator lbs))
	(setq ret (prin1-to-string cell))
	(if separator
	    (if (< (+ (current-column)
		      (length ret)
		      (length separator))
		   76)
		(insert separator)
	      (insert lbs)))
	(insert ret)
	(setq separator " "))
      (setq value (cdr value)))
    (insert " ) ;")
    'with-separator))

(defun char-db-turtle-insert-source-list (value &optional readable)
  (let (lbs separator rest cell al cal key ret)
    (setq lbs (concat " ,\n" (make-string (current-column) ?\ ))
	  separator nil)
    (while (consp value)
      (setq cell (car value))
      (if (and (consp cell)
	       (consp (car cell))
	       (setq ret (condition-case nil
			     (find-char cell)
			   (error nil))))
	  (progn
	    (setq rest cell
		  al nil
		  cal nil)
	    (while rest
	      (setq key (car (car rest)))
	      (if (find-charset key)
		  (setq cal (cons key cal))
		(setq al (cons key al)))
	      (setq rest (cdr rest)))
	    (if separator
		(insert lbs))
	    (char-db-turtle-insert-char-features ret
						 readable
						 al
						 nil 'for-sub-node)
	    (setq separator lbs))
	(setq ret (prin1-to-string cell))
	(if separator
	    (if (< (+ (current-column)
		      (length ret)
		      (length separator))
		   76)
		(insert separator)
	      (insert lbs)))
	(if (string-match "=" ret)
	    (insert (format "%s:%s"
			    (substring ret 0 (match-beginning 0))
			    (substring ret (match-end 0))))
	  (insert (format "chisebib:%s" ret)))
	(setq separator " , "))
      (setq value (cdr value)))
    (insert " ;")
    'with-separator))

(defun char-db-turtle-insert-relations (value &optional readable)
  (let ((lbs (concat "\n" (make-string (current-column) ?\ )))
	separator cell)
    (if (characterp value)
	(setq value (list value)))
    (while (consp value)
      (setq cell (car value))
      (if (integerp cell)
	  (setq cell (decode-char '=ucs cell)))
      (if separator
	  (insert separator)
	(setq separator (format " ,%s" lbs)))
      (if (characterp cell)
	  (insert (format "%-20s" (chise-turtle-encode-char cell)))
	(char-db-turtle-insert-char-ref cell '<-formed))
      (setq value (cdr value)))
    nil))

(defun char-db-turtle-insert-target-value (value feature-name-base &optional readable)
  (cond ((eq feature-name-base 'ideographic-radical)
	 (char-db-turtle-insert-radical value)
	 )
	((or (eq feature-name-base 'ideographic-combination)
	     (eq feature-name-base '=decomposition)
	     (eq feature-name-base '<-formed)
	     (string-match "^\\(->\\|<-\\)[^*]*$" (symbol-name feature-name-base)))
	 (char-db-turtle-insert-relations value readable)
	 )
	((eq feature-name-base 'comment)
	 (insert (format "%S" value))
	 nil)
	((eq feature-name-base 'sources)
	 (char-db-turtle-insert-source-list value readable)
	 )
	((consp value)
	 (char-db-turtle-insert-list value readable)
	 )
	(t
	 (insert (format " %-14s" value))
	 nil)))

(defun char-db-turtle-insert-feature-value (value metadata domain feature-name-base)
  (let (indent0 indent rest mdname mdval lb)
    (cond
     ((or metadata domain)
      (setq indent0 (make-string (current-column) ?\ ))
      (insert "[ ")
      (setq indent (make-string (current-column) ?\ ))
      (when domain
	(insert (format ":context domain:%-7s ;"
			(chise-turtle-uri-encode-ccs-name domain)))
	(setq lb t))
      (setq rest metadata)
      (while rest
	(setq mdname (pop rest)
	      mdval  (pop rest))
	(if lb
	    (insert (format "\n%s" indent))
	  (setq lb t))
	(unless (char-db-turtle-insert-metadata mdname mdval)
	  (insert " ;")))
      (if lb
	  (insert (format "\n%s" indent)))
      (insert ":target  ")
      (if (char-db-turtle-insert-target-value value feature-name-base)
	  (insert (format "\n%s] ;" indent0))
	(insert " ] ;"))
      'with-separator)
     (t
      (char-db-turtle-insert-target-value value feature-name-base)
      ))))

(defun char-db-turtle-insert-char-ref (char-ref feature-name-base)
  (let (indent0 indent rest mdname mdval lb last-sep)
    (setq indent0 (make-string (current-column) ?\ ))
    (insert "[ ")
    (setq indent (make-string (current-column) ?\ ))
    (setq rest char-ref)
    (while rest
      (setq mdname (pop rest)
	    mdval  (pop rest))
      (if lb
	  (insert (format "%s\n%s"
			  (if last-sep
			      ""
			    " ;")
			  indent))
	(setq lb t))
      (setq last-sep
	    (cond ((eq mdname :target)
		   (insert ":target  ")
		   (char-db-turtle-insert-target-value mdval feature-name-base)
		     )
		  (t
		   (char-db-turtle-insert-metadata mdname mdval)))))
    (if last-sep
	(insert (format "\n%s]" indent0))
      (insert " ]"))
    nil))
  
(defun char-db-turtle-insert-char-features (char
					    &optional readable attributes column
					    for-sub-node)
  (unless column
    (setq column (current-column)))
  (let ((est-coded-charset-priority-list est-coded-charset-priority-list)
	(est-view-url-prefix "http://chise.org/est/view")
	id obj-id type domain
	name value metadata
	name-base name-domain
	radical strokes
	(line-breaking (concat "\n" (make-string column ?\ )))
	line-separator
	ret
	skey
	dest-ccss ; sources required-features
	ccss eq-cpos-list
	uri-ccs uri-cpos ccs-base children child-ccs-list col indent)
    (let (atr-d)
      (setq attributes
	    (sort (if attributes
		      (if (consp attributes)
			  (progn
			    (dolist (name attributes)
			      (unless (or (memq name char-db-ignored-attributes)
					  (string-match "\\*" (symbol-name name)))
				(if (find-charset name)
				    (push name ccss))
				(push name atr-d)))
			    atr-d))
		    (dolist (name (char-attribute-list))
		      (unless (or (memq name char-db-ignored-attributes)
				  (string-match "\\*" (symbol-name name)))
			(if (find-charset name)
			    (push name ccss))
			(push name atr-d)))
		    atr-d)
		  #'char-attribute-name<)))
    (setq line-separator line-breaking)
    (setq id (chise-turtle-encode-char char))
    (setq obj-id (file-name-nondirectory id))
    (string-match ":" obj-id)
    (setq uri-ccs (substring obj-id 0 (match-beginning 0))
	  uri-cpos (substring obj-id (match-end 0)))
    (insert (format "%s" obj-id))
    (setq ret (assoc uri-ccs chise-turtle-ccs-prefix-alist))
    (setq dest-ccss (list (cdr ret)))
    (setq ret (chise-split-ccs-name (cdr ret)))
    (setq ccs-base (car ret)
	  type (nth 1 ret)
	  domain (nth 2 ret))
    (insert (format "%s    a chisegg:%s ;" line-separator type))
    (insert (format "%s    :%s-of" line-breaking type))
    (if (null domain)
	(insert (format " %s:%s ;"
			(chise-turtle-uri-encode-ccs-name ccs-base) uri-cpos))
      (insert " [ ")
      (setq col (current-column))
      (insert (format ":context domain:%-7s ;\n%s:target %7s:%-7s ] ;"
		      (chise-turtle-uri-encode-ccs-name domain)
		      (make-string col ?\ )
		      (chise-turtle-uri-encode-ccs-name ccs-base) uri-cpos)))
    (when (memq '<-subsumptive attributes)
      (when (or readable (not for-sub-node))
	(when (setq value (get-char-attribute char '<-subsumptive))
	  (insert line-separator)
	  (char-db-turtle-insert-relation-feature char '<-subsumptive value
						  line-breaking
						  ccss readable)
	  ))
      (setq attributes (delq '<-subsumptive attributes))
      )
    (when (and (memq '<-denotational attributes)
	       (setq value (get-char-attribute char '<-denotational)))
      (insert line-separator)
      (char-db-turtle-insert-relation-feature char '<-denotational value
					    line-breaking
					    ccss readable)
      (setq attributes (delq '<-denotational attributes)))
    (when (and (memq '<-denotational@component attributes)
	       (setq value
		     (get-char-attribute char '<-denotational@component)))
      (insert line-separator)
      (char-db-turtle-insert-relation-feature
       char '<-denotational@component value
       line-breaking
       ccss readable)
      (setq attributes (delq '<-denotational@component attributes)))
    (when (and (memq 'name attributes)
	       (setq value (get-char-attribute char 'name)))
      (insert (format "%s    " line-separator))
      (insert (format
	       (if (> (+ (current-column) (length value)) 48)
		   ":name %S ;"
		 ":name                 %S ;")
	       value))
      (setq attributes (delq 'name attributes))
      )
    (when (and (memq 'name* attributes)
	       (setq value (get-char-attribute char 'name*)))
      (insert line-separator)
      (insert (format
	       (if (> (+ (current-column) (length value)) 48)
		   " \"name*\": %S"
		 " \"name*\":                 %S")
	       value))
      (setq attributes (delq 'name* attributes))
      )
    (when (and (memq 'script attributes)
	       (setq value (get-char-attribute char 'script)))
      (insert (format "%s    :script\t\t  ( %s ) ;"
		      line-separator
		      (mapconcat (lambda (cell)
				   (format "script:%s" cell))
				 value " ")))
      (setq attributes (delq 'script attributes))
      )
    ;; (dolist (name '(=>ucs =>ucs*))
    ;;   (when (and (memq name attributes)
    ;;              (setq value (get-char-attribute char name)))
    ;;     (insert line-separator)
    ;;     (insert (format " \"%-20s\":  #x%04X,\t\"_comment\": \"%c\""
    ;;                     name value (decode-char '=ucs value)))
    ;;     (setq attributes (delq name attributes))))
    (when (and (memq '=>ucs attributes)
	       (setq value (get-char-attribute char '=>ucs)))
      (insert (format "%s    :to.ucs\t\t  a.ucs:0x%04X ; # %c"
		      line-separator value (decode-char '=ucs value)))
      (setq attributes (delq '=>ucs attributes))
      )
    (when (setq value (get-char-attribute char '=>ucs*))
      (insert (format "%s    :to.canonical-ucs\ta.ucs:0x%04X ; # %c"
		      line-separator value (decode-char '=ucs value)))
      (setq attributes (delq '=>ucs* attributes))
      )
    (dolist (name '(=>ucs@gb =>ucs@big5))
      (when (and (memq name attributes)
		 (setq value (get-char-attribute char name)))
	(insert line-separator)
	(insert (format " \"%-20s\":  #x%04X,\t\"_comment\": \"%c\"%s"
			name value
			(decode-char (intern
				      (concat "="
					      (substring
					       (symbol-name name) 2)))
				     value)
			line-breaking))
	(setq attributes (delq name attributes))
	))
    (when (and (memq 'general-category attributes)
	       (setq value (get-char-attribute char 'general-category)))
      (insert (format "%s    :general-category     \"%s\" ; # %s"
		      line-separator value
		      (cond ((rassoc value unidata-normative-category-alist)
			     "Normative Category")
			    ((rassoc value unidata-informative-category-alist)
			     "Informative Category")
			    (t
			     "Unknown Category"))))
      (setq attributes (delq 'general-category attributes))
      )
    (when (and (memq 'bidi-category attributes)
	       (setq value (get-char-attribute char 'bidi-category)))
      (insert (format "%s    :bidi-category        %S ;"
		      line-separator
		      value))
      (setq attributes (delq 'bidi-category attributes))
      )
    (unless (or (not (memq 'mirrored attributes))
		(eq (setq value (get-char-attribute char 'mirrored 'empty))
		    'empty))
      (insert (format "%s    :mirrored             \"%s\" ;"
		      line-separator
		      value))
      (setq attributes (delq 'mirrored attributes))
      )
    (cond
     ((and (memq 'decimal-digit-value attributes)
	   (setq value (get-char-attribute char 'decimal-digit-value)))
      (insert (format "%s    :decimal-digit-value  %2d ;"
		      line-separator value))
      (setq attributes (delq 'decimal-digit-value attributes))
      (when (and (memq 'digit-value attributes)
		 (setq value (get-char-attribute char 'digit-value)))
	(insert (format "%s    :digit-value\t  %2d ;"
			line-separator value))
	(setq attributes (delq 'digit-value attributes))
	)
      (when (and (memq 'numeric-value attributes)
		 (setq value (get-char-attribute char 'numeric-value)))
	(insert (format "%s    :numeric-value\t  %2d ;"
			line-separator value))
	(setq attributes (delq 'numeric-value attributes))
	)
      )
     (t
      (when (and (memq 'digit-value attributes)
		 (setq value (get-char-attribute char 'digit-value)))
	(insert line-separator)
	(insert (format "%s    :digit-value\t  %2d ;"
			line-separator value))
	(setq attributes (delq 'digit-value attributes))
	)
      (when (and (memq 'numeric-value attributes)
		 (setq value (get-char-attribute char 'numeric-value)))
	(insert line-separator)
	(insert (format "%s    :numeric-value\t  %2d ;"
			line-separator value))
	(setq attributes (delq 'numeric-value attributes))
	)))
    (when (and (memq 'iso-10646-comment attributes)
	       (setq value (get-char-attribute char 'iso-10646-comment)))
      (insert line-separator)
      (insert (format "{\"iso-10646-comment\":\t %S}%s"
		      value
		      line-breaking))
      (setq attributes (delq 'iso-10646-comment attributes))
      )
    (when (and (memq 'morohashi-daikanwa attributes)
	       (setq value (get-char-attribute char 'morohashi-daikanwa)))
      (insert line-separator)
      (insert (format "%s    :morohashi-daikanwa\t  %S ;"
		      line-separator value))
      (setq attributes (delq 'morohashi-daikanwa attributes))
      )
    (setq radical nil
	  strokes nil)
    (when (and (memq 'ideographic-radical attributes)
	       (setq value (get-char-attribute char 'ideographic-radical)))
      (setq radical value)
      (insert (format "%s    ideo:radical         %3d ; # %c "
		      line-separator
		      radical
		      (ideographic-radical radical)
		      ))
      (setq attributes (delq 'ideographic-radical attributes))
      )
    (when (and (memq 'shuowen-radical attributes)
	       (setq value (get-char-attribute char 'shuowen-radical)))
      (insert line-separator)
      (insert (format " \"shuowen-radical\":\t %S,\t\"_comment\": \"%c\""
		      value
		      (shuowen-radical value)))
      (setq attributes (delq 'shuowen-radical attributes))
      )
    (let (key)
      (dolist (domain
	       (append
		char-db-feature-domains
		(let (dest domain)
		  (dolist (feature (char-attribute-list))
		    (setq feature (symbol-name feature))
		    (when (string-match
			   "\\(radical\\|strokes\\)@\\([^@*]+\\)\\(\\*\\|$\\)"
			   feature)
		      (setq domain (intern (match-string 2 feature)))
		     (unless (memq domain dest)
		       (setq dest (cons domain dest)))))
		  (sort dest #'string<))))
	(setq key (intern (format "%s@%s" 'ideographic-radical domain)))
	(when (and (memq key attributes)
		   (setq value (get-char-attribute char key)))
	  (setq radical value)
          (insert (format "%s    ideo:radical           "
			  line-separator))
	  (char-db-turtle-insert-feature-value value nil domain 'ideographic-radical)
	  (setq attributes (delq key attributes))
	  )
	(setq key (intern (format "%s@%s" 'ideographic-strokes domain)))
	(when (and (memq key attributes)
		   (setq value (get-char-attribute char key)))
	  (setq strokes value)
	  (insert (format "%s    ideo:strokes           [ "
			  line-separator))
	  (setq col (current-column))
	  (setq indent (make-string col ?\ ))
	  (insert (format ":context domain:%-7s ;\n%s:target  %S"
			  (chise-turtle-uri-encode-ccs-name domain)
			  indent strokes))
	  (setq attributes (delq key attributes))
	  (setq skey (intern (format "%s*sources" key)))
	  (when (and (memq skey attributes)
		     (setq value (get-char-attribute char skey)))
	    (insert (format " ;\n%s:sources (" indent))
	    (setq col (current-column))
	    (setq indent (make-string col ?\ ))
	    (insert (format " chisebib:%s" (car value)))
	    (dolist (cell (cdr value))
	      (insert (format "\n%s chisebib:%s" indent cell)))
	    (insert " )"))
	  (setq attributes (delq skey attributes))
	  (insert " ] ;")
	  )
	(setq key (intern (format "%s@%s" 'total-strokes domain)))
	(when (and (memq key attributes)
		   (setq value (get-char-attribute char key)))
	  (insert (format "%s    ideo:total-strokes     [ "
			  line-separator))
	  (setq col (current-column))
	  (insert (format ":context domain:%-7s ;\n%s:target  %S"
			  domain
			  (make-string col ?\ )
			  value))
	  (setq attributes (delq key attributes))
	  (setq skey (intern (format "%s*sources" key)))
	  (insert " ] ;")
	  )
	(dolist (feature '(ideographic-radical
			   ideographic-strokes
			   total-strokes))
	  (setq key (intern (format "%s@%s*sources" feature domain)))
	  (when (and (memq key attributes)
		     (setq value (get-char-attribute char key)))
	    (insert line-separator)
	    (insert (format " \"%s\":%s" key line-breaking))
	    (dolist (cell value)
	      (insert (format " %s" cell)))
	    (setq attributes (delq key attributes))
	    ))
	))
    (when (and (memq 'ideographic-strokes attributes)
	       (setq value (get-char-attribute char 'ideographic-strokes)))
      (setq strokes value)
      (insert (format "%s    ideo:strokes          %2d ;"
		      line-separator strokes))
      (setq attributes (delq 'ideographic-strokes attributes))
      )
    (when (and (memq 'kangxi-radical attributes)
	       (setq value (get-char-attribute char 'kangxi-radical)))
      (unless (eq value radical)
	(insert line-separator)
	(insert (format "{\"kangxi-radical\":\t%S},\t\"_comment\": \"%c\"%s"
			value
			(ideographic-radical value)
			line-breaking))
	(or radical
	    (setq radical value)))
      (setq attributes (delq 'kangxi-radical attributes))
      )
    (when (and (memq 'kangxi-strokes attributes)
	       (setq value (get-char-attribute char 'kangxi-strokes)))
      (unless (eq value strokes)
	(insert line-separator)
	(insert (format "{\"kangxi-strokes\":\t%S}%s"
			value
			line-breaking))
	(or strokes
	    (setq strokes value)))
      (setq attributes (delq 'kangxi-strokes attributes))
      )
    (when (and (memq 'japanese-strokes attributes)
	       (setq value (get-char-attribute char 'japanese-strokes)))
      (unless (eq value strokes)
	(insert line-separator)
	(insert (format "{\"japanese-strokes\":\t%S}%s"
			value
			line-breaking))
	(or strokes
	    (setq strokes value)))
      (setq attributes (delq 'japanese-strokes attributes))
      )
    (when (and (memq 'cns-radical attributes)
	       (setq value (get-char-attribute char 'cns-radical)))
      (insert line-separator)
      (insert (format "{\"cns-radical\":\t%S},\t\"_comment\": \"%c\"%s"
		      value
		      (ideographic-radical value)
		      line-breaking))
      (setq attributes (delq 'cns-radical attributes))
      )
    (when (and (memq 'cns-strokes attributes)
	       (setq value (get-char-attribute char 'cns-strokes)))
      (unless (eq value strokes)
	(insert line-separator)
	(insert (format "{\"cns-strokes\":\t%S}%s"
			value
			line-breaking))
	(or strokes
	    (setq strokes value)))
      (setq attributes (delq 'cns-strokes attributes))
      )
    (when (and (memq 'total-strokes attributes)
	       (setq value (get-char-attribute char 'total-strokes)))
      (insert (format "%s    ideo:total-strokes    %2d ;"
		      line-separator value))
      (setq attributes (delq 'total-strokes attributes))
      )
    (if (equal (get-char-attribute char '->titlecase)
	       (get-char-attribute char '->uppercase))
	(setq attributes (delq '->titlecase attributes)))
    (unless readable
      (dolist (ignored '(composition
			 ->denotational <-subsumptive ->ucs-unified
			 ->ideographic-component-forms))
	(setq attributes (delq ignored attributes))))
    (while attributes
      (setq name (car attributes))
      (setq ret (chise-split-feature-name name))
      (setq name-base (car ret)
	    name-domain (nth 1 ret))
      (when (setq value (chise-get-char-attribute-with-metadata
			 char name-base name-domain))
	(setq metadata (nth 1 value)
	      value (car value))
	(cond ((setq ret (find-charset name))
	       (setq name (charset-name ret))
	       (when (not (memq name dest-ccss))
		 (setq dest-ccss (cons name dest-ccss))
		 (if (null value)
		     (insert (format "%s    :%-25s rdf:nil ;" line-separator
				     (chise-turtle-uri-encode-ccs-name name)))
		   (setq ret (chise-turtle-format-ccs-code-point name value))
		   (insert (format "%s    :eq %-25s ; # %c" line-separator
				   ret
				   (char-db-decode-isolated-char name value)))
		   (setq eq-cpos-list (cons (list ret name value) eq-cpos-list))))
	       (if (find-charset
		    (setq ret (if (eq name '=ucs)
				  (if (< value #x10000)
				      '==ucs@unicode
				    '==ucs@iso)
				(intern (format "=%s" name)))))
		   (setq child-ccs-list (cons ret child-ccs-list)))
	       )
	      ((and
		(not readable)
		(not (eq name '->subsumptive))
		(not (eq name '->uppercase))
		(not (eq name '->lowercase))
		(not (eq name '->titlecase))
		(not (eq name '->canonical))
		(not (eq name '->Bopomofo))
		(not (eq name '->mistakable))
		(not (eq name '->ideographic-variants))
		(or (eq name '<-identical)
		    (eq name '<-uppercase)
		    (eq name '<-lowercase)
		    (eq name '<-titlecase)
		    (eq name '<-canonical)
		    (eq name '<-ideographic-variants)
		    ;; (eq name '<-synonyms)
		    (string-match "^<-synonyms" (symbol-name name))
		    (eq name '<-mistakable)
		    (when (string-match "^->" (symbol-name name))
		      (cond
		       ((string-match "^->fullwidth" (symbol-name name))
			(not (and (consp value)
				  (characterp (car value))
				  (encode-char
				   (car value) '=ucs 'defined-only)))
			)
		       (t)))
		    ))
	       )
	      ((eq name 'ideographic-structure)
	       (insert (isd-turtle-format-char nil nil value nil
					       'isd 'without-head-char))
	       (insert " ;")
	       )
	      ((eq name '->subsumptive)
	       (insert line-separator)
	       (char-db-turtle-insert-relation-feature char name value
						       line-breaking
						       ccss readable)
	       (setq children value)
	       )
              (t
	       (insert (format "%s    %-20s "
			       line-separator
			       (chise-turtle-uri-encode-feature-name name-base)))
	       (unless (char-db-turtle-insert-feature-value
			value metadata name-domain name-base)
		 (insert " ;"))
	       )
	      ))
      (setq attributes (cdr attributes)))
    (insert (format "%s    ." line-breaking))
    (dolist (eq-cpos (nreverse eq-cpos-list))
      (setq ret (chise-split-ccs-name (nth 1 eq-cpos)))
      (insert (format "%s    %s" line-breaking
		      (car eq-cpos)))
      (insert (format "%s        %25s" line-breaking
		      (format ":%s-of" (nth 1 ret))))
      (if (null (nth 2 ret))
	  (insert (format " %14s:%-7s ."
			  (chise-turtle-uri-encode-ccs-name (car ret))
			  (nth 1 (split-string (car eq-cpos) ":"))))
	(insert " [ ")
	(setq col (current-column))
	(insert (format ":context domain:%-7s ;\n%s:target %7s:%-7s ] ."
			(chise-turtle-uri-encode-ccs-name (nth 2 ret))
			(make-string col ?\ )
			(chise-turtle-uri-encode-ccs-name (car ret))
			(nth 1 (split-string (car eq-cpos) ":"))))))
    (setq est-coded-charset-priority-list
	  (append est-coded-charset-priority-list
		  (nreverse child-ccs-list)))
    (when children
      (dolist (child children)
	(insert (format "%s    " line-breaking))
	(char-db-turtle-insert-char-features child nil nil nil 'for-sub-node)))
    ))

(defun char-db-turtle-insert-char-data (char &optional readable attributes)
  (save-restriction
    (narrow-to-region (point)(point))
    (char-db-turtle-insert-char-features char readable attributes)
    (insert "\n\n")
    ))

(defun char-db-turtle-insert-prefix ()
  (let (base-ccs-list ret)
    (insert "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix : <http://rdf.chise.org/rdf/property/character/main/> .
@prefix ideo: <http://rdf.chise.org/rdf/property/character/ideo/> .
@prefix isd: <http://rdf.chise.org/rdf/property/character/isd/> .
@prefix idc: <http://rdf.chise.org/rdf/type/character/idc/> .
@prefix chisegg: <http://rdf.chise.org/rdf/type/character/ggg/> .
@prefix domain: <http://rdf.chise.org/data/domain/> .
@prefix script: <http://rdf.chise.org/data/script/> .
@prefix ideocomb: <http://rdf.chise.org/data/character/ideo/combination/> .
@prefix chisebib: <http://rdf.chise.org/data/bibliography/> .
@prefix ruimoku: <http://www.chise.org/est/view/article@ruimoku/rep.id=/> .
@prefix zob1959: <http://chise.zinbun.kyoto-u.ac.jp/koukotsu/rubbings/> .

")
    (dolist (cell (sort chise-turtle-ccs-prefix-alist
			(lambda (a b)
			  (char-attribute-name< (cdr a)(cdr b)))))
      (insert (format "@prefix %s: <%s/%s=> .\n"
		      (car cell)
		      "http://www.chise.org/est/view/character"
		      (www-uri-encode-feature-name (cdr cell))))
      (setq ret (chise-split-ccs-name (cdr cell)))
      (unless (memq (car ret) base-ccs-list)
	(setq base-ccs-list (cons (car ret) base-ccs-list))))
    (insert "\n")
    (dolist (base-ccs (nreverse base-ccs-list))
      (insert (format "@prefix %s: <%s/%s/code-point/> .\n"
		      (chise-turtle-uri-encode-ccs-name base-ccs)
		      "http://rdf.chise.org/data/ccs"
		      (www-uri-encode-feature-name base-ccs))))))

(defun char-db-turtle-insert-ideograph-radical-char-data (radical)
  (let ((chars
	 (sort (copy-list (aref ideograph-radical-chars-vector radical))
	       (lambda (a b)
		 (ideograph-char< a b radical))))
	attributes)
    (dolist (name (char-attribute-list))
      (unless (memq name char-db-ignored-attributes)
	(push name attributes)
	))
    (setq attributes (sort attributes #'char-attribute-name<))
    (aset ideograph-radical-chars-vector radical chars)
    (dolist (char chars)
      (when (not (some (lambda (atr)
			 (get-char-attribute char atr))
		       char-db-ignored-attributes))
	(char-db-turtle-insert-char-data char nil attributes)))
    ))

(defun char-db-turtle-write-ideograph-radical-char-data (radical file)
  (if (file-directory-p file)
      (let ((name (char-feature (decode-char 'ucs (+ #x2EFF radical))
				'name)))
	(if (string-match "KANGXI RADICAL " name)
	    (setq name (capitalize (substring name (match-end 0)))))
	(setq name (mapconcat (lambda (char)
				(if (eq char ? )
				    "-"
				  (char-to-string char))) name ""))
	(setq file
	      (expand-file-name
	       (format "Ideograph-R%03d-%s.ttl" radical name)
	       file))))
  (let (chise-turtle-ccs-prefix-alist)
    (with-temp-buffer
      (char-db-turtle-insert-ideograph-radical-char-data radical)
      (goto-char (point-min))
      (char-db-turtle-insert-prefix)
      (insert "\n")
      (goto-char (point-min))
      (insert (format "# -*- coding: %s -*-\n"
		      char-db-file-coding-system))
      (let ((coding-system-for-write char-db-file-coding-system))
	(write-region (point-min)(point-max) file)))))


;;; @ end
;;;

(provide 'char-db-turtle)

;;; char-db-turtle.el ends here
