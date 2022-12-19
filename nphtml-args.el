;;first there exists a plist(=nphtml-arg) which has the surjective mapping to a (at least part of) html code
;;someFunction : plist ---> html-string
;;nphtml-arg is a plist
;;whose properties are
;;;;;| :tag      The name of tag or element that (string or symbol)
;;;;;| :alias    The name to which the whole nphtml-arg would be associated (string or symbol)
;;;;;| :spec     The string appended to the open tag, perhaps used only when adding `async' or `defer'
;;;;;| :type     The value is either of 'blocky, 'inline 'void or 'comment determines form of html
;;;;;| :para     The paragraph inserted into the html (string or expression evaled into a string)
;;;;;
;;;;;| :hidden   The boolean if it is non-nil tag name or alias disappear from the interface
;;;;;
;;;;;| :attrs    The list of string or symbol whose length need to be a even number
;;;;;            ex) (href "http://..." class someClassName)
;;;;;
;;;;;| :prepend  The list whose elems are npthml-arg or aliased-string html codes prepended
;;;;;| :append   The list whose elems are npthml-arg or aliased-string html codes appended
;;;;;| :nest     The list whose elems are npthml-arg or aliased-string html codes nested inside
;;;;;
;;;;;| :repeat   The integer more than or equal to 1 number of repeats of self

;;;;;| :forcing    The boolean variable if this is non-nil always uses its nesting tags
;;;;;            (this may infuse infinite recusive calls though)

(require 'cl-lib)
(defconst nphtml--args-type-kinds
  '(blocky inline void comment))
(defun nphtml--args-stringise (obj &optional nil-to-string)
  (cond ((null obj) (if nil-to-string "" nil))
	((symbolp obj) (symbol-name obj))
	(t obj)))
;; (nphtml--args-stringise 'nil t)
;; (nphtml--args-stringise "something" t)

(defconst nphtml--args-kinds
  `(:tag :alias :hidden :spec :type :para :attrs :prepend :append :nest :repeat :forcing))
(defun nphtml--args-get-kind (kind arg)
  ;; (when (cl-oddp (cl-list-length arg))
  ;;   (error "nphtml-arg must suffice at least being even number length"))
  (when (not (memq kind nphtml--args-kinds))
    (error "given kind %S is not a valid kind" kind))
  (let ((pgot (plist-get arg kind)));;yup, here begins shitty codes!!(｀・ω・´)
    (cond ((eq kind :tag) (cond ((and (null pgot)
				      (not (eq (plist-get arg :type) 'comment)))
				 (error "'%S' doesn't have :tag" arg))
				((not (or (symbolp pgot) (stringp pgot)))
				     (error "%S doesn't have valid :tag" arg)))
	   (nphtml--args-stringise pgot t)); :tag end always string!
	  ((eq kind :alias) (nphtml--args-stringise (or pgot (nphtml--args-get-kind :tag arg))))
	  ; :alias end    if it is nil :tag wins always string!
	  ((eq kind :spec) (nphtml--args-stringise pgot t))
	  ;  :spec end always string!
	  ((eq kind :para) (let ((evaled (eval pgot)))
			     (unless (or (null evaled) (stringp evaled))
			       (error "%S doesn't have valid :para" arg))
			     (nphtml--args-stringise evaled t)))
	  ;  :para end always string!
	  ((eq kind :hidden) (if pgot t nil))
	  ;  :hidden end always t or nil
	  
	  ((eq kind :type) (unless (and (symbolp pgot) (or (null pgot)
							   (memq pgot nphtml--args-type-kinds)))
			     (error "%S doesn't have valid :type" arg))
	   pgot); :type end always nil or vsymbol!
	  ((eq kind :attrs) (cond ((not (listp pgot)) (error "%S doesn't have valid :attrs" arg))
				  ((not (cl-evenp (length pgot)))
				   (error "%S's :attrs is not even length" arg)))
	   (seq-map (lambda (x) (nphtml--args-stringise x t)) pgot))
	    ;  :attrs end always list of strings or nil
	  ((eq kind :repeat) (cond ((and pgot (not (integerp pgot)))
				    (error "$S doesn't have valid :repeat" arg))
				   ((and pgot (not (<= 1 pgot)))
				    (error "%S's :repeat less than one" arg)))
	   (or pgot 1))
		  ;  :repeat end always 0 < integer
		((eq kind :forcing) (if pgot t nil))
		;;forcing end always returns t or nil
	  (t (unless (listp pgot) (error "%S doesn't have valid %S" arg kind))
	     pgot))))


;; (nphtml--args-get-kind :hidden '(:tag something :hidden "yes"))
;; (nphtml--args-get-kind :type '(:tag div :type "block")) -> throws error
;; (nphtml--args-get-kind :attrs '(:tag div :type blocky :attrs (href "address" class))) -> throws error
;; (nphtml--args-get-kind :attrs '(:tag div :type blocky :attrs (href nil class cname)))
;; (nphtml--args-get-kind :para '(:tag div :type blocky :para (current-time-string)))
;; (nphtml--args-get-kind :para '(:tag div :type blocky))
;; (nphtml--args-get-kind :tag '(:tag div :alias sam41 :type blocky))
;; (nphtml--args-get-kind :type '(:tag  :type blocky)) -> throws error
;;(nphtml--args-get-kind :tag '(:type comment :para "something written in comment"))
;;(nphtml--args-get-kind :repeat '(:type void :para "something" :repeat 3))


(defun nphtml--replace-string-with-arg (obj &optional weakly)
  (cond ((eq nil obj) nil)
	((eq t   obj) (error "um strange thing happened"))
	((or (symbolp obj) (stringp obj)) (if weakly (nphtml--get-hash-weakly obj)
					    (nphtml--get-hash obj)))
	(t obj)))
;;(nphtml--replace-string-with-arg 'html )
(defun nphtml--merge-args (main sub);FIXME(´・ω・｀)should make optional arg 'prefer-sub'
  "*Overwrites main's :para :type :spec :repeat
*Appends sub's :attrs :prepend :nest :append to main's ones"
  (let ((main-arg (cl-copy-list (nphtml--replace-string-with-arg main)))
	(sub-arg  (nphtml--replace-string-with-arg sub)))
    (plist-put main-arg :para (plist-get sub-arg :para))
    (plist-put main-arg :repeat (plist-get sub-arg :repeat))
    (plist-put main-arg :type (or (plist-get main-arg :type) (plist-get sub-arg :type)))
    (plist-put main-arg :spec (or (plist-get main-arg :spec) (plist-get sub-arg :spec)))

    (plist-put main-arg :attrs (append (plist-get main-arg :attrs) (plist-get sub-arg :attrs)))
    (plist-put main-arg :prepend (append (plist-get main-arg :prepend) (plist-get sub-arg :prepend)))
    (plist-put main-arg :nest (append (plist-get main-arg :nest) (plist-get sub-arg :nest)))
    (plist-put main-arg :append (append (plist-get main-arg :append) (plist-get sub-arg :append)))

    main-arg))
(defun nphtml--args-push-self-to-other-nest (self other)
  (let* ((self-arg (nphtml--replace-string-with-arg self t))
	 (other-arg (nphtml--replace-string-with-arg other t))
	 (output (cl-copy-list other-arg))
	 (other-nest (plist-get other-arg :nest)))
    (push self-arg other-nest)
    (setf output (plist-put output :nest other-nest))))

;(nphtml--merge-args '(:tag div :repeat 3 :para "something") '(:para "some" ))

  
;; (nphtml--make-molecule
;; (insert 
;;  (nphtml--make-molecule
;;   (nphtml--merge-args 'a '(:attrs (class some h "http://myHomePage.com") :repeat 4))))

;; (nphtml--replace-string-with-arg 'a)

(provide 'nphtml-args)
