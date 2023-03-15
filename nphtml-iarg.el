;;based on committed string i wanna make the due nphtmlarg

;;;;committed string ====> "ol li a **4 ~i,olID ~~c,liClass ~~~href,http://" etc
;;     [  ~  :attrs]  [  %  :para]  [  +   :append  ] [  -  :prepend  ]
;;     [  *  :repeat ]
;;     should add [ .  :attrs (class ...)] [#]
(defconst nphtml--analyse-committed-special-char-prefixes
	(list ?~ ?@ ?! ?+ ?* ?. ?#))
;;is a super-set of nphtml--analyse-command-chars
(defconst nphtml--analyse-command-chars
  (list ?~ ?@ ?! ?+ ?* ?. ?#))
(defcustom nphtml-analyse-attr-separator ?,
  "seperator when you feeding the nphtml interactive arg this need to be a outsider of `nphtml--analyse-command-chars'"
  :type 'character)
(defun nphtml--analyse-simple-cut nil
  (if (null nphtml--overlay-committed-string)
      (error "committed string haven't assined yet"))
  (split-string nphtml--overlay-committed-string "[\s\t]+" t))
(defvar nphtml--analyse-main-parts nil)
(defun nphtml--analyse-get-main-parts nil
  (let ((splitted (nphtml--analyse-simple-cut)))
    (setf nphtml--analyse-main-parts
	  (seq-take-while
	   (lambda (str)
	     (not (member (seq-first str) nphtml--analyse-committed-special-char-prefixes)))
	   splitted))))
;; (setf nphtml--overlay-committed-string "ol li a **4 #olID ..liClass ~~~href,http://")
;; (nphtml--analyse-get-main-parts)==> ("ol" "li" "a")
;;;ol must nest li, li must nest a
;;;but before doing nest piramid i have to add command chars to each
(defun nphtml--analyse-complex-cut nil
  (let* ((string nphtml--overlay-committed-string)
	 (output nil) (pt 0) (pt-begin 0) (max-pt (length string)) (substr string)
	 (sps (regexp-opt-charset nphtml--analyse-command-chars))
	 (nsps (replace-regexp-in-string "^\\[" "[^" sps))
	 (regp (format "\\(%s+%s+\\)" sps nsps)))
    (while (and  (setf pt-begin  (string-match regp substr)
		       pt         (+ (or pt-begin max-pt) pt))
		 (and pt (< pt max-pt));FIXME(´・ω・｀)redesign needed!!
		 (not (string-empty-p substr)));;should add kill time to avoid infinite cycle
      (let ((pushed (match-string 1 substr)))
	(push (string-trim-right pushed) output)
	(setf substr (substring substr  (+ pt-begin (length pushed))))))
      ;;(insert (format "\npt is '%S' substr is '%S' output is %S" pt substr output)))
    (seq-reverse output)))
;;(nphtml--analyse-complex-cut)

(defun nphtml--analyse-sub-inner-filter (string char depth)
  (let ((shorter  (make-string depth      char))
	(longer  (make-string (1+ depth) char)))
    (and (string-match-p (concat "^" (regexp-quote shorter)) string)
	 (not (string-match-p (concat "^" (regexp-quote longer)) string)))))

(defun nphtml--analyse-sub-filter (strls depth)
  "returns list whose elems are only depth length commands char"
  (seq-filter
   (lambda (str)
     (seq-some (lambda (ch) (nphtml--analyse-sub-inner-filter str ch depth))
			  nphtml--analyse-command-chars))
   strls))
;;(nphtml--analyse-sub-filter (nphtml--analyse-complex-cut) 2)

;;(nphtml--analyse-remove-prefix "**some")
;; (setf some '(:tag dummy))
;; (seq-reduce #'nphtml--merge-args '((:type void) (:repeat 3) (:attrs (some thing))) some)
;;ok good shit
(defun nphtml--concat-pcells (cell1 cell2)
  (let ((output (cl-copy-list cell1))
	(type  (car cell2))
	(symplet '(:type :tag :alias :para :repeat :spec)))
    (if (memq type symplet)
	(setf output (plist-put output type (or (plist-get output type) (plist-get cell2 type))))
      (setf output (plist-put output type (append (plist-get output type) (plist-get cell2 type)))))
    output))
;(seq-reduce #'nphtml--concat-pcells '((:type void) (:repeat 3) (:attrs (some thing))) nil)
(defun nphtml--analyse-remove-prefix (string)
  (string-trim-left string
   (concat (regexp-opt-charset nphtml--analyse-command-chars) "+")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;FOR EXPANDING FEATURE FIDDLE HERE(´・ω・｀);;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nphtml--analyse-str-plistnize (string)
  (let ((val (nphtml--analyse-remove-prefix string))
	(ch (seq-first string)));?~ ?@ ?! ?+ ?* ?. ?#
    (cond
		 ((eq ch ?~) `(:attrs ,(split-string val "," nil "\s*")))
	   ((eq ch ?.) `(:attrs ,(list "class" (string-trim val))))
	   ((eq ch ?#) `(:attrs ,(list "id" (string-trim val))))
	   ((eq ch ?@) `(:para  ,val))
	   ((eq ch ?!) `(:prepend ,(split-string val "," nil "\s*")))
	   ((eq ch ?+) `(:append  ,(split-string val "," nil "\s*")))
	   ((eq ch ?*) `(:repeat ,(string-to-number val))))))
(defvar nphtml--analyse-sub-parts nil)
(defun nphtml--analyse-get-sub-parts nil
  (when nphtml--analyse-main-parts
    (let ((output nil)
	  (mdepth (length nphtml--analyse-main-parts))
	  (csplitted (nphtml--analyse-complex-cut)))
      (dotimes (depth mdepth)
	(let* ((cells (nphtml--analyse-sub-filter csplitted (1+ depth)))
	       (pcells (seq-map #'nphtml--analyse-str-plistnize cells)))
	  (push (seq-reduce #'nphtml--concat-pcells pcells nil) output)))
      (setf nphtml--analyse-sub-parts (seq-reverse output)))))

;; (setf nphtml--overlay-committed-string "ol li a  #shadowy #olID ..liClass ** 34 ~~~href,http://")
;; (nphtml--analyse-sub-filter (nphtml--analyse-complex-cut) 2)
;; (nphtml--analyse-get-main-parts)
;; (nphtml--analyse-get-sub-parts)


(defun nphtml--zip-list (list1 list2 function)
  (let ((min-len (min (length list1) (length list2)))
	(output   nil))
    (dotimes (time min-len)
      (push (funcall function (seq-elt list1 time) (seq-elt list2 time)) output))
    (seq-reverse output)))
;;(nphtml--zip-list '(3 5) '(3 4) #'+)
(defun nphtml--nplist-nest-piramid (npargls)
  (let* ((rev (seq-reverse npargls))
	 (seed (car rev)))
    (dolist (arg (cdr rev))
      (setf seed (nphtml--args-push-self-to-other-nest seed arg)))
    seed))
(defun nphtml--analyse-comitted nil
  (nphtml--analyse-get-main-parts)
  (nphtml--analyse-get-sub-parts)
  (cond ((and nphtml--analyse-main-parts
	      (seq-every-p #'null nphtml--analyse-sub-parts))
	 (if (eq (length nphtml--analyse-main-parts) 1)
	     (nphtml--replace-string-with-arg (car nphtml--analyse-main-parts))
	   (let ((args (seq-map (lambda (x) (nphtml--replace-string-with-arg x t))
													nphtml--analyse-main-parts)))
	     (nphtml--nplist-nest-piramid args))))
	((and nphtml--analyse-main-parts nphtml--analyse-sub-parts)
	 (let ((zipped (nphtml--zip-list nphtml--analyse-main-parts nphtml--analyse-sub-parts
					 #'nphtml--merge-args)))
	   (nphtml--nplist-nest-piramid zipped)))
	(t nil)))

(provide 'nphtml-iarg)
