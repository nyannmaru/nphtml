(require 'nphtml-args)
(require 'nphtml-args-builtin)
(require 'nphtml-attrs-hash)

(defgroup nphtml nil
  "convension for writing html or svg")
(defcustom nphtml-default-format-type 'blocky
  "determines which format would be used when type is not feeded"
  :type 'symbol);FIXME(´・ω・｀)should add choice
;;<src ${attrs}>paragraph${cur}</div>
(defconst nphtml--type-formats
  '((blocky  . "<${tag}${attrs}${spec}>\n${para}${cur}${nest}\n</${tag}>")
    (inline  . "<${tag}${attrs}${spec}>${para}${cur}${nest}</${tag}>")
    (void    . "<${tag}${attrs}${spec}>${cur}")
    (comment . "<!-- ${para}${cur} -->")))
(defconst nphtml--tag   "${tag}")
(defconst nphtml--attrs "${attrs}")
(defconst nphtml--spec "${spec}")
(defconst nphtml--para "${para}")
(defconst nphtml--cur   "${cur}")

(defun nphtml--make-atom-seed (arg)
  (let ((type (nphtml--args-get-kind :type arg))
	(nestp (nphtml--args-get-kind :nest arg)))
    (when (and nestp (memq type '(void comment)))
      (error "void or comment type elements can nest nothing"))
    (cdr (assoc (or type nphtml-default-format-type) nphtml--type-formats))))
;;(nphtml--make-atom-seed '(:tag div :type void :nest (some))) -->throws error
;;(nphtml--make-atom-seed '(:tag div :type comment))
(defun nphtml--make-atom-replace-tag (seed arg)
  (string-replace nphtml--tag (nphtml--args-get-kind :tag arg) seed))
(defun nphtml--make-atom-replace-attrs (seed arg)
  (let ((attrls (nphtml--args-get-kind :attrs arg)))
    (if (null attrls) (string-replace nphtml--attrs "" seed)
      (let* ((divided (seq-partition attrls 2))
	     (hash    (make-hash-table :test 'equal))
	     (fmt     "%s=\"%s\""))
	(seq-do (lambda (div);FIXME(´・ω・｀)too ugly
		  (puthash (nphtml--attrs-gethash (car div))
			   (push (cadr div) (gethash (nphtml--attrs-gethash (car div)) hash)) hash))
		divided)
	(let ((attrstrls
	       (seq-map (lambda (key) (format fmt key (string-join (gethash key hash) " ")))
			(hash-table-keys hash))))
	  (string-replace nphtml--attrs
			  (concat " " (string-clean-whitespace (string-join attrstrls " "))) seed))))))

;; (nphtml--make-atom-replace-attrs (nphtml--make-atom-seed '(:tag div :type inline))
;;                                          '(:attrs (class "something" class someone)))
(defun nphtml--make-atom-replace-spec (seed arg)
  (let ((spec (nphtml--args-get-kind :spec arg)))
    (string-replace nphtml--spec (if (string-empty-p spec) "" (concat " " spec)) seed)))
(defun nphtml--make-atom-replace-para (seed arg)
  (string-replace nphtml--para (nphtml--args-get-kind :para arg) seed))
(defun nphtml--string-times (string times &optional joint-string)
  (cond ((<= times 0) "")
	((eq times 1) string)
	(t (let ((output "")
		 (ltime (1- times)))
	     (dotimes (time times)
	       (setf output (concat output string
				    (if (and joint-string (not (eq ltime time)))
					joint-string ""))))
	     output))))
;(insert "\n" (nphtml--string-times "some" 4 "\s"))
;some some some some
(defun nphtml--make-atom (arg &optional remove-cur)
  (let* ((seed (nphtml--make-atom-seed arg))
	 (tdone (nphtml--make-atom-replace-tag seed arg))
	 (atdone (nphtml--make-atom-replace-attrs tdone arg))
	 (satdone (nphtml--make-atom-replace-spec atdone arg))
	 (atom (nphtml--make-atom-replace-para satdone arg))
	 (atom-no-cur (string-replace nphtml--cur "" atom)))
    (if remove-cur atom-no-cur atom)))


;;(insert (nphtml--make-atom '(:tag div :attrs (class bright id title class some) :para "insertion as a para") t))
;; (insert (nphtml--make-atom '(:tag div :attrs (class bright id title class some)
;; 				  :para "something" :repeat 3)))



(provide 'nphtml-atom)
