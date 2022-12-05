(defvar nphtml-attrs-completion-hash
  #s(hash-table test equal
   data ("c" "class"      "cl" "class"       "cls" "class"
	 "s" "src" "h" "href" "i" "id" )))

(defun nphtml--attrs-gethash (key)
  (gethash key nphtml-attrs-completion-hash key))

;; (insert (nphtml--make-molecule
;; 	 (nphtml--merge-args (nphtml--replace-string-with-arg 'a) '(:repeat 8))))
(provide 'nphtml-attrs-hash)
