(require 'nphtml-atom)
(defconst nphtml--nest "${nest}")
;;first treats the pattern all args nphtml-arg
;;perhaps it may incur fuckinly many rec call

;;FIXME !!! it may need some arg modifiers in oreder to avoid infinite exitless calls
;;though I absolutely reluctant to fabricate the fuckin complex graph algo for this.
;;is there any cleverer way?(´・ω・｀)
(defun nphtml--make-molecule (arg &optional remove-cur)
  (let* ((seed   (nphtml--make-atom (nphtml--replace-string-with-arg arg)));centering string
	 (self   (lambda (arg) (nphtml--make-molecule arg t)))
	 (pls    (seq-map #'nphtml--replace-string-with-arg
			  (nphtml--args-get-kind :prepend arg)))
	 (psls   (if (null pls) '("") (seq-map self pls)))
	 (nls  (seq-map #'nphtml--replace-string-with-arg
			(nphtml--args-get-kind :nest    arg)))
	 (nsls (if (null nls) '("") (seq-map self nls)))
	 (als (seq-map #'nphtml--replace-string-with-arg
		       (nphtml--args-get-kind :append  arg)))
	 (asls (if (null als) '("") (seq-map self als)))

	 (ndone (string-replace nphtml--nest (string-join nsls "\n") seed))
	 (molec (concat (concat (string-join psls "\n") (if pls "\n" ""))
			ndone;code of beuty of symmetry! putting aside works properly or not(´・ω・｀)
			(concat (if als "\n" "") (string-join asls "\n"))))
	 (molec-no-cur (string-replace nphtml--cur "" molec))
	 (rtime (1- (nphtml--args-get-kind :repeat arg))))
    (concat (if remove-cur molec-no-cur molec)
	    (if (eq rtime 0) "" "\n")
	    (nphtml--string-times molec-no-cur rtime "\n"))))

;; (insert (nphtml--make-molecule (insert (format "%S" (nphtml--replace-string-with-arg 'html)))
;; (insert (nphtml--make-molecule '(:tag some :append ( appended))))))
;; (insert "\n" (nphtml--make-molecule
;; 	 '(:tag html :attrs (lang en)
;; 		:prepend ((:tag !DOCTYPE :type void :spec html))
;; 		:nest ((:tag head :type blocky
;; 			     :nest ((:tag meta :type void )))
;; 		       (:tag body :type blocky
;; 			     :nest ((:tag p :type inline
;; 					  :para "difficult to read than i've expected...(´・ω・｀)"))))
;; 		:append ((:tag dummy :type comment :para (current-time-string))))))


;; ;;========================OUTPUT================================
;; <!DOCTYPE html>
;; <html lang="en">
;; ${cur}<head>
;; <meta>
;; </head>
;; <body>
;; <p>difficult to read than i've expected...(´・ω・｀)</p>
;; </body>
;; </html>
;; <!-- Mon Dec  5 15:39:47 2022 -->
;; ;;==============================================================

;; (seq-map (lambda (x) (insert x)) '(nil)) throws error
;; (seq-map (lambda (x) (insert x)) 'nil)  --> nil

(provide 'nphtml-molecule)
