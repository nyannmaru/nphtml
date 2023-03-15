(require 'nphtml-molecule)
(require 'nphtml-iarg)
(require 'npring)
(require 'nphtml-args)
(require 'nphtml-args-builtin-gblogger)
(require 'nphtml-args-builtin)
(require 'seq)

;;;;this package is for using nphtml--make-molecule
;;;;this package defines the special overlay and committed string(inside the special overlay you can commit)

(defgroup nphtml nil
  "convension for writing html or svg")
(defcustom nphtml-insert-before-hook nil
  "Hook runs before `nphtml-insert-html' is called"
  :type 'hook)
(defcustom nphtml-insert-after-hook nil
  "Hook runs after `nphtml-insert-html' is called"
  :type 'hook)


;;arg-seed is a string that would be transformed into a nphtml-arg
;;li a -class:LiClass --class:aClass --href(:"")? (*5)?(number-of-repeat)
;;gethash-weakly ===> get at least about (:tag :type :attrs :para) **implement it with removing**
;;and then merge gotten one into the seeded arg

;;arg-seed ==> [argls modargls repeat] = [(li a) ((:attrs (class liclass)) (attrs (...))) 5]
;;(li a)=>(arg(li) arg(a)) mergeEach with Each modargls
;;

(defconst nphtml--overlay-bstring (propertize "NPHTML [ " 'face '(:foreground "red")))
(defconst nphtml--overlay-astring (propertize " ]"        'face '(:foreground "red")))


(defconst nphtml--overlay-container
  (make-npring 1
    (lambda (val) (and (consp val))) nil
    (lambda (val) (let ((ol (make-overlay (car val) (cdr val))))
										(overlay-put ol 'local-map nphtml--overlay-localmap)
										(overlay-put ol 'face 'highlight);FIXME(´・ω・｀)
										(overlay-put ol 'before-string nphtml--overlay-bstring)
										(overlay-put ol 'after-string  nphtml--overlay-astring)
										ol))
    (lambda (ol val) (move-overlay ol (car val) (cdr val) (current-buffer)))))


;;(defface nphtml--overlay-face )

(defvar nphtml--overlay-committed-string nil)
(defvar-local nphtml--overlay-committed-string-history nil)
(defun nphtml--overlay-commit nil
  (interactive)
  (when (nphtml--overlay-hibernatedp) (error "currently nphtml-overlay is hibernating"))
  (let* ((ol (nphtml--overlay-get)) (spt (overlay-start ol)) (ept (overlay-end ol))
				 (committed (buffer-substring-no-properties  spt ept)))
    (nphtml--overlay-force-hibernate) ;but without cancelling timer
    (when (null nphtml--overlay-committed-string-history)
      (setf nphtml--overlay-committed-string-history
						(make-npring 10 #'stringp)))
    (npring-enqueue nphtml--overlay-committed-string-history committed)
    (setf nphtml--overlay-committed-string committed)))


;;FIXME(´・ω・｀)should make more earnestly
(defun nphtml--overlay-completion-at-point nil ;; for C-M-i
	(interactive)
	(while (not (eq (char-after) ?\s)) (forward-char));move to the last point of the word
	(unless nphtml--completion-list (nphtml--init-hash))
	(let* ((sym (symbol-at-point))
				 (str (if (null sym) "" (symbol-name sym)))
				 (tried (try-completion str nphtml--completion-list)))
		(cond ((null tried) (message (format "No candidate for '%s' found" str)))
					((stringp tried)
					 (if (eq (length str) (length tried));;if completion doesn't work message them
							 (let* ((trail (propertize "=>" 'face '(:foreground "red")))
											(filtered (seq-filter (lambda (lstr) (string-prefix-p str lstr t)) nphtml--completion-list))
											(mapped   (seq-map    (lambda (lstr) (concat trail (propertize 
																																					(substring lstr (length str))
																																					'face '(:foreground "green"))))
																						filtered)))
								 (message (if mapped (string-join mapped "\s"))))
						 (unless (string-empty-p tried)
							 (delete-char (- (length str)))
							 (insert tried (if (member tried nphtml--completion-list) " " "")))))
					(t (insert "\s")))))




(defun nphtml--overlay-kill-overlay nil;;for C-g to cancel
  (interactive)
	(nphtml--overlay-force-hibernate t));FIXME(´・ω・｀)

(defvar nphtml--overlay-localmap (let ((map (make-sparse-keymap)));FIXME(´・ω・｀)
																	 (define-key map (kbd "C-m") 'nphtml--overlay-commit)
																	 (define-key map (kbd "C-g") 'nphtml--overlay-kill-overlay)
																	 (define-key map (kbd "C-M-i") 'nphtml--overlay-completion-at-point)
																	 (define-key map (kbd "TAB") 'nphtml--overlay-completion-at-point)
																	 (define-key map (kbd "C-a") 'nphtml--overlay-move-beginning-of-line)
																	 (define-key map (kbd "C-e") 'nphtml--overlay-move-end-of-line)
																	 map))
(defun nphtml--overlay-move-beginning-of-line nil
	(interactive)
	(goto-char (overlay-start (nphtml--overlay-get))))
(defun nphtml--overlay-move-end-of-line nil
	(interactive)
	(goto-char (- (overlay-end (nphtml--overlay-get)) 2)))

(defun nphtml--overlay-init nil
  (let ((spt (point)) (ept (point-at-eol)))
    (when (eq spt ept) (save-excursion (insert "  ")))
    (npring-enqueue nphtml--overlay-container (cons (point) (point-at-eol))))
  ;;this let seg builds an overlay in where you can commit a string
  (add-hook 'post-command-hook 'nphtml--overlay-observer);this may be dangerous...(´・ω・｀)
  )
;;combination of overlay and its observer makes up the utility i want

(defun nphtml--overlay-get nil
  (when (npring-empty-p nphtml--overlay-container)
    (error "overlay-container is empty yet"))
  (npring-oldest nphtml--overlay-container))
(defun nphtml--overlay-hibernatedp nil
  (null (overlay-end (nphtml--overlay-get))))
(defun nphtml--overlay-should-be-continuedp nil
  (let ((ol (nphtml--overlay-get)))
    (and (<= (overlay-start ol) (point) (overlay-end ol))
				 (eq (overlay-buffer ol) (current-buffer)))))


(defvar nphtml--overlay-message-str ;FIXME(´・ω・｀)should make face
	(let  ((tilda  (propertize "~" 'face '(:foreground "red")))
				 (dot    (propertize "." 'face '(:foreground "red")))
				 (sharp  (propertize "#" 'face '(:foreground "red")))
				 (atmark (propertize "@" 'face '(:foreground "red")))
				 (excram (propertize "!" 'face '(:foreground "red")))
				 (plus   (propertize "+" 'face '(:foreground "red")))
				 (times  (propertize "*" 'face '(:foreground "red"))))
		(concat  sharp " -> id " dot " -> class " tilda " -> attrs" "\n"
						 excram " -> prepends tags " plus " -> appends tags " times " -> repeats a tag" "\n"
						 atmark " -> is for insert paragraph")))

(defun nphtml--overlay-force-hibernate (&optional cancelling-timer)
  "***this moves the overlay to nil***
***delete-region***
***remove observer from the hook***
***(&optional)kills the timer of the fucntion-timer***."
  (interactive)
  (let* ((ol (nphtml--overlay-get))
				 (spt (overlay-start ol)) (ept (overlay-end ol)))
    (when (eq (overlay-buffer ol) (current-buffer)) (delete-region spt ept))
    (delete-overlay ol));;move to nil point not deletion 
  (remove-hook 'post-command-hook #'nphtml--overlay-observer)
  (when cancelling-timer (cancel-timer nphtml--insert-timer)))


(defun nphtml--overlay-observer nil
  (interactive)
  (cond ((nphtml--overlay-should-be-continuedp);point inside the overlay and on the same buffer
				 (unless (current-message)
					 (message "%s" nphtml--overlay-message-str)));jus shows string and wait for commit of the string
				(t (nphtml--overlay-force-hibernate t))));leads to self-firing




;; (nphtml--overlay-init)
;; (npring-enqueue nphtml--overlay-container (cons (point-at-bol) (point-at-eol)))
;; (delete-overlay (npring-oldest nphtml--overlay-container))






;;hey el handy async is where...async async ashole!!!!(´・ω・｀)
(defvar nphtml--insert-timer nil)
(defun nphtml--insert-init-timer nil
  (if (null nphtml--insert-timer)
      (setf nphtml--insert-timer
						(run-with-timer 0.5 0.45  #'nphtml--insert-insert-string-when-good))
    (timer-activate nphtml--insert-timer)));FIXME(´・ω・｀)should add timer custom
(defun nphtml--insert-revert-vars nil
  (cancel-timer nphtml--insert-timer)
  (setf nphtml--overlay-committed-string nil
				nphtml--analyse-main-parts nil
				nphtml--analyse-sub-parts  nil)
  (remove-hook 'post-command-hook #'nphtml--overlay-observer))
(defun nphtml--insert-insert-string-when-good nil
  (condition-case err
      (if (null nphtml--overlay-committed-string) nil;adds daemon like running
				(cond (nphtml--overlay-committed-string
							 (cancel-timer nphtml--insert-timer)
							 (run-hooks 'nphtml-insert-before-hook)
							 (let* ((spt (point))
											(seed (nphtml--analyse-comitted));based on committed-string makeup nphtml-arg
											(molec (nphtml--make-molecule seed)))
								 (insert molec)
								 (nphtml--insert-revert-vars)
								 (let ((ept (point)))
									 (search-backward nphtml--cur spt nil)
									 (delete-char (length nphtml--cur))
									 (indent-region spt ept)
									 (run-hooks 'nphtml-insert-after-hook))))
							(t (error "one point i can say is something bad happened...(´・ω・｀)"))))
																				;if ends here begins finally error handling
    (error
     (nphtml--insert-revert-vars)
     (cancel-timer nphtml--insert-timer)
     (remove-hook 'post-command-hook #'nphtml--overlay-observer)
     (error "%s" (error-message-string err)))));;should i change to message?(´・ω・｀)
;; (condition-case err
;;     (progn (message some) (insert "before-insertion"))
;;   (error (message "say yes") (insert "error happened"))
;;   (insert "after-insertion"))

;;;###autoload
(defun nphtml-insert-html nil
  (interactive)
  (nphtml--insert-init-timer);this forces emacs call insert-when-good sparsely
  (nphtml--overlay-init)

  ;;(=assigns nonnil val to committed-string)
  ;;this triggers the insertion of molec 

  ;;overlay spanned!!!
  ;;when-good waits for the assignment of a committed string like daemon
  ;;when-good kills timer after the insertion irrelevant to its success or not
  ;;when-good may not be called that means the overlay killed before the commit have done
  ;;             in that case the overlay needs to kill the timer as last job
  ;;this may implies that the redesign of the supervisor is inevitable...(´・ω・｀)
  )


;;(local-set-key (kbd "<") 'nphtml-insert-html)





(provide 'nphtml)

