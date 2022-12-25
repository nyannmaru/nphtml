
(defgroup nphtml nil
  "convension for writing html or svg")
(defcustom nphtml-lang-attr "en"
	"lang attribute for html tag")
(defconst nphtml--builtin-html-args
  `((:alias dateComment :para (concat "FirstEdit: " (current-time-string)) :type comment)
    (:alias comment     :para ""                                           :type comment)

		(:tag p  :type blocky)
    (:tag h1 :type inline)
    (:tag h2 :type inline)
    (:tag h3 :type inline)
    (:tag h4 :type inline)
    (:tag h5 :type inline)
    (:tag h6 :type inline)
    (:tag !DOCTYPE :hidden t :type void :spec html)
    (:tag html :type blocky :attrs (lang ,nphtml-lang-attr)
	  :prepend (dateComment !DOCTYPE)
	  :nest (head body))
		(:tag head :type blocky :nest (title favlink chmeta vpmeta script style) :hidden t)
    (:tag body :type blocky :nest (header main footer) :hidden t)

		(:tag link :alias favlink :type void :attrs (rel icon type "" sizes "" href ""))
    (:tag meta :alias chmeta :type void :attrs (charset utf-8) :hidden t)
		(:tag meta :alias vpmeta :type void
					:attrs (name "viewport" content "width=device-width, initial-scale=1"))
    (:tag meta :type void :attrs (name "" content ""))
    (:tag title :type inline :hidden t)

		(:tag section :type blocky)
		(:tag div     :type blocky)
		(:tag header  :type blocky)
    (:tag main    :type blocky)
		(:tag footer  :type blocky)
		(:tag article :type blocky)
		(:tag aside   :type blocky)

		(:tag nav  :type blocky)
		(:tag ol   :type blocky)
		(:tag ul   :type blocky)
		(:tag li   :type inline)

		(:tag dl   :type blocky
					:nest (dt dd))
		(:tag dt   :type inline) (:tag dd :type inline)
		
		(:tag img    :type void :attrs (src "" alt ""))
		(:tag audio  :type blocky)
		(:tag canvas :type blocky)
		(:tag video  :type blocky)

		(:tag a  :type inline :attrs (href ""))
		(:tag button :type inline)
		(:tag input :type inline)
		(:tag select :type inline)
		(:tag textarea :type inline)

		(:tag ins :type inline)
		(:tag del :type inline)

		(:tag q          :type inline)
		(:tag cite       :type inline)
		(:tag blockquote :type blocky :attrs (cite ""))

		(:tag dfn        :type inline)
		(:tag abbr       :type inline :attrs (title ""))
		(:tag time       :type inline :attrs (datetime ""))

		


		(:alias code :tag pre :type inline :nest ((:tag code :type blocky)))
		(:tag samp :type inline)
		(:tag kbd  :type inline)
		(:tag var  :type inline)
		(:tag map)
    (:tag mark)
    (:tag math)
    
    (:tag meter)
    
		
    (:tag nobr)
    (:tag noframes)
    (:tag noscript)
    (:tag object)
    (:tag optgroup)
    (:tag option)
    (:tag output)
    (:tag over)
    (:tag param)
    (:tag person)

    (:tag progress)

    (:tag rev)
    (:tag rp)
    (:tag rt)
    (:tag ruby)
    (:tag s)
    (:tag samp)
    (:tag script)

		(:tag figure :type blocky :nest ((:tag figcaption :type inline) img))
		
    (:tag source)


    (:tag style)
		
		(:tag picture :type blocky :nest (sorce img))
		(:tag source  :type inline :attrs (media "" srcset ""))

    (:tag span   :type inline) 
    (:tag sub    :type inline)
    (:tag sup    :type inline)
		(:tag s      :type inline)
		(:tag mark   :type inline)
		(:tag small  :type inline)
		(:tag strong :type inline)



    (:tag track)
    (:tag tt)
    (:tag u)
    (:tag var)

		(:tag pre :type blocky)
		(:tag br  :type void)
		(:tag wbr :type inline) ;what's this?(´・ω・｀)
		(:tag hr  :type void)


		(:tag table :type blocky
					:nest ((:tag caption :type inline)
								 (:tag thead :type blocky :nest
											 ((:type comment :para "<tr><th>header1</th> <th colspan=\"m\">header2</th> ...</tr>")))
								 (:tag tbody :type blocky :nest
											 ((:type comment :para "<tr><td>data1</td> <td rowspan=\"m\">data2</td> ...</tr>")))))
		(:tag th :type inline)
		(:tag tr :type inline)
		(:tag details :type blocky :nest ((:tag summary :type inline)))
    ))


(defvar nphtml--str-arg-hash nil)
(defvar nphtml--hiddens nil)
(defvar nphtml--completion-list nil)
(defun nphtml--push-hash-internal (nphtml-arg)
  (let ((alias (nphtml--args-get-kind :alias nphtml-arg)))
    (puthash alias nphtml-arg nphtml--str-arg-hash)
    (if (nphtml--args-get-kind :hidden nphtml-arg)
	(cl-pushnew alias nphtml--hiddens :test #'string-equal)
      (cl-pushnew alias nphtml--completion-list :test #'string-equal))))
(defun nphtml--push-hash (nphtml-args)
  (dolist (nphtml-arg nphtml-args)
    (nphtml--push-hash-internal nphtml-arg)))
(defun nphtml--init-hash nil
  (setf nphtml--str-arg-hash (make-hash-table :test 'equal))
  (nphtml--push-hash nphtml--builtin-html-args)
  (when nphtml-add-gblogger
    (nphtml--push-hash nphtml--builtin-gblogger-args)))
(defun nphtml-reinit-hash nil
	(interactive)
	(setf nphtml--str-arg-hash nil
				nphtml--completion-list nil)
	(nphtml--init-hash))
(defcustom nphtml-immediate-error nil
  "determines behaviour when you try to gethash with unregisted key"
  :type 'boolean)
(defun nphtml--get-hash (alias)
  (when (null alias) (error "had tryied to gethash with nil key"))
  (when (null nphtml--str-arg-hash) (nphtml--init-hash));just init when hash is not inited
  (let* ((astr   (if (symbolp alias) (symbol-name alias) alias))
	 (default (list :tag astr))
	 (hashv (gethash astr nphtml--str-arg-hash nil)))
    (if hashv hashv;hashv exists immediately returns it
      (if nphtml-immediate-error (error "you tried to get unhashed key val %s" alias)
	default))))

(defun nphtml--get-hash-weakly (alias)
  (let ((gotten (nphtml--get-hash alias)))
		(if (eq t (plist-get gotten :forcing))
				gotten
			(plist-put gotten :prepend nil)
			(plist-put gotten :nest nil)
			(plist-put gotten :append nil)
			gotten)))
;;(nphtml--make-molecule (nphtml--replace-string-with-arg 'gbImtitle t))

(provide 'nphtml-args-builtin)
