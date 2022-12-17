(defgroup nphtml nil
  "convension for writing html or svg")
(defcustom nphtml-add-gblogger t
  "decide whether add to gblogger or not when init"
  :type 'boolean)

(defconst nphtml--builtin-gblogger-args
	(let ((archUrls "<$BlogArchiveURL$>") (archNames "<$BlogArchiveName$>"))
		;;crucially important tags
		`((:type blocky :tag Blogger  :alias gbBlogger)
			
			(:type blocky :tag BloggerArchives :alias gbArchives
						 :nest ((:tag li :type inline :nest ((:tag a :attrs (href ,archUrls) :para ,archNames
																											 :type inline)))))

			;;condition case like tags
			(:type blocky :tag ItemPage :alias gbCondItempage)
			(:type blocky :tag MainPage :alias gbCondMainpage)
			(:type blocky :tag ArchivePage :alias gbCondArchivepage)
			(:type blocky :tag MainOrArchivePage :alias gbCondMainorarch)
			(:type blocky :tag BlogItemCommentsEnabled :alias gbCondCenabled)

			;;templates
			(:type blocky :tag BlogItemTitle :nest (BlogItemUrl) :alias gbItemtitle)
			(:type inline :tag BlogItemUrl   :nest ((:tag a :attrs(href <$BlogItemUrl$>))) :hidden t)
			(:type blocky :tag BloggerPreviousItems :alias gbPrevs
						 :nest ((:tag li
												 :nest
												 ((:tag a :attrs (href <$BlogItemPermalinkURL$>) :para <$BlogPreviousItemTitle$>)))))
			(:type inline :tag title :alias gbBlogPageTitle :para <$BlogPageTitle$> :hidden t)
			(:type void   :tag $BlogMetaData$                           :hidden t)
			
			(:type blocky :tag h1 ))))


(provide 'nphtml-args-builtin-gblogger)
