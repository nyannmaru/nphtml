(defgroup nphtml nil
  "convension for writing html or svg")
(defcustom nphtml-add-gblogger t
  "decide whether add to gblogger or not when init"
  :type 'boolean)

(defconst nphtml--builtin-gblogger-args
	(let ((archUrls "<$BlogArchiveURL$>") (archNames "<$BlogArchiveName$>")
				(prevTitle "<$BlogPreviousItemTitle$>") (prevLink "<$BlogItemPermalinkURL$>"))
		;;crucially important tags
		`((:type blocky :tag Blogger  :alias gb-blogger)
			
			(:type blocky :tag BloggerArchives :alias gb-archives
						 :nest ((:tag li :type inline
										:nest ((:tag a :attrs (href ,archUrls) :para ,archNames :type inline)))))

			;;condition case like tags
			(:type blocky :tag ItemPage :alias gbCond-itempage)
			(:type blocky :tag MainPage :alias gbCond-mainpage)
			(:type blocky :tag ArchivePage :alias gbCond-archivepage)
			(:type blocky :tag MainOrArchivePage :alias gbCond-mainorarch)
			(:type blocky :tag BlogItemCommentsEnabled :alias gbCond-cenabled)

			;;templates
			(:type blocky :tag BlogItemTitle :nest (BlogItemUrl) :alias gb-itemtitle)
			(:type inline :tag BlogItemUrl   :nest ((:tag a :type inline :attrs (href <$BlogItemUrl$>))) :hidden t)
			(:type blocky :tag BloggerPreviousItems :alias gbPrevs
						 :nest ((:tag li :type inline
									:nest ((:tag a :attrs (href ,prevLink) :para ,prevTitle :type inline)))))
			(:type inline :tag title :alias gbBlogPageTitle :para <$BlogPageTitle$> :hidden t)
			(:type void   :tag $BlogMetaData$                           :hidden t)
			
			(:type blocky :tag h1 ))))


(provide 'nphtml-args-builtin-gblogger)
