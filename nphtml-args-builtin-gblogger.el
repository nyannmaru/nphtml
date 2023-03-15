(defgroup nphtml nil
  "convension for writing html or svg")
(defcustom nphtml-add-gblogger t
  "decide whether add to gblogger or not when init"
  :type 'boolean)

;;^gbBg* is blog specific templates such as like a blog owner, discription or title etc
;;^gbIm* is blog item(=the page you've written as a content of the blog)
;;       such as like a editDate or title etc
;;^gbCn* is blogger special conditional blocky tags
;;       if a certain codition is sufficed then its internal body would be 
;;       shows up as a part of html document when blog is loaded on the browser.
;;       for example if you write <MainPage><p>paragraph!!</p></MainPaga> paragraph!! would be written only
;;       in main page(=root page of your blog)
;;^gbAv* is for archive specific tags
;;^gbCt* is for comment specifiec tags
;;^gbAr* is for blog item author specific tags


(defconst nphtml--builtin-gblogger-args
  (let ((blogUrl  "<$BlogURL$>") (blogTitle "<$BlogTitle$>")
	(blogDesc "<$BlogDescription$>")
	(archUrls "<$BlogArchiveURL$>") (archNames "<$BlogArchiveName$>")
	;;perhaps need blogger > gbArchive tags (´・ω・｀)
	(prevTitle "<$BlogPreviousItemTitle$>")
	(siteFeedLink "<$BlogSiteFeedLink$>") (siteFeedUrl "<$BlogSiteFeedUrl$>")
	;;one of them is exported as an inteface but what's the hell the differece of them?(´・ω・｀)
	(encoding "<$BlogEncoding$>")
	(pageTitle "<$BlogPageTitle$>")
	

	;;owner relative strings
	(ownerPhotoUrl "<$BlogOwnerPhotoUrl$>") (ownerProfileUrl "<$BlogOwnerProfileUrl$>")
	(ownerLocation "<$BlogOwnerLocation$>") (ownerAboutMe    "<$BlogOwnerAboutMe$>")
	(ownerEmail "<$BlogOwnerEmail$>")
	(ownerFull "<$BlogOwnerFullName$>")
	(ownerFirst "<$BlogOwnerFirstName$>") (ownerlast "<$BlogOwnerLastName$>")
	(ownerNickname "<$BlogOwnerNickname$>")
	;;item relative strings
	(itemBody  "<$BlogItemBody$>") (itemUrl "<$BlogItemPermalinkURL$>")
	(itemDate  "<$BlogItemDateTime$>") (itemNumber "<$BlogItemNumber$>");these works well(´・ω・｀)
	(itemTitle "<$BlogItemTitle$>")
	(itemArchFile "<$BlogItemArchiveFileName$>")
	(itemAuthor "<$BlogItemAuthor$>")
	(itemAuthorUrl "<$BlogItemAuthorURL$>")
	(itemAuthorNickname "<$BlogItemAuthorNickname$>")
	(itemAuthorEmail "<$BlogItemAuthorEmail$>")
	(itemControl     "<$BlogItemControl$>")

	;;comment relative strings
	(comntNum   "<$BlogCommentNumber$>") (comntBody "<$BlogCommentBody$>")
	(comntEachLink "<$BlogCommentPermalinkURL$>")
	(comntId   "<$BlogCommentAnchorName$>");used as #id maybe unique number generator?(´・ω・｀)
	(comntAuthor "<$BlogCommentAuthor$>") (comntDate "<$BlogCommentDateTime$>")
	(comntDelUrl "<$BlogCommentDeleteIcon$>")
	(comntCreUrl "<$BlogItemCommentCreate$>")
	)
    ;;crucially important tags
    `((:alias gbBlogger :type blocky :tag Blogger)
      ;;^gbBg*
      (:alias gbBgtitle :type void :tag ,(string-trim blogTitle "<" ">"))
      (:alias gbBgdisc  :type void :tag ,(string-trim blogDesc  "<" ">"))
      (:alias gbBgfeedlink :type void :tag ,(string-trim siteFeedLink "<" ">"))
      ;;^gbCn*
      (:type blocky :tag ItemPage                :alias gbCnitempage)
      (:type blocky :tag MainPage                :alias gbCnmainpage)
      (:type blocky :tag ArchivePage             :alias gbCnarchivepage)
      (:type blocky :tag MainOrArchivePage       :alias gbCnmainorarch)
      (:type blocky :tag BlogItemCommentsEnabled :alias gbCncommentenabled)
      (:type blocky :tag BlogSiteFeed            :alias gbCnsitefeed)

      (:type blocky :tag BlogItemURL             :alias gbCnhasurl)
      (:type blocky :tag BlogItemTitle           :alias gbCnhastitle);i cant understand these guys(´・ω・｀)

      (:type blocky :tag BlogDateHeader          :alias gbCndateheader)
      (:type blocky :tag BlogDateFooter          :alias gbCndatefooter)
      ;;^gbCt*
      (:type blocky :tag Blogger                          :alias gbCtblogger
	     :nest ((:tag BlogItemComments :type blocky)))
      (:type void :tag <$BlogItemCommentCount$>           :alias gbCtnum);should be gbIm?(´・ω・｀)
      (:type void :tag ,(string-trim comntAuthor "<" ">") :alias gbCtauthor)
      (:type void :tag ,(string-trim comntDate "<" ">")   :alias gbCtdate)
      (:type void :tag ,(string-trim comntBody "<" ">")   :alias gbCtbody)
      (:type inline :tag a
	     :attrs (href ,comntEachLink)                 :alias gbCteachlink)
      ;;what's the hell this one?(´・ω・｀)
      (:type inline :tag a
             :attrs (href ,comntCreUrl)                   :alias gbCtcreatelink)
      (:type inline :tag a
             :attrs (href ,comntDelUrl)                   :alias gbCtdeletelink)

      
      ;;means if it is head or tail monthly archive of the blog...perhaps(´・ω・｀)
      ;;^gbIm*  theses are needed to be nested inside <blogger> tags
      (:alias gbImtitle :type inline :tag BlogItemTitle  :forcing t
	      :nest ((:tag ,(string-trim itemTitle "<" ">") :type void)))
      (:alias gbImdate :type void :tag ,(string-trim itemDate "<" ">"))
      (:alias gbImlink :type inline :tag BlogItemURL     :forcing t
	      :nest ((:tag a :type inline :attrs (href ,itemUrl))))
      (:alias gbImbody :type void  :tag ,(string-trim itemBody "<" ">"))


      
      (:type blocky :tag BloggerArchives :alias gbArchive-internal :hidden t
	     :nest ((:tag li :type inline
			  :nest ((:tag a :attrs (href ,itemUrl) :para ,itemTitle :type inline)))))
      (:type blocky :tag Blogger :alias gbAv
	     :nest (gbArchive-internal))
      (:type blocky :tag BloggerPreviousItems :alias gbPrevs-internal :hidden t
	     :nest ((:tag li :type inline
			  :nest ((:tag a :attrs (href ,itemUrl) :para ,prevTitle :type inline)))))
      (:type blocky :tag Blogger :alias gbPrevs
	     :nest (gbPrevs-internal))
      
      

      
      (:type void   :tag $BlogMetaData$                           :hidden t)
      (:type blocky :tag html :attrs (lang ,nphtml-lang-attr) :alias gbhtml
	     :prepend (!DOCTYPE dateComment)
	     :nest (gbhead gbbody))
      (:type blocky :tag head :alias gbhead :hidden t
	     :nest ((:tag title :type inline :para ,pageTitle)
		    favlink chmeta vpmeta 
		    (:tag $BlogMetaData$ :type void);should be commented(´・ω・｀))
		    script style))
      (:type blocky :tag body :alias gbbody :hidden t
	     :nest (header gbmain footer))
      (:type blocky :tag main :alias gbmain :hidden t
	     :nest (gbCnmainpage gbCnarchivepage gbCnitempage)))))


(provide 'nphtml-args-builtin-gblogger)
