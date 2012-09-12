;; tumblesocks-view.el -- Provide an interface to view tumblr blog posts.

(require 'tumblesocks-api)
(require 'tumblesocks-compose)
(require 'shr)
(provide 'tumblesocks-view)

(defcustom tumblesocks-posts-per-page 20
  "How many posts per page to show"
  :type 'number
  :group 'tumblesocks)

(defcustom tumblesocks-desired-image-size 400
  "The desired width of images to display in post listings.

If an alternative image exists that's this many pixels wide, we
will show it, otherwise we will show the original size image. Use
0 to always show the full-isze original image.

This both 1. saves bandwith, 2. keeps Tumblesocks from clogging
up your emacs with too many slow network connections to download
all those huge images"
  :type 'number
  :group 'tumblesocks)

(defcustom tumblesocks-show-full-images-in-post t
  "Whether to show full-sized images when viewing individual posts.

This causes Tumblesocks to ignore the setting of
`tumblesocks-desired-image-size' when viewing posts."
  :type 'boolean
  :group 'tumblesocks)

(defvar tumblesocks-view-mode-map
  (let ((tumblesocks-view-mode-map (make-keymap)))
    (define-key tumblesocks-view-mode-map "q" 'quit-window)
    (define-key tumblesocks-view-mode-map "p" 'tumblesocks-view-previous-post)
    (define-key tumblesocks-view-mode-map "n" 'tumblesocks-view-next-post)
    (define-key tumblesocks-view-mode-map "c" 'tumblesocks-view-compose-new-post)
    (define-key tumblesocks-view-mode-map "g" 'tumblesocks-view-refresh)
    (define-key tumblesocks-view-mode-map "s" 'tumblesocks-view-posts-tagged)
    (define-key tumblesocks-view-mode-map "r" 'tumblesocks-view-reblog-post-at-point)
    (define-key tumblesocks-view-mode-map (kbd "RET") 'tumblesocks-view-post-at-point)
    (define-key tumblesocks-view-mode-map (kbd "SPC") 'forward-page)
    (define-key tumblesocks-view-mode-map "b" 'tumblesocks-view-blog)
    (define-key tumblesocks-view-mode-map "d" 'tumblesocks-view-delete-post-at-point)
    (define-key tumblesocks-view-mode-map "e" 'tumblesocks-view-edit-post-at-point)
    (define-key tumblesocks-view-mode-map "f" 'tumblesocks-view-follow-blog-at-point)
    (define-key tumblesocks-view-mode-map "l" 'tumblesocks-view-like-post-at-point)
    (define-key tumblesocks-view-mode-map "o" 'tumblesocks-view-post-url-at-point)
    (define-key tumblesocks-view-mode-map "y" 'tumblesocks-view-yank-post-url-at-point)
    tumblesocks-view-mode-map))

(defun tumblesocks-view-compose-new-post ()
  "Like `tumblesocks-compose-new-post', but refresh the view when we're done."
  (interactive)
  (tumblesocks-compose-new-post 'tumblesocks-view-refresh))

(defun tumblesocks-view-previous-post ()
  "Go to the previous post or the previous page if we're at the beginning."
  (interactive)
  (cond
   ((get-text-property (point) 'tumblesocks-post-data)
    (goto-char (previous-single-property-change (point) 'tumblesocks-post-data
                                                nil
                                                tumblesocks-view-content-start)))
   ((and (button-at (point))
         (eq 'forward (button-get (button-at (point)) 'tumblesocks-direction)))
    (goto-char (previous-single-property-change (point) 'tumblesocks-post-data
                                                nil
                                                tumblesocks-view-content-start)))
   ((and (button-at (point))
         (eq 'back (button-get (button-at (point)) 'tumblesocks-direction)))
    (button-activate (button-at (point))))
   (t (previous-line))))

(defun tumblesocks-view-next-post ()
  "Go to the next post or the next page if we're at the end."
  (interactive)
  (cond
   ((get-text-property (point) 'tumblesocks-post-data)
    (goto-char (next-single-property-change (point) 'tumblesocks-post-data
                                            nil (- (point-max) 1))))
   ((and (button-at (point))
         (eq 'forward (button-get (button-at (point)) 'tumblesocks-direction)))
    (button-activate (button-at (point))))
   ((and (button-at (point))
         (eq 'back (button-get (button-at (point)) 'tumblesocks-direction)))
    (goto-char (next-single-property-change (point) 'tumblesocks-post-data
                                            nil (- (point-max) 1))))
   (t (next-line))))

(defvar tumblesocks-view-refresh-action nil)

(defun tumblesocks-view-refresh ()
  "Refresh our view of posts (download new ones)"
  (interactive)
  (when tumblesocks-view-refresh-action
    (funcall tumblesocks-view-refresh-action)))

(defun tumblesocks-view-post-at-point ()
  "Open the post under point in a new buffer, showing notes, etc"
  (interactive)
  (when (get-text-property (point) 'tumblesocks-post-data)
    (tumblesocks-view-post
     (plist-get (get-text-property (point) 'tumblesocks-post-data) :id))))

(defun tumblesocks-view-post-url-at-point ()
  "Open the post under point in your browser"
  (interactive)
  (when (get-text-property (point) 'tumblesocks-post-data)
    (let ((post_url (plist-get (get-text-property (point) 'tumblesocks-post-data)
                               :post_url)))
      (browse-url post_url)
      (message (concat "Opening " post_url " in your browser...")))))

(defun tumblesocks-view-yank-post-url-at-point ()
  "Copy the URL of the post under point to the kill ring"
  (interactive)
  (when (get-text-property (point) 'tumblesocks-post-data)
    (let ((post_url (plist-get (get-text-property (point) 'tumblesocks-post-data)
                                :post_url)))
      (kill-new post_url)
      (message (concat "Yanked " post_url)))))

(defun tumblesocks-view-follow-blog-at-point (follow-p)
  "Follow the blog at point. With prefix arg, UNfollow the blog at point."
  (interactive "P")
  (when (get-text-property (point) 'tumblesocks-post-data)
    (let ((blog-name (plist-get (get-text-property (point) 'tumblesocks-post-data)
                                :blog_name)))
      (if (not follow-p)
          (when (yes-or-no-p (concat "Really follow "
                                     (concat blog-name ".tumblr.com")
                                     "?"))
            (tumblesocks-follow-blog (concat blog-name ".tumblr.com")))
        (when (yes-or-no-p (concat "Really UN-follow "
                                   (concat blog-name ".tumblr.com")
                                   "?"))
          (tumblesocks-unfollow-blog (concat blog-name ".tumblr.com")))))))

(defun tumblesocks-view-delete-post-at-point ()
  (interactive)
  (when (yes-or-no-p "Really try to delete this post? ")
    (tumblesocks-api-delete-post
     (format "%d"
             (plist-get (get-text-property (point) 'tumblesocks-post-data) :id)))
    (message "Post deleted.")
    (let ((pos (point)))
      (tumblesocks-view-refresh)
      (goto-char pos))))

(defun tumblesocks-view-edit-post-at-point ()
  (interactive)
  (when (yes-or-no-p "Really try to edit this post? ")
    (tumblesocks-compose-edit-post
     (format "%d"
             (plist-get (get-text-property (point) 'tumblesocks-post-data) :id)))
    '(lambda ()
       (let ((pos (point)))
         (tumblesocks-view-refresh)
         (goto-char pos)))))

(defun tumblesocks-view-reblog-post-at-point ()
  "Reblog the post at point, if there is one."
  (interactive)
  (when (get-text-property (point) 'tumblesocks-post-data)
    ;; Get the reblog key.
    (let* ((post_id
            (format "%d"
                    (plist-get
                     (get-text-property (point) 'tumblesocks-post-data) :id)))
           ;; we need to do another API fetch because
           ;; tumblesocks-post-data doesn't have reblog keys, by design
           (blog (tumblesocks-api-blog-posts
                  nil post_id nil "1" nil "true" nil "html"))
           (post (car (plist-get blog :posts)))
           (reblog_key (plist-get post :reblog_key)))
      (tumblesocks-api-reblog-post
       post_id reblog_key
       (read-string "(Optional) comments to add: "))
      (message "Reblogged.")
      (tumblesocks-view-refresh))))



(defvar tumblesocks-view-current-offset 0)
(defvar tumblesocks-view-content-start nil)

(define-derived-mode tumblesocks-view-mode fundamental-mode "Tumblr"
  "Major mode for reading Tumblr blogs and posts.

\\{tumblesocks-view-mode-map}"
  (make-local-variable 'tumblesocks-view-refresh-action)
  (make-local-variable 'tumblesocks-view-current-offset)
  (make-local-variable 'tumblesocks-view-content-start)
  ;;(visual-line-mode t) ;shr.el takes care of this...
  )

(defun tumblesocks-view-insert-parsed-html-fragment (html-frag-parsed &optional inline)
  "Renders and inserts an HTML sexp. If inline is t, then <p> tags will have no effect."
  (let ((shr-width nil))
    (if inline
        (flet ((shr-ensure-paragraph () 0))
          ;; disable newlines, for now ...
          (condition-case nil
              ;; this must go in the flet, sorry!
              (shr-insert-document html-frag-parsed)
            (error (message "Couldn't insert HTML."))))
      (condition-case nil
          (shr-insert-document html-frag-parsed)
        (error (message "Couldn't insert HTML."))))))
(defun tumblesocks-view-insert-html-fragment (html-fragment &optional inline)
  "Renders and inserts an HTML fragment. If inline is t, then <p> tags will have no effect."
  (let (html-frag-parsed)
    (with-temp-buffer
      (insert html-fragment)
      (setq html-frag-parsed (libxml-parse-html-region (point-min) (point-max))))
    (tumblesocks-view-insert-parsed-html-fragment html-frag-parsed inline)))


(defun tumblesocks-view-insert-prevpage-button ()
  (insert-button "[<< Previous Page...]"
                 'action 'tumblesocks-view-previous-page-button-action
                 'tumblesocks-direction 'back)
  (let ((start (point)))
    (insert (format "\nPage %d:"
                    (1+ (floor (/ tumblesocks-view-current-offset
                                  tumblesocks-posts-per-page)))))
    (put-text-property start (point) 'face font-lock-comment-face))
  (insert "\n\n"))
(defun tumblesocks-view-insert-nextpage-button ()
  (insert-button "[Next Page... >>]"
                 'action 'tumblesocks-view-next-page-button-action
                 'tumblesocks-direction 'forward))

(defun tumblesocks-view-previous-page-button-action (button)
  (tumblesocks-view-previous-page))
(defun tumblesocks-view-previous-page ()
  "Go back a page (into younger posts)

We show `tumblesocks-posts-per-page' posts per page."
  (interactive)
  (setq tumblesocks-view-current-offset
        (max
         (- tumblesocks-view-current-offset tumblesocks-posts-per-page)
         0))
  (tumblesocks-view-refresh)
  (goto-char (point-max))
  (previous-line)
  (tumblesocks-view-previous-post))
(defun tumblesocks-view-next-page-button-action (button)
  (tumblesocks-view-next-page))
(defun tumblesocks-view-next-page ()
  "Go forward a page (into older posts)

We show `tumblesocks-posts-per-page' posts per page."
  (interactive)
  (setq tumblesocks-view-current-offset
        (+ tumblesocks-view-current-offset tumblesocks-posts-per-page))
  (tumblesocks-view-refresh))

(defun tumblesocks-view-render-blogdata (blogdata total-posts)
  "Render blogdata into the current buffer.

Blogdata should be the JSON result of a call to Tumblr's
/blog/posts or /user/dashboard API. (We expect each post in
blogdata to be filtered with the 'text' filter.)"
  ;; See http://www.tumblr.com/docs/en/api/v2#posts for more
  ;; info about the post API.
  (setq tumblesocks-view-content-start (point-marker))
  (when (> tumblesocks-view-current-offset 0)
    (tumblesocks-view-insert-prevpage-button))
  (if (> (length blogdata) 0)
      (progn
        (dolist (post blogdata)
          (tumblesocks-view-render-post post))
        ;; Pagination button anyone?
        (if (> total-posts (+ tumblesocks-view-current-offset
                              (length blogdata)))
            (tumblesocks-view-insert-nextpage-button)))
    (let ((start (point)))
      (insert "No posts.\n")
      (put-text-property start (point) 'face font-lock-comment-face))))

;; thanks to jlf who wrote this function: http://paste.lisp.org/display/131689
(defmacro tumblesocks-bind-plist-keys (plist key-vars &rest body)
  "Bind each KEY to its associated value in PLIST and execute BODY."
  (let ((temp (make-symbol "--cl-var--")))
    `(let* ,(cons (list temp plist)
                  (mapcar #'(lambda (v)
                              (list
                               v
                               `(plist-get ,temp
                                           ,(intern (concat ":" (symbol-name v))))))
                          key-vars))
	   . ,body)))

(defun tumblesocks-view-render-post (post &optional verbose-header)
  "Render the post into the current buffer.

This function internally dispatches to other functions that are
better suited to inserting each post."
  (tumblesocks-bind-plist-keys post
    (blog_name id post_url type date reblog_key tags liked note_count liked
               ;; For photo posts:
               photos caption width
               ;; For quote posts:
               text source
               ;; For link posts:
               title url description
               ;; For chat posts:
               body dialogue
               ;; For answer posts:
               asking_name asking_url question answer)
    (let ((begin-post-area (point)))
      (tumblesocks-view-insert-header verbose-header)
      (cond
       ((string= type "text") (tumblesocks-view-insert-text))
       ((string= type "quote") (tumblesocks-view-insert-quote))
       ((string= type "link") (tumblesocks-view-insert-link))
       ((string= type "answer") (tumblesocks-view-insert-answer))
       ;;((string= type "video") (tumblesocks-view-insert-video))
       ;;((string= type "audio") (tumblesocks-view-insert-audio))
       ((string= type "photo") (tumblesocks-view-insert-photo))
       ((string= type "chat") (tumblesocks-view-insert-chat))
       (t (tumblesocks-view-insert-i-have-no-clue-what-this-is)))
      (insert "\n")
      ;; Record this post data so we know how to read it next
      (put-text-property begin-post-area (point)
                         'tumblesocks-post-data
                         post))))

(defun tumblesocks-view-insert-header (&optional verbose)
  "Draw the header for the current post, optionally being verbose."
  (let (begin end_bname)
    (setq begin (point))
    (insert blog_name ":")
    (setq end_bname (point))
    ;; Title
    (insert " ")
    (cond
     (title (tumblesocks-view-insert-html-fragment title t))
     (caption (tumblesocks-view-insert-html-fragment caption t))
     (question (tumblesocks-view-insert-html-fragment question t))
     (t (insert " ")))
    ;; Notes
    (when (and note_count (> note_count 0))
      (insert " (" (format "%d" note_count) " note"
              (if (= 1 note_count) "" "s") ")"))
    (when liked
      (insert " (Liked)"))
    (insert "\n")
    (when verbose
      (insert
       "Date: " date
       "\nTags: " (mapconcat '(lambda (x) (concat "#" x)) tags ", ")
       "\nPermalink: ")
      (tumblesocks-view-insert-parsed-html-fragment
       `(a ((href . ,post_url)) ,post_url) t)
      (insert "\n"))
    (put-text-property begin end_bname 'face
                       (list '(:inverse-video t)
                             '(:weight bold)
                             font-lock-keyword-face))
    (put-text-property end_bname (point) 'face
                       (list '(:weight bold)
                             'highlight))))

(defun tumblesocks-view-insert-text ()
  (tumblesocks-view-insert-html-fragment body)
  (insert "\n"))

(defun tumblesocks-view-insert-photo ()
  (let ((photo-html-frag
         `(p () .
             ,(apply 'append
                     (mapcar
                      '(lambda (photodata)
                         ;; There could be several photos here, and
                         ;; each photo has several alternative sizes.
                         ;; The first is usually the biggest, the
                         ;; third is a good compromise
                         (let* ((alts (plist-get photodata :alt_sizes))
                                (desired-size-alts
                                 (delq nil
                                  (mapcar '(lambda(alt)
                                             (and (= (plist-get alt :width)
                                                     tumblesocks-desired-image-size)
                                                  alt))
                                          alts)))
                                (alt (car (or desired-size-alts alts))))
                           (list `(img ((src . ,(plist-get alt :url))))
                                 ;;`(br)
                                 (plist-get photodata :caption))))
                      photos)))))
    (tumblesocks-view-insert-parsed-html-fragment photo-html-frag)
    (when caption
      (tumblesocks-view-insert-html-fragment caption))
    (insert "\n")))

(defun tumblesocks-view-insert-quote ()
  (tumblesocks-view-insert-html-fragment text t)
  (insert "\n")
  (tumblesocks-view-insert-html-fragment source t)
  (insert "\n"))

(defun tumblesocks-view-insert-answer ()
  (insert asking_name " asks: \n  ")
  (let ((start (point))
        (shr-indentation 4))
    (tumblesocks-view-insert-html-fragment question t)
    (put-text-property start (point) 'face font-lock-comment-face))
  (tumblesocks-view-insert-html-fragment answer))

(defun tumblesocks-view-insert-link ()
  (tumblesocks-view-insert-parsed-html-fragment `(a ((href . ,url)) ,url) t)
  (insert "\n")
  (tumblesocks-view-insert-html-fragment description)
  (insert "\n"))

(defun tumblesocks-view-insert-chat ()
  (dolist (message dialogue)
    (let ((start (point)))
      (tumblesocks-view-insert-html-fragment (plist-get message :label) t)
      (put-text-property start (point) 'face '(:weight bold))
      (insert " ")
      (tumblesocks-view-insert-html-fragment (plist-get message :phrase) t)
      (insert "\n"))))

(defun tumblesocks-view-insert-i-have-no-clue-what-this-is ()
  (let ((begin (point)))
    (insert "this is a " (format "%S" type) " post\n")
    (put-text-property begin (point) 'face font-lock-comment-face)))



(defun tumblesocks-view-prepare-buffer (blogtitle &optional preserve-page-offset)
  "Create a new buffer to begin viewing a blog."
  (pop-to-buffer-same-window (concat "*Tumblr: " blogtitle "*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  ;; We must save the current pagination offset;
  ;; tumblesocks-refresh-view is called when we move through pages.
  (let ((offset tumblesocks-view-current-offset))
    (tumblesocks-view-mode)
    (when preserve-page-offset
      (setq tumblesocks-view-current-offset offset))))
(defun tumblesocks-view-finishrender ()
  "Finish creating the blog buffer, ready to present to the user"
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (goto-char (or tumblesocks-view-content-start (point-min))))

(defun tumblesocks-view-blog (blogname &optional preserve-page-offset)
  "View the given blog (URL or name)"
  (interactive
   (list (read-string
          "Blog to view: "
          (if (get-text-property (point) 'tumblesocks-post-data)
              (concat
               (plist-get (get-text-property (point) 'tumblesocks-post-data)
                          :blog_name)
               ".tumblr.com")
            ""))))
  (let* ((tumblesocks-blog blogname) ; dynamic binding the blog!
         (blog-info (plist-get (tumblesocks-api-blog-info) :blog))
         (returned-data (tumblesocks-api-blog-posts
                         nil nil nil tumblesocks-posts-per-page
                         tumblesocks-view-current-offset nil nil "html")))
    (tumblesocks-view-prepare-buffer
     (plist-get blog-info :title)
     preserve-page-offset)
    ;; Draw blog info
    (let ((begin (point)))
      (tumblesocks-view-insert-parsed-html-fragment
       `(img ((src . ,(tumblesocks-api-avatar-url)))) t)
      (insert (plist-get blog-info :title) " - "
              (plist-get blog-info :url))
      (insert (format "\n%d post%s"
                      (plist-get blog-info :posts)
                      (if (= 1 (plist-get blog-info :posts)) "" "s")))
      (when (plist-get blog-info :likes)
        (insert (format ", %d like%s"
                        (plist-get blog-info :likes)
                        (if (= 1 (plist-get blog-info :likes)) "" "s"))))
      (insert "\n\n")
      (put-text-property begin (point) 'face font-lock-comment-face))
    (tumblesocks-view-render-blogdata
     (plist-get returned-data :posts)
     (plist-get returned-data :total_posts))
    (tumblesocks-view-finishrender)
    (setq tumblesocks-view-refresh-action
          `(lambda () (tumblesocks-view-blog ,blogname t))))) ; <-- CLOSURE HACK :p

;;;###autoload
(defun tumblesocks-view-dashboard (&optional preserve-page-offset)
  "View the posts on your dashboard.

You can browse around, edit, and delete posts from here.

\\{tumblesocks-view-mode-map}"
  (interactive)
  (tumblesocks-view-prepare-buffer "Dashboard" preserve-page-offset)
  (let ((dashboard-data (tumblesocks-api-user-dashboard
                         tumblesocks-posts-per-page
                         tumblesocks-view-current-offset nil nil nil nil)))
    (let ((begin (point)))
      (insert "Dashboard")
      (center-line)
      (insert "\n\n")
      (put-text-property begin (point) 'face font-lock-comment-face))
    (tumblesocks-view-render-blogdata
     (plist-get dashboard-data :posts)
     99999) ; allow them to browse practically infinite posts
    (tumblesocks-view-finishrender)
    (setq tumblesocks-view-refresh-action
          '(lambda () (tumblesocks-view-dashboard t)))))

(defun tumblesocks-view-post (post_id)
  "View a post in its own dedicated buffer, with notes"
  (interactive "sPost ID: ")
  (unless (stringp post_id)
    (setq post_id (format "%d" post_id)))
  (let* ((blog (tumblesocks-api-blog-posts
                nil post_id nil "1" nil nil "true" "html"))
         (post (car (plist-get blog :posts)))
         (notes (plist-get post :notes)))
    (tumblesocks-view-prepare-buffer
     (format "Viewing post %s: %s"
             (plist-get post :blog_name)
             post_id))
    (setq tumblesocks-view-content-start (point-marker))
    (if tumblesocks-show-full-images-in-post
        (let ((tumblesocks-desired-image-size 0))
          (tumblesocks-view-render-post post t))
      (tumblesocks-view-render-post post t))
    (tumblesocks-view-render-notes notes)
    (tumblesocks-view-finishrender)
    (setq tumblesocks-view-refresh-action
          `(lambda () (tumblesocks-view-post ,post_id)))))

(defun tumblesocks-view-render-notes (notes)
  "Render the given notes into the current buffer."
  (let ((start (point)))
    (flet ((comment-that ()
              (put-text-property start (point) 'face font-lock-comment-face)
              (setq start (point)))
           (bold-that ()
              (put-text-property start (point) 'face
                                 (cons '(:weight bold) font-lock-comment-face))
              (setq start (point))))
      (insert "-- Notes:\n")
      (comment-that)
      (dolist (note notes)
        (tumblesocks-bind-plist-keys note
           (type post_id blog_name blog_url reply_text answer_text added_text)
           (cond ((string= type "posted")
                  (insert blog_name " posted this"))
                 ((string= type "answer")
                  (insert blog_name " answers:\n  ")
                  (comment-that)
                  (tumblesocks-view-insert-html-fragment answer_text t)
                  (bold-that))
                 ((string= type "reblog")
                  (insert blog_name " reblogged this on " blog_url))
                 ((string= type "like")
                  (insert blog_name " liked this"))
                 ((string= type "reply")
                  (insert blog_name " says: ")
                  (comment-that)
                  (tumblesocks-view-insert-html-fragment reply_text t)
                  (bold-that))
                 (t (insert (format "%S" note))))
           (when added_text
             (insert "\n  ")
             (comment-that)
             (insert added_text)
             (bold-that))
           (insert "\n")
           (comment-that))))))

(defun tumblesocks-view-like-post-at-point (like-p)
  "Like the post underneath point. With prefix arg (C-u), unlike it."
  (interactive "P")
  (when (get-text-property (point) 'tumblesocks-post-data)
    ;; Get the reblog key.
    (let* ((post_id
            (format "%d" (plist-get
                          (get-text-property (point) 'tumblesocks-post-data)
                          :id)))
           (blog (tumblesocks-api-blog-posts
                  nil post_id nil "1" nil "true" nil "html"))
           (post (car (plist-get blog :posts)))
           (reblog_key (plist-get post :reblog_key)))
      (if (not like-p)
          (progn
            (tumblesocks-api-user-like post_id reblog_key)
            (message "Liked this post."))
        (tumblesocks-api-user-unlike post_id reblog_key)
        (message "Unliked this post."))
      (let ((pos (point)))
        (tumblesocks-view-refresh)
        (goto-char pos)))))

(defun tumblesocks-view-posts-tagged (tag)
  "Search for posts with the given tag."
  (interactive "sSearch for posts with tag: ")
  (tumblesocks-view-prepare-buffer
   (concat "Tag search: " tag))
  (tumblesocks-view-render-blogdata
   (tumblesocks-api-tagged tag nil nil "html")
   0) ; don't allow them to browse next (this isn't possible in general anyways)
  (tumblesocks-view-finishrender)
  (setq tumblesocks-view-refresh-action
        `(lambda () (tumblesocks-view-posts-tagged ,tag))))
