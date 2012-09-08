;; tumblesocks-view.el -- Provide an interface to view tumblr blog posts.

(require 'tumblesocks-api)
(require 'tumblesocks-compose)
(require 'shr)
(provide 'tumblesocks-view)

(defvar tumblesocks-view-mode-map
  (let ((tumblesocks-view-mode-map (make-keymap)))
    (define-key tumblesocks-view-mode-map "q" 'quit-window)
    (define-key tumblesocks-view-mode-map "n" 'tumblesocks-view-next-post)
    (define-key tumblesocks-view-mode-map "c" 'tumblesocks-compose-new-post)
    (define-key tumblesocks-view-mode-map "g" 'tumblesocks-view-refresh)
    (define-key tumblesocks-view-mode-map "p" 'tumblesocks-view-previous-post)
    (define-key tumblesocks-view-mode-map (kbd "RET") 'tumblesocks-view-post-at-point)
    (define-key tumblesocks-view-mode-map "b" 'tumblesocks-view-blog-at-point)
    (define-key tumblesocks-view-mode-map "d" 'tumblesocks-view-delete-post-at-point)
    (define-key tumblesocks-view-mode-map "e" 'tumblesocks-view-edit-post-at-point)
    (define-key tumblesocks-view-mode-map "f" 'tumblesocks-view-follow-blog-at-point)
    (define-key tumblesocks-view-mode-map "l" 'tumblesocks-view-like-post-at-point)
    tumblesocks-view-mode-map))

(defun tumblesocks-view-previous-post ()
  (interactive)
  (when (get-text-property (point) 'tumblesocks-post-data)
    (goto-char (previous-single-property-change (point) 'tumblesocks-post-data
                                            nil (point-min)))))

(defun tumblesocks-view-next-post ()
  (interactive)
  (when (get-text-property (point) 'tumblesocks-post-data)
    (goto-char (next-single-property-change (point) 'tumblesocks-post-data
                                            nil (- (point-max) 1)))))

(defvar tumblesocks-view-refresh-action nil)

(defun tumblesocks-view-refresh ()
  (interactive)
  (when tumblesocks-view-refresh-action
    (funcall tumblesocks-view-refresh-action)
    (message "View refreshed.")))

(defun tumblesocks-view-post-at-point ()
  (interactive)
  (when (get-text-property (point) 'tumblesocks-post-data)
    (tumblesocks-view-post
     (cdr (assq 'id (get-text-property (point) 'tumblesocks-post-data))))))

(defun tumblesocks-view-blog-at-point ()
  (interactive)
  (when (get-text-property (point) 'tumblesocks-post-data)
    (let ((blog-name (cdr (assq 'blog_name (get-text-property (point) 'tumblesocks-post-data)))))
      (tumblesocks-view-blog (concat blog-name ".tumblr.com")))))

(defun tumblesocks-view-follow-blog-at-point (follow-p)
  "Follow the blog at point. With prefix arg, UNfollow the blog at point."
  (interactive "P")
  (when (get-text-property (point) 'tumblesocks-post-data)
    (let ((blog-name (cdr (assq 'blog_name (get-text-property (point) 'tumblesocks-post-data)))))
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
     (cdr (assq 'id (get-text-property (point) 'tumblesocks-post-data))))
    (message "Post deleted.")))

(defun tumblesocks-view-edit-post-at-point ()
  (interactive)
  (when (yes-or-no-p "Really try to edit this post? ")
    (tumblesocks-compose-edit-post
     (cdr (assq 'id (get-text-property (point) 'tumblesocks-post-data))))))



(define-derived-mode tumblesocks-view-mode fundamental-mode "Tumblr"
  "Major mode for reading Tumblr blogs."
  (make-local-variable 'tumblesocks-view-refresh-action)
  ;;(visual-line-mode t) ;shr.el takes care of this...
)

(defun tumblesocks-view-insert-parsed-html-fragment (html-frag-parsed &optional inline)
  "Renders and inserts an HTML sexp. If inline is t, then <p> tags will have no effect."
  (let ((shr-width nil))
    (if inline
        (flet ((shr-ensure-paragraph () 0))
          ;; disable newlines, for now ...
          (shr-insert-document html-frag-parsed))
      (shr-insert-document html-frag-parsed))))
(defun tumblesocks-view-insert-html-fragment (html-fragment &optional inline)
  "Renders and inserts an HTML fragment. If inline is t, then <p> tags will have no effect."
  (let (html-frag-parsed)
    (with-temp-buffer
      (insert html-fragment)
      (setq html-frag-parsed (libxml-parse-html-region (point-min) (point-max))))
    (tumblesocks-view-insert-parsed-html-fragment html-frag-parsed inline)))


(defun tumblesocks-view-render-blogdata (blogdata)
  "Render blogdata into the current buffer.

Blogdata should be the JSON result of a call to Tumblr's
/blog/posts or /user/dashboard API. (We expect each post in
blogdata to be filtered with the 'text' filter.)

This function internally dispatches to other functions that are better suited to inserting each post."
  ; See http://www.tumblr.com/docs/en/api/v2#posts for more
  ; info about the post API.
  (dolist (post (append blogdata nil))
    (tumblesocks-view-render-post post)))

(defun tumblesocks-view-render-post (post &optional verbose-header)
  "Render the post into the current buffer."
  (let ((blog_name (cdr-safe (assq 'blog_name post)))
        (id (cdr-safe (assq 'id post)))
        (post_url (cdr-safe (assq 'post_url post)))
        (type (cdr-safe (assq 'type post)))
        (date (cdr-safe (assq 'date post)))
        (reblog_key (cdr-safe (assq 'reblog_key post)))
        (tags (cdr-safe (assq 'tags post)))
        (liked (cdr-safe (assq 'liked post)))
        (note_count (cdr-safe (assq 'note_count post)))
        ;; For photo posts:
        (photos (cdr-safe (assq 'photos post)))
        (caption (cdr-safe (assq 'caption post)))
        (width (cdr-safe (assq 'width post)))
        (height (cdr-safe (assq 'height post)))
        ;; For quote posts:
        (text (cdr-safe (assq 'text post)))
        (source (cdr-safe (assq 'source post)))
        ;; For link posts:
        (title (cdr-safe (assq 'title post)))
        (url (cdr-safe (assq 'url post)))
        (description (cdr-safe (assq 'description post)))
        ;; For chat posts:
        (body (cdr-safe (assq 'body post)))
        (dialogue (cdr-safe (assq 'dialogue post)))
        ;; For answer posts:
        (asking_name (cdr-safe (assq 'asking_name post)))
        (asking_url (cdr-safe (assq 'asking_url post)))
        (question (cdr-safe (assq 'question post)))
        (answer (cdr-safe (assq 'answer post))))
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
    (insert "\n")
    (when verbose
      (insert
            "Date: "date "\nTags: " (mapconcat '(lambda (x) (concat "#" x)) tags ", ")
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
                      '(lambda (photdata)
                         ;; There could be several photos here, and
                         ;; each photo has several alternative sizes.
                         ;; The first is usually the biggest, the
                         ;; third is a good compromise
                         (let* ((alts (cdr (assq 'alt_sizes photdata)))
                                (alt (elt alts (if (> (length alts) 2)
                                                   2
                                                 0))))
                           (list `(img ((src . ,(cdr (assq 'url alt)))))
                                 (cdr (assq 'caption photdata)))))
                      photos)))))
    (tumblesocks-view-insert-parsed-html-fragment photo-html-frag)
    (insert "\n")))

(defun tumblesocks-view-insert-quote ()
  (tumblesocks-view-insert-html-fragment source t))

(defun tumblesocks-view-insert-link ()
  (tumblesocks-view-insert-parsed-html-fragment
   `(a ((href . ,url))
       ,(if (> (length description) 0) description url))))

(defun tumblesocks-view-insert-chat ()
  (dolist (message (append dialogue nil))
    (let ((start (point)))
      (tumblesocks-view-insert-html-fragment (cdr (assq 'label message)) t)
      (put-text-property start (point) 'face '(:weight bold))
      (insert " ")
      (tumblesocks-view-insert-html-fragment (cdr (assq 'phrase message)) t)
      (insert "\n"))))

(defun tumblesocks-view-insert-i-have-no-clue-what-this-is ()
  (let ((begin (point)))
    (insert "this is a " (format "%S" type) " post\n")
    (put-text-property begin (point) 'face font-lock-comment-face)))



(defun tumblesocks-view-prepare-buffer (blogtitle)
  "Create a new buffer to begin viewing a blog."
  (pop-to-buffer-same-window (concat "*Tumblr: " blogtitle "*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (tumblesocks-view-mode))
(defun tumblesocks-view-finishrender ()
  "Finish creating the blog buffer, ready to present to the user"
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun tumblesocks-view-blog (blogname)
  "View the given blog (URL or name)"
  (interactive (list (read-string "Blog to view: " tumblesocks-blog)))
  (let ((tumblesocks-blog blogname))
    (tumblesocks-view-prepare-buffer
     (cdr (assq 'title (cdr (assq 'blog (tumblesocks-api-blog-info))))))
    (tumblesocks-view-render-blogdata
     (cdr (assq 'posts
                (tumblesocks-api-blog-posts nil nil nil nil nil nil nil "html"))))
    (tumblesocks-view-finishrender)
    (setq tumblesocks-view-refresh-action
          `(lambda () (tumblesocks-view-blog ,blogname))))) ; <-- CLOSURE HACK :p

(defun tumblesocks-view-dashboard ()
  "View your dashboard"
  (interactive)
  (tumblesocks-view-prepare-buffer "Dashboard")
  (tumblesocks-view-render-blogdata
   (cdr (assq 'posts
              (tumblesocks-api-user-dashboard nil nil nil nil nil nil))))
  (tumblesocks-view-finishrender)
  (setq tumblesocks-view-refresh-action
        '(lambda () (tumblesocks-view-dashboard))))

(defun tumblesocks-view-post (post_id)
  "View a post in its own dedicated buffer, with notes"
  (interactive "sPost ID: ")
  (let* ((blog (tumblesocks-api-blog-posts
                nil post_id nil "1" nil nil "true" "html"))
         (post (elt (cdr (assq 'posts blog)) 0))
         (notes (cdr (assq 'notes post))))
    (tumblesocks-view-prepare-buffer
     (format "Viewing post %s: %s"
             (cdr (assq 'blog_name post))
             post_id))
    (tumblesocks-view-render-post post t)
    (tumblesocks-view-render-notes notes)
    (tumblesocks-view-finishrender)
    (setq tumblesocks-view-refresh-action
          `(lambda () (tumblesocks-view-post ,post_id)))))

(defun tumblesocks-view-render-notes (notes)
  "Render the given notes into the given buffer"
  (let ((start (point)))
    (insert "-- Notes:\n")
    (dolist (note (append notes nil))
      (let ((type (cdr-safe (assq 'type note)))
            (post_id (cdr-safe (assq 'post_id note)))
            (blog_name (cdr-safe (assq 'blog_name note)))
            (blog_url (cdr-safe (assq 'blog_url note)))
            (added_text (cdr-safe (assq 'added_text note))))
        (cond ((string= type "posted")
               (insert blog_name " posted this"))
              ((string= type "reblog")
               (insert blog_name " reblogged this on " blog_url))
              ((string= type "like")
               (insert blog_name " liked this")))
      (when added_text
        (insert " (adding text: " added_text ")"))
      (insert "\n")))
    (put-text-property start (point) 'face font-lock-comment-face)))

(defun tumblesocks-view-like-post-at-point (like-p)
  "Like the post underneath point. With prefix arg (C-u), unlike it."
  (interactive "P")
  (when (get-text-property (point) 'tumblesocks-post-data)
    ;; Get the reblog key.
    (let* ((post_id
            (format "%d" (cdr (assq 'id (get-text-property (point) 'tumblesocks-post-data)))))
           (blog (tumblesocks-api-blog-posts
                  nil post_id nil "1" nil "true" nil "html"))
           (post (elt (cdr (assq 'posts blog)) 0))
           (reblog_key (cdr (assq 'reblog_key post))))
      (if (not like-p)
          (progn
            (tumblesocks-api-user-like post_id reblog_key)
            (message "Liked this post."))
        (tumblesocks-api-user-unlike post_id reblog_key)
        (message "Unliked this post.")))))

(defun tumblesocks-view-posts-tagged (tag)
  "Search for posts with the given tag"
  (interactive "sSearch for posts with tag: ")
  (tumblesocks-view-prepare-buffer
   (concat "Tag search: " tag))
  (tumblesocks-view-render-blogdata
   (tumblesocks-api-tagged tag nil nil "html"))
  (tumblesocks-view-finishrender)
  (setq tumblesocks-view-refresh-action
        `(lambda () (tumblesocks-view-posts-tagged ,tag))))

;; tumblesocks-view should have pagination
;; reblog posts from tumblesocks-view (how do notes work?)
     ;; only fetch reblog info when reblogging

;; documentation
;; better authentication
