;; tumblesocks-view.el -- Provide an interface to view tumblr blog posts.

(require 'tumblesocks-api)
(require 'shr)
(provide 'tumblesocks-view)

(defvar tumblesocks-view-mode-map
  (let ((tumblesocks-view-mode-map (make-keymap)))
    (define-key tumblesocks-view-mode-map "q" 'quit-window)
    (define-key tumblesocks-view-mode-map "n" 'tumblesocks-view-next-post)
    (define-key tumblesocks-view-mode-map "g" 'tumblesocks-view-refresh)
    (define-key tumblesocks-view-mode-map "p" 'tumblesocks-view-previous-post)
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

(define-derived-mode tumblesocks-view-mode fundamental-mode "Tumblr"
  "Major mode for reading Tumblr blogs."
  ;;(visual-line-mode t) ;shr.el takes care of this...
)

(defun tumblesocks-view-insert-html-fragment (html-fragment &optional inline)
  "Renders and inserts an HTML fragment. If inline is t, then <p> tags will have no effect."
  (let (html-frag-parsed)
    (with-temp-buffer
      (insert html-fragment)
      (setq html-frag-parsed (libxml-parse-html-region (point-min) (point-max))))
    (let ((shr-width nil))
      (if inline
          (flet ((shr-ensure-paragraph () 0))
            ;; disable newlines, for now ...
            (shr-insert-document html-frag-parsed))
        (shr-insert-document html-frag-parsed)))))

(defun tumblesocks-view-render-blogdata (blogdata)
  "Render blogdata into the current buffer.

Blogdata should be the JSON result of a call to Tumblr's
/blog/posts or /user/dashboard API. (We expect each post in
blogdata to be filtered with the 'text' filter.)

This function internally dispatches to other functions that are better suited to inserting each post."
  ; See http://www.tumblr.com/docs/en/api/v2#posts for more
  ; info about the post API.
  (dolist (post (append blogdata nil))
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
        (tumblesocks-view-insert-header)
        (cond
          ((string= type "text") (tumblesocks-view-insert-text))
          ;;("quote" (tumblesocks-view-insert-quote))
          ;;("link" (tumblesocks-view-insert-link))
          ;;("answer" (tumblesocks-view-insert-answer))
          ;;("video" (tumblesocks-view-insert-video))
          ;;("audio" (tumblesocks-view-insert-audio))
          ;;("photo" (tumblesocks-view-insert-photo))
          ;;("chat" (tumblesocks-view-insert-chat))
          (t (tumblesocks-view-insert-i-have-no-clue-what-this-is)))
        (insert "\n")
        ;; Record this post data so we know how to read it next
        (put-text-property begin-post-area (point)
                           'tumblesocks-post-data
                           post)))))

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
    (when (> note_count 0)
      (insert " (" (format "%d" note_count) " note"
              (if (= 1 note_count) "" "s") ")"))
    (insert "\n")
    (when verbose
      (insert
            date " " (mapconcat '(lambda (x) (concat "#" x)) tags ", ")
            post_url "\n"))
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

(defun tumblesocks-view-insert-i-have-no-clue-what-this-is ()
  (let ((begin (point)))
    (insert "this is a " (format "%S" type) "\n")
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
  (interactive "sBlog to view: ")
  (let ((tumblesocks-blog blogname))
    (tumblesocks-view-prepare-buffer
     (cdr (assq 'title (cdr (assq 'blog (tumblesocks-api-blog-info))))))
    (tumblesocks-view-render-blogdata
     (cdr (assq 'posts
                (tumblesocks-api-blog-posts nil nil nil nil nil nil nil "html"))))
    (tumblesocks-view-finishrender)))

(defun tumblesocks-view-my-blog ()
  "View your blog"
  (interactive)
  (tumblesocks-view-prepare-buffer
   (cdr (assq 'title (cdr (assq 'blog (tumblesocks-api-blog-info))))))
  (tumblesocks-view-render-blogdata
   (cdr (assq 'posts
              (tumblesocks-api-blog-posts nil nil nil nil nil t nil "html"))))
  (tumblesocks-view-finishrender))

(defun tumblesocks-view-dashboard ()
  "View your dashboard"
  (interactive)
  (tumblesocks-view-prepare-buffer "Dashboard")
  (tumblesocks-view-render-blogdata
   (cdr (assq 'posts
              (tumblesocks-api-user-dashboard nil nil nil nil t nil))))
  (tumblesocks-view-finishrender))
