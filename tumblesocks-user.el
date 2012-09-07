;; tumblesocks-user.el -- higher-level functions for the tumblr api
(require 'tumblesocks-api)
(provide 'tumblesocks-user)

(defun tumblesocks-follow-blog (blog)
  "Follow the given Tumblr blog"
  (interactive "sTumblr blog to follow (URL): ")
  (tumblesocks-api-user-follow blog)
  (message (concat "Now following " blog)))

(defun tumblesocks-unfollow-blog (blog-url)
  "Unfollow a certain Tumblr blog on your list, with tab completion."
  (interactive (list (completing-read
                      "Blog URL to unfollow (TAB to complete): "
                      (let ((bloglist (cdr (assq 'blogs
                                                 (tumblesocks-api-user-following)))))
                        (mapcar '(lambda (blog) (cdr (assq 'url blog)))
                                bloglist))
                      nil t)))
  (tumblesocks-api-user-unfollow blog-url)
  (message (concat "No longer following " blog-url)))

(defun tumblesocks-text-post-from-region (begin end title &optional tags)
  "Create a new Tumblr markdown text post from the given region, returning the ID and copying the URL to the clipboard."
  (interactive "r\nsTitle: \nsTags (optional, comma separated): ")
  (when (and tags (string= tags "")) (setq tags nil))
  (when (string= title "") (error "You must provide a title."))
  (let ((args '()))
    (aput 'args "type" "text")
    (aput 'args "body" (buffer-substring begin end))
    (aput 'args "title" title)
    (when tags (aput 'args "tags" tags))
    (let* ((blog-url (cdr (assq 'url (cdr (assq 'blog (tumblesocks-api-blog-info))))))
           (new-post-id (format "%d" (cdr (assq 'id (tumblesocks-api-new-post args)))))
           (new-post-url
            (let* ((last-char (substring blog-url -1)))
              (cond ((string= last-char "/")
                     (concat blog-url new-post-id)) ; url has a trailing slash
                    (t (concat blog-url (concat "/" new-post-id)))))))
      ;; So we need to both return this ID
      ;; and copy the URL to the clipboard (and message it too.)
      ;; Thanks to tumble.el for this:
      (kill-new new-post-url)
      (message (concat "New post created at " new-post-url))
      new-post-id)))

(defun tumblesocks-text-post-from-buffer (title &optional tags)
  "Create a new Tumblr markdown text post from the current buffer, returning the ID and copying the URL to the clipboard."
  (interactive "sTitle: \nsTags (optional, comma separated): ")
  (tumblesocks-text-post-from-region (point-min) (point-max) title tags))



;; Tumblr compose mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'markdown-mode)

(defvar tumblesocks-compose-finish-action
  '(lambda () (call-interactively 'tumblesocks-text-post-from-buffer))
  "The action to run when finishing posting")
(defvar tumblesocks-compose-editing-id nil
  "If editing, the ID of the post that we are editing")
(defvar tumblesocks-compose-editing-args nil
  "If editing, which args to use while we're editing")

(defun tumblesocks-compose-finish ()
  (interactive)
  (funcall tumblesocks-compose-finish-action)
  (quit-window))

(defvar tumblesocks-compose-mode-map
  (let ((tumblesocks-compose-mode-map (make-keymap)))
    ;; Element insertion
    (define-key tumblesocks-compose-mode-map "\C-c\C-c" 'tumblesocks-compose-finish)
    tumblesocks-compose-mode-map))

(define-derived-mode tumblesocks-compose-mode markdown-mode "Tumblr/C"
  "Major mode for composing a new Tumblr post."
  (make-local-variable 'tumblesocks-compose-finish-action)
  (setq tumblesocks-compose-finish-action
          '(lambda () (call-interactively 'tumblesocks-text-post-from-buffer)))
  (make-local-variable 'tumblesocks-compose-editing-args)
  (make-local-variable 'tumblesocks-compose-editing-id))


(defun tumblesocks-compose-new-post ()
  "Open a new buffer containing a fresh post to begin authoring."
  (interactive)
  (pop-to-buffer "*New tumblr post")
  (erase-buffer)
  (tumblesocks-compose-mode))

(defun tumblesocks-compose-edit-post (post-id)
  "Open a new buffer containing a fresh post to begin authoring."
  (interactive "sPost ID: ")
  (let* ((returned-posts
          (cdr (assq 'posts
                     (tumblesocks-api-blog-posts nil post-id nil "1"
                                            nil nil nil "raw"))))
         (the-post (elt returned-posts 0))
         (title (cdr (assq 'title the-post)))
         (id (format "%d" (cdr (assq 'id the-post))))
         (body (cdr (assq 'body the-post))))
    (pop-to-buffer (concat "*Tumblr post: " title "*"))
    (erase-buffer)
    (tumblesocks-compose-mode)
    (setq tumblesocks-compose-editing-args the-post)
    (setq tumblesocks-compose-editing-id id)
    (aput 'tumblesocks-compose-editing-args
          'tags
          (mapconcat 'identity (cdr (assq 'tags tumblesocks-compose-editing-args)) ","))
    (delq (assq 'format tumblesocks-compose-editing-args)
          tumblesocks-compose-editing-args)
    (delq (assq 'id tumblesocks-compose-editing-args)
          tumblesocks-compose-editing-args)
    (setq tumblesocks-compose-finish-action 'tumblesocks-compose-edit-finish)
    (insert body)
    (goto-char (point-min))))

(defun tumblesocks-compose-edit-finish ()
  "Finish editing the given post"
  ;; Optionally prompt for title
  (let ((new-title (read-string "Title: " (cdr (assq 'title tumblesocks-compose-editing-args))))
        (new-tags (read-string "Tags: " (cdr (assq 'tags tumblesocks-compose-editing-args)))))
    ;; Set tags
    (when (and new-tags (string= new-tags "")) (setq new-tags nil))
    (when (string= new-title "") (error "You must provide a title."))
    (aput 'tumblesocks-compose-editing-args 'title new-title)
    (if (string= "" new-tags)
        (delq (assq 'tags tumblesocks-compose-editing-args) tumblesocks-compose-editing-args)
      (aput 'tumblesocks-compose-editing-args 'tags new-tags))
    (aput 'tumblesocks-compose-editing-args
          'body
          (buffer-string))
    (tumblesocks-api-edit-post tumblesocks-compose-editing-id tumblesocks-compose-editing-args)
    (message "Post edited.")))

;; Begin composing from things in region
;; Begin composing from fontlock-highlighted things in region
;; Insert new fontlock-highlighted region into buffer at point
