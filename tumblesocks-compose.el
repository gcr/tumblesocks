
(require 'tumblesocks-user)
(require 'tumblesocks-api)
(require 'htmlize)
(require 'markdown-mode)
(provide 'tumblesocks-compose)

;; Tumblr compose mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tumblesocks-compose-finish-action
  '(lambda () (call-interactively 'tumblesocks-text-post-from-buffer))
  "The action to run when finishing posting")
(defvar tumblesocks-compose-continuation nil
  "Optional action to run when finishing editing or posting.

tumblesocks-view uses this to refresh buffers, for example.")
(defvar tumblesocks-compose-editing-id nil
  "If editing, the ID of the post that we are editing")
(defvar tumblesocks-compose-editing-args nil
  "If editing, which args to use while we're editing")

(defun tumblesocks-compose-finish ()
  "Actually send the new/updated post to the server."
  (interactive)
  (funcall tumblesocks-compose-finish-action)
  (let ((cc tumblesocks-compose-continuation))
    (quit-window)
    (when cc
      (funcall cc))))

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
  (make-local-variable 'tumblesocks-compose-editing-id)
  (make-local-variable 'tumblesocks-compose-continuation))



;;;###autoload
(defun tumblesocks-compose-new-post (&optional continuation)
  "Open a new buffer containing a fresh post to begin authoring.

Once you're ready to submit your post, press C-c C-c"
  (interactive)
  (pop-to-buffer "*Tumblr: New post*")
  (erase-buffer)
  (tumblesocks-compose-mode)
  (setq header-line-format "New tumblr post")
  (setq tumblesocks-compose-continuation continuation))

(defun tumblesocks-compose-new-from-region (begin end &optional continuation)
  "Open a new buffer containing a fresh post, but initially
populate it with the contents of the region.

Once you're ready to submit your post, press C-c C-c"
  (interactive "r")
  (let ((initial-body (buffer-substring begin end)))
    (tumblesocks-compose-new-post)
    (insert "\n \n")
    (insert initial-body)
    (goto-char (point-min)))
  (setq tumblesocks-compose-continuation continuation))

(defun tumblesocks-compose-new-from-highlighted-region (begin end &optional continuation)
  "Open a new buffer containing a fresh post, but initially
populate it with the contents of the region. The region is
syntax-highlighted using Emacs' htmlize library."
  (interactive "r")
  (deactivate-mark)
  (let ((initial-body (htmlize-region-for-paste begin end)))
    (tumblesocks-compose-new-post)
    (insert "\n \n")
    (insert initial-body)
    (goto-char (point-min)))
  (setq tumblesocks-compose-continuation continuation))

(defun tumblesocks-compose-insert-highlighted-region (beg end)
  "Add new syntax-highlighted text from the region into the tumblr
post buffer"
  (interactive "r")
  (deactivate-mark)
  (let ((htmlstring (htmlize-region-for-paste beg end)))
    (pop-to-buffer "*Tumblr: New post*")
    (goto-char (point-max))
    (insert "\n\n")
    (insert htmlstring)))



(defun tumblesocks-plist-delete (plist prop)
  "Delete PROPERTY from PLIST."
  (let ((p (memq prop plist)))
    (if (not p)
        plist
      (setcdr p (cddr p))
      (delq prop plist))))

(defun tumblesocks-compose-edit-post (post-id &optional continuation)
  "Open a new buffer containing a fresh post to begin authoring.

Once you're ready to finish editing, press C-c C-c. You will be
prompted for a new title and new tags."
  (interactive "sPost ID: ")
  (let* ((returned-posts
          (plist-get
           (tumblesocks-api-blog-posts nil post-id nil "1"
                                       nil nil nil "raw")
           :posts))
         (the-post (car returned-posts))
         (type (plist-get the-post :type))
         (title (plist-get the-post :title))
         (id (format "%d" (plist-get the-post :id)))
         (body (plist-get the-post :body)))
    (unless (string= type "text")
      (error "We can only edit text posts."))
    (pop-to-buffer (concat "*Tumblr: Ediitng " title "*"))
    (erase-buffer)
    (tumblesocks-compose-mode)
    (setq header-line-format (concat "Editing: " title))
    (setq tumblesocks-compose-editing-args the-post)
    (setq tumblesocks-compose-editing-id id)
    (setq tumblesocks-compose-editing-args
          (plist-put tumblesocks-compose-editing-args
                     :tags
                     (mapconcat 'identity
                                (plist-get tumblesocks-compose-editing-args :tags)
                                ",")))
    (setq tumblesocks-compose-editing-args
          (tumblesocks-plist-delete tumblesocks-compose-editing-args :format))
    (setq tumblesocks-compose-editing-args
          (tumblesocks-plist-delete tumblesocks-compose-editing-args :id))
    (setq tumblesocks-compose-finish-action 'tumblesocks-compose-edit-finish)
    (insert body)
    (goto-char (point-min))
    (setq tumblesocks-compose-continuation continuation)))

(defun tumblesocks-compose-edit-finish ()
  "Finish editing the given post"
  ;; Optionally prompt for title
  (let ((new-title (read-string "New title: " (plist-get tumblesocks-compose-editing-args :title)))
        (new-tags (read-string "New tags: " (plist-get tumblesocks-compose-editing-args :tags))))
    ;; Set tags
    (when (and new-tags (string= new-tags "")) (setq new-tags nil))
    (when (string= new-title "") (error "You must provide a title."))
    (setq tumblesocks-compose-editing-args
          (plist-put tumblesocks-compose-editing-args :title new-title))
    (if (string= "" new-tags)
        (setq tumblesocks-compose-editing-args
              (tumblesocks-plist-delete tumblesocks-compose-editing-args :tags))
      (setq tumblesocks-compose-editing-args
            (plist-put tumblesocks-compose-editing-args :tags new-tags)))
    (setq tumblesocks-compose-editing-args
          (plist-put tumblesocks-compose-editing-args :body (buffer-string)))
    (tumblesocks-api-edit-post tumblesocks-compose-editing-id tumblesocks-compose-editing-args)
    (message "Post edited.")))
