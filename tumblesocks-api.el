;; tumblesocks-api.el -- functions for talking with tumblr
;; Copyright (C) 2012 gcr

(require 'oauth)
(require 'json)
(provide 'tumblesocks-api)

(defcustom tumblesocks-consumer-key
  "5xqkcJNRSGj3TokMQDJf3FzE8246DHvw8sNJWNn54fs2z0AhYr"
  "Our Tumblr OAuth consumer API key.

This goes hand-in-hand with `tumblesocks-secret-key'.

If you need to register your own app, do that at
http://www.tumblr.com/oauth/apps"
  :type 'string
  :group 'tumblesocks)

(defcustom tumblesocks-secret-key
  "juLG1T866ZG964ybgGCu1EntFMo5eQuHth1SKCqL2mdMzNIL1Q"
  "Our Tumblr OAuth consumer secret key.

This goes hand-in-hand with `tumblesocks-consumer-key'.

If you need to register your own app, do that at
http://www.tumblr.com/oauth/apps"
  :type 'string
  :group 'tumblesocks)

(defcustom tumblesocks-blog nil
  "Your blog name, like xxx.tumblr.com.

This variable affects many functions that depend on blogs. For
example, `tumblesocks-api-blog-posts' will consult this variable
to pick which block to list posts for, so if you want to temporarily ask for a different blog, rebind this."
  :type 'string
  :group 'tumblesocks)

(defvar tumblesocks-token nil)

(defun tumblesocks-api-forget-authentication ()
  "Forget our authentication and delete the token file. You must
call `tumblesocks-api-reauthenticate' after this."
  (interactive)
  (setq tumblesocks-token nil)
  (let ((tumblesocks-token-file (concat (file-name-as-directory user-emacs-directory)
                                        "tumblr-oauth-token")))
    (when (file-exists-p tumblesocks-token-file)
      (delete-file tumblesocks-token-file))))

(defun tumblesocks-api-reauthenticate ()
  "Read our tumblr token from the tumblr token file, or generate a new one."
  (when (or (not tumblesocks-secret-key)
            (not tumblesocks-consumer-key))
    (error "You MUST set both `tumblesocks-secret-key' and `tumblesocks-consumer-key' to use tumblesocks."))
  (let ((tumblesocks-token-file (concat (file-name-as-directory user-emacs-directory)
                                        "tumblr-oauth-token")))
    (when (file-exists-p tumblesocks-token-file)
      (save-excursion
        (find-file tumblesocks-token-file)
        (let ((str (buffer-substring (point-min) (point-max))))
          (if (string-match "\\([^:]*\\):\\(.*\\)"
                            (buffer-substring (point-min) (point-max)))
              (setq tumblesocks-token
                    (make-oauth-access-token
                     :consumer-key tumblesocks-consumer-key
                     :consumer-secret tumblesocks-secret-key
                     :auth-t (make-oauth-t
                              :token (match-string 1 str)
                              :token-secret (match-string 2 str))))))
        (kill-this-buffer)))
    (unless tumblesocks-token
      (setq tumblesocks-token (oauth-authorize-app
                               tumblesocks-consumer-key
                               tumblesocks-secret-key
                               "https://www.tumblr.com/oauth/request_token"
                               "https://www.tumblr.com/oauth/access_token"
                               "https://www.tumblr.com/oauth/authorize"))
      (save-excursion
        (find-file tumblesocks-token-file)
        (erase-buffer)
        (let ((token (oauth-access-token-auth-t tumblesocks-token)))
          (insert (format "%s:%s\n"
                          (oauth-t-token token)
                          (oauth-t-token-secret token))))
        (save-buffer)
        (kill-this-buffer)))))

(defun tumblesocks-api-test-auth ()
  (interactive)
  (unless tumblesocks-blog (error "Please set the `tumblesocks-blog' variable. See https://github.com/gcr/tumblesocks for help getting Tumblesocks working."))
  (condition-case nil
      (message (concat "Hello, "
                       (plist-get (plist-get (tumblesocks-api-user-info)
                                             :user)
                                  :name)
                       "! Tumblesocks is working properly."))
    (error
     (if (yes-or-no-p "Looks like something broke. Try again? (yes/no) ")
         (progn
           (tumblesocks-api-forget-authentication)
           (tumblesocks-api-test-auth))
       (message "Please see http://github.com/gcr/tumblesocks for help.")))))

(defun tumblesocks-api-url (&rest args)
  (apply 'concat "https://api.tumblr.com/v2" args))

(defun tumblesocks-api-http-noauth-get (url)
  "Post to an unauthenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (with-current-buffer (url-retrieve-synchronously url)
    (tumblesocks-api-process-response)))

(defun tumblesocks-plist-to-alist (plist)
  (when plist
    (let ((key (car plist))
          (value (cadr plist))
          (rest (cddr plist)))
      (cons (cons (intern (substring (symbol-name key) 1)) value)
            (tumblesocks-plist-to-alist rest)))))

(defun tumblesocks-api-http-oauth-get (url params)
  "Post to an API-key-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, a keyword plist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (with-current-buffer (oauth-url-retrieve
                        tumblesocks-token
                        (concat url "?api_key=" tumblesocks-consumer-key
                                (mapconcat
                                 '(lambda (x)
                                    (concat "&" (url-hexify-string (format "%s" (car x)))
                                            "=" (url-hexify-string (format "%s" (cdr x)))))
                                 (tumblesocks-plist-to-alist params) "")))
    (tumblesocks-api-process-response)))

(defun tumblesocks-api-http-apikey-get (url params)
  "Post to an API-key-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, a keyword plist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (with-current-buffer (url-retrieve-synchronously
                        (concat url "?api_key=" tumblesocks-consumer-key
                                (mapconcat
                                 '(lambda (x)
                                    (concat "&" (url-hexify-string (format "%s" (car x)))
                                            "=" (url-hexify-string (format "%s" (cdr x)))))
                                 (tumblesocks-plist-to-alist params) "")))
    (tumblesocks-api-process-response)))

(defun tumblesocks-api-http-oauth-post (url params)
  "Post to an OAuth-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (unless tumblesocks-token (tumblesocks-api-reauthenticate))
  (with-current-buffer
      (oauth-post-url
       tumblesocks-token url
       (mapcar '(lambda (x)
                  (cons (format "%s" (car x))
                        (format "%s" (cdr x))))
               (tumblesocks-plist-to-alist params)))
    (tumblesocks-api-process-response)))

(defun tumblesocks-api-process-response ()
  "Process Tumblr's response in the current buffer,
returning JSON or signaling an error for other requests."
  (decode-coding-region (point-min) (point-max) 'utf-8-dos)
  ;; the following copied from url.el
  (goto-char (point-min))
  (skip-chars-forward " \t\n")		; Skip any blank crap
  (skip-chars-forward "HTTP/")		; Skip HTTP Version
  (skip-chars-forward "[0-9].")
  (let ((pointpos (point))
        (code (read (current-buffer))))
    (cond
     ((= code 100) ;; Gotta clean up the buffer and try again
      (search-forward-regexp "^$" nil t)
      (delete-region (point-min) (point))
      (tumblesocks-api-process-response))
     ((not (and (<= 200 code) (<= code 299)))
      (error (buffer-substring pointpos
                               (line-end-position))))
     (t
      (search-forward-regexp "^$" nil t)
      ;; body
      (let* ((json-response (buffer-substring (1+ (point)) (point-max)))
             (json-object-type 'plist)
             (json-array-type 'list)
             (json-false nil))
        (plist-get (json-read-from-string json-response)
                   :response))))))



(defun tumblesocks-api-avatar-url (&optional size)
  (tumblesocks-api-url
   "/blog/" tumblesocks-blog "/avatar/"
   (format "%d" (or size 128))))

(defun tumblesocks-api-user-info ()
  "Gather information about the logged in user"
  (tumblesocks-api-http-oauth-post (tumblesocks-api-url "/user/info") '()))

(defun tumblesocks-api-user-dashboard (&optional limit offset type since_id reblog_info notes_info)
  "Gather information about the logged in user's dashboard"
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append
               (and limit `(:limit ,limit))
               (and offset `(:offset ,offset))
               (and type `(:offset ,type))
               (and since_id `(:since_id ,since_id))
               (and reblog_info `(:reblog_info ,reblog_info))
               (and notes_info `(:notes_info ,notes_info)))))
    (tumblesocks-api-http-oauth-post (tumblesocks-api-url "/user/dashboard") args)))

(defun tumblesocks-api-user-likes (&optional limit offset)
  "Gather information about the logged in user's likes"
  (let ((args (append
               (and limit `(:limit ,limit))
               (and offset `(:offset ,offset)))))
    (tumblesocks-api-http-oauth-post (tumblesocks-api-url "/user/likes") args)))

(defun tumblesocks-api-user-following (&optional limit offset)
  "Gather information about which blogs are followed by the logged-in user"
  (let ((args (append
               (and limit `(:limit ,limit))
               (and offset `(:offset ,offset)))))
    (tumblesocks-api-http-oauth-post
     (tumblesocks-api-url "/user/following") args)))

(defun tumblesocks-api-user-follow (url)
  "Follow the given blog URL."
  (tumblesocks-api-http-oauth-post (tumblesocks-api-url "/user/follow")
                                   `(:url ,url)))
(defun tumblesocks-api-user-unfollow (url)
  "Unfollow the given blog URL."
  (tumblesocks-api-http-oauth-post (tumblesocks-api-url "/user/unfollow")
                                   `(:url ,url)))
(defun tumblesocks-api-user-like (id reblog_key)
  "Like a given post"
  (tumblesocks-api-http-oauth-post (tumblesocks-api-url "/user/like")
                                   `(:id ,id :reblog_key ,reblog_key)))
(defun tumblesocks-api-user-unlike (id reblog_key)
  "Unlike a given post"
  (tumblesocks-api-http-oauth-post (tumblesocks-api-url "/user/unlike")
                                   `(:id ,id :reblog_key ,reblog_key)))

(defun tumblesocks-api-blog-info ()
  "Gather information about the blog listed in
`tumblesocks-blog'."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (tumblesocks-api-http-apikey-get
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/info")
   '()))

;; TODO This returns actual image data, not JSON!
;; (defun tumblesocks-api-blog-avatar ()
;;   "Gathers info about the given blog's avatar. Defaults to `tumblesocks-blog'"
;;   (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
;;   (tumblesocks-api-http-noauth-get
;;    (tumblesocks-api-url "/blog/" tumblesocks-blog "/avatar")))

(defun tumblesocks-api-blog-followers ()
  "Gathers info about the `tumblesocks-blog''s followers.

See http://www.tumblr.com/docs/en/api/v2 for information about the returned JSON."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (tumblesocks-api-http-oauth-post
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/followers") '()))



(defun tumblesocks-api-blog-posts (&optional type id tag limit offset reblog_info notes_info filter)
  "Gather info about the blog posts for `tumblesocks-blog'.

Type should be one of text, quote, link, answer, video, audio, photo, chat.

If given, retrieve just posts with the given attributes (args)

See http://www.tumblr.com/docs/en/api/v2 for information about
the returned JSON."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append
               (and id `(:id ,id))
               (and tag `(:tag ,tag))
               (and limit `(:limit ,limit))
               (and offset `(:offset ,offset))
               (and reblog_info `(:reblog_info ,reblog_info))
               (and notes_info `(:notes_info ,notes_info))
               (and filter `(:filter ,filter)))))
    (tumblesocks-api-http-oauth-get
     (tumblesocks-api-url "/blog/"
                          tumblesocks-blog
                          "/posts"
                          (if type (concat "/" type) ""))
     args)))

(defun tumblesocks-api-blog-queued-posts (&optional offset filter)
  "Retrieve queued blog posts from `tumblesocks-blog'."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append
               (and offset `(:offset ,offset))
               (and filter `(:filter ,filter)))))
    (tumblesocks-api-http-oauth-post
     (tumblesocks-api-url "/blog/" tumblesocks-blog "/posts/queue")
     args)))

(defun tumblesocks-api-blog-draft-posts (&optional filter)
  "Retrieve draft blog posts from `tumblesocks-blog'. You need
write access to it!"
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (and filter `(:filter ,filter))))
    (tumblesocks-api-http-oauth-post
     (tumblesocks-api-url "/blog/" tumblesocks-blog "/posts/draft")
     args)))

(defun tumblesocks-api-blog-submission-posts (&optional offset filter)
  "Retrieve submission blog posts from `tumblesocks-blog'."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append
               (and offset `(:offset ,offset))
               (and filter `(:filter ,filter)))))
    (tumblesocks-api-http-oauth-post
     (tumblesocks-api-url "/blog/" tumblesocks-blog "/posts/submission")
     args)))

(defun tumblesocks-api-new-post (&optional args)
  "Create a new post, using Tumblr's post API. Post it to `tumblrsocks-blog'.
Args must be an alist of arguments to use.

If you're making a text post, for example, args should be something like
'(:type \"text\"
  :title \"How to use the Tumblr API\"
  :body \"...\")"
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (tumblesocks-api-http-oauth-post
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/post")
   args))

(defun tumblesocks-api-edit-post (id &optional args)
  "Edit the post with the given id. args should be as in `tumblesocks-new-post'."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (setq args (plist-put args :id id))
  (tumblesocks-api-http-oauth-post
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/post/edit")
   args))

(defun tumblesocks-api-reblog-post (id reblog_key &optional comment)
  "Reblog a post with the given id and reblog key."
  (unless tumblesocks-blog (error "Which blog? Please set `tumblesocks-blog'"))
  (let ((args (append `(:id  ,id :reblog_key ,reblog_key)
                      (and comment (not (string= comment ""))
                           `(:comment ,comment)))))
    (tumblesocks-api-http-oauth-post
     (tumblesocks-api-url "/blog/" tumblesocks-blog "/post/reblog")
     args)))

(defun tumblesocks-api-delete-post (id)
  "Delete the post with the given id. args should be as in `tumblesocks-new-post'."
  (tumblesocks-api-http-oauth-post
   (tumblesocks-api-url "/blog/" tumblesocks-blog "/post/delete")
   `(:id ,id)))

(defun tumblesocks-api-tagged (tag &optional before limit filter)
  (let ((args (append
               `(:tag ,tag)
               (and before `(:before ,before))
               (and limit `(:limit ,limit))
               (and filter `(:filter ,filter)))))
    (tumblesocks-api-http-apikey-get
     (tumblesocks-api-url "/tagged")
     args)))
