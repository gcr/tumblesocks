;; tumblr-api.el -- functions for talking with tumblr
;; Copyright (C) 2012 gcr

(require 'oauth)
(provide 'tumblr-api)

(defcustom tumblr-consumer-key nil
  "Your tumblr app's consumer API key"
  :type 'string)

(defcustom tumblr-secret-key nil
  "Your tumbler app's secret key"
  :type 'string)

(defcustom tumblr-blog nil
  "Your blog name, like xxx.tumblr.com"
  :type 'string)

(defvar tumblr-token nil)

(defun tumblr-api-reauthenticate ()
  "Read our tumblr token from the tumblr token file, or generate a new one."
  (let ((tumblr-token-file (concat (file-name-as-directory user-emacs-directory)
                                   "tumblr-oauth-token")))
    (when (file-exists-p tumblr-token-file)
      (save-excursion
        (find-file tumblr-token-file)
        (let ((str (buffer-substring (point-min) (point-max))))
          (if (string-match "\\([^:]*\\):\\(.*\\)"
                            (buffer-substring (point-min) (point-max)))
              (setq tumblr-token
                    (make-oauth-access-token
                     :consumer-key tumblr-consumer-key
                     :consumer-secret tumblr-secret-key
                     :auth-t (make-oauth-t
                              :token (match-string 1 str)
                              :token-secret (match-string 2 str))))))
        (kill-this-buffer)))
    (unless tumblr-token
      (setq tumblr-token (oauth-authorize-app
                          tumblr-consumer-key
                          tumblr-secret-key
                          "https://www.tumblr.com/oauth/request_token"
                          "https://www.tumblr.com/oauth/access_token"
                          "https://www.tumblr.com/oauth/authorize"))
      (save-excursion
        (find-file tumblr-token-file)
        (erase-buffer)
        (let ((token (oauth-access-token-auth-t tumblr-token)))
          (insert (format "%s:%s\n"
                          (oauth-t-token token)
                          (oauth-t-token-secret token))))
        (save-buffer)
        (kill-this-buffer)))))


(defun tumblr-api-url (&rest args)
  (apply 'concat "https://api.tumblr.com/v2" args))

(defun tumblr-api-http-noauth-get (url)
  "Post to an unauthenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (with-current-buffer (url-retrieve-synchronously url)
    (tumblr-api-process-response)))

(defun tumblr-api-http-apikey-get (url params)
  "Post to an API-key-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (with-current-buffer (url-retrieve-synchronously
                        (concat url "?api_key=" tumblr-consumer-key
                                (mapconcat
                                 '(lambda (x)
                                    (concat "&" (url-hexify-string (car x))
                                            "=" (url-hexify-string (cdr x))))
                                 params "")))
    (tumblr-api-process-response)))

(defun tumblr-api-http-oauth-post (url params)
  "Post to an OAuth-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (unless tumblr-token (tumblr-api-reauthenticate))
  (with-current-buffer (oauth-post-url tumblr-token url params)
    (tumblr-api-process-response)))

(defun tumblr-api-process-response ()
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
      (tumblr-api-process-response))
     ((not (and (<= 200 code) (<= code 299)))
      (error (buffer-substring pointpos
                               (line-end-position))))
     (t
      (search-forward-regexp "^$" nil t)
      ;; body
      (let* ((json-response (buffer-substring (1+ (point)) (point-max)))
             (jsonobj (json-read-from-string json-response)))
        (cdr (assq 'response jsonobj)))))))



(defun tumblr-api-user-info ()
  "Gather information about the logged in user"
  (tumblr-api-http-oauth-post (tumblr-api-url "/user/info") '()))

(defun tumblr-api-user-dashboard (&optional limit offset type since_id reblog_info notes_info)
  "Gather information about the logged in user's dashboard"
  (let ((args '()))
    (when limit (aput 'args "limit" limit))
    (when offset (aput 'args "offset" offset))
    (when type (aput 'args "type" type))
    (when since_id (aput 'args "since_id" since_id))
    (when reblog_info (aput 'args "reblog_info" reblog_info))
    (when notes_info (aput 'args "notes_info" notes_info))
    (tumblr-api-http-oauth-post (tumblr-api-url "/user/dashboard") args)))

(defun tumblr-api-user-likes (&optional limit offset)
  "Gather information about the logged in user's likes"
  (let ((args '()))
    (when limit (aput 'args "limit" limit))
    (when offset (aput 'args "offset" offset))
    (tumblr-api-http-oauth-post (tumblr-api-url "/user/likes") args)))

(defun tumblr-api-user-following (&optional limit offset)
  "Gather information about which blogs are followed by the logged-in user"
  (let ((args '()))
    (when limit (aput 'args "limit" limit))
    (when offset (aput 'args "offset" offset))
    (tumblr-api-http-oauth-post (tumblr-api-url "/user/following") args)))

(defun tumblr-api-user-follow (url)
  "Follow the given blog URL."
  (tumblr-api-http-oauth-post (tumblr-api-url "/user/follow")
                          `(("url" . ,url))))
(defun tumblr-api-user-unfollow (url)
  "Unfollow the given blog URL."
  (tumblr-api-http-oauth-post (tumblr-api-url "/user/unfollow")
                          `(("url" . ,url))))
(defun tumblr-api-user-like (id reblog_key)
  "Like a given post"
  (tumblr-api-http-oauth-post (tumblr-api-url "/user/like")
                          `(("id" . ,id)
                            ("reblog_key" . ,reblog_key))))
(defun tumblr-api-user-unlike (id reblog_key)
  "Unlike a given post"
  (tumblr-api-http-oauth-post (tumblr-api-url "/user/unlike")
                          `(("id" . ,id)
                            ("reblog_key" . ,reblog_key))))

(defun tumblr-api-blog-info (&optional blog)
  "Gather information about the blog. If not given, this defaults to `tumblr-blog'."
  (tumblr-api-http-apikey-get
   (tumblr-api-url "/blog/" (or blog tumblr-blog) "/info")
   '()))

;; TODO This returns actual image data, not JSON!
;; (defun tumblr-api-blog-avatar (&optional blog)
;;   "Gathers info about the given blog's avatar. Defaults to `tumblr-blog'"
;;   (tumblr-api-http-noauth-get
;;    (tumblr-api-url "/blog/" (or blog tumblr-blog) "/avatar")))

(defun tumblr-api-blog-followers (&optional blog)
  "Gathers info about the blog's followers. Defaults to `tumblr-blog'.

See http://www.tumblr.com/docs/en/api/v2 for information about the returned JSON."
  (tumblr-api-http-oauth-post
   (tumblr-api-url "/blog/" (or blog tumblr-blog) "/followers") '()))



(defun tumblr-api-blog-posts (&optional blog type id tag limit offset reblog_info notes_info filter)
  "Gather info about the blog posts for the given blog (defaults to `tumblr-blog').

Type should be one of text, quote, link, answer, video, audio, photo, chat.

If given, retrieve just posts with the given attributes (args)

See http://www.tumblr.com/docs/en/api/v2 for information about
the returned JSON."
  (let ((args '()))
    (when id (aput 'args "id" id))
    (when tag (aput 'args "tag" tag))
    (when limit (aput 'args "limit" limit))
    (when offset (aput 'args "offset" offset))
    (when reblog_info (aput 'args "reblog_info" reblog_info))
    (when notes_info (aput 'args "notes_info" notes_info))
    (when filter (aput 'args "filter" filter))
    (tumblr-api-http-apikey-get
     (tumblr-api-url "/blog/"
                 (or blog tumblr-blog)
                 "/posts"
                 (if type (concat "/" type) ""))
     args)))

(defun tumblr-api-blog-queued-posts (&optional blog offset filter)
  "Retrieve queued blog posts from blog. Defaults to `tumblr-blog'."
  (let ((args '()))
    (when offset (aput 'args "offset" offset))
    (when filter (aput 'args "filter" filter))
    (tumblr-api-http-oauth-post
     (tumblr-api-url "/blog/" (or blog tumblr-blog) "/posts/queue")
     args)))

(defun tumblr-api-blog-draft-posts (&optional blog filter)
  "Retrieve draft blog posts from blog. Defaults to `tumblr-blog'."
  (let ((args '()))
    (when filter (aput 'args "filter" filter))
    (tumblr-api-http-oauth-post
     (tumblr-api-url "/blog/" (or blog tumblr-blog) "/posts/draft")
     args)))

(defun tumblr-api-blog-submission-posts (&optional blog offset filter)
  "Retrieve submission blog posts from blog. Defaults to `tumblr-blog'."
  (let ((args '()))
    (when offset (aput 'args "offset" offset))
    (when filter (aput 'args "filter" filter))
    (tumblr-api-http-oauth-post
     (tumblr-api-url "/blog/" (or blog tumblr-blog) "/posts/submission")
     args)))

(defun tumblr-api-new-post (&optional blog args)
  "Create a new post, using Tumblr's post API. Blog defaults to
`tumblr-blog' and args must be an alist of arguments to use.

If you're making a text post, for example, args should be something like
'((\"type\" . \"text\")
  (\"title\" . \"How to use the Tumblr API\")
  (\"body\" . \"...\"))"
  (tumblr-api-http-oauth-post
   (tumblr-api-url "/blog/" (or blog tumblr-blog) "/post")
   args))

(defun tumblr-api-edit-post (id &optional blog args)
  "Edit the post with the given id. args should be as in `tumblr-new-post'."
  (aput 'args "id" id)
  (tumblr-api-http-oauth-post
   (tumblr-api-url "/blog/" (or blog tumblr-blog) "/post/edit")
   args))

(defun tumblr-api-reblog-post (id reblog_key &optional blog)
  "Reblog a post with the given id and reblog key."
  (tumblr-api-http-oauth-post
   (tumblr-api-url "/blog/" (or blog tumblr-blog) "/post/reblog")
   `(("id" . ,id)
     ("reblog_key" . ,reblog_key))))

(defun tumblr-api-delete-post (id &optional blog)
  "Delete the post with the given id. args should be as in `tumblr-new-post'."
  (tumblr-api-http-oauth-post
   (tumblr-api-url "/blog/" (or blog tumblr-blog) "/post/delete")
   `(("id" . ,id))))

(defun tumblr-api-tagged (tag &optional before limit filter)
  (let ((args '()))
    (aput 'args "tag" tag)
    (when before (aput 'args "before" before))
    (when limit (aput 'args "limit" limit))
    (when filter (aput 'args "filter" filter))
    (tumblr-api-http-oauth-post
     (tumblr-api-url "/tagged")
     args)))
