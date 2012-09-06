;; HOW TO AUTHORIZE:
;; (setq tumblr-token
;;       (oauth-authorize-app
;;        tumblr-consumer-key
;;        tumblr-secret-key
;;        "https://www.tumblr.com/oauth/request_token"
;;        "https://www.tumblr.com/oauth/access_token"
;;        "https://www.tumblr.com/oauth/authorize"))

;; so the callback will send you to a page; give me back the
;; oauth_verifier URL ID.
;; so hopefully that went well ... ... ...

(defcustom tumblr-consumer-key nil
  "Your tumblr app's consumer API key"
  :type 'string)

(defcustom tumblr-secret-key nil
  "Your tumbler app's secret key"
  :type 'string)

(defcustom tumblr-blog nil
  "Your blog name, like xxx.tumblr.com"
  :type 'string)



(defun tumblr-url (&rest args)
  (apply 'concat "https://api.tumblr.com/v2" args))

(defun tumblr-http-noauth-get (url)
  "Post to an unauthenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (with-current-buffer (url-retrieve-synchronously url)
    (tumblr-process-response)))

(defun tumblr-http-apikey-get (url params)
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
    (tumblr-process-response)))

(defun tumblr-http-oauth-post (url params)
  "Post to an OAuth-authenticated Tumblr API endpoint (url),
using the given POST parameters (params, an alist).

This function will return the response as JSON, or will signal an
error if the error code is not in the 200 category."
  (with-current-buffer (oauth-post-url tumblr-token url params)
    (tumblr-process-response)))

(defun tumblr-process-response ()
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
    (if (not (and (<= 200 code) (<= code 299)))
        (error (buffer-substring pointpos
                                 (line-end-position)))
      (search-forward-regexp "^$" nil t)
      (let ((json-response (buffer-substring (1+ (point)) (point-max))))
        (json-read-from-string json-response)))))

(defun tumblr-user-info ()
  "Gather information about the logged in user"
  (tumblr-http-oauth-post (tumblr-url "/user/info") '()))

(defun tumblr-user-dashboard (&optional limit offset type since_id reblog_info notes_info)
  "Gather information about the logged in user's dashboard"
  (let ((args '()))
    (when limit (aput 'args "limit" limit))
    (when offset (aput 'args "offset" offset))
    (when type (aput 'args "type" type))
    (when since_id (aput 'args "since_id" since_id))
    (when reblog_info (aput 'args "reblog_info" reblog_info))
    (when notes_info (aput 'args "notes_info" notes_info))
    (tumblr-http-oauth-post (tumblr-url "/user/dashboard") args)))

(defun tumblr-user-likes (&optional limit offset)
  "Gather information about the logged in user's likes"
  (let ((args '()))
    (when limit (aput 'args "limit" limit))
    (when offset (aput 'args "offset" offset))
    (tumblr-http-oauth-post (tumblr-url "/user/likes") args)))

(defun tumblr-user-following (&optional limit offset)
  "Gather information about which blogs are followed by the logged-in user"
  (let ((args '()))
    (when limit (aput 'args "limit" limit))
    (when offset (aput 'args "offset" offset))
    (tumblr-http-oauth-post (tumblr-url "/user/following") args)))

(defun tumblr-user-follow (url)
  "Follow the given blog URL."
  (tumblr-http-oauth-post (tumblr-url "/user/follow")
                          `(("url" . ,url))))
(defun tumblr-user-unfollow (url)
  "Unfollow the given blog URL."
  (tumblr-http-oauth-post (tumblr-url "/user/unfollow")
                          `(("url" . ,url))))
(defun tumblr-user-like (id reblog_key)
  "Like a given post"
  (tumblr-http-oauth-post (tumblr-url "/user/like")
                          `(("id" . ,id)
                            ("reblog_key" . ,reblog_key))))
(defun tumblr-user-unlike (id reblog_key)
  "Unlike a given post"
  (tumblr-http-oauth-post (tumblr-url "/user/unlike")
                          `(("id" . ,id)
                            ("reblog_key" . ,reblog_key))))

(defun tumblr-blog-info (&optional blog)
  "Gather information about the blog. If not given, this defaults to `tumblr-blog'."
  (tumblr-http-apikey-get
   (tumblr-url "/blog/" (or blog tumblr-blog) "/info")
   '()))

;; TODO This returns actual image data, not JSON!
;; (defun tumblr-blog-avatar (&optional blog)
;;   "Gathers info about the given blog's avatar. Defaults to `tumblr-blog'"
;;   (tumblr-http-noauth-get
;;    (tumblr-url "/blog/" (or blog tumblr-blog) "/avatar")))

(defun tumblr-blog-followers (&optional blog)
  "Gathers info about the blog's followers. Defaults to `tumblr-blog'.

See http://www.tumblr.com/docs/en/api/v2 for information about the returned JSON."
  (tumblr-http-oauth-post
   (tumblr-url "/blog/" (or blog tumblr-blog) "/followers") '()))



(defun tumblr-blog-posts (&optional blog type id tag limit offset reblog_info notes_info filter)
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
    (tumblr-http-apikey-get
     (tumblr-url "/blog/"
                 (or blog tumblr-blog)
                 "/posts"
                 (if type (concat "/" type) ""))
     args)))

(defun tumblr-blog-queued-posts (&optional blog offset filter)
  "Retrieve queued blog posts from blog. Defaults to `tumblr-blog'."
  (let ((args '()))
    (when offset (aput 'args "offset" offset))
    (when filter (aput 'args "filter" filter))
    (tumblr-http-oauth-post
     (tumblr-url "/blog/" (or blog tumblr-blog) "/posts/queue")
     args)))

(defun tumblr-blog-draft-posts (&optional blog filter)
  "Retrieve draft blog posts from blog. Defaults to `tumblr-blog'."
  (let ((args '()))
    (when filter (aput 'args "filter" filter))
    (tumblr-http-oauth-post
     (tumblr-url "/blog/" (or blog tumblr-blog) "/posts/draft")
     args)))

(defun tumblr-blog-submission-posts (&optional blog offset filter)
  "Retrieve submission blog posts from blog. Defaults to `tumblr-blog'."
  (let ((args '()))
    (when offset (aput 'args "offset" offset))
    (when filter (aput 'args "filter" filter))
    (tumblr-http-oauth-post
     (tumblr-url "/blog/" (or blog tumblr-blog) "/posts/submission")
     args)))

(defun tumblr-new-post (&optional blog args)
  "Create a new post, using Tumblr's post API. Blog defaults to
`tumblr-blog' and args must be an alist of arguments to use."
  (tumblr-http-oauth-post
   (tumblr-url "/blog/" (or blog tumblr-blog) "/post")
   args))

(defun tumblr-new-text-post (title body &optional blog args)
  "Create a new text post using title and body."
  (aput 'args "type" "text")
  (aput 'args "title" title)
  (aput 'args "body" body)
  (tumblr-new-post blog args))
(defun tumblr-new-quote-post (quote &optional source blog args)
  "Create a new quote post."
  (aput 'args "type" "quote")
  (aput 'args "quote" quote)
  (when source (aput 'args "source" source))
  (tumblr-new-post blog args))
(defun tumblr-new-link-post (url &optional title description blog args)
  "Create a new link post."
  (aput 'args "type" "link")
  (aput 'args "url" url)
  (when title (aput 'args "title" title))
  (when description (aput 'args "description" description))
  (tumblr-new-post blog args))
(defun tumblr-new-chat-post (conversation &optional title blog args)
  "Create a new chat post."
  (aput 'args "type" "chat")
  (aput 'args "conversation" conversation)
  (when title (aput 'args "title" title))
  (tumblr-new-post blog args))

;; TODO: make photo posts
;; TODO: make audio posts
;; TODO: make video posts

;; TODO: edit posts
;; TODO: reblog posts
;; TODO: delete posts

;; TODO: search for tagged posts
