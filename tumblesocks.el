;;; tumblesocks.el --- An Emacs tumblr client.

;; Copyright 2012 gcr

;; Author: gcr <gcr@sneakygcr.net>
;; URL: http://github.com/gcr/tumblesocks
;; License: zlib
;; Package-Version: 0.0.6
;; Package-Requires: ((htmlize "1.39") (oauth "1.0.3") (markdown-mode "1.8.1"))

;;; Commentary:

;; An Emacs tumblr client.

;;; Code:

(defgroup tumblesocks nil
  "Emacs tumblr client"
  :group 'applications)

(require 'tumblesocks-api)
(require 'tumblesocks-user)
(require 'tumblesocks-compose)
(require 'tumblesocks-view)

(provide 'tumblesocks)

;;; tumblesocks.el ends here
