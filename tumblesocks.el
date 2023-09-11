;;; tumblesocks.el --- An Emacs tumblr client.

;; Copyright 2012 gcr
;; Copyright 2023 gargle

;; Author: gcr <gcr@sneakygcr.net>
;;         gargle <johan.laenen+codeberg@gmail.com>
;; URL: http://github.com/gcr/tumblesocks
;;      https://codeberg.org/gargle/tumblesocks
;; License: zlib

(defgroup tumblesocks nil
  "Emacs tumblr client"
  :group 'applications)

(require 'tumblesocks-api)
(require 'tumblesocks-user)
(require 'tumblesocks-compose)
(require 'tumblesocks-view)

(provide 'tumblesocks)
