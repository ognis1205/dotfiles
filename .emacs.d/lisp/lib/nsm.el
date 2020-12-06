;;; nsm.el --- Initialize Emacs networking -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib/path)

(defconst *user-url-cache-directory*
  (lib/path/join *user-cache-directory* "url")
  "Path to user's url data store.")

(defconst *user-nsm-data-directory*
  (lib/path/join *user-data-directory* "nsm")
  "Path to user's Wanderlust data store.")

(with-eval-after-load 'url
  (setq
   url-configuration-directory *user-url-cache-directory*
   url-cookie-file (lib/path/join *user-url-cache-directory* "cookies")
   url-history-file (lib/path/join *user-url-cache-directory* "history")
   url-automatic-caching t))

(make-directory *user-nsm-data-directory* t)

(with-eval-after-load 'nsm
  (setq
   nsm-settings-file
   (lib/path/join *user-nsm-data-directory* "network-security.data")))

(provide 'lib/nsm)

;;; nsm.el ends here
