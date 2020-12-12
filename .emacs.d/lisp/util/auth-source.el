;;; auth-source.el --- Configure Emacs authentication sources -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package auth-source
  :no-require
  t
  :ensure
  nil
  :defer
  :custom
  (auth-sources
   `(,(lib/path/join *user-data-directory* "authinfo.gpg")
     ,(lib/path/join *user-data-directory* "authinfo")))
  :config
  (dolist (source auth-sources)
    (when (file-exists-p source)
      (set-file-modes source #o0600)))
  (when (eq system-type 'darwin)
    (lib/list/add-many-to-list 'auth-sources
			       'macos-keychain-internet
			       'macos-keychain-generic)))

(provide 'util/auth-source)

;;; auth-source.el ends here
