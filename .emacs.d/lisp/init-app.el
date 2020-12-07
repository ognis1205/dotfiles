;;; init-app.el --- initializes applications -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(lib/bootstrap/load-all-files-from-dir (lib/path/join *user-emacs-lisp-directory* "app"))

(provide 'init-app)

;;; init-app.el ends here
