;;; init-ux.el --- initializes user experience -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(lib/bootstrap/load-all-files-from-dir (lib/path/join *user-emacs-lisp-directory* "ux"))

(provide 'init-ux)

;;; init-ux.el ends here
