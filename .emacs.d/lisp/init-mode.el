;;; init-mode.el --- initializes major modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(lib/bootstrap/load-all-files-from-dir (lib/path/join *user-emacs-lisp-directory* "mode"))

(provide 'init-mode)

;;; init-mode.el ends here
