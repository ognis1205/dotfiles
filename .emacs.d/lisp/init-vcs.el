;;; init-vcs.el --- initializes version control systems -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(lib/bootstrap/load-all-files-from-dir (lib/path/join *user-emacs-lisp-directory* "vcs"))

(provide 'init-vcs)

;;; init-vcs.el ends here
