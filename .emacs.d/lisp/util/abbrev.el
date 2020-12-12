;;; abbrev.el --- Configure Emacs abbreviations -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package abbrev
  :no-require
  t
  :ensure
  nil
  :defer
  :diminish
  abbrev-mode
  :custom
  (abbrev-file-name (lib/path/join *user-data-directory* "abbrev")))

(provide 'util/abbrev)

;;; abbrev.el ends here
