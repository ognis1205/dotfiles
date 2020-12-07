;;; abbrev.el --- Configure Emacs abbreviations -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package abbrev
  :ensure
  nil
  :diminish
  abbrev-mode
  :config
  (validate-setq
   abbrev-file-name (lib/path/join *user-data-directory* "abbrev")))

(provide 'util/abbrev)

;;; abbrev.el ends here
