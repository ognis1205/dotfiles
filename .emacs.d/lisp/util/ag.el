;;; ag.el --- interface to The Silver Searcher -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ag
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "ag")
  :init
  (if (and (lib/util/feature-p 'projectile) (fboundp 'projectile-ag))
      (global-set-key [remap find-grep] 'projectile-ag)
    (global-set-key [remap find-grep] 'ag))
  :config
  (add-to-list 'ag-arguments "--search-zip"))

(use-package helm-ag
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (and (executable-find "ag") (lib/util/feature-p 'helm))
  :custom
  (helm-ag-insert-at-point 'word))

(provide 'util/ag)

;;; ag.el ends here
