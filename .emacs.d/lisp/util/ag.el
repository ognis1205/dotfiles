;;; ag.el --- interface to The Silver Searcher -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(lib/with/executable 'ag
  (when (feature-p 'helm)
    (use-package helm-ag
      :defer
      :config
      (validate-setq
       helm-ag-insert-at-point 'word)))
  (use-package ag
    :defer
    :init
    (if (and (feature-p 'projectile) (fboundp 'projectile-ag))
        (global-set-key [remap find-grep] 'projectile-ag)
      (global-set-key [remap find-grep] 'ag))
    :config
    (add-to-list 'ag-arguments "--search-zip")))

(provide 'util/ag)

;;; ag.el ends here
