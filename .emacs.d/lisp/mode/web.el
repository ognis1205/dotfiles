;;; web.el --- Web development -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :no-require
  t
  :ensure
  t
  :preface
  (defun mode/web--hook ()
    "Web mode hook."
    (setq indent-tabs-mode nil)
    (lib/with/feature 'company-web (add-company-sources 'company-web-html))
    (lib/with/feature 'pandoc-mode (pandoc-mode t)))
  :hook
  (web-mode-hook . mode/web--hook)
  :mode
  (("\\.js\\'"   . web-mode)
   ("\\.jsx\\'"  . web-mode)
   ("\\.ts\\'"   . web-mode)
   ("\\.tsx\\'"  . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.vue\\'"  . web-mode)
   ("\\.json\\'" . web-mode))
  :commands
  web-mode
  :config
  (setq company-tooltip-align-annotations t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-part-face t)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package lsp-mode
  :hook
  (web-mode-hook . lsp)
  :init
  (setq lsp-enable-completion-at-point t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-on-type-formatting t)
  :commands
  lsp
  lsp-deferred)

(use-package restclient
  :no-require
  t
  :ensure
  t
  :defer)

(use-package restclient-helm
  :no-require
  t
  :ensure
  t
  :defer)

(use-package company-restclient
  :no-require
  t
  :ensure
  t
  :after
  (company)
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient)))

(use-package tern
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "npm")
  :hook
  (tern-mode-hook . user--tern-mode-hook))

(use-package company-web
  :no-require
  t
  :ensure
  t
  :after
  (company))

(use-package skewer-mode
  :no-require
  t
  :ensure
  t
  :defer)

(use-package prettier
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "prettier")
  :hook
  (after-init-hook . global-prettier-mode))

(provide 'mode/web)

;;; web.el ends here
