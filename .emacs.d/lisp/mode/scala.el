;;; scala.el --- initializes Scala modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :config
  (use-package lsp-metals
    :if
    (executable-find "metals-emacs")
    :config
    (setq lsp-metals-treeview-show-when-views-received t)
    :hook
    (scala-mode-hook . lsp))
  (use-package posframe)
  (use-package dap-mode
    :hook
    (lsp-mode . dap-mode)
    (lsp-mode . dap-ui-mode)))

(provide 'mode/scala)

;;; scala.el ends here
