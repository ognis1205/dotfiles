;;; jq.el --- jq script mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package jq-mode :no-require t :ensure t :defer
  :ensure-system-package
  jq
  :mode
  "\\.jq$")

(provide 'mode/jq)

;;; jq.el ends here
