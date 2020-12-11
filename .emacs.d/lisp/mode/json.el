;;; json.el --- JSON mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package json-mode :no-require t :ensure t :defer
  :mode
  "\\.bowerrc$")

(use-package jq-format :no-require t :ensure t :defer
  :ensure-system-package
  jq)

(use-package jsonnet-mode :no-require t :ensure t :defer)

(provide 'mode/json)

;;; json.el ends here
