;;; json.el --- JSON mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :defer
  :mode
  "\\.bowerrc$"
  :config
  (use-package jq-format
    :if
    (executable-find "jq")))

(use-package jsonnet-mode)

(provide 'mode/json)

;;; json.el ends here
