;;; terraform --- initializes Terraform modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package terraform-mode
  :no-require
  t
  :ensure
  t
  :defer
  :mode
  "\\.tf$"
  :custom
  (custom-set-variables '(terraform-indent-level 4)))

(provide 'mode/terraform)

;;; terraform.el ends here
