;;; init-interactives.el --- initializes interactives -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/insert-template-atcoder-cc ()
  "Insert AtCoder template for C++."
  (interactive)
  (ux/template/insert "C++/atcoder.cc"))

(defun user/insert-template-atcoder-py ()
  "Insert AtCoder template for Python."
  (interactive)
  (ux/template/insert "Python/atcoder.py"))

(defun user/insert-template-use-package-el ()
  "Insert AtCoder template for Python."
  (interactive)
  (ux/template/insert "EmacsLisp/use-package-template.el"))

(provide 'init-interactives)

;;; init-interactives.el ends here
