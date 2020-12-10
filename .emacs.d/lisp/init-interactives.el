;;; init-interactives.el --- initializes interactives -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/insert-template-atcoder-cc ()
  "Insert AtCoder template for C++."
  (interactive)
  (ux/template/insert "competition/C++/atcoder.cc"))

(defun user/insert-template-atcoder-py ()
  "Insert AtCoder template for Python."
  (interactive)
  (ux/template/insert "competition/Python/atcoder.py"))

(provide 'init-interactives)

;;; init-interactives.el ends here
