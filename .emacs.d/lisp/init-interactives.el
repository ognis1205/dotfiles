;;; init-interactives.el --- initializes interactives -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/insert-template-atcoder-cc ()
  "Insert AtCoder template for C++."
  (interactive)
  (ux/template/insert "C++/atcoder.cc"))

(defun user/insert-template-compile-flags-txt ()
  "Insert compile_flags.txt template for C++."
  (interactive)
  (ux/template/insert "C++/compile_flags.txt"))

(defun user/insert-template-atcoder-java ()
  "Insert AtCoder template for Java."
  (interactive)
  (ux/template/insert "Java/AtCoder.java"))

(defun user/insert-template-atcoder-py ()
  "Insert AtCoder template for Python."
  (interactive)
  (ux/template/insert "Python/atcoder.py"))

(defun user/insert-template-pyrightconfig-json ()
  "Insert pyrightconfig.json template for Python."
  (interactive)
  (ux/template/insert "Python/pyrightconfig.json"))

(defun user/insert-template-use-package-el ()
  "Insert use-package template for Emacs Lisp."
  (interactive)
  (ux/template/insert "EmacsLisp/use-package-template.el"))

(provide 'init-interactives)

;;; init-interactives.el ends here
