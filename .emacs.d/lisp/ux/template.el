;;; template.el --- simple template insertion support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/template/atcoder-cc ()
  "Insert AtCoder template for C++ file."
  (interactive)
  (lib/template/insert "competition/C++/main_atcoder.cc"))

(provide 'util/template)

;;; template.el ends here
