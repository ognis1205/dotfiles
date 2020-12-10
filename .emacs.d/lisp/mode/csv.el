;;; csv --- initializes CSV modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/csv--mode-hook ()
  "CSV mode hook."
  (setq
   truncate-lines t)
  (turn-off-auto-fill)
  (user/whitespace-disable-style '(lines)))

(use-package csv-mode
  :defer
  :mode
  "\\.[CcTt][Ss][Vv]$"
  :init
  (add-hook 'csv-mode-hook 'user/csv--mode-hook)
  :config
  (validate-setq
   csv-separators '("," ";" "|" " " "\t")
   csv-header-lines 1))

(provide 'mode/csv)

;;; csv.el ends here
