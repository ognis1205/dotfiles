;;; csv --- initializes CSV modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mode/csv--hook ()
  "CSV mode hook."
  (setq truncate-lines t)
  (turn-off-auto-fill))

(use-package csv-mode
  :defer
  :mode
  "\\.[CcTt][Ss][Vv]$"
  :init
  (add-hook 'csv-mode-hook 'mode/csv--hook)
  :config
  (validate-setq
   csv-separators '("," ";" "|" " " "\t")
   csv-header-lines 1))

(provide 'mode::csv)

;;; csv.el ends here
