;;; csv --- initializes CSV modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package csv-mode
  :no-require
  t
  :ensure
  t
  :defer
  :mode
  "\\.[CcTt][Ss][Vv]$"
  :custom
  (csv-separators '("," ";" "|" " " "\t"))
  (csv-header-lines 1)
  :defines
  truncate-lines
  :config
  (setq truncate-lines t)
  (turn-off-auto-fill))

(provide 'mode/csv)

;;; csv.el ends here
