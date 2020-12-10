;;; csv --- initializes CSV modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package csv-mode :no-require t :ensure t
  ;; Package Loading Section.
  ;;:disabled
  :defer
  ;;:demand

  ;; Package Dependency Section.
  ;;:ensure-system-package
  ;;:load-path
  ;;:quelpa
  ;;:pin
  ;;:requires
  ;;:after

  ;; Package Mode Section.
  ;;:delight
  ;;:interpreter
  ;;:hook
  ;;:magic
  ;;:magic-fallback
  :mode
  "\\.[CcTt][Ss][Vv]$"

  ;; Package Command Section.
  ;;:commands
  ;;:bind
  ;;:bind-key

  ;; Package Custom Section.
  :custom
  (csv-separators '("," ";" "|" " " "\t"))
  (csv-header-lines 1)
  ;;:custom-face

  ;; Package Configuration Section.
  ;;:if :when :unless
  :defines
  truncate-lines
  ;;:functions
  ;;:preface
  ;;:init
  :config
  (setq truncate-lines t)
  (turn-off-auto-fill))

(provide 'mode/csv)

;;; csv.el ends here
