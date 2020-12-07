;;; alert.el --- Emacs notifications. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/alert/style ()
  "Get the preferred alert style."
  (cond
   ((eq system-type 'darwin) 'growl)
   ((executable-find "notify-send") 'libnotify)
   ((executable-find "dbus-send") 'notifications)
   (t 'mode-line)))

(use-package alert
  :defer
  :config
  (setq
   alert-default-style (user/alert/style))
  (validate-setq alert-log-messages nil))

(provide 'util/alert)

;;; alert.el ends here
