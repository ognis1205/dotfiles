;;; alert.el --- Emacs notifications. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package alert
  :no-require
  t
  :ensure
  t
  :defer
  :preface
  (defun util/alert--style ()
    "Get the preferred alert style."
    (cond
     ((eq system-type 'darwin) 'growl)
     ((executable-find "notify-send") 'libnotify)
     ((executable-find "dbus-send") 'notifications)
     (t 'mode-line)))
  :custom
  (alert-default-style (util/alert--style))
  (alert-log-messages nil))

(provide 'util/alert)

;;; alert.el ends here
