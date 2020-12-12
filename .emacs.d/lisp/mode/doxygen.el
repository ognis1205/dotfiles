;;; doxygen.el --- Doxygen mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defgroup user/doxygen nil
  "Support for Doxygen documentation."
  :group 'convenience)

(defvar user/doxygen-mode nil
  "Doxygen mode enabled when non-nil.")

(make-variable-buffer-local 'user/doxygen-mode)

(defcustom user/doxygen-mode-hook nil
  "Hook to run when `user/doxygen-mode' is turned on."
  :group 'user/doxygen
  :type '(hook))

(defun user/doxygen-mode-hook ()
  "Mode hook for Doxygen."
  (lib/with/feature 'doc-mode
    (user/bindings/bind-key-local :code :document 'doc-mode-fix-tag-doc)))

(defun user/doxygen-mode (&optional arg)
  "Minor mode for Doxygen documentation, if ARG is nil the mode is toggled."
  (interactive "P")
  (setq user/doxygen-mode
        (if (null arg)
            (not user/doxygen-mode)
          (> (prefix-numeric-value arg) 0)))
  (when user/doxygen-mode
    (lib/with/feature 'doxymacs (doxymacs-mode t))
    (lib/with/feature 'doc-mode
      (doc-mode t)
      (remove-hook 'semantic-after-idle-scheduler-reparse-hooks
                   'doc-mode-check-buffer t))
    (run-hooks 'doxygen-mode-hook)))

(lib/with/executable 'doxygen
  (add-hook 'doxygen-mode-hook 'user/doxygen-mode-hook))

(use-package highlight-doxygen
  :no-require
  t
  :ensure
  t
  :defer
  :hook
  (doxygen-mode-hook . highlight-doxygen-mode))

(use-package doc-mode
  :no-require
  t
  :ensure
  t
  :defer
  :quelpa
  (doc-mode
   :fetcher github
   :repo "nschum/doc-mode")
  :diminish
  doc-mode)

(provide 'user/doxygen)

;;; doxygen.el ends here
