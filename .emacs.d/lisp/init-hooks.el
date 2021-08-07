;;; init-hooks.el --- Set up hooks required during initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib/path)
(require 'lib/env)

;;; (Hooks) ;;;
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

(provide 'init-hooks)

;;; init-constants.el ends here
