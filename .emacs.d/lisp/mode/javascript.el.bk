;;; javascript.el --- initializes JavaScript modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package js
  :no-require
  t
  :ensure
  t
  :defer
  :preface
  (defun mode/js--common-hook ()
    "JavaScript common mode hook."
    ;;TODO: Fix this.
    ;;(tern-mode t))
    )
  (defun mode/js--hook ()
    "JavaScript mode hook."
    (mode/js--common-hook))
  (defun mode/js--inferior-js-mode-hook ()
    "Inferior JavaScript mode hook."
    (ansi-color-for-comint-mode-on))
  :hook
  ((javascript-mode-hook  . mode/js--hook)
   (inferior-js-mode-hook . mode/js--inferior-javascript-mode-hook)))

(use-package js2-mode
  :no-require
  t
  :ensure
  t
  :defer
  :preface
  (defun mode/js2--hook ()
    "JS2 mode hook."
    (mode/js--common-hook)
    (smart-tabs-mode t)
    (flycheck-mode t))
  :hook
  (js2-mode-hook . mode/js2--hook)
  :magic
  "#!/usr/bin/env node"
  :mode
  "\.js$"
  :custom
  (js2-enter-indents-newline t)
  (js2-auto-indent-p t)
  (js2-idle-timer-delay 0.5)
  (js2-strict-missing-semi-warning nil))

(use-package js-comint
  :no-require
  t
  :ensure
  t
  :defer
  :config
  (inferior-js-program-command
   (cond
    ((executable-find "js")
     (executable-find "js"))
    ((executable-find "node")
     (concat (executable-find "node") " --interactive"))
    (t "java org.mozilla.javascript.tools.shell.Main")))
  (setenv "NODE_NO_READLINE" "1"))

(use-package prettier-js
  :no-require
  t
  :ensure
  t
  :defer)

(use-package xref-js2
  :no-require
  t
  :ensure
  t
  :defer
  :init
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

(provide 'mode/javascript)

;;; javascript.el ends here
