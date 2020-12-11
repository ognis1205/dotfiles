;;; scala.el --- initializes Scala modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package scala-mode :no-require t :ensure t :defer
  :interpreter
  ("scala" . scala-mode)
  :mode
  "scala")

(use-package posframe :no-require t :ensure t :defer)

(use-package dap-mode :no-require t :ensure t :defer
  :hook
  (lsp-mode-hook . lsp-lens-mode)
  (lsp-mode-hook . dap-mode)
  (lsp-mode-hook . dap-ui-mode))

(use-package lsp-metals :no-require t :ensure t :defer
  :preface
  (defconst *mode/scala/lsp-trace-json*
    (lib/path/join *user-lib-cache-directory* "lsp.trace.json")
    "Path to the LSP tracing log.")

  (defconst *mode/scala/bsp-trace-json*
    (lib/path/join *user-lib-cache-directory* "bsp.trace.json")
    "Path to the BSP tracing log.")

  (defmacro mode/scala--with-bloop-server-started (&rest body)
    "Ensure that Bloop server is started."
    `(progn (unless (= 0 (call-process-shell-command "bloop about"))
	      (start-process "*user/scala/bloop*" "*bloop*" "bloop" "server"))
	    (unless (file-exists-p *mode/scala/lsp-trace-json*)
	      (make-empty-file *mode/scala/lsp-trace-json* t))
	    (unless (file-exists-p *mode/scala/bsp-trace-json*)
	      (make-empty-file *mode/scala/bsp-trace-json* t))
	    (,@body)))
  ;;TODO: implement no-wait version of build import. See lsp-send-execute-command macro.
  :ensure-system-package
  metals-emacs
  :hook
  (scala-mode-hook . (lambda () (mode/scala--with-bloop-server-started lsp)))
  :custom
  (lsp-metals-treeview-show-when-views-received nil))

(provide 'mode/scala)

;;; scala.el ends here
