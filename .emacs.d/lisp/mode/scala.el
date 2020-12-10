;;; scala.el --- initializes Scala modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user/scala/lsp-trace-json*
  (lib/path/join *user-lib-cache-directory* "lsp.trace.json")
  "Path to the LSP tracing log.")

(defconst *user/scala/bsp-trace-json*
  (lib/path/join *user-lib-cache-directory* "bsp.trace.json")
  "Path to the BSP tracing log.")

(defun user/scala--start-bloop-server ()
  "Start bloop server."
  (start-process "*user/scala/bloop*" "*bloop*" "bloop" "server"))

(defmacro user/scala--with-bloop-server-started (&rest body)
  "Ensure that Bloop server is started.  If there is one somewhere on your machine, it does nothing.
Otherwise, a server will be started as a process which appears in `list-processes' and whose logs
appear in '*bloop-server*' buffer."
  `(progn (unless (= 0 (call-process-shell-command "bloop about"))
	    (user/scala--start-bloop-server))
	  (unless (file-exists-p *user/scala/lsp-trace-json*)
	    (make-empty-file *user/scala/lsp-trace-json* t))
	  (unless (file-exists-p *user/scala/bsp-trace-json*)
	    (make-empty-file *user/scala/bsp-trace-json* t))
	  (,@body)))

;;TODO: implement no-wait version of build import
;;(defun user/lsp-metals/build-import ()
;;  "Unconditionally run `sbt bloopInstall` and re-connect to the build server."
;;  (interactive)
;;  (lsp-send-execute-command "build-import" ()))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :config
  (use-package lsp-metals
    :if
    (executable-find "metals-emacs")
    :config
    (setq lsp-metals-treeview-show-when-views-received nil)
    :hook
    (scala-mode-hook . (lambda () (user/scala--with-bloop-server-started lsp))))
  (use-package posframe)
  (use-package dap-mode
    :hook
    (lsp-mode . lsp-lens-mode)
    (lsp-mode . dap-mode)
    (lsp-mode . dap-ui-mode)))

(provide 'mode/scala)

;;; scala.el ends here
