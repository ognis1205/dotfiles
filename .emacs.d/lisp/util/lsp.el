;;; lsp.el --- Language server protocol. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defcustom user/lsp/prefix "C-l"
  "Keyboard prefix to use for LSP commands."
  :type 'key-sequence
  :group 'user)

(defun user/lsp-ui/config-face ())
;;(defun user/lsp-ui/config-face ()
;;  (eval-when-compile (require 'lsp-ui))
;;  (set-face-attribute 'lsp-ui-peek-filename nil
;;		      :foreground (face-attribute 'font-lock-constant-face :foreground))
;;  (set-face-attribute 'lsp-ui-peek-header nil
;;		      :background (face-attribute 'highlight :background)
;;		      :foreground (face-attribute 'default :foreground))
;;  (set-face-attribute 'lsp-ui-peek-highlight nil
;;		      :background (face-attribute 'highlight :background)
;;		      :foreground (face-attribute 'highlight :foreground)
;;		      :distant-foreground (face-attribute 'highlight :foreground))
;;  (set-face-attribute 'lsp-ui-peek-selection nil
;;		      :background (face-attribute 'highlight :background)
;;		      :foreground (face-attribute 'default :foreground)))

(use-package lsp-mode
  :commands
  lsp
  :custom
  (create-lockfiles nil)
  (lsp-document-sync-method 2)
  (lsp-enable-indentation nil)
  (lsp-enable-snippet t)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-prefer-capf t)
  (lsp-prefer-flymake nil)
;;  :init
;;  (unbind-key "C-l")
  :bind
  ("C-l <f5>" . lsp-workspace-restart)
  ("C-l C-l"  . lsp)
  ("C-l h"    . lsp-describe-session)
  ("C-l l"    . lsp-lens-mode)
  ("C-l r"    . lsp-rename)
  ("C-l t"    . lsp-goto-type-definition)
  :hook
  (prog-major-mode . lsp-prog-major-mode-enable)
  :config
  (use-package lsp-ui
    :ensure
    lsp
    :commands
    lsp-ui-mode
    :after
    lsp-mode
    :custom
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header nil)
    (lsp-ui-doc-include-signature nil)
    (lsp-ui-flycheck-enable t)
    (lsp-ui-imenu-enable nil)
    (lsp-ui-peek-always-show t)
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-fontify 'always)
    (lsp-ui-peek-list-width 30)
    (lsp-ui-peek-peek-height 30)
    (lsp-ui-sideline-enable nil)
    :hook
    (lsp-mode . lsp-ui-mode)
    :bind
    ("C-l C-d" . lsp-ui-peek-find-definitions)
    ("C-l C-r" . lsp-ui-peek-find-references)
    ("C-l s"   . lsp-ui-sideline-mode)
    :config
    (user/lsp-ui/config-face)))

(provide 'util/lsp)

;;; lsp.el ends here
