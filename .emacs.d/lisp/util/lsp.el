;;; lsp.el --- Language server protocol. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun util/lsp-ui--config-face ()
  (eval-when-compile (require 'lsp-ui))
  (set-face-attribute 'lsp-ui-peek-filename nil :background nil :foreground "steelblue")
  (set-face-attribute 'lsp-ui-peek-header nil :background "gray50" :foreground "white")
  (set-face-attribute 'lsp-ui-peek-list nil :background "gray10" :foreground nil)
  (set-face-attribute 'lsp-ui-peek-peek nil :background "gray10" :foreground nil)
  (set-face-attribute 'lsp-ui-peek-highlight nil :background "gray10" :foreground "brightred" :distant-foreground nil)
  (set-face-attribute 'lsp-ui-peek-selection nil :background "steelblue" :foreground "coral"))

;; TODO: implement navigatable version of lsp-ui-flycheck-list
;;(defun util/lsp-ui/navigatable-flycheck-list ())

(use-package lsp-mode
  :commands
  lsp
  :custom
  (create-lockfiles nil)
  (lsp-document-sync-method 1)
  (lsp-enable-indentation nil)
  (lsp-enable-snippet t)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-prefer-capf t)
  (lsp-prefer-flymake t)
  :bind
  ("C-l <f5>" . lsp-workspace-restart)
  ("C-l C-l"  . lsp)
  ("C-l h"    . lsp-describe-session)
  ("C-l l"    . lsp-lens-mode)
  ("C-l r"    . lsp-rename)
  ("C-l t"    . lsp-goto-type-definition)
  :hook
  ((prog-major-mode . lsp-prog-major-mode-enable)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  (use-package lsp-ui
    :commands
    lsp-ui-mode
    :after
    lsp-mode
    :custom
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header nil)
    (lsp-ui-doc-include-signature nil)
    (lsp-ui-flycheck-enable t)
    (lsp-ui-imenu-enable t)
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
    ("C-l f"   . lsp-ui-flycheck-list)
    :config
    (util/lsp-ui--config-face)))

(provide 'util/lsp)

;;; lsp.el ends here
