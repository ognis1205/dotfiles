;; Cask
(require 'cask "/usr/local/opt/cask/cask.el")
(cask-initialize)

;; use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; General settings.
(add-to-list 'load-path "~/.emacs.d/lisp/")
(when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-inhibited t)
(setq next-line-add-newlines nil)
(setq-default tab-width 4 indent-tabs-mode nil)
(setq default-major-mode 'text-mode)
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")
(require 'use-package)
(use-package exec-path-from-shell
             :ensure t
             :config
             :disabled t
             (exec-path-from-shell-initialize))
(defun clear-screen ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; Cython
(require 'cython-mode)
(add-to-list 'auto-mode-alist '("\\.pyx$" . cython-mode))

;; Scala
(use-package scala-mode
  :mode "^\w+\\.s\\(cala\\|bt\\)$")
(add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook
  (python-mode . lsp)
  (scala-mode . lsp)
  (c-mode . lsp)
  (cpp-mode . lsp)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-clients-clangd-executable "clangd-6.0"))

(use-package flycheck
  :init (global-flycheck-mode))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  (setq lsp-ui-doc-use-webkit t)
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(setq treemacs-space-between-root-nodes nil)
(lsp-treemacs-sync-mode 1)

(use-package company
  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  :bind
  (("C-M-c" . company-complete))
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ("C-i" . company-complete-selection)
        ([tab] . company-complete-selection))
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :init
  (global-company-mode t)
  :config
  ;; lowercaseを優先にするソート
  (defun my-sort-uppercase (candidates)
    (let (case-fold-search
          (re "\\`[[:upper:]]*\\'"))
      (sort candidates
            (lambda (s1 s2)
              (and (string-match-p re s2)
                   (not (string-match-p re s1)))))))

  (push 'my-sort-uppercase company-transformers)

  ;; yasnippetとの連携
  (defvar company-mode/enable-yas t)
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; Haskell
(require 'haskell-mode)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-to-list 'auto-mode-alist '("\\.fr$" . haskell-mode))
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(custom-set-variables '(haskell-tags-on-save t))
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
(custom-set-variables '(haskell-process-type 'cabal-repl))
