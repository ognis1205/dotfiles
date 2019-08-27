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
(require 'use-package)
(use-package exec-path-from-shell
             :ensure t
             :config
             :disabled t
             (exec-path-from-shell-initialize))

;; Scala
(require 'scala-mode)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (hindent cython-mode haskell-mode use-package ensime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Haskell
(require 'haskell-mode)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-to-list 'auto-mode-alist '("\\.fr$" . haskell-mode))
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(defun insert-haddock-header ()
  "All Haskell source files are prefered to be started with a haddock header."
  (interactive)
  (setq cur-file (read-from-minibuffer "file name? "
                   (file-name-nondirectory (buffer-file-name))))
  (setq cur-author "Shingo OKAWA")
  (setq cur-licence "TBD")
  (setq cur-email "shingo_okawa@megagon.ai")
  (setq cur-description (read-from-minibuffer "description? "))
  (insert "{- |\n")
  (insert (format "Module      :  %s\n" cur-file))
  (insert (format "Description :  %s\n" cur-description))
  (insert (format "Copyright   :  (c) %s, %s\n" cur-author (format-time-string "%Y")))
  (insert (format "License     :  %s\n" cur-licence))
  (insert "\n")
  (insert (format "Maintainer  :  %s\n" cur-email))
  (insert (format "Stability   :  unstable | experimental | provisional | stable | frozen\n" cur-email))
  (insert (format "Portability :  portable | non-portable (<reason>)\n" cur-email))
  (insert "\n")
  (insert "Here is a longer description of this module, containing some\n")
  (insert "commentary with @some markup@.\n")
  (insert "-}\n")
)

;; Protobuf
(require 'protobuf-mode)
(setq auto-mode-alist (append '(("\\.proto" . protobuf-mode)) auto-mode-alist))

;; Cython
(require 'cython-mode)
(add-to-list 'auto-mode-alist '("\\.pyx$" . cython-mode))
