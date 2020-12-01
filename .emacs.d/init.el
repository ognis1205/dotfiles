;;; init.el --- The ognis1205's .emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020  Shingo OKAWA

;; Author: Shingo OKAWA <shingo.okawa.g.h.c@gamil.com>
;; Keywords: internal, local

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Nobiscum Sexp.  - S-expression is with us.

;;; Code:

(if window-system
    (tool-bar-mode -1)
  (menu-bar-mode -1))

;;; Color-theme:

(custom-set-variables
 '(custom-safe-themes
   '("7f968c172d6ec46766773a8304c7570bdff45f1220d3700008a437d9529ca3e4"
     "89fc84ffb9681d9bf8c05a5642dff5f1078fd8b892e974bcfd400f17929cdead"
     "b4117b5e16a0d1d9a265cbdc0c4062f4b3f832da38316f9d65ea39f1b2dd0063"
     default)))

(defvar my/load-themes '(manoj-dark tango))
(load-theme (car my/load-themes) t)

;;; Variables:

(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(let ((default-directory (locate-user-emacs-file "./site-lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))
(load (locate-user-emacs-file "./site-lisp/site-lisp-autoloads.el") t)

;;; Font:

(defvar my/font-family "Migu 2M")
(defvar my/font-size
  (let ((size-by-hostname
         '(("MegurineUbu1410"  . 12.5)
           ("MegurineUbu1510"  . 12.5)
           ("MegurineUbu1604"  . 12.5)
           ("megbook" . 12.5)
           ("tadsan-tpx"       . 12.5)
           ("tadsan-ret.local" . 16.5))))
    (or (cdr (assoc (system-name) size-by-hostname))
        15.5)))

(when window-system
  ;; http://d.hatena.ne.jp/kitokitoki/20110502/p2
  (let ((fontset (format "%s-%.1f" my/font-family my/font-size)))
    (add-to-list 'default-frame-alist `(font . ,fontset)))
  (add-to-list 'default-frame-alist `(cursor-type . (hbar . ,(1+ (ceiling (/ my/font-size 2)))))))

;;; Environment:

;; PATH
(custom-set-variables
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables '("PATH" "TEST_SERVER" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "MANPATH" "GOROOT" "GOPATH")))

;;; Coding:

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(custom-set-variables '(uniquify-buffer-name-style 'post-forward-angle-brackets))
(show-paren-mode t)
(column-number-mode t)

(require 'dash)

;;; Packages:

;; Bind-Key

(use-package bind-key
  :config
  (bind-key  "M-ESC ESC"   'keyboard-quit)
  (bind-key  "C-S-n"       'make-frame)
  (bind-key  "C-S-w"       'delete-frame)
  (bind-key  "M-N"         'untitled-new-buffer)
  (bind-key  "C-M-S-d"     'projectile-dired)
  (bind-key  "C-c :"       'right-click-context-menu)
  (bind-key  "C-c ;"       'imenu)
  (bind-key  "C-c R"       'revert-buffer)
  (bind-key  "C-x j"       'dired-jump)
  (bind-key  "C-x C-S-e"   'pp-eval-last-sexp)
  (bind-key  "C-S-v"       'scroll-down-command)
  (bind-key  "M-o"         'swoop)
  (bind-key  "C-M-o"       'swoop-multi)
  (bind-key  "M-："        'eval-expression)
  (bind-key  "M-i"         'helm-imenu prog-mode-map)
  (bind-key  "M-ESC ："    'eval-expression)
  (bind-key  "C-M-y"       'helm-show-kill-ring)
  (bind-key  "M-<left>"    'bs-cycle-previous)
  (bind-key  "M-<right>"   'bs-cycle-next)
  (bind-key  "M-<f5>"      'compile)
  (bind-key  "<f5>"        'quickrun))

;; LSP/Company/Yasnippet

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
  :init
  (unbind-key "C-l")
  :bind
  ("C-l <f5>" . lsp-workspace-restart)
  ("C-l C-l"  . lsp)
  ("C-l h"    . lsp-describe-session)
  ("C-l l"    . lsp-lens-mode)
  ("C-l r"    . lsp-rename)
  ("C-l t"    . lsp-goto-type-definition)
  :hook
  (prog-major-mode . lsp-prog-major-mode-enable))

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
  ("C-l s"   . lsp-ui-sideline-mode))

(use-package company
  :custom
  (company-echo-delay 0)
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-transformers '(company-sort-by-backend-importance))
  (completion-ignore-case t)
  :bind
  ("C-M-c" . company-complete)
  (:map company-active-map
        ("C-i" . company-complete-selection)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ([tab] . company-complete-selection))
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :init
  (global-company-mode t)
  :config
  (defun my/sort-uppercase (candidates)
    (let (case-fold-search
          (re "\\`[[:upper:]]*\\'"))
      (sort candidates
            (lambda (s1 s2)
              (and (string-match-p re s2)
                   (not (string-match-p re s1)))))))
  (push 'my/sort-uppercase company-transformers)
  (defvar company-mode/enable-yas t)
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar 'company-mode/backend-with-yas company-backends))
  (set-face-attribute 'company-preview nil :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-preview-common nil :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-preview-search nil :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-bg nil :background "gray50")
  (set-face-attribute 'company-scrollbar-fg nil :background "steelblue")
  (set-face-attribute 'company-tooltip nil :foreground "lightcyan" :background "gray10")
  (set-face-attribute 'company-tooltip-annotation nil :foreground "linen" :background "grey10")
  (set-face-attribute 'company-tooltip-annotation-selection nil :foreground "linen" :background "steelblue")
  (set-face-attribute 'company-tooltip-common nil :foreground "lightcyan" :background "gray10")
  (set-face-attribute 'company-tooltip-common-selection nil :foreground "coral" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil :foreground "coral" :background "steelblue"))

(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
        ("C-M-i"   . yas-insert-snippet)
        ("C-x i n" . yas-new-snippet)
        ("C-x i v" . yas-visit-snippet-file))
  (:map yas-keymap
        ("<tab>" . nil)) ;; because of avoiding conflict with company keymap
  :init
  (yas-global-mode t))

;; C/C++ Mode

(use-package lsp-clangd
  :load-path
  "/usr/local/opt/llvm/bin/clangd"
  :hook
  ((c-mode c++-mode objc-mode) . (lambda () (require 'lsp-clangd) (lsp))))

(use-package cmake-mode
  :mode
  "CMakeLists.txt")

;; LSP + Flycheck + Company
;;(use-package company
;;  :custom
;;  (company-transformers '(company-sort-by-backend-importance))
;;  (company-idle-delay 0)
;;  (company-echo-delay 0)
;;  (company-minimum-prefix-length 2)
;;  (company-selection-wrap-around t)
;;  (completion-ignore-case t)
;;  :bind
;;  (("C-M-c" . company-complete))
;;  (:map company-active-map
;;	("C-n" . company-select-next)
;;	("C-p" . company-select-previous)
;;	("C-s" . company-filter-candidates)
;;	("C-i" . company-complete-selection)
;;	([tab] . company-complete-selection))
;;  (:map company-search-map
;;        ("C-n" . company-select-next)
;;        ("C-p" . company-select-previous))
;;  :init
;;  (global-company-mode t)
;;  :config
;;  (defun my/sort-uppercase (candidates)
;;    (let (case-fold-search
;;          (re "\\`[[:upper:]]*\\'"))
;;      (sort candidates
;;            (lambda (s1 s2)
;;              (and (string-match-p re s2)
;;                   (not (string-match-p re s1)))))))
;;  (push 'my/sort-uppercase company-transformers)
;;  (defvar company-mode/enable-yas t)
;;  (defun my/support-company-mode-backend-with-yas (backend)
;;    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;        backend
;;      (append (if (consp backend) backend (list backend))
;;              '(:with company-yasnippet))))
;;  (setq company-backends (mapcar #'my/support-company-mode-backend-with-yas company-backends))
;;  (set-face-attribute 'company-tooltip nil :foreground "black" :background "lightgrey")
;;  (set-face-attribute 'company-tooltip-common nil :foreground "black" :background "lightgrey")
;;  (set-face-attribute 'company-tooltip-common-selection nil :foreground "white" :background "steelblue")
;;  (set-face-attribute 'company-tooltip-selection nil :foreground "black" :background "steelblue")
;;  (set-face-attribute 'company-preview-common nil :background nil :foreground "lightgrey" :underline t)
;;  (set-face-attribute 'company-scrollbar-fg nil :background "orange")
;;  (set-face-attribute 'company-scrollbar-bg nil :background "gray40"))
;;
;;(use-package company-box :ensure t)
;;
;;(use-package company-lsp
;;  :commands company-lsp
;;  :custom
;;  (company-lsp-cache-candidates nil)
;;  (company-lsp-async t)
;;  (company-lsp-enable-recompletion t)
;;  (company-lsp-enable-snippet t)
;;  :after
;;  (:all lsp-mode lsp-ui company yasnippet)
;;  :init
;;  (push 'company-lsp company-backends))
;;
;;(use-package flycheck
;;  :init (global-flycheck-mode)
;;  :config
;;  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11"))))
;;
;;(use-package helm-lsp
;;  :commands helm-lsp-workspace-symbol)
;;
;;(use-package lsp-java
;;  :ensure t
;;  :init
;;  (setq lsp-java-vmargs
;;        (list
;;         "-noverify"
;;         "-Xmx3G"
;;         "-XX:+UseG1GC"
;;         "-XX:+UseStringDeduplication"
;;         "-javaagent:/home/torstein/.m2/repository/org/projectlombok/lombok/1.18.4/lombok-1.18.4.jar"
;;         )
;;
;;        ;; Don't organise imports on save
;;        lsp-java-save-action-organize-imports nil
;;        ;; Fetch less results from the Eclipse server
;;        lsp-java-completion-max-results 20
;;        ;; Currently (2019-04-24), dap-mode works best with Oracle
;;        ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
;;        ;; lsp-java-java-path "~/.emacs.d/oracle-jdk-12.0.1/bin/java"
;;        ;; lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java")
;;        lsp-java-java-path "/Users/shin/.jenv/versions/oracle64-11.0.9/bin/java"))
;;
;;;;(use-package lsp-java
;;;;  :custom
;;;;;;  (lsp-java-java-path (substitute-in-file-name "${JAVA_HOME}/bin/java"))
;;;;;;  (lsp-java-java-path (substitute-in-file-name "${HOME}/.jenv/versions/`jenv version-name`"))
;;;;  (lsp-java-java-path
;;;;   (expand-file-name
;;;;    (concat "~/.jenv/versions/" (shell-command-to-string "printf %s \"$(jenv version-name)\""))))
;;;;  :config
;;;;  (add-hook 'java-mode-hook 'lsp))
;;
;;(use-package lsp-metals
;;  :config
;;  (setq lsp-metals-treeview-show-when-views-received t))
;;
;;(use-package lsp-mode
;;  :commands lsp
;;  :custom
;;  (lsp-auto-guess-root nil)
;;  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
;;  :hook
;;  (c-mode . lsp)
;;  (cpp-mode . lsp)
;;  (java-mode . lsp)
;;  (python-mode . lsp)
;;  (scala-mode . lsp)
;;  (lsp-mode . lsp-lens-mode))
;;
;;(use-package lsp-treemacs
;;  :commands lsp-treemacs-errors-list
;;  :config
;;  (setq treemacs-space-between-root-nodes nil)
;;  (lsp-treemacs-sync-mode 1))
;;
;;(use-package projectile :ensure t)
;;
;;(use-package yasnippet :ensure t)

(use-package dash :ensure t)

(use-package drag-stuff :ensure t)

(use-package exec-path-from-shell
  :ensure
  t
  :config
  :disabled
  t
  (exec-path-from-shell-initialize))

(use-package cython-mode
  :mode
  "\\.pyx$")

(use-package expand-region :ensure t)

(use-package f :ensure t)

(use-package haskell-mode :ensure t)

(use-package hindent :ensure t)

(use-package htmlize :ensure t)

(use-package idle-highlight-mode :ensure t)

(use-package magit :ensure t)

(use-package multiple-cursors :ensure t)

(use-package nyan-mode :ensure t)

(use-package popwin :ensure t)

(use-package prodigy :ensure t)

(use-package s :ensure t)

(use-package sbt-mode :ensure t)

(use-package scala-mode
  :mode
  "^\w+\\.s\\(cala\\|bt\\)$")
(add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode))

(use-package smartparens :ensure t)

(use-package smex :ensure t)

(use-package typescript-mode :ensure t)

(use-package use-package :ensure t)

(use-package web-mode :ensure t)

;;; Interactives:

(use-package my-insert
  :custom
  (my-insert-directory "~/.emacs.d/templates")
  (my-insert-config-file "my-insert.conf")
  :config
  (defun my/insert-atcoder-cc ()
    "Insert AtCoder template for C++ file."
    (interactive)
    (my-insert-insert-template "competition/C++/main_atcoder.cc")))

(use-package my-utils :ensure t)

;;; init.el ends here
