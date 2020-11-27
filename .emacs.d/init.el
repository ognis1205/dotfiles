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

;; Set and load custom-vars.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'dash)

;;ya-insert
;;(require 'ya-insert)
;;(setq ya-insert-directory "~/.emacs.d/templates")
;;(setq ya-insert-config-file "ya-insert.conf")
;;(defun insert-atcoder-cc-template ()
;;  "Insert the template for C++ file."
;;  (interactive)
;;  (ya-insert-insert-template "C++/atcoder.cc"))
;;
;;(require 'ya-utils)

(use-package bind-key :ensure t)

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
  (defun ya/sort-uppercase (candidates)
    (let (case-fold-search
          (re "\\`[[:upper:]]*\\'"))
      (sort candidates
            (lambda (s1 s2)
              (and (string-match-p re s2)
                   (not (string-match-p re s1)))))))

  (push 'ya/sort-uppercase company-transformers)
  (defvar company-mode/enable-yas t)
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package company-box :ensure t)

(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates nil)
  (company-lsp-async t)
  (company-lsp-enable-recompletion t)
  (company-lsp-enable-snippet t)
  :after
  (:all lsp-mode lsp-ui company yasnippet)
  :init
  (push 'company-lsp company-backends))

(use-package cython-mode :ensure t)

(use-package dash :ensure t)

(use-package drag-stuff :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  :disabled t
  (exec-path-from-shell-initialize))

(use-package cython-mode
  :mode "\\.pyx$")

(use-package expand-region :ensure t)

(use-package f :ensure t)

(use-package flycheck
  :init (global-flycheck-mode))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

(use-package flycheck-cask :ensure t)

(use-package haskell-mode :ensure t)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package hindent :ensure t)

(use-package htmlize :ensure t)

(use-package idle-highlight-mode :ensure t)

(use-package lsp-metals :ensure t)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
(setq treemacs-space-between-root-nodes nil)
(lsp-treemacs-sync-mode 1)

(use-package lsp-mode
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

(use-package magit :ensure t)

(use-package multiple-cursors :ensure t)

(use-package nyan-mode :ensure t)

(use-package popwin :ensure t)

(use-package prodigy :ensure t)

(use-package projectile :ensure t)

(use-package s :ensure t)

(use-package sbt-mode :ensure t)

(use-package scala-mode
  :mode "^\w+\\.s\\(cala\\|bt\\)$")
(add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode))

(use-package smartparens :ensure t)

(use-package smex :ensure t)

(use-package typescript-mode :ensure t)

(use-package use-package :ensure t)

(use-package web-mode :ensure t)

(use-package yasnippet :ensure t)


;;; init.el ends here
