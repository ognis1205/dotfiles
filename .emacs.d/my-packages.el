;;; init.el --- The ognis1205's .emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020  Shingo OKAWA

;; Author: Shingo OKAWA <shingo.okawa.g.h.c@gmail.com>
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

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

(when window-system
  (exec-path-from-shell-initialize))

;;; Packages:

(use-package bind-key :ensure t)
(use-package cmake-mode :ensure t)
(use-package color :ensure t)
(use-package company :ensure t)
(use-package company-box :ensure t)
(use-package company-lsp :ensure t)
(use-package cython-mode :ensure t)
(use-package cython-mode :ensure t)
(use-package dash :ensure t)
(use-package drag-stuff :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package expand-region :ensure t)
(use-package f :ensure t)
(use-package flycheck :ensure t)
(use-package haskell-mode :ensure t)
(use-package helm-lsp :ensure t)
(use-package hindent :ensure t)
(use-package htmlize :ensure t)
(use-package idle-highlight-mode :ensure t)
(use-package lsp-java :ensure t)
(use-package lsp-metals :ensure t)
(use-package lsp-mode :ensure t)
(use-package lsp-treemacs :ensure t)
(use-package lsp-ui :ensure t)
(use-package magit :ensure t)
(use-package multiple-cursors :ensure t)
(use-package nyan-mode :ensure t)
(use-package popwin :ensure t)
(use-package prodigy :ensure t)
(use-package projectile :ensure t)
(use-package s :ensure t)
(use-package sbt-mode :ensure t)
(use-package scala-mode :ensure t)
(use-package smartparens :ensure t)
(use-package smex :ensure t)
(use-package typescript-mode :ensure t)
(use-package use-package :ensure t)
(use-package web-mode :ensure t)
(use-package yasnippet :ensure t)

;;; my-packages.el ends here
