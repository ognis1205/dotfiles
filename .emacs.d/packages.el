;;; use-packages.el --- The ognis1205's .emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives
   '(("gnu"          . "https://elpa.gnu.org/packages/")
     ("melpa stable" . "https://stable.melpa.org/packages/")
     ("melpa"        . "https://melpa.org/packages/")
     ("org"          . "https://orgmode.org/elpa/"))))

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(when window-system
  (exec-path-from-shell-initialize))

;; Packages
(use-package abbrev :ensure nil)
(use-package ag :ensure t)
(use-package alert :ensure t)
(use-package auth-source :ensure nil)
(use-package benchmark-init :ensure t)
(use-package bind-key :ensure t)
(use-package bm :ensure t)
(use-package bookmark :ensure t)
;;(use-package bookmark+ :ensure nil)
;;(use-package bookmark+-bmu :ensure nil)
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
(use-package google-c-style :ensure t)
(use-package haskell-mode :ensure t)
(use-package helm :ensure t)
(use-package helm-adaptive :ensure nil)
(use-package helm-ag :ensure t)
(use-package helm-bookmark :ensure nil)
(use-package helm-buffers :ensure nil)
(use-package helm-build-command :ensure nil)
(use-package helm-command :ensure nil)
(use-package helm-descbinds :ensure t)
(use-package helm-eval :ensure nil)
(use-package helm-external :ensure nil)
(use-package helm-files :ensure nil)
(use-package helm-icons :ensure t)
(use-package helm-imenu :ensure nil)
(use-package helm-lsp :ensure t)
(use-package helm-misc :ensure nil)
(use-package helm-pages :ensure t)
(use-package helm-ring :ensure nil)
(use-package helm-swoop :ensure t)
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
(use-package pcache :ensure t)
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

;;; use-packages.el ends here
