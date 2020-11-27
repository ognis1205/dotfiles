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

(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq use-dialog-box nil)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

;;; Packages:
(require 'package)
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)

(let ((default-directory (locate-user-emacs-file "./el-get")))
  (normal-top-level-add-subdirs-to-load-path))

(when (eval-when-compile (version< emacs-version "27"))
  (package-initialize))

;;; early-init.el ends here
