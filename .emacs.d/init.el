;;; init.el --- Emacs main initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< "27.1" emacs-version)
  ;; Workaround for deprecated parameter referenced by Helm.
  (defvar browse-url-mosaic-program "xmosaic"))

;; Bring in package.
(setq-default
  package--init-file-ensured t
  package-enable-at-startup nil)
(require 'package)

(eval-and-compile
  ;; Load Emacs init prologue.
  (load (expand-file-name "prologue.el" user-emacs-directory)))

;; Set up package management.
(require 'lib/packaging)

;; Load utilities.
(require 'lib/util)
(require 'lib/hashtable)
(require 'lib/bootstrap)
(require 'lib/osx)
(require 'lib/introspection)

;; Load full configuration.
(require 'init-util)
(require 'init-vcs)
(require 'init-mode)
(require 'init-app)
(require 'init-interactives)

;; Load Emacs init epilogue.
(load (expand-file-name "epilogue.el" user-emacs-directory))

;;; init.el ends here
