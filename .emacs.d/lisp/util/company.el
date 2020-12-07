;;; lsp.el --- Language server protocol. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun company-config/transformers ()
  (eval-when-compile (require 'company))
  (defun sort-uppercase (candidates)
    (let (case-fold-search
          (re "\\`[[:upper:]]*\\'"))
      (sort candidates
            (lambda (s1 s2)
              (and (string-match-p re s2)
                   (not (string-match-p re s1)))))))
  (push 'sort-uppercase company-transformers))

(defun company-config/backend-with-yas ()
  (eval-when-compile (require 'company))
  (defvar company-config/enable-yas t)
  (defun backend-with-yas (backend)
    (if (or (not company-config/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar 'backend-with-yas company-backends)))

(defun company-config/sync-face ()
  (eval-when-compile (require 'company))
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
  (company-config/transformers)
  (company-config/backend-with-yas)
  (company-config/sync-face))

;;; lsp.el ends here
