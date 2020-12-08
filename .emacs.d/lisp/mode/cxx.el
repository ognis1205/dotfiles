;;; cxx.el --- initializes C/C++ modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/cxx--common-hook ()
  (setq c-basic-offset 2)
  (c-set-offset 'case-label '+)
  (c-set-offset 'inextern-lang 0)
  ;; Enable Doxygen support.
  (doxygen-mode t)
  ;; Separate camel-case into separate words.
  (subword-mode t)

  (when (feature-p 'mic-paren)
    ;; Match context to open parentheses.
    (paren-toggle-open-paren-context t))

  ;; Load CEDET.
  (user--c-mode-common-cedet-hook)

  (when (and *user-cedet-ectags-enabled*
             (feature-p 'helm-etags-plus))
    ;; Automatically update tags.
    (turn-on-ctags-auto-update-mode))

  (user/gnu-global-enable)
  (user/cscope-enable)

  (when (user/auto-complete-p)
    (when (and (user/use-rtags)
               (require 'rtags-ac nil :noerror))
      (add-ac-sources 'ac-source-rtags))
    (with-feature 'auto-complete-c-headers
      (add-ac-sources 'ac-source-c-headers)))

  (when (user/company-mode-p)
    (when (and (user/use-rtags)
               (require 'company-rtags nil :noerror))
      (validate-setq rtags-completions-enabled t)
      (add-company-sources 'company-rtags))
    (with-feature 'company-c-headers
      (add-company-sources 'company-c-headers)))

  (when (derived-mode-p 'c-mode 'c++-mode)
    (with-feature 'cpputils-cmake
      ;; Enable CMake C/C++ utilities.
      (cppcm-reload-all)))

  (with-feature 'irony
    (when (member major-mode irony-supported-major-modes)
      ;; Better auto completion.
      (irony-mode t)))

  ;; Enable LSP components, if installed.
  (cond
   ((feature-p 'ccls) (ignore-errors (lsp-ccls-enable)))
   ((feature-p 'cquery) (ignore-errors (lsp-cquery-enable))))

  (when (require 'flycheck-rtags nil :noerror)
    (if (user/use-rtags)
        (flycheck-select-checker 'rtags)
      (flycheck-disable-checker 'rtags)))

  (user/smartparens-enable))

(provide 'modes/cxx)

;;; cxx.el ends here
