;;; java --- Describe the File -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cc-mode
  :no-require
  t
  :ensure
  t
  :defer
  :preface
  (defun mode/java--common-hook ()
    "Mode hook for Java."
    (user/doxygen-mode t)
    (subword-mode t)
    (when (lib/util/feature-p 'mic-paren)
      (paren-toggle-open-paren-context t)))
  :hook
  (java-mode-hook . mode/java--common-hook)
  :custom
  (c-basic-offset 2)
  (c-set-offset 'case-label '+)
  (c-set-offset 'inextern-lang 0)
  :init
  (setq tab-width 4)
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  (setq lsp-java-format-settings-profile "GoogleStyle")
  (setq lsp-java-format-on-type-enabled nil))

(provide 'mode/java)

;;; java ends here
