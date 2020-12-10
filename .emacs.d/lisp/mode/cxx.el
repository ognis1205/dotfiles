;;; cxx.el --- initializes C/C++ modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mode/cxx--common-hook ()
  "Mode hook for CXX."
  (setq c-basic-offset 2)
  (c-set-offset 'case-label '+)
  (c-set-offset 'inextern-lang 0)
  (user/doxygen-mode t)
  (subword-mode t)
  (when (lib/util/feature-p 'mic-paren) (paren-toggle-open-paren-context t))
  (when (derived-mode-p 'c-mode 'c++-mode)
    (lib/with/feature 'cpputils-cmake
      (cppcm-reload-all))))

(defun mode/cxx--header-file-p ()
  "Return non-nil if in a C++ header."
  (and (string-match "\\.h$"
                   (or (buffer-file-name)
                      (buffer-name)))
     (save-excursion
       (re-search-forward "\\_<class\\_>" nil t))))

(use-package cc-mode
  :defer
  :hook
  (c-mode-common-hook . mode/cxx--common-hook)
  :init
  (lib/util/add-magic-mode 'c++-mode 'mode/cxx--header-file-p)
  :config
  (lib/list/add-many-to-list
   'c-default-style
   '(c-mode . "K&R")
   '(c++-mode . "Stroustrup"))
  (use-package cc-vars
    :ensure
    cc-mode
    :config
    (validate-setq
     c-tab-always-indent nil
     c-insert-tab-function 'indent-for-tab-command))
  (use-package auto-complete-c-headers
    :after
    (auto-complete))
  (use-package company-c-headers
    :after
    (company))
  (use-package flycheck-pkg-config
    :if
    (executable-find "pkg-config"))
  (use-package cpputils-cmake
    :if
    (executable-find "cmake")
    :config
    (validate-setq cppcm-write-flymake-makefile nil))
  (use-package cmake-ide
    :if
    (executable-find "cmake"))
  (use-package clang-format
    :if
    (executable-find "clang")
    :bind-wrap
    (:map c-mode-base-map
          ((:key :code :tidy) . clang-format-region)))
  (use-package flycheck-clang-analyzer
    :after
    flycheck
    :if
    (executable-find "scan-build")
    :config
    (flycheck-clang-analyzer-setup))
  (use-package flycheck-clang-tidy
    :if
    (executable-find "clang-tidy")
    :config
    (flycheck-clang-tidy-setup))
  (use-package flycheck-clangcheck
    :if
    (executable-find "clang-check"))
  (use-package function-args
    :diminish
    function-args-mode
    :config
    (fa-config-default))
  (use-package flycheck-flawfinder
    :if
    (executable-find "flawfinder")
    :config
    (flycheck-flawfinder-setup))
  (use-package google-c-style)
  (use-package lsp-clangd
    :if
    (executable-find "clangd")
    :hook
    ((c-mode--hook   . lsp)
     (c++-mode-hook  . lsp)
     (objc-mode-hook . lsp))))

(provide 'mode/cxx)

;;; cxx.el ends here
