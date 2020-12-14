;;; cxx.el --- initializes C/C++ modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cc-mode
  :no-require
  t
  :ensure
  t
  :defer
  :preface
  (defun mode/cxx--common-hook ()
    "Mode hook for CXX."
    (user/doxygen-mode t)
    (subword-mode t)
    (when (lib/util/feature-p 'mic-paren)
      (paren-toggle-open-paren-context t))
    (when (derived-mode-p 'c-mode 'c++-mode)
      (lib/with/feature 'cpputils-cmake
	(cppcm-reload-all))))

  (defun mode/cxx--header-file-p ()
    "Return non-nil if in a C++ header."
    (and (string-match "\\.h$"
                       (or (buffer-file-name)
			   (buffer-name)))
	 (save-excursion
	   (re-search-forward "\\_<class\\_>|\\_<namespace\\_>" nil t))))
  :hook
  (c-mode-common-hook . mode/cxx--common-hook)
  :custom
  (c-basic-offset 2)
  (c-set-offset 'case-label '+)
  (c-set-offset 'inextern-lang 0)
  :init
  (lib/util/add-magic-mode 'c++-mode 'mode/cxx--header-file-p)
  :config
  (lib/list/add-many-to-list
   'c-default-style
   '(c-mode   . "K&R")
   '(c++-mode . "Stroustrup")))

(use-package auto-complete-c-headers
  :no-require
  t
  :ensure
  t
  :defer
  :after
  (auto-complete))

(use-package cc-vars
  :no-require
  t
  :ensure
  cc-mode
  :defer
  :custom
  (c-tab-always-indent nil)
  (c-insert-tab-function 'indent-for-tab-command))

(use-package clang-format
  :no-require
  t
  :ensure
  cc-mode
  :defer
  :if
  (executable-find "clang")
  :bind-wrap
  (:map c-mode-base-map
        ((:key :code :tidy) . clang-format-region)))

(use-package cmake-ide
  :no-require
  t
  :ensure
  cc-mode
  :defer
  :if
  (executable-find "cmake"))

(use-package company-c-headers
  :no-require
  t
  :ensure
  t
  :defer
  :after
  (company))

(use-package cpputils-cmake
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "cmake")
  :custom
  (cppcm-write-flymake-makefile nil))

(use-package flycheck-clang-analyzer
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "scan-build")
  :after
  flycheck
  :config
  (flycheck-clang-analyzer-setup))

(use-package flycheck-clangcheck
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "clang-check"))

(use-package flycheck-clang-tidy
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "clang-tidy")
  :config
  (flycheck-clang-tidy-setup))

(use-package flycheck-flawfinder
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "flawfinder")
  :config
  (flycheck-flawfinder-setup))

(use-package flycheck-pkg-config
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "pkg-config"))

(use-package function-args
  :no-require
  t
  :ensure
  t
  :defer
  :diminish
  function-args-mode
  :config
  (fa-config-default))

(use-package google-c-style
  :no-require
  t
  :ensure
  t
  :defer)

(use-package lsp-clangd
  :no-require
  t
  :ensure
  nil
  :defer
  :if
  (executable-find "clangd")
  :hook
  ((c-mode-hook    . lsp)
   (c++-mode-hook  . lsp)
   (objc-mode-hook . lsp)))

(provide 'mode/cxx)

;;; cxx.el ends here
