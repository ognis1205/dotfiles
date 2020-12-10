;;; cmake.el --- Initializes CMake mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mode/cmake--hook ()
  "Initialize makefile mode."
  (subword-mode t)
  (validate-setq indent-tabs-mode nil))

(use-package cmake-mode
  :if
  (executable-find "cmake")
  :defer
  :mode
  "\(CMakeLists\.txt|\.cmake\)$"
  :hook
  (cmake-mode-hook . mode/cmake--hook)
  :config
  (use-package company-cmake
    :after
    (company)
    :config
    (add-to-list 'company-backends 'company-cmake))
  (use-package cmake-font-lock
    :hook
    (cmake-mode-hook . cmake-font-lock-activate))
  (use-package eldoc-cmake
    :hook
    (cmake-mode . eldoc-cmake-enable)))

(provide 'mode/cmake)

;;; cmake.el ends here
