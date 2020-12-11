;;; cmake.el --- Initializes CMake mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cmake-mode :no-require t :ensure t :defer
  :ensure-system-package
  cmake
  :mode
  "\(CMakeLists\.txt|\.cmake\)$"
  :defines
  indent-tabs-mode
  :config
  (subword-mode t)
  (setq indent-tabs-mode nil))

(use-package cmake-font-lock :no-require t :ensure t :defer
  :hook
  (cmake-mode-hook . cmake-font-lock-activate))

(use-package eldoc-cmake :no-require t :ensure t :defer
  :hook
  (cmake-mode-hook . eldoc-cmake-enable))

(provide 'mode/cmake)

;;; cmake.el ends here
