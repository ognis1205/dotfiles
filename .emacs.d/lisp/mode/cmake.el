;;; cmake.el --- Initializes CMake mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cmake-mode :no-require t :ensure t
  ;; Package Loading Section.
  ;;:disabled
  :defer
  ;;:demand

  ;; Package Dependency Section.
  :ensure-system-package
  cmake
  ;;:load-path
  ;;:quelpa
  ;;:pin
  ;;:requires
  ;;:after

  ;; Package Mode Section.
  ;;:delight
  ;;:interpreter
  ;;:hook
  ;;:magic
  ;;:magic-fallback
  :mode
  "\(CMakeLists\.txt|\.cmake\)$"

  ;; Package Command Section.
  ;;:commands
  ;;:bind
  ;;:bind-key

  ;; Package Custom Section.
  ;;:custom
  ;;:custom-face

  ;; Package Configuration Section.
  ;;:if :when :unless
  ;;:defines
  :functions
  indent-tabs-mode
  ;;:preface
  ;;:init
  :config
  (subword-mode t)
  (setq indent-tabs-mode nil))

(use-package cmake-font-lock :no-require t :ensure t
  ;; Package Loading Section.
  ;;:disabled
  :defer
  ;;:demand

  ;; Package Dependency Section.
  ;;:ensure-system-package
  ;;:load-path
  ;;:quelpa
  ;;:pin
  ;;:requires
  ;;:after

  ;; Package Mode Section.
  ;;:delight
  ;;:interpreter
  :hook
  (cmake-mode . cmake-font-lock-activate)
  ;;:magic
  ;;:magic-fallback
  ;;:mode

  ;; Package Command Section.
  ;;:commands
  ;;:bind
  ;;:bind-key

  ;; Package Custom Section.
  ;;:custom
  ;;:custom-face

  ;; Package Configuration Section.
  ;;:if :when :unless
  ;;:defines
  ;;:functions
  ;;:preface
  ;;:init
  ;;:config
  )


(use-package eldoc-cmake :no-require t :ensure t
  ;; Package Loading Section.
  ;;:disabled
  :defer
  ;;:demand

  ;; Package Dependency Section.
  ;;:ensure-system-package
  ;;:load-path
  ;;:quelpa
  ;;:pin
  ;;:requires
  ;;:after

  ;; Package Mode Section.
  ;;:delight
  ;;:interpreter
  :hook
  (cmake-mode . eldoc-cmake-enable)
  ;;:magic
  ;;:magic-fallback
  ;;:mode

  ;; Package Command Section.
  ;;:commands
  ;;:bind
  ;;:bind-key

  ;; Package Custom Section.
  ;;:custom
  ;;:custom-face

  ;; Package Configuration Section.
  ;;:if :when :unless
  ;;:defines
  ;;:functions
  ;;:preface
  ;;:init
  ;;:config
  )

;;(defun mode/cmake--hook ()
;;  "Initialize makefile mode."
;;  (subword-mode t)
;;  (validate-setq indent-tabs-mode nil))
;;
;;(use-package cmake-mode
;;  :if
;;  (executable-find "cmake")
;;  :defer
;;  :mode
;;  "\(CMakeLists\.txt|\.cmake\)$"
;;  :hook
;;  (cmake-mode-hook . mode/cmake--hook)
;;  :config
;;  (use-package company-cmake
;;    :after
;;    (company)
;;    :config
;;    (add-to-list 'company-backends 'company-cmake))
;;  (use-package cmake-font-lock
;;    :hook
;;    (cmake-mode-hook . cmake-font-lock-activate))
;;  (use-package eldoc-cmake
;;    :hook
;;    (cmake-mode . eldoc-cmake-enable)))

(provide 'mode/cmake)

;;; cmake.el ends here
