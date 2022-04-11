;;; glsl.el --- Initializes GLSL mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package glsl-mode
  :no-require
  t
  :ensure
  t
  :defer
  :mode
  "\\(\\.glsl|\\.vert|\\.frag||\\.geom\\)$")

(use-package company-glsl
  :if
  (executable-find "glslangValidator")
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl)))

(provide 'mode/glsl)

;;; glsl ends here
