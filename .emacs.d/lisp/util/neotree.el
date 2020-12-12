;;; neotree.el --- Configure Emacs abbreviations -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package neotree
  :no-require
  t
  :ensure
  t
  :defer
  :preface
  ;; neotree enter hide. tips from https://github.com/jaypei/emacs-neotree/issues/77
  (defun user/neotree/open-file-hide (full-path &optional arg)
    "Open file specified with `FULL-PATH' and hiding neotree.
The description of `FULL-PATH' and `ARG' is in `neotree-enter'."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide))

  (defun user/neotree/enter-hide (&optional arg)
    "Neo-open-file-hide if the target is a file, Neo-open-dir if the target is a directory.
The description of `ARG' is in `neo-buffer--execute'."
    (interactive "P")
    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))
  :bind
  ([f8] . neotree-toggle)
  :custom
  (neo-smart-open t)
  (neo-create-file-auto-open t)
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  :custom-face
  (neo-file-link-face  ((t (:foreground nil))))
  (neo-vc-default-face ((t (:foreground nil)))))

(provide 'util/neotree)

;;; neotree.el ends here
