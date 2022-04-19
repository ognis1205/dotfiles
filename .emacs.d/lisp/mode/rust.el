;;; rust.el --- Initialize Rust mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :no-require
  t
  :ensure
  t
  :defer
  :custom
  rust-format-on-save t)

(use-package cargo
  :no-require
  t
  :ensure
  t
  :hook
  (rust-mode . cargo-minor-mode))

(provide 'mode/rust)

;;; rust ends here
