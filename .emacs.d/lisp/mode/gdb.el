;;; gdb.el --- Initializes GDB script mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gdb-scritp-mode
  :no-require t
  :ensure nil
  :defer
  :if
  (executable-find "gdb")
  :init
  (lib/util/add-auto-mode 'gdb-script-mode "\\.gdb$" "\\.gdbinit$"))

(provide 'mode/gdb)

;;; gdb.el ends here
