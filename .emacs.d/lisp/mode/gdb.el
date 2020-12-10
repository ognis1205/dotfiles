;;; gdb.el --- Initializes GDB script mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mode/gdb--mode-config ()
  "Initialize GDB script mode."
  (add-auto-mode 'gdb-script-mode "\\.gdb$" "\\.gdbinit$"))

(lib/with/executable 'gdb (mode/gdb--mode-config))

(provide 'mode/gdb)

;;; gdb.el ends here
