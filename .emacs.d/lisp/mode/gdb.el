;;; gdb.el --- Initializes GDB script mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/gdb--mode-config ()
  "Initialize GDB script mode."
  (add-auto-mode 'gdb-script-mode "\\.gdb$" "\\.gdbinit$"))

(lib/with/executable 'gdb (user/gdb--mode-config))

(provide 'mode/gdb-script)

;;; gdb-script.el ends here
