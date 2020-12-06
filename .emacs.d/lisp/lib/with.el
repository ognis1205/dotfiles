;;; with.el --- conditional eval wrappers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defmacro lib/with/feature (feature &rest body)
  "If `FEATURE' is available, load it and evaluate `BODY'."
  (declare (indent defun))
  `(when (require ,feature nil :noerror)
     ,@body))

(defmacro lib/with/function (function &rest body)
  "If `FUNCTION' is available, evaluate `BODY'."
  (declare (indent defun))
  `(when (functionp ,function)
     ,@body))

(defmacro lib/with/executable (executable &rest body)
  "If `EXECUTABLE' is available in path, evaluate `BODY'."
  (declare (indent defun))
  `(when (executable-find (symbol-name ,executable))
     ,@body))

(defmacro lib/with/any-executable (executables &rest body)
  "If any of `EXECUTABLES' are available in the path, evaluate `BODY'."
  (declare (indent defun))
  `(when (some (lambda (x) (executable-find (symbol-name x))) ,executables)
     ,@body))

(provide 'lib/with)

;;; with.el ends here
