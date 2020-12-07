;;; string.el --- Emacs string functions. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defmacro lib/string/with-face (str &rest properties)
  "Print `STR' using `PROPERTIES'."
  `(propertize ,str 'face (list ,@properties)))

(defmacro lib/string/with-prefix (prefix &rest keys)
  "Make key-bind string using `PREFIX' and `KEYS'."
  `(string-join (list ,prefix ,@keys) " "))

(provide 'lib/string)

;;; string.el ends here
