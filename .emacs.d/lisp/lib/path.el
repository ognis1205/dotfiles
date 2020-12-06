;;; path.el --- support functions for working with paths -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun lib/path/abs-buffer ()
  "Get the current buffer absolute path."
  (file-truename (or (buffer-file-name) default-directory)))

(defun lib/path/dirname (path)
  "Get the parent directory of `PATH'."
  (file-name-directory (directory-file-name path)))

(defun lib/path/join (root &rest dirs)
  "Join paths together starting at `ROOT' and proceeding with `DIRS'.
Ex: (lib/path/join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'lib/path/join
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(provide 'lib/path)

;;; path.el ends here
