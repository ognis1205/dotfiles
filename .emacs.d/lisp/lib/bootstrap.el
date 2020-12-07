;;; bootstrap.el --- Helpers for bootstrapping Emacs. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(declare-function 'lib/path/join "path")

(defun lib/bootstrap/load-all-files-from-dir (dir)
  "Load all Emacs Lisp files in `DIR'."
  (dolist (f (directory-files dir))
    (when (and
           (file-directory-p (lib/path/join dir f))
           (not (string= "." f))
           (not (string= ".." f)))
      (load-all-files-from-dir (lib/path/join dir f)))
    (when (and
           (not (file-directory-p (lib/path/join dir f)))
           (not (string= "bootstrapper.el" f))
           (not (string= ".#" (substring f 0 2)))
           (string= ".el" (substring f (- (length f) 3))))
      (load-file (lib/path/join dir f)))))

(provide 'lib/bootstrap)

;;; bootstrap.el ends here
