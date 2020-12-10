;;; hashtable.el --- utilities for working with LISP hash tables -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl-lib))

(defun lib/hashtable/to-list (hashtable)
  "Return a list that represent the `HASHTABLE'."
  (let (ll)
    (maphash (lambda (kk vv) (setq ll (cons (list kk vv) ll))) hashtable)
    ll))

(defun lib/hashtable/keys (hashtable)
  "Return a list of the keys in the `HASHTABLE'."
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hashtable)
    keys))

(cl-defmacro lib/hashtable/sort-by-value ((x hashtable) &rest body)
  "Iterates over keys in `HASHTABLE' sorted by value with keys accessible as `X' to `BODY'."
  (declare (indent defun))
  `(dolist (,x (sort (hash-table-keys ,hash-table)
                     (lambda (k1 k2)
                       (< (gethash k1 ,hash-table)
                          (gethash k2 ,hash-table)))))
     ,@body))

(provide 'lib/hashtable)

;;; hashtable.el ends here
