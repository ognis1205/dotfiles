;;; list.el --- Emacs list utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun lib/list/add-many-to-list (the-list &rest entries)
  "Add to `THE-LIST' any specified `ENTRIES'."
  (dolist (entry entries)
    (add-to-list the-list entry))
  (eval the-list))

(defmacro lib/list/filter-form (form the-list)
  "Return list with elements for which `FORM' are non-nil in `THE-LIST'."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,the-list (when ,form (!cons it ,r)))
       (nreverse ,r))))

(defun lib/list/filter-list (condp the-list)
  "Return list with elements for which `CONDP' are non-nil in `THE-LIST'."
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) the-list)))

(defun lib/list/toggle-element (the-list element)
  "Return `THE-LIST' with `ELEMENT' removed if present or added if not present."
  (if (member element the-list)
      (lib/list/filter-form (not (eq element it)) the-list)
    (cons element the-list)))

(defun lib/list/all-asscs (asslist query)
  "A list of all values in `ASSLIST' corresponding to `QUERY' (like rassoc)."
  (cond
   ((null asslist) nil)
   (t
    (if (equal (cdr (car asslist)) query)
        (cons (car (car asslist))
              (lib/list/all-asscs (cdr asslist) query))
      (lib/list/all-asscs (cdr asslist) query)))))

(provide 'lib/list)

;;; list.el ends here
