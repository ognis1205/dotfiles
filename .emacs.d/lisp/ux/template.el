;;; template.el --- simple template insertion support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'lib/path)

(defconst *user-template-directory*
  (lib/path/join *user-data-directory* "templates")
  "Path to user's templates and its configuration file.")

(defconst *user-template-config*
  (lib/path/join *user-template-directory* "template.conf")
  "Path to user's machine-local configuration file.")

(defun ux/template--template-file-path (name)
  "Get path to the template file specified by the given `NAME'."
  (concat (file-name-as-directory *user-template-directory*) name))

(defun ux/template--buffer-string (path)
  "Get string of the template file specified by the given `PATH'."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun ux/template--eval (string)
  "Evaluate `STRING' as 'progn' form."
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun ux/template--parse-config (path)
  "Parse config file specified by the given `PATH' and return the parsed result as a list."
  (let ((conf (ux/template--buffer-string path))
        (the-list '())
        (pair nil)
        (key "")
        (value ""))
    (setq conf (split-string conf "\n"))
    (dolist (line conf)
      (when (not (string-match-p "#" line))
        (setq pair (split-string line "="))
        (setq key (nth 0 pair))
        (setq value (nth 1 pair))
        (when (and (not (string= key ""))
                   (not (equal value nil)))
          (push key the-list)
          (push value the-list))))
    (reverse the-list)))

(defun ux/template--expand (skelton)
  "Expand template `SKELTON' and return the expanded template as a string."
  (let ((the-list '())
        (key "")
        (value "")
        (index 0))
    (setq the-list (ux/template--parse-config *user-template-config*))
    (while (< index (length the-list))
      (setq key (nth index the-list))
      (setq key (concat "#" key "#"))
      (setq value (nth (1+ index) the-list))
      (when (string-match-p key skelton)
        (if (string-match-p "(" value)
            (progn
              (setq skelton (s-replace key (ux/template--eval value) skelton)))
          (setq skelton (s-replace key value skelton))))
      (setq index (+ index 2))))
  skelton)

(defun ux/template--get (path)
  "Get template file specified by the `PATH' and expand as a string."
  (ux/template--expand (ux/template--buffer-string path)))

(defun ux/template/insert (name)
  "Insert template specified by the `NAME' to the current buffer."
  (insert (ux/template--get (ux/template--template-file-path name))))

(provide 'ux/template)

;;; template.el ends here
