;;; template.el --- template expansion functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(defconst *user-template-directory*
  (path-join *user-data-directory* "templates")
  "Path to user's templates and its configuration file.")

(defconst *user-template-config*
  (path-join *user-template-directory* ".template.conf")
  "Path to user's machine-local configuration file.")

(defun lib/template/config-file-path ()
  "Get path to the configuration file of this lib/template."
  (concat (file-name-as-directory *user-template-directory*) *user-template-config*))

(defun lib/template/template-file-path (name)
  "Get path to the template file specified by the given `NAME'."
  (concat (file-name-as-directory *user-template-directory*) name))

(defun lib/template/buffer-string (path)
  "Get string of the template file specified by the given `PATH'."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun lib/template/eval (string)
  "Evaluate `STRING' as 'progn' form."
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun lib/template/parse-config (path)
  "Parse config file specified by the given `PATH' and return the parsed result as a list."
  (let ((conf (lib/template/buffer-string path))
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

(defun lib/template/expand (skelton)
  "Expand template `SKELTON' and return the expanded template as a string."
  (let ((the-list '())
        (key "")
        (value "")
        (index 0))
    (setq the-list (lib/template/parse-config (lib/template/config-file-path)))
    (while (< index (length the-list))
      (setq key (nth index the-list))
      (setq key (concat "#" key "#"))
      (setq value (nth (1+ index) the-list))
      (when (string-match-p key skelton)
        (if (string-match-p "(" value)
            (progn
              (setq skelton (s-replace key (lib/template/eval value) skelton)))
          (setq skelton (s-replace key value skelton))))
      (setq index (+ index 2))))
  skelton)

(defun lib/template/get (path)
  "Get template file specified by the `PATH' and expand as a string."
  (my/template/expand (lib/template/buffer-string path)))

(defun lib/template/insert (name)
  "Insert template specified by the `NAME' to the current buffer."
  (insert (lib/template/get (lib/template/template-file-path name))))

;;(defun user/template/atcoder-cc ()
;;  "Insert AtCoder template for C++ file."
;;  (interactive)
;;  (lib/template/insert "competition/C++/main_atcoder.cc"))

(provide 'lib/template)

;;; template.el ends here
