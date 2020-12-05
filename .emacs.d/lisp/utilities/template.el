(require 's)

(defvar my/template/directory "~/.emacs.d/templates")

(defvar my/template/config "template.conf")

(defun my/template/get-config-file-path ()
  (concat (file-name-as-directory my/template/directory) my/template/config))

(defun my/template/get-template-file-path (name)
  (concat (file-name-as-directory my/template/directory) name))

(defun my/template/get-string-from-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun my/template/eval (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun my/template/parse-config (path)
  (let ((conf (my/template/get-string-from-file path)) (list '()) (pair nil) (key "") (value ""))
    (setq conf (split-string conf "\n"))
    (dolist (line conf)
      (when (not (string-match-p "#" line))
        (setq pair (split-string line "="))
        (setq key (nth 0 pair))
        (setq value (nth 1 pair))
        (when (and (not (string= key ""))
                   (not (equal value nil)))
          (push key list)
          (push value list))))
    (reverse list)))

(defun my/template/expand-skelton (skelton)
  (let ((list '()) (key "") (value "") (index 0))
    (setq list (my/template/parse-config (my/template/get-config-file-path)))
    (while (< index (length list))
      (setq key (nth index list))
      (setq key (concat "#" key "#"))
      (setq value (nth (1+ index) list))
      (when (string-match-p key skelton)
        (if (string-match-p "(" value)
            (progn
              (setq skelton (s-replace key (my/template/eval value) skelton)))
          (setq skelton (s-replace key value skelton))))
      (setq index (+ index 2))))
  skelton)

(defun my/template/get-template (path)
  (my/template/expand-skelton (my/template/get-string-from-file path)))

(defun my/template/insert (name)
  (insert (my/template/get-template (my/template/get-template-file-path name))))

(defun my/template/atcoder-cc ()
  "Insert AtCoder template for C++ file."
  (interactive)
  (my/template/insert "competition/C++/main_atcoder.cc"))
