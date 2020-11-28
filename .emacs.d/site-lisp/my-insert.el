;;; my-insert.el --- Light-weight self design template insert library  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shingo OKAWA

;; Author: Shingo OKAWA <shingo.okawa.g.h.c@gmail.com>
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Light-weight self design template insert library.

;;; Code:

(eval-when-compile (require 's))

(defgroup my-insert nil
  "Light-weight self design template insert library."
  :prefix "my-insert-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/ognis1205/dotfiles/.emacs.d/lisp"))

(defcustom my-insert-directory ""
  "File path to template directory."
  :type 'string
  :group 'my-insert)

(defcustom my-insert-config-file ""
  "File name to template config properties."
  :type 'string
  :group 'my-insert)

(defun my-insert--get-config-file-path ()
  "Return my-insert absolute config file path."
  (concat (file-name-as-directory my-insert-directory) my-insert-config-file))

(defun my-insert--get-template-file-path (name)
  "Return my-insert absolute template file path to NAME."
  (concat (file-name-as-directory my-insert-directory) name))

(defun my-insert--get-string-from-file (path)
  "Return PATH's file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun my-insert--parse-config (path)
  "Parse .conf file from PATH."
  (let ((conf (my-insert--get-string-from-file path)) (list '()) (pair nil) (key "") (value ""))
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

(defun my-insert--expand-skelton (skelton)
  "Expand all keyword in SKELTON to proper information."
  (let ((list '()) (key "") (value "") (index 0))
    (setq list (my-insert--parse-config (my-insert--get-config-file-path)))
    (while (< index (length list))
      (setq key (nth index list))
      (setq key (concat "#" key "#"))
      (setq value (nth (1+ index) list))
      (when (string-match-p key skelton)
        (if (string-match-p "(" value)
            (progn
              (setq value (s-replace "(" "" value))
              (setq value (s-replace ")" "" value))
              (setq skelton (s-replace key (funcall (intern value)) skelton)))
          (setq skelton (s-replace key value skelton))))
      (setq index (+ index 2))))
  skelton)

(defun my-insert--get-template (path)
  "Expand all keywords then return it from the PATH."
  (my-insert--expand-skelton (my-insert--get-string-from-file path)))

;;;###autoload
(defun my-insert-insert-template (name)
  "Expand all keywords from the NAME then insert it to current buffer."
  (insert (my-insert--get-template (my-insert--get-template-file-path name))))

(provide 'my-insert)

;;; my-insert.el ends here
