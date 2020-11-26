;;; yainsert.el --- Light-weight self design template insert library  -*- lexical-binding: t; -*-

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

(require 's)

(defgroup yainsert nil
  "Light-weight self design template insert library."
  :prefix "yainsert-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/ognis1205/dotfiles/.emacs.d/lisp"))

(defcustom yainsert-directory ""
  "File path to template directory."
  :type 'string
  :group 'yainsert)

(defcustom yainsert-config-file ""
  "File name to template config properties."
  :type 'string
  :group 'yainsert)

(defun yainsert--get-config-file-path ()
  "Return yainsert absolute config file path."
  (concat (file-name-as-directory yainsert-directory) yainsert-config-file))

(defun yainsert--get-template-file-path (name)
  "Return yainsert absolute template file path to NAME."
  (concat (file-name-as-directory yainsert-directory) name))

(defun yainsert--get-string-from-file (path)
  "Return PATH's file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun yainsert--parse-config (path)
  "Parse .conf file from PATH."
  (let ((conf (yainsert--get-string-from-file path)) (list '()) (pair nil) (key "") (value ""))
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

(defun yainsert--expand-skelton (skelton)
  "Expand all keyword in SKELTON to proper information."
  (let ((list '()) (key "") (value "") (index 0))
    (setq list (yainsert--parse-config (yainsert--get-config-file-path)))
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

;;;###autoload
(defun yainsert-get-template (path)
  "Expand all keywords then return it from the PATH."
  (yainsert--expand-skelton (yainsert--get-string-from-file path)))

;;;###autoload
(defun yainsert-insert-template (name)
  "Expand all keywords from the NAME then insert it to current buffer."
  (insert (yainsert-get-template (yainsert--get-template-file-path name))))

(provide 'yainsert)

;;; yainsert.el ends here
