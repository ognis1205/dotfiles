;;; ya-utils.el --- Miscellaneous utilities  -*- lexical-binding: t; -*-

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

;; Miscellaneous utilities.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup ya-utils nil
  "Miscellaneous utilities."
  :prefix "ya-utils-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/ognis1205/dotfiles/.emacs.d/lisp"))

(defvar ya-utils-default-timestamp-format "%Y-%m-%d %H:%M:%S %z"
  "Default format for `ya-utils-timestamp-format'.")

;;;###autoload
(cl-defun ya-utils-get-file-name (&key (display-directory nil))
  "Return file name.
Additional slot options and values:
`:display-only-basename`: if DISPLAY-DIRECTORY set to be true,
return full path to the file, otherwise return basename of the file."
  (if (eq display-directory t)
      (buffer-file-name)
    (file-name-nondirectory (buffer-file-name))))

;;;###autoload
(defun ya-utils-get-timestamp ()
  "Return file name without directory."
  (current-time-string))

(provide 'ya-utils)

;;; ya-utils.el ends here
