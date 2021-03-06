;;; buffers.el --- Configure Emacs buffers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/buffers/kill-matching-buffers (regexp &optional exclude-p)
  "Kill buffers whose name match the specified `REGEXP' but not `EXCLUDE-P'."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (/= (aref name 0) ?\s)
                 (string-match regexp name))
        (unless (and exclude-p (funcall exclude-p buffer))
          (kill-buffer buffer))))))

(defun user/buffers/kill-all-buffers ()
  "Close all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (switch-to-buffer "*scratch*"))

(defun user/buffers/kill-other-buffers ()
  "Close all open buffers except current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun ux/buffers--config ()
  "Initialize Emacs buffers."
  (use-package uniquify
    :ensure nil
    :config
    (validate-setq
     ;; Set up uniquify's style.
     uniquify-buffer-name-style 'reverse
     uniquify-separator " • "
     uniquify-after-kill-buffer-p t
     uniquify-ignore-buffers-re "^\\*"))
  ;; Bindings
  (user/bindings/bind-key-global :basic :open-file 'find-file)
  (user/bindings/bind-key-global :basic :open-buffer 'switch-to-buffer)
  (user/bindings/bind-key-global :basic :save 'save-buffer)
  (user/bindings/bind-key-global :basic :save-as 'write-file)
  (user/bindings/bind-key-global :basic :close 'kill-buffer)
  (user/bindings/bind-key-global :basic :quit 'save-buffers-kill-terminal))

(ux/buffers--config)

(provide 'ux/buffers)

;;; buffers.el ends here
