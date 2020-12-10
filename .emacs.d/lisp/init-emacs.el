;;; init-emacs.el --- initializes basic Emacs settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-custom-file* (lib/path/join *user-data-directory* "custom.el"))

;; Improve init performance.
(setq gc-cons-threshold (* 128 1024 1024))
(run-with-idle-timer
 2 nil
 (lambda ()
   (validate-setq
    gc-cons-threshold (* 50 1024 1024)
    gc-cons-percentage 0.5)))

(setq read-process-output-max (* 4 1024 1024))

;; Create data and cache directories
(make-directory *user-cache-directory* t)
(make-directory *user-data-directory* t)

(setq
 ;; Lines of history in the message buffer.
 message-log-max 10000
 ;; Path to custom-file
 custom-file *user-custom-file*)

(provide 'init-emacs)

;;; init-emacs.el ends here
