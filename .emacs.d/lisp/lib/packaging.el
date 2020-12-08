;;; packaging.el --- initialize package management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib/nsm)

(lib/with/feature 'package
  (setq
   ;; Configure GNU/Emacs package repositories.
   package-archives
   '(("gnu"   . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org"   . "https://orgmode.org/elpa/"))
   package-archive-priorities
   '(("gnu"          . 15)
     ("melpa"        . 10)
     ("org"          . 5))))

(package-initialize)

(unless (and (package-installed-p 'quelpa-use-package) (package-installed-p 'validate))
  (package-refresh-contents)
  (package-install 'quelpa-use-package)
  (package-install 'validate))

(eval-when-compile
  (require 'quelpa-use-package)
  (require 'validate))

(use-package use-package
  :config
  (validate-setq
   ;; Hooks are verbatim.
   use-package-hook-name-suffix nil)

  (use-package quelpa-use-package
    :config
    (validate-setq
     ;; Only use quelpa for custom packages.
     quelpa-checkout-melpa-p nil
     ;; Only load quelpa on demand.
     quelpa-use-package-inhibit-loading-quelpa t)
    ;; Protect quelpa recipes when forcing ensure.
    (quelpa-use-package-activate-advice))

  ;; Support using keys from init-bindings by using (:key <group> <function>).
  (push :bind-wrap (cdr (member :bind use-package-keywords)))
  (push :bind*-wrap (cdr (member :bind* use-package-keywords)))
  (defun use-package-normalize-bind-wrap (name keyword args)
    (let ((arg args)
          args*)
      (while arg
        (let ((x (car arg)))
          (cond
           ;; ((:key :category :function) . COMMAND)
           ((and (consp x)
                 (consp (car x))
                 (equal (caar x) :key))
            (setq args* (nconc args*
                               (list (cons (apply 'user/bindings/get-key (cdar x))
                                     (cdar arg)))))
            (setq arg (cdr arg)))
           ;; (KEY . COMMAND)
           ((and (consp x)
                 (or (stringp (car x))
                     (vectorp (car x)))
                 (or (use-package-recognize-function (cdr x) t #'stringp)))
            (setq args* (nconc args* (list x)))
            (setq arg (cdr arg)))
           ;; Nested list.
           ((listp x)
            (setq args*
                  (nconc args* (use-package-normalize/:bind-wrap name keyword x)))
            (setq arg (cdr arg)))
           (t
            (setq args* (nconc args* (list x)))
            (setq arg (cdr arg))))))
      (use-package-normalize/:bind name keyword args*)))
  (defalias 'use-package-normalize/:bind-wrap 'use-package-normalize-bind-wrap)
  (defalias 'use-package-normalize/:bind*-wrap 'use-package-normalize-bind-wrap)
  (defun use-package-handler/:bind-wrap (name keyword arg rest state)
    (use-package-handler/:bind name keyword arg rest state))
  (defun use-package-handler/:bind*-wrap (name keyword arg rest state)
    (use-package-handler/:bind name keyword arg rest state 'bind-keys*)))

(provide 'lib/packaging)

;;; packaging.el ends here
