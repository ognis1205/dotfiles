;;; python.el --- initializes Python modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python
  :no-require t
  :ensure t
  :defer
  :if
  (executable-find "python")
  :preface
  (defun mode/python--hook ()
    "Python mode hook."
    (eldoc-mode t)
    (subword-mode t)
    (when (lib/util/feature-p 'pyvenv)
      (pyvenv-mode t))
    (when (lib/util/feature-p 'pipenv)
      (pipenv-mode t))
    (when (lib/util/feature-p 'anaconda-mode)
      (anaconda-mode t))
    (when (lib/util/feature-p 'nose)
      (user/bindings/bind-key-local :code :test 'nosetests-all))
    (when (lib/util/feature-p 'pyvenv)
      (user/bindings/bind-key-local :code :virtual 'pyvenv-workon)))
  :interpreter
  ("python[0-9.]*" . python-mode)
  :hook
  (python-mode-hook . mode/python--hook)
  :mode
  ("SCon\(struct\|script\)$" . python-mode)
  :custom
  (python-indent-guess-indent-offset nil)
  :config
  (validate-setq 
   python-shell-interpreter 
   "python3")
  (lib/with/executable 'ipython3
    (validate-setq
     python-shell-interpreter
     "ipython3"
     python-shell-interpreter-args
     ""
     python-shell-prompt-regexp
     "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp
     "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(use-package anaconda-mode
  :no-require
  t
  :ensure
  t
  :defer)

(use-package flycheck-mypy
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "mypy"))

(use-package flycheck-prospector
  :no-require
  t
  :ensure
  t
  :defer
  :if
  (executable-find "prospector")
  :hook
  (flycheck-mode-hook . flycheck-prospector-setup))

(use-package flycheck-pycheckers
  :no-require
  t
  :ensure
  t
  :defer
  :after
  flycheck
  :hook
  (flycheck-mode-hook . flycheck-pycheckers-setup))

(use-package helm-pydoc
  :no-require
  t
  :ensure
  t
  :defer)

(use-package lsp-pyright
  :no-require
  nil
  :ensure
  t
  :defer
  :if
  (executable-find "pyright-langserver")
  :hook
  (python-mode-hook . (lambda () (require 'lsp-pyright) (lsp)))
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3")))
    
(use-package pipenv
  :if
  (executable-find "pipenv"))

(use-package pylookup
  :no-require
  t
  :ensure
  t
  :defer
  :quelpa
  (pylookup
   :fetcher github
   :repo "tsgates/pylookup"))

(use-package python-environment
  :no-require
  t
  :ensure
  t
  :defer
  :custom
  (python-environment-directory (lib/path/join *user-cache-directory* "python-environment")))

(use-package py-autopep8
  :no-require
  t
  :ensure
  t
  :defer)

(provide 'mode/python)

;;; python.el ends here
