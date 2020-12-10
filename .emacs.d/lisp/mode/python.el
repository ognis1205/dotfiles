;;; python.el --- initializes Python modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(use-package python
  :if
  (executable-find "python")
  :defer
  :mode
  ("SCon\(struct\|script\)$" . python-mode)
  :interpreter
  ("python[0-9.]*" . python-mode)
  :hook
  (python-mode-hook . mode/python--hook)
  :config
  (validate-setq python-indent-guess-indent-offset nil)
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
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
  (use-package anaconda-mode)
  (use-package py-autopep8)
  (use-package pylookup
    :quelpa
    (pylookup
     :fetcher github
     :repo "tsgates/pylookup"))
  (use-package python-environment
    :config
    (validate-setq python-environment-directory (lib/path/join *user-cache-directory* "python-environment")))
  (use-package pipenv
    :if
    (executable-find "pipenv"))
  (use-package helm-pydoc)
  (use-package flycheck-pycheckers
    :after
    flycheck
    :hook
    (flycheck-mode-hook . flycheck-pycheckers-setup))
  (use-package flycheck-mypy
    :if
    (executable-find "mypy"))
  (use-package flycheck-prospector
    :if
    (executable-find "prospector")
    :hook
    (flycheck-mode-hook . flycheck-prospector-setup))
  (use-package lsp-pyright
    :if
    (executable-find "pyright-langserver")
    :hook
    (python-mode-hook . (lambda () (require 'lsp-pyright) (lsp)))))

(provide 'mode/python)

;;; python.el ends here
