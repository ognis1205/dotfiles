;;; helm.el --- improved Emacs control -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun util/helm--apropos ()
  "A context-aware helm apropos."
  (interactive)
  (let ((buffer-name "*helm-apropos*"))
    (cond
     ((derived-mode-p 'emacs-lisp-mode) (helm-apropos))
     ((derived-mode-p 'sh-mode) (helm-other-buffer '(helm-source-man-pages helm-source-info-pages) buffer-name))
     ((derived-mode-p 'c-mode-common) (helm-other-buffer '(helm-source-man-pages) buffer-name))
     ((derived-mode-p 'python-mode) (helm-pydoc))
     (t (message (format "Apropos is unavailable for %S" major-mode))))))

(defun util/helm--navigate ()
  "A context-aware helm navigation aid."
  (interactive)
  (cond
     ((derived-mode-p 'prog-mode) (util/helm--navigate-prog))
     (t (util/helm--navigate-generic))))

(defun util/helm--navigate-prog ()
  "A context-aware helm for programming modes."
  (interactive)
  (let ((helm-sources '(helm-source-buffers-list))
        (current-file (or (buffer-file-name) default-directory)))
    (lib/with/feature 'helm-misc
      (add-to-list 'helm-sources 'helm-source-fixme)
      (add-to-list 'helm-sources 'helm-source-emacs-source-defun)
      (add-to-list 'helm-sources 'helm-source-emacs-lisp-expectations)
      (add-to-list 'helm-sources 'helm-source-emacs-lisp-toplevels))
    (with-project project current-file (add-to-list 'helm-sources 'helm-source-bookmarks))
    (helm-other-buffer helm-sources "*helm-navigate-prog*")))

(defun util/helm--navigate-generic ()
  "A somewhat context-aware generic helm."
  (interactive)
  (condition-case nil
      (let ((helm-sources '(helm-source-buffers-list
                            helm-source-recentf
                            helm-source-file-name-history
                            helm-source-file-cache
                            helm-source-buffer-not-found
                            helm-source-man-pages
                            helm-source-info-pages)))
        (cond
         ((eq system-type 'darwin)
          (progn
            (add-to-list 'helm-sources 'helm-source-mac-spotlight)))
         ((eq system-type 'gnu/linux)
          (progn
            (add-to-list 'helm-sources 'helm-source-tracker-search))))
        (helm-other-buffer helm-sources "*helm-navigate-generic*"))
    (error (helm-mini))))

(defun util/helm--mode-hook ()
  "Start helm-mode."
  (helm-mode t)
  (lib/with/feature 'helm-descbinds (helm-descbinds-mode t))
  (dolist (pattern
           (list "\\*clang-complete" "\\*CEDET global" "\\*tramp/scpc" "\\*epc con" "\\*Pymacs" "\\*Completions\\*"))
    (add-to-list 'helm-boring-buffer-regexp-list pattern))
  (dolist (pattern
           (list "\\.elc$" "\\.pyc$" "^#.+#$" "^G[R]TAGS$" "^GPATH$" "^ID$"))
    (add-to-list 'helm-boring-file-regexp-list pattern)))

(use-package helm
  :diminish
  helm-mode
  :hook
  (after-init-hook . util/helm--mode-hook)
  :init
  (user/bindings/bind-key-global :nav :context 'util/helm--navigate)
  (user/bindings/bind-key-global :doc :apropos 'util/helm--apropos)
  (user/bindings/bind-key-global :emacs :elisp-search 'helm-info-elisp)
  (user/bindings/bind-key-global :basic :alternate-paste 'helm-show-kill-ring)
  :config
  (validate-setq
   helm-input-idle-delay 0.0
   helm-candidate-number-limit 75)
  (with-eval-after-load 'popwin
    (add-to-list
     'popwin:special-display-config
     '("helm" :regexp t :height 0.4 :position bottom)))
  (use-package helm-descbinds
    :defer)
  (use-package helm-swoop
    :defer
    :init
    (user/bindings/bind-key-global :basic :swoop 'helm-swoop)
    (user/bindings/bind-key-global :basic :swoop-multi 'helm-multi-swoop)
    :config
    (validate-setq
     helm-swoop-split-direction
     'split-window-horizontally)
    (define-key isearch-mode-map (user/bindings/get-key :basic :swoop) 'helm-swoop-from-isearch)
    (with-eval-after-load 'helm-swoop
      (define-key helm-swoop-map
        (user/bindings/get-key :basic :swoop)
        'helm-multi-swoop-all-from-helm-swoop)))
  (use-package helm-adaptive
    :ensure
    helm
    :config
    (validate-setq
     helm-adaptive-history-file (lib/path/join *user-cache-directory* "helm-adaptive-history")))
  (use-package helm-command
    :ensure
    helm
    :bind*
    ([remap execute-extended-command] . helm-M-x))
  (use-package helm-files
    :ensure
    helm
    :bind*
    (([remap find-file] . helm-find-files)
     :map helm-find-files-map
     ("C-k" . helm-ff-persistent-delete))
    :config
    ;; Reason: https://emacs.stackexchange.com/a/106/5514
    (validate-setq
     helm-ff-file-name-history-use-recentf t
     helm-ff-newfile-prompt-p nil
     helm-input-idle-delay 0.1
     helm-ff-skip-boring-files t
     helm-ff-search-library-in-sexp t
     helm-ff-auto-update-initial-value t))
  (use-package helm-misc
    :ensure
    helm
    :bind*
    ([remap switch-to-buffer] . helm-mini))
  (use-package helm-buffers
    :ensure
    helm
    :bind
    (:map helm-buffer-map ("C-k" . helm-buffer-run-kill-persistent))
    :config
    (validate-setq
     helm-buffers-fuzzy-matching t
     helm-buffer-skip-remote-checking t))
  (use-package helm-ring
    :ensure
    helm
    :bind*
    (([remap yank-pop] . helm-show-kill-ring) ("C-c SPC" . helm-all-mark-rings)))
  (use-package helm-imenu
    :ensure
    helm
    :bind
    (("C-c n i" . helm-imenu-in-all-buffers)
     ("C-c n t" . helm-imenu))
    :config
    (validate-setq helm-imenu-fuzzy-match t)
    (setq helm-imenu-execute-action-at-once-if-one nil))
  (use-package helm-bookmark
    :ensure
    helm
    :defer
    :bind
    ("C-c r l" . helm-filtered-bookmarks))
  (use-package helm-pages
    :ensure
    helm
    :defer
    :bind
    ("C-c n P" . helm-pages))
  (use-package helm-eval
    :ensure
    helm
    :defer
    :bind
    (("C-c h M-:" . helm-eval-expression-with-eldoc)
     ("C-c h *" . helm-calcul-expression)))
  (use-package helm-external
    :ensure
    helm
    :defer
    :bind
    ("C-c h x" . helm-run-external-command))
  (use-package helm-build-command
    :defer
    :quelpa
    (helm-build-command
     :fetcher github
     :repo "tkf/helm-build-command"))
  (use-package helm-icons
    :if
    (display-graphic-p)
    :config
    (helm-icons-enable)))

(provide 'util/helm)

;;; helm.el ends here
