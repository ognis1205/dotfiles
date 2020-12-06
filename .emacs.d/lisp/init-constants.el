;;; init-constants.el --- Set up constants required during initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib/path)
(require 'lib/env)

;;; (Directories) ;;;
(defconst *user-home-directory*
  (lib/env/getenv-or "HOME" (concat (expand-file-name "~") "/"))
  "Path to user home directory.")

(defconst *user-local-directory*
  (if (getenv "DATA_HOME")
      (lib/path/dirname (getenv "DATA_HOME"))
    (lib/path/join *user-home-directory* ".local"))
  "Path to user's local store.")

(defconst *user-config-directory*
  (lib/path/join (lib/env/getenv-or "CONFIG_HOME" (lib/path/join *user-home-directory* ".config")) "emacs")
  "Path to user's local cache store.")

(defconst *user-data-directory*
  (lib/path/join (lib/env/getenv-or "DATA_HOME" (lib/path/join *user-local-directory* "share")) "emacs")
  "Path to user's local data store.")

(defconst *user-cache-directory*
  (lib/path/join (lib/env/getenv-or "CACHE_HOME" (lib/path/join *user-home-directory* ".cache")) "emacs")
  "Path to user's local cache store.")

(defconst *user-documents-directory*
  (lib/path/join *user-home-directory* "Documents")
  "Path to user's documents directory.")

(defconst *user-local-init*
  (lib/path/join *user-home-directory* ".emacs.local.el")
  "Path to user's machine-local configuration file.")

(provide 'init-constants)

;;; init-constants.el ends here
