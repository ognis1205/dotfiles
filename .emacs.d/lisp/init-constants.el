;;; init-constants.el --- Set up constants required during initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib/path)
(require 'lib/env)

;;; (Directories) ;;;
(defconst *user-home-directory*
  (getenv-or "HOME" (concat (expand-file-name "~") "/"))
  "Path to user home directory.")

(defconst *user-local-directory*
  (if (getenv "DATA_HOME")
      (path-dirname (getenv "DATA_HOME"))
    (path-join *user-home-directory* ".local"))
  "Path to user's local store.")

(defconst *user-config-directory*
  (path-join (getenv-or "CONFIG_HOME" (path-join *user-home-directory* ".config")) "emacs")
  "Path to user's local cache store.")

(defconst *user-data-directory*
  (path-join (getenv-or "DATA_HOME" (path-join *user-local-directory* "share")) "emacs")
  "Path to user's local data store.")

(defconst *user-cache-directory*
  (path-join (getenv-or "CACHE_HOME" (path-join *user-home-directory* ".cache")) "emacs")
  "Path to user's local cache store.")

(defconst *user-documents-directory*
  (path-join *user-home-directory* "Documents")
  "Path to user's documents directory.")

(defconst *user-local-init*
  (path-join *user-home-directory* ".emacs.local.el")
  "Path to user's machine-local configuration file.")

(defconst *user-template-directory*
  (path-join *user-data-directory* "templates")
  "Path to user's templates and its configuration file.")

(defconst *user-template-config*
  (path-join *user-data-directory* ".template.conf")
  "Path to user's machine-local configuration file.")

(provide 'init-constants)

;;; init-constants.el ends here
