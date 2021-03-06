;;; osx.el --- Support functions for applications -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun lib/osx/installed-p (app)
  "Return t if `APP' is installed."
  (when (eq system-type 'darwin)
    (let ((lsregister
           "/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister"))
      (and (file-executable-p lsregister)
         (not (string-equal "" (shell-command-to-string
                              (concat lsregister " -dump|grep " app))))))))

(provide 'lib/osx)

;;; osx.el ends here
