;;; package --- Summary
;;; config/pp-core.el -*- lexical-binding: t; -*-
;;; core.el
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Initialized in %.3fs]" elapsed)))))

(setq custom-file (concat user-emacs-directory "custom.el"))

(add-to-list 'load-path (concat user-emacs-directory "config"))

(put 'narrow-to-region 'disabled nil)

(defconst pp-packages-dir
  (expand-file-name
   (format ".cask/%s.%s/elpa" emacs-major-version emacs-minor-version)
   user-emacs-directory)
  "Where plugins are installed (by cask).")

(setq-default
 package--init-file-ensured t
 package-user-dir pp-packages-dir
 package-enable-at-startup nil
 package-archives
 '(("gnu"   . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ("org"   . "http://orgmode.org/elpa/")))

(package-initialize)

(require 'use-package)

(defmacro pp-emacs (&rest packages)
  "Bootstrap Emacs and initialize PACKAGES."
  (setq-default gc-cons-threshold 536870912
                gc-cons-percentage 0.6)
  (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (cask-initialize)
  `(let (file-name-handler-alist)
     (unless noninteractive
       ,@(mapcar (lambda (pkg)
                   (let ((lib-path (locate-library (symbol-name pkg))))
                     (unless lib-path
                       (error "Initfile not found: %s" pkg))
                     `(require ',pkg ,(file-name-sans-extension lib-path))))
                 packages))
     (setq-default gc-cons-threshold 16777216
                   gc-cons-percentage 0.4)))

(provide 'pp-core)
;;; pp-core.el ends here
