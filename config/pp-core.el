;;; core.el

(eval-when-compile (require 'cl))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Initialized in %.3fs]" elapsed)))))

(add-to-list 'load-path (concat user-emacs-directory "config"))

(defmacro pp-emacs (&rest packages)
  "Bootstrap emacs and initialize packages"
  (setq-default gc-cons-threshold 339430400
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
     (setq-default gc-cons-threshold 4388608
                   gc-cons-percentage 0.4)))
