(eval-when-compile (require 'cl))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Initialized in %.3fs]" elapsed)))))


(let ((gc-cons-threshold (* 256 1024 1024)))
  (require 'package)
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (setq package-enable-at-startup nil)

  (add-to-list 'load-path (concat user-emacs-directory "config"))

  (setq package-load-list '(all))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)

  (require 'pp-core-defuns)
  (require 'pp-core-ui)
  (require 'pp-settings)
  (require 'pp-editor)
  (require 'pp-theme)
  (require 'pp-interface)
  (require 'pp-evil)
  (require 'pp-scratch)
  (require 'pp-development)
  (require 'pp-org-mode)
  (require 'pp-ruby)
  (require 'pp-web)
  (require 'pp-ledger)
  (require 'pp-modeline)
  (require 'pp-hydras)
  (require 'pp-keybindings)
  (require 'pp-funcs)
)
