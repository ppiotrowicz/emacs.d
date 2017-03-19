(defconst pp-packages-dir
  (expand-file-name
   (format ".cask/%s.%s/elpa" emacs-major-version emacs-minor-version)
   user-emacs-directory)
  "Where plugins are installed (by cask)")

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

(provide 'pp-packages)
