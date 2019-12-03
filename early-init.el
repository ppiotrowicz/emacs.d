(setq custom-file (concat user-emacs-directory "custom.el"))

(add-to-list 'load-path (concat user-emacs-directory "config"))

(defconst pp-packages-dir
  (expand-file-name
   (format ".cask/%s.%s/elpa" emacs-major-version emacs-minor-version)
   user-emacs-directory)
  "Where plugins are installed (by cask).")

(setq
 package-user-dir pp-packages-dir
 package-enable-at-startup t
 package-archives
 '(("gnu"   . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ("org"   . "http://orgmode.org/elpa/")))
