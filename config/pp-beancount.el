;; beancount mode

(add-to-list 'load-path (concat user-emacs-directory "elisp/beancount/editors/emacs"))

(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

(setq beancount-use-ido nil)
(add-hook! 'beancount-mode-hook (setq-local yas/indent-line nil))
(add-hook! 'beancount-mode-hook (yas-activate-extra-mode 'beancount-mode))

(provide 'pp-beancount)
