(add-to-list 'load-path (concat user-emacs-directory "elisp/beancount/editors/emacs"))
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

(provide 'pp-beancount)
