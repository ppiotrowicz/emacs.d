;;; pp-js

(use-package js2-mode
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode)
                             (company-mode)))
  (add-hook 'js2-mode-hook #'add-node-modules-path))

;; Disable completion keybindings, as we use xref-js2 instead
(add-hook 'tern-mode-hook (lambda ()
                            (define-key tern-mode-keymap (kbd "M-.") nil)
                            (define-key tern-mode-keymap (kbd "M-,") nil)))

(use-package xref-js2
  :config
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(provide 'pp-js)
