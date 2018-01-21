;; general development config

(electric-pair-mode -1)

(use-package projectile
  :config
  (progn
    (projectile-mode +1)
    (require 'counsel-projectile)
    (setq projectile-switch-project-action 'counsel-projectile-find-file)))

(use-package dumb-jump
  :general (:keymaps 'evil-normal-state-map "C-]" 'dumb-jump-go)
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-aggressive nil)
  (setq dumb-jump-force-searcher 'ag))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (progn
    ;; don't show squiggly lines
    (setq flycheck-highlighting-mode nil)
    ;; fringe indicators on left side
    (setq flycheck-indication-mode 'left-fringe)
    ;; don't show errors in minibuffer
    (setq flycheck-display-errors-function nil)
    (define-key flycheck-error-list-mode-map "\C-h" 'evil-window-left)
    (define-key flycheck-error-list-mode-map "\C-j" 'evil-window-down)
    (define-key flycheck-error-list-mode-map "\C-k" 'evil-window-up)
    (define-key flycheck-error-list-mode-map "\C-l" 'evil-window-right)
    (define-key flycheck-error-list-mode-map "k" 'flycheck-error-list-previous-error)
    (define-key flycheck-error-list-mode-map "j" 'flycheck-error-list-next-error)

    (def-popup! "\\*Flycheck errors\\*" :align below :size 14 :noselect t :regexp t :popup t)))

;; eshell
(def-popup! "\\*eshell\\*" :align below :size 14 :select t :regexp t :popup t)
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-arguments (list "-l"))
  (exec-path-from-shell-initialize))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(provide 'pp-development)
;;; pp-development ends here
