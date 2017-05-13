;; general development config

(use-package projectile
  :config
  (progn
    (require 'counsel-projectile)
    (setq projectile-switch-project-action 'counsel-projectile-find-file)))

(use-package dumb-jump
  :general (:keymaps 'evil-normal-state-map "C-]" 'dumb-jump-go)
  :config
  (progn
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-aggressive nil)))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (progn
    ;; don't show squiggly lines
    (setq flycheck-highlighting-mode nil)
    ;; fringe indicators on left side
    (setq flycheck-indication-mode 'left-fringe)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side            . bottom)
                   (reusable-frames . visible)
                   (window-height   . 0.33)))))

(provide 'pp-development)
