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

(provide 'pp-development)
