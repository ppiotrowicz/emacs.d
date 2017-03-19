;; general development config

(use-package projectile
  :config
  (progn
    (require 'counsel-projectile)
    (setq projectile-switch-project-action 'counsel-projectile-find-file)))

(use-package dumb-jump
  :general (:keymaps 'evil-normal-state-map
                     "C-]" 'dumb-jump-go
                     "C-[" 'dump-jump-quick-look)
  :config
  (setq dumb-jump-selector 'ivy))

(provide 'pp-development)
