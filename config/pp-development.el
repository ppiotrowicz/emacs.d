;; general development config

(use-package magit
  :commands (magit-status)
  :defer t
  :ensure t
  :config
  (use-package evil-magit
    :ensure t))

(use-package magithub
  :ensure t
  :defer t
  :after magit
  :config (magithub-feature-autoinject t))

(use-package projectile
  :ensure t
  :config
  (progn
    (use-package counsel-projectile
      :ensure t)
    (setq projectile-switch-project-action 'counsel-projectile-find-file)))

(use-package github-browse-file
  :ensure t
  :defer t)

(use-package dumb-jump
  :ensure t
  :general (:keymaps 'evil-normal-state-map
                     "C-]" 'dumb-jump-go
                     "C-[" 'dump-jump-quick-look)
  :config
  (setq dumb-jump-selector 'ivy))

(provide 'pp-development)
