;; pp-core-vcs.el

(use-package gitconfig-mode
  :mode ("/\\.?git/?config$" "/\\.gitmodules$"))

(use-package gitignore-mode
  :mode ("/\\.gitignore$"
         "/\\.git/info/exclude$"
         "/git/ignore$"))

(use-package git-gutter
  :commands (git-gutter-mode)
  :init
  (add-hook! (text-mode prog-mode conf-mode) 'git-gutter-mode)
  :config
  (require 'git-gutter-fringe)

  ;; colored fringe "bars"
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center)

  ;; Refreshing git-gutter
  (advice-add 'evil-force-normal-state :after 'git-gutter)
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows))

(use-package magit
  :commands (magit-status)
  :config
  (progn
    (def-popup! "^\\*magit.+" :align right :regexp t)
    (require 'evil-magit)
    ;; Do not override window movement
    (evil-define-key evil-magit-state magit-mode-map "\C-j" nil)
    (evil-define-key evil-magit-state magit-mode-map "\C-k" nil)

    (use-package magithub
      :config (magithub-feature-autoinject t))))

(use-package github-browse-file
  :defer t)

(provide 'pp-core-vcs)
