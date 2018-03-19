;; pp-interface.el

(use-package general
  :demand general
  :config
  (progn
    (general-evil-setup)))

(use-package which-key
  :config
  (progn
    (setq which-key-idle-delay 0.4)
    (which-key-setup-side-window-bottom)
    (which-key-mode)))

(use-package bind-map
  :demand bind-map)

(use-package smex)

(use-package ivy
  :config
  (progn
    (require 'counsel)
    (require 'ivy-hydra)

    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 15)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-use-selectable-prompt t)
    (setq ivy-on-del-error-function nil)

    (use-package swiper
      :config
      :general (
         "C-s" 'swiper
         "C-c C-r" 'ivy-resume)
        )))

(use-package avy
  :commands (avy-goto-char)
  :config
  (avy-setup-default))

(use-package all-the-icons
  :demand t)

(use-package neotree
  :config
  (progn
    (setq neo-show-updir-line nil
          neo-window-width 35
          neo-persist-show nil
          neo-create-file-auto-open t)
    (add-hook 'neotree-mode-hook (lambda () (setq-local line-spacing 3)))
    (add-hook 'neotree-mode-hook (lambda () (setq-local mode-line-format nil)))
    (add-hook 'neotree-mode-hook (lambda () (setq-local tab-width 1)))

    (defun neotree-projectile ()
      (interactive )
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (neotree-find (or (ignore-errors (projectile-project-root))
                          (and (buffer-file-name) (file-name-nondirectory (buffer-file-name)))
                          (getenv "HOME")))))

    (defun neotree-projectile-find ()
      (interactive)
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (let ((origin-buffer-file-name (buffer-file-name)))
          (neotree-find (projectile-project-root))
          (neotree-find origin-buffer-file-name))))

    (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "g")   'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "q")   'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "c")   'neotree-create-node)
                (define-key evil-normal-state-local-map (kbd "d")   'neotree-delete-node)
                (define-key evil-normal-state-local-map (kbd "r")   'neotree-rename-node)
                ))
    ))

(use-package company
  :init
  (global-company-mode))

(use-package ace-window)

(use-package yafolding
  :init
  (progn
    (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
    (define-key yafolding-mode-map (kbd "<C-M-return>") nil)
    (define-key yafolding-mode-map (kbd "<C-return>") nil)))

(use-package wgrep)

(use-package highlight-symbol
  :init
  (progn
    (setq highlight-symbol-foreground-color "#fdf4c1")
    (setq highlight-symbol-colors '("#504945"))))

(provide 'pp-interface)
