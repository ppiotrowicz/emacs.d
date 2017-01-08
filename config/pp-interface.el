;; interface
(require 'diminish)

(use-package general
  :ensure t
  :demand general
  :config
  (progn
    (general-evil-setup)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (progn
    (setq which-key-idle-delay 0.4)
    (which-key-setup-side-window-bottom)
    (which-key-mode)))

(use-package bind-map
  :ensure t
  :demand bind-map)

(use-package powerline
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (progn
    (use-package counsel
      :ensure t)

    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 15)
    (setq ivy-count-format "(%d/%d) ")

    (use-package swiper
      :ensure t
      :config
      :general (
         "C-s" 'swiper
         "C-c C-r" 'ivy-resume)
        )))

(use-package avy
  :commands (avy-goto-char)
  :ensure t
  :config
  (avy-setup-default))

(use-package all-the-icons
  :ensure t
  :demand t)

(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-show-updir-line nil
          neo-window-width 35
          neo-persist-show nil
          neo-create-file-auto-open t)
    (add-hook 'neotree-mode-hook (lambda () (setq-local line-spacing 3)))
    (add-hook 'neotree-mode-hook (lambda () (setq-local mode-line-format nil)))
    (add-hook 'neotree-mode-hook (lambda () (setq-local tab-width 1)))
    (defun neo-buffer--insert-fold-symbol (name &optional file-name)
      "Custom overriding function for the fold symbol."
      (or (and (equal name 'open)  (insert (all-the-icons-icon-for-dir file-name "down")))
          (and (equal name 'close) (insert (all-the-icons-icon-for-dir file-name "right")))
          (and (equal name 'leaf)  (insert (format "\t\t\t%s\t" (all-the-icons-icon-for-file file-name))))))

    (defun neo-buffer--insert-dir-entry (node depth expanded)
      (let ((node-short-name (neo-path--file-short-name node)))
        (insert-char ?\s (* (- depth 1) 2)) ; indent
        (when (memq 'char neo-vc-integration)
          (insert-char ?\s 2))
        (neo-buffer--insert-fold-symbol
         (if expanded 'open 'close) node)
        (insert-button (concat node-short-name "/")
                       'follow-link t
                       'face neo-dir-link-face
                       'neo-full-path node
                       'keymap neotree-dir-button-keymap)
        (neo-buffer--node-list-set nil node)
        (neo-buffer--newline-and-begin)))

    (defun neo-buffer--insert-file-entry (node depth)
      (let ((node-short-name (neo-path--file-short-name node))
            (vc (when neo-vc-integration (neo-vc-for-node node))))
        (insert-char ?\s (* (- depth 1) 2)) ; indent
        (when (memq 'char neo-vc-integration)
          (insert-char (car vc))
          (insert-char ?\s))
        (neo-buffer--insert-fold-symbol 'leaf node-short-name)
        (insert-button node-short-name
                       'follow-link t
                       'face (if (memq 'face neo-vc-integration)
                                 (cdr vc)
                               neo-file-link-face)
                       'neo-full-path node
                       'keymap neotree-file-button-keymap)
        (neo-buffer--node-list-set nil node)
        (neo-buffer--newline-and-begin)))

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
  :ensure t
  :init
  (global-company-mode))

(use-package ace-window
  :ensure t)

(use-package yafolding
  :ensure t
  :init
  (progn
    (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
    (define-key yafolding-mode-map (kbd "<C-M-return>") nil)
    (define-key yafolding-mode-map (kbd "<C-return>") nil)))

(use-package wgrep
  :ensure t)

(use-package highlight-symbol
  :ensure t
  :init
  (progn
    (setq highlight-symbol-foreground-color "#fdf4c1")
    (setq highlight-symbol-colors '("#504945"))))

(use-package paradox
  :commands (paradox-list-packages)
  :ensure t
  :config
  (progn
    (setq paradox-github-token t)
    (defvar pp/paradox-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "x") 'paradox-menu-execute)
        (define-key map (kbd "h") 'paradox-menu-visit-homepage)
        (define-key map (kbd "u") 'paradox-upgrade-packages)
        (define-key map (kbd "f") 'hydra-paradox-filter/body)
        (define-key map (kbd "q") 'paradox-quit-and-close)
        map)
      "Paradox keymap.")

    (bind-map pp/paradox-map
      :evil-keys (",")
      :major-modes (paradox-menu-mode))
    )
  )

(use-package f
  :ensure t)

(use-package dash
  :ensure t)

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)))



(provide 'pp-interface)
