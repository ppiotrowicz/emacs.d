(setq gc-cons-threshold 100000000)
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)


(add-to-list 'load-path (concat user-emacs-directory "config"))

(setq package-load-list '(all))
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)

(put 'narrow-to-page 'disabled nil)

;; ---------------------
;; ------ GENERAL ------
;; ---------------------

;; interface tweaks
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-visual-line-mode -1)
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq create-lockfiles nil)
(setq exec-path (append exec-path '("/usr/local/bin")))

;; encoding
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq-default indent-tabs-mode nil)

;; scratch
(setq initial-scratch-message
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; " ; comment each line
        (replace-regexp-in-string
         "\n$" ""    ; remove trailing linebreak
         (shell-command-to-string "fortune")))))

;; better scrolling
(setq scroll-conservatively 9999)
(setq scroll-preserve-screen-position t)
(setq scroll-margin 3)

;; backups
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-default nil)
(setq make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq vc-follow-symlinks t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; server
(server-start)

;; hippie expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; -----------------------
;; ------ INTERFACE ------
;; -----------------------

;; theme
(add-to-list 'load-path "~/.emacs.d/themes/emacs-doom-theme")
(require 'doom-themes)
(load-theme 'doom-one t)
;; brighter source buffers
(add-hook 'find-file-hook 'doom-buffer-mode)
;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook 'doom-buffer-mode)

;; font
(set-default-font "Fira Mono for Powerline")
(set-face-attribute 'default nil :height 120)

;; diminish
(require 'diminish)

;; parens
(show-paren-mode)
(setq show-paren-delay 0)

;; whitespace
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs trailing))
(global-whitespace-mode 1)

;; ----------------------
;; ------ PACKAGES ------
;; ----------------------

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

;; == EVIL ==
(require 'pp-evil)

(use-package yafolding
  :ensure t
  :init
  (progn
    (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
    (define-key yafolding-mode-map (kbd "<C-M-return>") nil)
    (define-key yafolding-mode-map (kbd "<C-return>") nil)))

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

(use-package magit
  :commands (magit-status)
  :ensure t
  :config
  (use-package evil-magit
    :ensure t))

(use-package magithub
  :ensure t
  :after magit)

(require 'pp-org-mode)

(use-package projectile
  :ensure t
  :config
  (progn
    (use-package counsel-projectile
      :ensure t)
    (setq projectile-switch-project-action 'counsel-projectile-find-file)))

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :config
  (progn
    (defvar pp/restclient-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "s") 'restclient-http-send-current)
        (define-key map (kbd "c") 'restclient-copy-curl-command)
        map)
      "Restclient keymap.")

    (bind-map pp/restclient-map
      :evil-keys (",")
      :major-modes (restclient-mode))))

(use-package github-browse-file
  :ensure t
  :defer t)

(use-package hydra
  :ensure t)

(use-package dumb-jump
  :ensure t
  :general (:keymaps 'evil-normal-state-map
                     "C-]" 'dumb-jump-go
                     "C-[" 'dump-jump-quick-look)

  :config
  (progn
    (setq dumb-jump-selector 'ivy)
    ))

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

(use-package wgrep
  :ensure t)

(use-package highlight-symbol
  :ensure t
  :init
  (progn
    (setq highlight-symbol-foreground-color "#fdf4c1")
    (setq highlight-symbol-colors '("#504945"))))

(use-package f
  :ensure t)

(use-package dash
  :ensure t)

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)))

(use-package ruby-mode
  :ensure t
  :config
   (progn
     (defvar pp/ruby-map (make-sparse-keymap) "Ruby keymap.")
     (general-define-key
      :keymaps 'pp/ruby-map
      ;; bundle
      "b"  '(:ignore t               :which-key "bundle")
      "bi" '(bundle-install          :which-key "bundle install")
      "bo" '(bundle-open             :which-key "bundle open")
      "be" '(bundle-exec             :which-key "bundle exec")
      "bc" '(bundle-console          :which-key "bundle console")
      "bu" '(bundle-update           :which-key "bundle update")
      ;; testing
      "t"  '(:ignore t               :which-key "rspec")
      "ta" '(rspec-verify-all        :which-key "run all")
      "tb" '(rspec-verify            :which-key "run buffer")
      "tl" '(rspec-run-last-failed   :which-key "last failed")
      "tr" '(rspec-rerun             :which-key "rerun")
      "tt" '(rspec-verify-single     :which-key "run")
      "tk" '((lambda () (interactive) (kill-buffer "*rspec-compilation*")) :which-key "stop")
      ;; rbenv
      "v"  '(:ignore t               :which-key "rbenv")
      "vc" '(rbenv-use-corresponding :which-key "use local")
      "vg" '(rbenv-use-global        :which-key "use global")
      )
    (bind-map pp/ruby-map
      :evil-keys (",")
      :major-modes (ruby-mode))
    (use-package inf-ruby
      :ensure t)
    (use-package rbenv
      :ensure t
      :config
      (progn
        (global-rbenv-mode)
        (set-face-attribute 'rbenv-active-ruby-face nil
                            :inherit 'mode-line-face
                            :foreground "#eab700")
        (setq rspec-autosave-buffer t)
        (setq rspec-spec-command "rspec --format progress --no-profile")
        (add-hook 'projectile-after-switch-project-hook 'rbenv-use-corresponding)))
    (use-package rspec-mode
      :ensure t
      :config
      (progn
        (setq compilation-scroll-output t)
        (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)))
    (use-package bundler
      :ensure t)))

(use-package coffee-mode
  :ensure t
  :config
  (progn
    (setq coffee-tab-width 2)
   ))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package ledger-mode
  :ensure t
  :config
  (progn
    (defvar pp/ledger-map (make-sparse-keymap) "Ledger keymap.")
    (general-define-key
     :keymaps 'pp/ledger-map
     "a"  '(ledger-add-transaction     :which-key "add transaction")
     "="  '(ledger-mode-clean-buffer   :which-key "clean")
     "r"  '(ledger-report              :which-key "reports")
     )
    (bind-map pp/ledger-map
      :evil-keys (",")
      :major-modes (ledger-mode)))
  :mode (("\\.dat" . ledger-mode)))

;; modeline
(require 'pp-modeline)

;; --------------------
;; ------ HYDRAS ------
;; --------------------

(defhydra hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

(defhydra hydra-org (:color red :columns 3)
  "Org Mode Movements"
  ("n" outline-next-visible-heading "next heading")
  ("p" outline-previous-visible-heading "prev heading")
  ("N" org-forward-heading-same-level "next heading at same level")
  ("P" org-backward-heading-same-level "prev heading at same level")
  ("u" outline-up-heading "up heading")
  ("g" org-goto "goto" :exit t))

(defhydra hydra-search (:post highlight-symbol-remove-all)
  "Search"
  ("n" highlight-symbol-next "next")
  ("p" highlight-symbol-prev "prev")
  ("/" find-symbol-at-point "in project")
  ("s" pp/swiper-at-point "swiper")
  ("q" highlight-symbol-remove-all "quit" :exit t))

;; == KEYBINDINGS ==
(require 'pp-keybindings)

;; == FUNCTIONS ==
(require 'pp-funcs)
