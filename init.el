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

(require 'pp-interface)

(require 'pp-evil)

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

(use-package github-browse-file
  :ensure t
  :defer t)

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

(require 'pp-ruby)

(require 'pp-web)

(require 'pp-ledger)

(require 'pp-modeline)

(require 'pp-hydras)

(require 'pp-keybindings)

(require 'pp-funcs)
