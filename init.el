(setq gc-cons-threshold 100000000)
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'load-path (concat user-emacs-directory "config"))

(setq package-load-list '(all))
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)

;; MISC
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq dotemacs-cache-directory (concat user-emacs-directory ".cache/"))
(setq custom-file (concat user-emacs-directory "custom.el"))

(setq inhibit-splash-screen t)

;; THEME
(use-package gruvbox-theme
  :ensure gruvbox-theme
  :config
   (progn
     (unless noninteractive
     (load-theme 'gruvbox t))))

(setq ring-bell-function 'ignore)

(set-default-font "M+ 1mn")
(set-face-attribute 'default nil :height 130)

(use-package smart-mode-line
  :ensure smart-mode-line
  :config
  (progn
    (sml/setup)))

(use-package which-key
  :ensure which-key
  :config
  (progn
    (setq which-key-idle-delay 0.4)
    (which-key-setup-side-window-bottom)
    (which-key-mode)
    ))

(use-package magit
  :ensure magit
  :config
  (progn
    (use-package evil-magit
      :ensure evil-magit)))

(use-package ivy
  :ensure ivy
  :config
  (progn
    (use-package counsel
      :ensure counsel)

    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 10)
    (setq ivy-count-format "(%d/%d) ")))

(use-package org
  :ensure org
  :config
  (progn
    (setq org-startup-indented nil)

    (setq org-directory "~/org")
    (setq org-link-abbrev-alist
	  '(("jira" . "https://getbase.atlassian.net/browse/")))
    (setq org-agenda-files (list "~/org/home.org" "~/org/work.org"))
    (setq org-log-into-drawer "LOGBOOK")
    (setq org-clock-into-drawer "CLOCKING")
    (setq org-refile-targets '((nil :maxlevel . 9)
			       (org-agenda-files :maxlevel . 9)))
    (setq org-refile-use-outline-path t)
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    (setq org-capture-templates
	  (quote
	   (("w" "Work")
	    ("wt" "Todo" entry
	     (file+headline "~/org/work.org" "INBOX")
	     "* TODO %?")
	    ("h" "Home")
	    ("ht" "Todo" entry
	     (file+headline "~/org/home.org" "INBOX")
	     "* TODO %?")
	    ("o" "Org")
	    ("ot" "Todo" entry
	     (file+headline "~/org/todo.org" "INBOX")
	     "* TODO %?")
	    ("l" "TIL" entry
	     (file+datetree "~/org/til.org")
	     "* %? %^g")
	    )))
    ))
;; Projectile
(use-package projectile
  :ensure projectile
  :config
  (progn
    (use-package counsel-projectile
      :ensure counsel-projectile)
    (setq projectile-switch-project-action 'counsel-projectile-find-file)
    ))

;; EVIL
;; evil-leader needs to be loaded before evil
(use-package evil-leader
  :commands (evil-leader-mode global-evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode t)))

(use-package evil
  :ensure evil
  :config
  (progn
    (evil-mode 1)

    ;; https://bitbucket.org/lyro/evil/issues/444/evils-undo-granularity-is-too-coarse
    (setq evil-want-fine-undo 'fine)

    (use-package evil-surround
      :ensure evil-surround
      :config
      (progn
        (global-evil-surround-mode 1)))

    (use-package evil-visualstar
      :ensure evil-visualstar
      :config
      (progn
        (global-evil-visualstar-mode)))

    (use-package evil-nerd-commenter
      :commands (evilnc-comment-or-uncomment-lines)
      :ensure evil-nerd-commenter)

    (define-key evil-normal-state-map (kbd "g c") 'evilnc-comment-or-uncomment-lines)

    (use-package evil-matchit
      :ensure evil-matchit
      :commands evilmi-jump-items
      :init
      (progn
        (setq global-evil-matchit-mode t)
        (define-key evil-normal-state-map "%" 'evilmi-jump-items)))

    ;; window movements
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

    ;; ESC quits stuff
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    ))

;; <leader> keybindings for evil-leader

(defmacro bind (&rest commands)
  "Convenience macro which creates a lambda interactive command."
  `(lambda (arg)
     (interactive "P")
     ,@commands))

;; windows
(evil-leader/set-key
  "ws" 'split-window-vertically
  "wS" 'split-window-below-and-focus
  "wv" 'split-window-horizontally
  "wV" 'split-window-right-and-focus
  "wc" 'delete-window
  "w=" 'balance-windows
  "ww"  'other-window
  "wf" 'toggle-fullscreen)

;; buffers
(evil-leader/set-key
  "b" (let ((map (make-sparse-keymap)))
        (define-key map (kbd "b") 'ivy-switch-buffer)
        (define-key map (kbd "d") 'kill-this-buffer)
        map))

(evil-leader/set-key
  "TAB" 'switch-to-previous-buffer)

;; files
(evil-leader/set-key
  "ff" 'counsel-find-file
  "fr" 'ivy-recentf)

;; magit
(evil-leader/set-key
  "gs" 'magit-status)

;; projectile
(evil-leader/set-key
  "pp" 'counsel-projectile
  "pf" 'counsel-projectile-find-file
  "p/" (bind(counsel-ag nil (projectile-project-root)))
  "pk" 'projectile-kill-buffers)

;; misc
(evil-leader/set-key
  ":" 'execute-extended-command)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))
(setq gc-cons-threshold 800000)
