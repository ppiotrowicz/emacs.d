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
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; better scrolling
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t
      scroll-margin 3)

(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil)

;; THEME
(use-package gruvbox-theme
  :ensure gruvbox-theme
  :config
   (progn
     (unless noninteractive
     (load-theme 'gruvbox t))))

;; Mode line setup
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%5l:" face mode-line-position-face)
   (:eval (propertize "%5c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "    ")))
   ; project name
   " ("
    (:propertize (:eval (projectile-project-name))
                 face mode-line-mode-face)
    ") "

   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "
   ))

;; Helper function
(eval-when-compile (require 'subr-x))
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."

  (let* ((root-folder
          (if (and (boundp 'projectile-project-root) (projectile-project-p))
              (projectile-project-root) ""))
         (path (reverse (split-string
                 (abbreviate-file-name
                  (string-remove-prefix root-folder dir)) "/")))
          (output ""))
         (when (and path (equal "" (car path)))
           (setq path (cdr path)))
         (while (and path (< (length output) (- max-length 4)))
           (setq output (concat (car path) "/" output))
           (setq path (cdr path)))
         (when path
           (setq output (concat ".../" output)))
         output))


;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
(make-face 'rbenv-active-ruby-face)

(set-face-attribute 'mode-line nil
    :foreground "gray60" :background "gray20"
    :inverse-video nil
    :box '(:line-width 6 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray22"
    :inverse-video nil
    :box '(:line-width 6 :color "gray22" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")


(setq ring-bell-function 'ignore)

(blink-cursor-mode -1)
(set-default-font "M+ 1mn")
(set-face-attribute 'default nil :height 130)

(show-paren-mode)
(setq show-paren-delay 0)

;; fringe
(when (display-graphic-p)
  (fringe-mode 16))

(use-package general
  :ensure general
  :demand general
  :config
  (progn
    (general-evil-setup)
    (setq general-default-keymaps 'evil-normal-state-map)))

(require 'diminish)

(use-package which-key
  :ensure which-key
  :diminish which-key-mode
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
  :diminish ivy-mode
  :config
  (progn
    (use-package counsel
      :ensure counsel)

    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 15)
    (setq ivy-count-format "(%d/%d) ")

    (use-package swiper
      :ensure swiper
      :config
      (progn
	(general-define-key
	 "C-s" 'swiper
	 "C-c C-r" 'ivy-resume)
	))))

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

;; RUBY
(use-package ruby-mode
  :ensure ruby-mode
  :config
  (progn
    (use-package inf-ruby
      :ensure inf-ruby)
    (use-package rbenv
      :ensure rbenv
      :config
      (progn
        (global-rbenv-mode)
        (set-face-attribute 'rbenv-active-ruby-face nil
                            :inherit 'mode-line-face
                            :foreground "#eab700")
        (add-hook 'projectile-after-switch-project-hook 'rbenv-use-corresponding)
        ))
    (use-package rspec-mode
      :ensure rspec-mode
      :general
      (general-define-key
       :prefix ","
       :predicate '(string= (file-name-extension (buffer-file-name)) "rb")
       "t"  '(:which-key "rspec" :ignore t)
       "ta" 'rspec-verify-all
       "tb" 'rspec-verify
       "tl" 'rspec-run-last-failed
       "tr" 'rspec-rerun
       "tt" 'rspec-verify-single)
      :config
      (progn
	(setq compilation-scroll-output t)
	(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
	))
    (use-package bundler
      :ensure bundler)))

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

;; global key bindings

(defmacro bind (&rest commands)
  "Convenience macro which creates a lambda interactive command."
  `(lambda (arg)
     (interactive "P")
     ,@commands))

;; buffers
(general-define-key :prefix "SPC"
		    "b"   '(:which-key "buffers" :ignore t)
		    "bb"  '(:which-key "switch" :command ivy-switch-buffer)
		    "bd"  '(:which-key "kill" :command kill-this-buffer)
		    "TAB" '(:which-key "toggle" :command switch-to-previous-buffer))

(general-define-key :prefix "SPC"
		    "h"  '(:which-key "help" :ignore t)
		    "hc" '(:which-key "edit config" :command edit-emacs-config)
		    "hv" '(:which-key "describe variable" :command counsel-describe-variable)
		    "hf" '(:which-key "describe function" :command counsel-describe-function))

;; files
(general-define-key :prefix "SPC"
		    "f"  '(:which-key "files" :ignore t)
		    "ff" '(:which-key "find" :command counsel-find-file)
		    "fr" '(:which-key "rename" :command rename-file))

;; magit
(general-define-key :prefix "SPC"
		    "g"  '(:which-key "git" :ignore t)
		    "gb" '(:which-key "blame" :command magit-blame)
		    "gs" '(:which-key "status" :command magit-status))

;; projectile
(general-define-key :prefix "SPC"
		    "p"  '(:which-key "project" :ignore t)
		    "pp" '(:which-key "switch" :command counsel-projectile)
		    "pf" '(:which-key "find file" :command counsel-projectile-find-file)
		    "p/" '(:which-key "search" :command find-in-project)
		    "/"  '(:which-key "search" :command find-in-project)
		    "pk" '(:which-key "kill buffers" :command projectile-kill-buffers))

;; windows
(general-define-key :prefix "SPC"
		    "w"  '(:which-key "windows" :ignore t)
		    "ws" '(:which-key "hsplit" :command split-window-vertically)
		    "wS" '(:which-key "hsplit!" :command split-window-below-and-focus)
		    "wv" '(:which-key "vsplit" :command split-window-horizontally)
		    "wV" '(:which-key "vsplit!" :command split-window-right-and-focus)
		    "wc" '(:which-key "kill" :command delete-window)
		    "w=" '(:which-key "balance" :command balance-windows)
		    "ww" '(:which-key "toggle" :command other-window)
		    "wf" '(:which-key "fullscreen" :command toggle-fullscreen))

;; ORG
(general-define-key :prefix "SPC"
		    "o"  '(:which-key "org" :ignore t)
		    "oh" '(:which-key "home tasks" :command (lambda () (interactive) (find-file "~/org/home.org")))
		    "ot" '(:which-key "todo tasks" :command (lambda () (interactive) (find-file "~/org/todo.org")))
		    "ow" '(:which-key "work tasks" :command (lambda () (interactive) (find-file "~/org/work.org")))
		    "ol" '(:which-key "work tasks" :command (lambda () (interactive) (find-file "~/org/til.org"))))

;; misc
(general-define-key :prefix "SPC" :keymaps 'normal
		    ":" '(:which-key "M-x" :command counsel-M-x))

(general-define-key :prefix "C-c" :keymaps 'normal
                    "/" 'find-symbol-at-point)

(defun edit-emacs-config ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun find-in-project ()
  "Searches in current project"
  (interactive)
  (counsel-ag nil (projectile-project-root)))

(defun find-symbol-at-point ()
  "Searches for symbol under cursor in current project"
  (interactive)
  (counsel-ag (thing-at-point 'symbol) (projectile-project-root)))

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

(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)

(setq gc-cons-threshold 800000)

