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

(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode 1)

    ;; https://bitbucket.org/lyro/evil/issues/444/evils-undo-granularity-is-too-coarse
    (setq evil-want-fine-undo 'fine)

    (use-package evil-surround
      :ensure t
      :config
      (progn
        (global-evil-surround-mode 1)))

    (use-package evil-nerd-commenter
      :commands (evilnc-comment-or-uncomment-lines)
      :ensure t)

    (use-package evil-matchit
      :ensure t
      :commands evilmi-jump-items
      :init
      (progn
        (global-evil-matchit-mode 1)))

    ;; On OSX, stop copying each visual state move to the clipboard:
    (when (or (featurep 'mac) (featurep 'ns)
              (advice-add 'evil-visual-update-x-selection :override 'ignore)))

    ;; ESC quits stuff
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    ))

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

(use-package org
  :ensure t
  :config
  (progn
    (setq org-export-coding-system 'utf-8)
    (setq org-indent-mode-turns-on-hiding-stars t)
    (setq org-adapt-indentation nil)
    (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . auto)))
    (setq org-cycle-separator-lines 1)
    (setq org-cycle-include-plain-lists t)
    (setq org-entities-user '(("flat" "\\flat" nil "" "" "266D" "♭")
                              ("sharp" "\\sharp" nil "" "" "266F" "♯")))
    (setq org-fontify-done-headline t)
    (setq org-fontify-quote-and-verse-blocks t)
    (setq org-fontify-whole-heading-line t)
    (setq org-footnote-auto-label 'plain)
    (setq org-hide-emphasis-markers t)
    (setq org-hide-leading-stars t)
    (setq org-image-actual-width nil)
    (setq org-pretty-entities t)
    (setq org-pretty-entities-include-sub-superscripts t)
    (setq org-startup-folded t)
    (setq org-startup-indented t)
    (setq org-startup-with-inline-images nil)
    (setq org-use-sub-superscripts '{})
    (setq org-src-fontify-natively t)
    (setq org-startup-indented t)
    (setq org-hide-leading-stars t)
    (setq org-directory "~/org")
    (setq org-link-abbrev-alist
          '(("SD"   . "https://getbase.atlassian.net/browse/SD-")
            ("jira" . "https://getbase.atlassian.net/browse/")
            ("conf" . "https://getbase.atlassian.net/wiki/display/%h")))
    (setq org-agenda-files (list "~/org/home.org" "~/org/work.org"))
    (setq org-log-into-drawer "LOGBOOK")
    (setq org-clock-into-drawer "CLOCKING")
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))
    (setq org-refile-use-outline-path t)
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    (setq org-tags-column -90)
    (setq org-export-html-postamble nil)


    ;; Fontify checkboxes and dividers
    (defface org-list-bullet '((t ())) "Face for list bullets")
    (font-lock-add-keywords
     'org-mode '(("^ *\\([-+]\\|[0-9]+[).]\\) "
                  (1 'org-list-bullet))
                 ("^ *\\(-----+\\)$"
                  (1 'org-meta-line))))
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

    ;; fix level 1 heading colors
    (set-face-attribute 'org-level-1 nil
                        :background "#262c34"
                        :foreground "#00B3EF"
                        :box nil
                        :height 1.2)
  ))

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

;; ----------------------
;; ------ MODELINE ------
;; ----------------------

(defvar mode-line-height 30
  "How tall the mode-line should be. This is only respected in GUI emacs.")

;; Load powerline only when uncompiled, in order to generate the xpm bitmaps for
;; the mode-line. This is the tall blue bar on the left of the mode-line.
;; NOTE Compile this file for a faster startup!
(eval-when-compile (require 'powerline))
;; FIXME Don't hardcode colors in
(defvar mode-line-bar          (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil)))
(defvar mode-line-eldoc-bar    (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil)))
(defvar mode-line-inactive-bar (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil)))

;; Custom faces
(defface mode-line-is-modified nil
  "Face for mode-line modified symbol")

(defface mode-line-is-saved nil
  "Face for mode-line modified symbol")

(defface mode-line-2 nil
  "The alternate color for mode-line text.")

(defface mode-line-highlight nil
  "Face for bright segments of the mode-line.")

(defface mode-line-count-face nil
  "Face for anzu/evil-substitute/evil-search number-of-matches display.")

(defun pp/project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

;; Initialization

;; So the mode-line can keep track of "the current window"
(defvar mode-line-selected-window nil)
(defun pp/set-selected-window (&rest _)
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq mode-line-selected-window window))))
(add-hook 'window-configuration-change-hook #'pp/set-selected-window)
(add-hook 'focus-in-hook #'pp/set-selected-window)
(advice-add 'select-window :after 'pp/set-selected-window)
(advice-add 'select-frame  :after 'pp/set-selected-window)

;;
;; Mode-line segments
;;

(defun *buffer-path ()
  "Displays the buffer's full path relative to the project root (includes the
project root). Excludes the file basename. See `*buffer-name' for that."
  (when buffer-file-name
    (propertize
     (f-dirname
      (let ((buffer-path (file-relative-name buffer-file-name (pp/project-root)))
            (max-length (truncate (/ (window-body-width) 1.75))))
        (concat (projectile-project-name) "/"
                (if (> (length buffer-path) max-length)
                    (let ((path (reverse (split-string buffer-path "/" t)))
                          (output ""))
                      (when (and path (equal "" (car path)))
                        (setq path (cdr path)))
                      (while (and path (<= (length output) (- max-length 4)))
                        (setq output (concat (car path) "/" output))
                        (setq path (cdr path)))
                      (when path
                        (setq output (concat "../" output)))
                      (when (string-suffix-p "/" output)
                        (setq output (substring output 0 -1)))
                      output)
                  buffer-path))))
     'face (if active 'mode-line-2))))


(defun *buffer-name ()
  "The buffer's base name or id."
  (s-trim-left (format-mode-line "%b")))

(defun *buffer-pwd ()
  "Displays `default-directory', for special buffers like the scratch buffer."
  (propertize
   (concat "[" (abbreviate-file-name default-directory) "]")
   'face 'mode-line-2))

(defun *buffer-state ()
  "Displays symbols representing the buffer's state
(non-existent/modified/read-only)"
  (when buffer-file-name
     (concat (if (not (file-exists-p buffer-file-name))
                 (propertize (all-the-icons-faicon "ban" :height 1.3 :v-adjust 0.0) 'face 'mode-line-is-modified))
               (if (buffer-modified-p)
                   (propertize (all-the-icons-faicon "circle" :height 1.3 :v-adjust 0.0) 'face 'mode-line-is-modified)
                   (propertize (all-the-icons-faicon "check-circle" :height 1.3 :v-adjust 0.0) 'face 'mode-line-is-saved))
             (if buffer-read-only
                 (propertize (all-the-icons-faicon "lock" :height 1.3 :v-adjust 0.0) 'face 'mode-line-is-modified)))))

(defun *buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
      ""
    (symbol-name buffer-file-coding-system)))

(defun *ruby-version ()
  "Currently active ruby version"
  (when (string-equal mode-name "Ruby")
    (concat " [" (rbenv--active-ruby-version) "]")))

(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (format-mode-line mode-name)
          (if (stringp mode-line-process) mode-line-process)
          (and (featurep 'face-remap)
               (/= text-scale-mode-amount 0)
               (format " (%+d)" text-scale-mode-amount))))

(defun *major-mode-icon ()
    (propertize (format "%s" (all-the-icons-icon-for-buffer)
                'help-echo (format "Major-mode: `%s`" major-mode)
                'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

(defun *org-timer ()
  "Displays org timers"
  (if (and (boundp 'org-mode-line-string) (stringp org-mode-line-string))
      (propertize
       (format " %s " (s-match "\\[.*\\]" org-mode-line-string))
       'face 'mode-line-2)))

(add-hook 'org-clock-out-hook
          '(lambda ()
             (setq org-mode-line-string nil)
             (force-mode-line-update)))

(defun *git-vc ()
  (when vc-mode
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                   'display '(raise -0.1))
       (propertize (format " %s" branch) 'face `(:height 0.9))
       " · "))))

(defun *selection-info ()
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and active (evil-visual-state-p))
    (propertize
     (let ((reg-beg (region-beginning))
           (reg-end (region-end))
           (evil (eq 'visual evil-state)))
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
             (chars (- (1+ reg-end) reg-beg))
             (cols (1+ (abs (- (evil-column reg-end)
                               (evil-column reg-beg))))))
         (cond
          ;; rectangle selection
          ((or (bound-and-true-p rectangle-mark-mode)
               (and evil (eq 'block evil-visual-selection)))
           (format " %dx%dB " lines (if evil cols (1- cols))))
          ;; line selection
          ((or (> lines 1) (eq 'line evil-visual-selection))
           (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
               (format " %dL " lines)
             (format " %dC %dL " chars lines)))
          (t (format " %dC " (if evil chars (1- chars)))))))
     'face 'mode-line-highlight)))

(defun *macro-recording ()
  "Display current macro being recorded."
  (when (and active defining-kbd-macro)
    (propertize
     (format " %s ▶ " (char-to-string evil-this-macro))
     'face 'mode-line-highlight)))

(defun *evil-substitute ()
  "Show number of :s matches in real time."
  (when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches "
                   (count-matches pattern (car range) (cdr range))
                   evil-ex-argument)
         " ... "))
     'face (if active 'mode-line-count-face))))

(defun *buffer-position ()
  "A more vim-like buffer position."
  (let ((start (window-start))
        (end (window-end))
        (pend (point-max)))
    (format "%d%%%%" (/ end 0.01 pend))))

(defun *time ()
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
    (concat
     (propertize (format-time-string "%H:%M ") 'face `(:height 0.9))
     (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))

(defun *dot-separator ()
    (propertize " · " 'face `(:height 0.9)))

(defun pp/mode-line (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) mode-line-selected-window))
           (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
                      (*macro-recording)
                      (*selection-info)
                      " "
                      (*buffer-path)
                      (*buffer-name)
                      " "
                      (*buffer-state)
                      ,(if (eq id 'scratch) '(*buffer-pwd))))
           (rhs (list
                      ;; (*org-timer)
                      (*git-vc)
                      (*major-mode-icon)
                      (*dot-separator)
                      (propertize
                       "(%l,%c)"
                       'face (if active 'mode-line-2))
                      (*dot-separator)
                      (*time)))
           (middle (propertize
                    " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                       ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs middle rhs))))

(setq-default mode-line-format (pp/mode-line))

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

;; -------------------------
;; ------ KEYBINDINGS ------
;; -------------------------

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "'"   '(terminal-focus                      :which-key "iTerm")
 ":"   '(execute-extended-command            :which-key "M-x")
 "SPC" '(avy-goto-char                       :which-key "avy char")
 "t"   '(neotree-projectile-find             :which-key "neotree find")
 "T"   '(neotree-projectile                  :which-key "neotree")
 ;; Buffers
 "b"   '(:ignore t                           :which-key "buffers")
 "bb"  '(ivy-switch-buffer                   :which-key "switch buffer")
 "bd"  '(kill-this-buffer                    :which-key "kill buffer")
 "bn"  '(evil-buffer-new                     :which-key "new buffer")
 "TAB" '(switch-to-previous-buffer           :which-key "previous buffer")
 ;; Help
 "h"   '(:ignore t                           :which-key "help")
 "hc"  '(edit-emacs-config                   :which-key "edit config")
 "hf"  '(describe-function                   :which-key "describe function")
 "hp"  '(paradox-list-packages               :which-key "paradox")
 "hv"  '(describe-variable                   :which-key "describe variable")
 ;; Files
 "f"   '(:ignore t                           :which-key "files")
 "fd"  '(pp/delete-file-and-buffer           :which-key "delete file")
 "ff"  '(counsel-find-file                   :which-key "find file")
 "fr"  '(pp/rename-file-and-buffer           :which-key "rename file")
 ;; Git
 "g"   '(:ignore t                           :which-key "magit")
 "gs"  '(magit-status                        :which-key "status")
 "gb"  '(magit-blame                         :which-key "blame")
 "go"  '(github-browse-file                  :which-key "github browse")
 ;; Open
 "o"   '(:ignore t                           :which-key "open")
 "oc"  '(org-capture                         :which-key "org capture")
 "ob"  '((lambda () (interactive) (find-file "~/org/bookmarks.org"))  :which-key "bookmarks")
 "oe"  '((lambda () (interactive) (find-file "~/org/emacs.org"))      :which-key "emacs tasks")
 "oh"  '((lambda () (interactive) (find-file "~/org/home.org"))       :which-key "home tasks")
 "ot"  '((lambda () (interactive) (find-file "~/org/today.org"))      :which-key "today tasks")
 "ol"  '((lambda () (interactive) (find-file "~/org/til.org"))        :which-key "today I learned")
 "ow"  '((lambda () (interactive) (find-file "~/org/work.org"))       :which-key "work tasks")
 "of"  '((lambda () (interactive) (find-file "~/finance/ledger.dat")) :which-key "ledger")
 ;; Project
 "p"   '(:ignore t                           :which-key "project")
 "pp"  '(counsel-projectile-switch-project   :which-key "switch project")
 "pf"  '(counsel-projectile-find-file        :which-key "find file")
 "pb"  '(counsel-projectile-switch-to-buffer :which-key "find buffer")
 "pk"  '(projectile-kill-buffers             :which-key "kill buffers")
 "p/"  '(find-in-project                     :which-key "find string")
 "/"   '(find-in-project                     :which-key "find string")
 ;; Windows
 "w"   '(:ignore t                           :which-key "windows")
 "ws"  '(split-window-vertically             :which-key "split -")
 "wS"  '(split-window-below-and-focus        :which-key "split - and focus")
 "wv"  '(split-window-horizontally           :which-key "split |")
 "wV"  '(split-window-right-and-focus        :which-key "split | and focus")
 "wc"  '(delete-window                       :which-key "delete window")
 "w="  '(balance-windows                     :which-key "balance windows")
 "ww"  '(ace-window                          :which-key "ace window")
 "wf"  '(toggle-fullscreen                   :which-key "fullscreen")
)

;;(define-key ruby-mode-map (kbd "C-c :") 'ruby_toggle_symbol)

(general-nmap "*"   'pp/highlight-symbol-hydra)
(general-nmap "C-y" 'counsel-yank-pop)
(general-nmap "gc"  'evilnc-comment-or-uncomment-lines)
(general-nmap "%"   'evilmi-jump-items)
;; window movement
(general-nmap "C-h" 'evil-window-left)
(general-nmap "C-j" 'evil-window-down)
(general-nmap "C-k" 'evil-window-up)
(general-nmap "C-l" 'evil-window-right)
;; folding
(general-nmap "zm"  'yafolding-toggle-all)
(general-nmap "zc"  'yafolding-hide-parent-element)
(general-nmap "za"  'yafolding-toggle-element)

;; -------------------------
;; ------ FUNCTIONS --------
;; -------------------------

(defun pp/highlight-symbol-hydra ()
  "Highlights symbol and begins a search hydra."
  (interactive)
  (highlight-symbol)
  (hydra-search/body))

(defun edit-emacs-config ()
  "Open emacs config file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun find-in-project ()
  "Searches in current project."
  (interactive)
  (counsel-ag nil (projectile-project-root)))

(defun find-symbol-at-point ()
  "Searches for symbol under cursor in current project."
  (interactive)
  (counsel-ag (thing-at-point 'symbol) (projectile-project-root)))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun toggle-fullscreen ()
  "Toggle full screen."
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

(defun pp/swiper-at-point ()
  (interactive)
  (swiper (thing-at-point 'symbol)))

(defun terminal-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iterm\"\n"
   ))

(defun pp/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun pp/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun pp/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)

(setq gc-cons-threshold 800000)

(fset 'ruby_toggle_symbol
      (lambda (&optional arg)
        "Keyboard macro for changing :symbol => '' into symbol: ''."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([66 120 69 97 58 kp-delete kp-delete kp-delete escape] 0 "%d")) arg)))
