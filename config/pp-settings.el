;;; pp-settings.el

;; interface tweaks
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-visual-line-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq create-lockfiles nil)
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq save-interprogram-paste-before-kill t)

(put 'narrow-to-page 'disabled nil)

;; encoding
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)

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

;; get rid of fucking "Keep current list of tags tables also?"
(setq tags-add-tables nil)

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

;; parens
(show-paren-mode)
(setq show-paren-delay 0)

(provide 'pp-settings)
