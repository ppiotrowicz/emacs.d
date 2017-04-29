;; theme

(add-to-list 'load-path "~/.emacs.d/themes/emacs-doom-theme")
(require 'doom-themes)
(require 'doom-neotree)

(load-theme 'doom-one t)
;; brighter source buffers
(add-hook 'find-file-hook 'doom-buffer-mode-maybe)
;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook 'doom-buffer-mode)

;; font
(set-default-font "Fira Mono for Powerline")
(set-face-attribute 'default nil :height 120)

(provide 'pp-theme)
