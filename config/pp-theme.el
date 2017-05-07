;; theme

(add-to-list 'load-path "~/.emacs.d/themes/emacs-doom-theme")
(require 'doom-themes)

;;; Settings (defaults)
(setq doom-enable-bold t    ; if nil, bolding are universally disabled
      doom-enable-italic t  ; if nil, italics are universally disabled

      ;; doom-one specific settings
      doom-one-brighter-modeline nil
      doom-one-brighter-comments nil)

(load-theme 'doom-one t)

;; brighter source buffers (that represent files)
(add-hook 'find-file-hook 'doom-buffer-mode-maybe)
;; ...if you use auto-revert-mode
(add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
;; And you can brighten other buffers (unconditionally) with:
(add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)

;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Enable nlinum line highlighting
(doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode

;; font
(set-default-font "Fira Mono for Powerline")
(set-face-attribute 'default nil :height 120)

(provide 'pp-theme)
