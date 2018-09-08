;;; pp-theme --- theme settings
;;; Commentary:
;;; Code:

;;; font

;; https://www.fontsquirrel.com/fonts/M-1m
;; (set-frame-font "M+ 1mn")

;; https://github.com/mozilla/Fira
;; (set-frame-font "Fira Mono")

;; http://input.fontbureau.com/
;; (set-frame-font "InputMonoCondensed")

(set-frame-font "Operator Mono")
(set-face-attribute 'default nil :height 150)
(setq-default line-spacing 3)

(custom-set-faces '(variable-pitch ((t nil))))

;;; Theme
(add-to-list 'load-path "~/.emacs.d/themes/emacs-doom-theme")
(require 'doom-themes)

;;; Doom theme settings
(setq doom-enable-bold t doom-enable-italic t)

(load-theme 'doom-one t)

;; Enable custom neotree theme
(doom-themes-neotree-config)

(doom-themes-org-config)

;;; Solaire - 'real' buffers are brighter
(use-package solaire-mode
  :config
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode))

(global-hl-line-mode -1)

(add-hook! (markdown-mode prog-mode)
  (setq display-line-numbers t))

(provide 'pp-theme)
;;; pp-theme ends here
