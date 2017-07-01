;;; pp-theme --- theme settings
;;; Commentary:
;;; Code:

;;; font

;; https://www.fontsquirrel.com/fonts/M-1m
;; (set-frame-font "M+ 1mn")

;; https://github.com/mozilla/Fira
;; (set-frame-font "Fira Mono")

;; http://input.fontbureau.com/
(set-frame-font "InputMonoCondensed")
(set-face-attribute 'default nil :height 130)
(setq-default line-spacing nil)

(custom-set-faces '(variable-pitch ((t nil))))

;;; Theme
(add-to-list 'load-path "~/.emacs.d/themes/emacs-doom-theme")
(require 'doom-themes)

;;; Doom theme settings
(setq doom-enable-bold t doom-enable-italic t)

(load-theme 'doom-one t)

;; Enable custom neotree theme
(doom-themes-neotree-config)

;;; Solaire - 'real' buffers are brighter
(use-package solaire-mode
  :config
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode))

(global-hl-line-mode -1)
;;; nlinum
(use-package nlinum
  :commands nlinum-mode
  :preface
  (setq linum-format "%3d ")
  (defvar nlinum-format "%4d ")
  :init
  (add-hook! (markdown-mode prog-mode) 'nlinum-mode)
  :config
  (setq nlinum-highlight-current-line t))

(use-package nlinum-hl
  :config
  ;; A shotgun approach that refreshes line numbers on a regular basis:
  ;; Runs occasionally, though unpredictably
  (add-hook 'post-gc-hook #'nlinum-hl-flush-all-windows)

  ;; whenever Emacs loses/gains focus
  (add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
  (add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)
  ;; ...or switches windows
  (advice-add #'select-window :before #'nlinum-hl-do-select-window-flush)
  (advice-add #'select-window :after  #'nlinum-hl-do-select-window-flush)

  ;; after X amount of idle time
  (run-with-idle-timer 5 t #'nlinum-hl-flush-window)
  (run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows))

(provide 'pp-theme)
;;; pp-theme ends here
