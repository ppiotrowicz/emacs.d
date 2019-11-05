;;; pp-theme --- theme settings
;;; Commentary:
;;; Code:

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . dark))


;;; font

;; https://www.fontsquirrel.com/fonts/M-1m
;; (set-frame-font "M+ 1m")

;; https://github.com/mozilla/Fira
;; (set-frame-font "Fira Mono")

;; https://github.com/tonsky/FiraCode
;; (set-frame-font "Fira Code")

(set-frame-font "Iosevka")
(setq-default line-spacing 1)

;; http://input.fontbureau.com/
;; (set-frame-font "InputMonoCondensed")

;; (set-frame-font "Operator Mono")
(set-face-attribute 'default nil :height 160)
;; (setq-default line-spacing 3)

(custom-set-faces '(variable-pitch ((t nil))))

;;; Theme
(add-to-list 'load-path "/Users/ppiotrowicz/.emacs.d/themes/emacs-doom-theme")
(setq custom-theme-directory "/Users/ppiotrowicz/.emacs.d/themes/emacs-doom-theme/themes")
(require 'doom-themes)
(require 'doom-themes-ext-neotree)
(require 'doom-themes-ext-org)
(require 'doom-themes-ext-visual-bell)

;;; Doom theme settings
(setq doom-enable-bold t doom-enable-italic t)

;; (load-theme 'doom-one t)
;; (load-theme 'doom-vibrant t)
(load-theme 'doom-gruvbox t)

;; Enable custom neotree theme
(doom-themes-neotree-config)

(doom-themes-org-config)

(doom-themes-visual-bell-config)
;; WORKAROUND: use legacy codes
;; (set-face-attribute 'doom-visual-bell nil
;;                     :background (face-foreground 'error)
;;                     :inverse-video nil)
(set-face-attribute 'doom-visual-bell nil
                    :background "#54252c"
                    :inverse-video nil)

(defvar doom-themes--bell-p nil)
(defun doom-themes-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (unless doom-themes--bell-p
    (let ((old-remap (copy-alist face-remapping-alist)))
      (setq doom-themes--bell-p t)
      (setq face-remapping-alist
            (append (delete (assq 'mode-line face-remapping-alist)
                            face-remapping-alist)
                    '((mode-line doom-visual-bell))))
      (force-mode-line-update)
      (run-with-timer 0.15 nil
                      (lambda (remap buf)
                        (with-current-buffer buf
                          (when (assq 'mode-line face-remapping-alist)
                            (setq face-remapping-alist remap
                                  doom-themes--bell-p nil))
                          (force-mode-line-update)))
                      old-remap
                      (current-buffer)))))

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
