(setq mode-line-default-help-echo nil
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      bidi-display-reordering nil
      blink-matching-paren nil
      visible-bell nil
      visible-cursor nil
      x-stretch-cursor t
      use-dialog-box nil
      show-help-function nil)

;; split window sensibly
(setq split-width-threshold nil
      split-height-threshold 80)

;; disable mouse acceleration
(setq mouse-wheel-progressive-speed nil)

;; minibuffer
(setq resize-mini-windows 'grow-only
      max-mini-window-height 0.3)

;; fringe
(setq indicate-buffer-boundaries 'left
      indicate-empty-lines nil
      fringes-outside-margins t
      fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist))

;; uniquify
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; parens
(setq show-paren-delay 0.075
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)

;; jit
(setq jit-lock-defer-time nil
      jit-lock-stealth-nice 0.1
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil)

;; Ask for confirmation on exit only if there are real buffers left
(setq confirm-kill-emacs (lambda (_) (y-or-n-p "››› Quit?")))

(defvar doom--modeline-bg nil)

(setq ring-bell-function 'pp-highlight-modeline)

(defun pp-highlight-modeline ()
    (unless doom--modeline-bg
      (setq doom--modeline-bg (face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line nil :background "#54252C")
  (run-with-timer
   0.1 nil
   (lambda () (set-face-attribute 'mode-line nil :background doom--modeline-bg))))

(defvar doom-hide-mode-line-format nil
  "Format to use when `doom-hide-mode-line-mode' replaces the modeline")

(define-minor-mode pp-hide-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if pp-hide-mode-line-mode
      (setq pp--mode-line mode-line-format
            mode-line-format doom-hide-mode-line-format)
    (setq mode-line-format doom--mode-line
          doom--mode-line doom-hide-mode-line-format)))

;; mode-line is unimportant in help/compile/completion candidate windows
;; (with-current-buffer "*Messages*" (doom-hide-mode-line-mode +1))
(add-hook! (help-mode
            compilation-mode
            messages-buffer-mode
            completion-list-mode)
  'pp-hide-mode-line-mode)

;; (add-hook 'evil-command-window-mode-hook 'doom-hide-mode-line-mode)

;; TODO/FIXME/NOTE highlighting in comments
(add-hook! (prog-mode emacs-lisp-mode css-mode)
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOTE\\(?:(.*)\\)?:?\\)\\>"  1 'success prepend))))

;; `window-divider-mode' gives us finer control over the border between windows.
;; The native border "consumes" a pixel of the fringe on righter-most splits (in
;; Yamamoto's emacs-mac at least), window-divider does not.
;; NOTE Only available on Emacs 25.1+
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(use-package shackle
  :config
  (shackle-mode 1))

(provide 'pp-core-ui)
