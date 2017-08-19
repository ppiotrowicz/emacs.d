(use-package evil
  :demand t
  :init
  (progn
    ;; shorter mode names
    (setq
        evil-normal-state-tag    "N"
        evil-insert-state-tag    "I"
        evil-visual-state-tag    "V"
        evil-emacs-state-tag     "E"
        evil-operator-state-tag  "O"
        evil-motion-state-tag    "M"
        evil-replace-state-tag   "R")

    ;; https://bitbucket.org/lyro/evil/issues/444/evils-undo-granularity-is-too-coarse
    (setq evil-want-fine-undo 'fine)

    ;; Cursors
    (setq
        evil-default-cursor (face-attribute 'cursor :background nil t)
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  `(,(face-attribute 'warning :foreground nil nil) box)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'box))
  :config
  (progn
    (evil-mode 1)

    ;; States that use emacs mode

    (mapc (lambda (r) (evil-set-initial-state (car r) (cdr r)))
          '((comint-mode              . emacs)
            (eshell-mode              . emacs)
            (term-mode                . emacs)
            (inf-ruby-mode            . emacs)
            (flycheck-error-list-mode . emacs)
            (ledger-report-mode       . emacs)))

    ;; ESC quits stuff
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    ;; On OSX, stop copying each visual state move to the clipboard:
    (when (or (featurep 'mac) (featurep 'ns)
              (advice-add 'evil-visual-update-x-selection :override 'ignore)))))

(use-package evil-matchit
  :commands evilmi-jump-items
  :init
  (progn
    (global-evil-matchit-mode 1)))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :commands (evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(use-package evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match)
  :init
  (setq evil-multiedit-use-symbols t)
  :config
  (progn
    (let ((me-map  evil-multiedit-state-map)
          (mei-map evil-multiedit-insert-state-map))
      (define-key me-map (kbd "M-d") #'evil-multiedit-match-and-next)
      (define-key me-map (kbd "M-D") #'evil-multiedit-match-and-prev)
      (define-key me-map (kbd "RET") #'evil-multiedit-toggle-or-restrict-region)

      (dolist (map (list me-map mei-map))
        (define-key map (kbd "C-n") #'evil-multiedit-next)
        (define-key map (kbd "C-p") #'evil-multiedit-prev)))))

(let ((map evil-visual-state-map))
  (define-key map (kbd "M-d")   #'evil-multiedit-match-and-next)
  (define-key map (kbd "M-D")   #'evil-multiedit-match-and-prev)
  (define-key map (kbd "C-M-d") #'evil-multiedit-restore)
  (define-key map (kbd "R")     #'evil-multiedit-match-all))

(let ((map evil-normal-state-map))
  (define-key map (kbd "M-d")   #'evil-multiedit-match-and-next)
  (define-key map (kbd "M-D")   #'evil-multiedit-match-and-prev)
  (define-key map (kbd "C-M-d") #'evil-multiedit-restore)
  (define-key map (kbd "R")     #'evil-multiedit-match-all))

(use-package evil-anzu
  :defer t
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250))

(use-package evil-snipe
  :config
  (evil-snipe-override-mode 1)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(provide 'pp-evil)
;;; pp-evil.el ends here
