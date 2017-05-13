(use-package evil
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
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines))


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
  :config (evil-multiedit-default-keybinds))

(use-package evil-anzu
  :defer t
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250))

(provide 'pp-evil)
;;; pp-evil.el ends here
