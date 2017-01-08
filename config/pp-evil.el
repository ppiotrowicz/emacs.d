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

(provide 'pp-evil)
