(use-package alchemist
  :config
  (progn
    (defvar pp/elixir-map (make-sparse-keymap) "Elixir keymap.")
    (general-define-key
     :keymaps 'pp/elixir-map
     ":"  '(alchemist-mix                           :which-key "Mix")
     "a"  '(alchemist-project-toggle-file-and-tests :which-key "alternate")
     "g"  '(alchemist-goto-list-symbol-definitions  :which-key "symbols")
     ;; tests
     "t"  '(:ignore t                               :which-key "test")
     "ta" '(alchemist-mix-test                      :whick-key "run all")
     "tb" '(alchemist-mix-test-this-buffer          :whick-key "run buffer")
     "tf" '(alchemist-mix-test-file                 :whick-key "run file")
     "tt" '(alchemist-mix-test-at-point             :whick-key "run")
     "tr" '(alchemist-mix-rerun-last-test           :whick-key "rerun")
     )
    (bind-map pp/elixir-map
      :evil-keys (",")
      :major-modes (elixir-mode))
    )
  )

(add-hook 'elixir-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map "C-]" 'alchemist-goto-definition-at-point)
            (define-key evil-normal-state-local-map "C-[" 'alchemist-goto-jump-back)
            ))

(provide 'pp-elixir)
