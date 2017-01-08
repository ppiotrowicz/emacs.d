;; ledger mode configuration

(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (progn
    (defvar pp/ledger-map (make-sparse-keymap) "Ledger keymap.")
    (general-define-key
     :keymaps 'pp/ledger-map
     "a"  '(ledger-add-transaction     :which-key "add transaction")
     "="  '(ledger-mode-clean-buffer   :which-key "clean")
     "r"  '(ledger-report              :which-key "reports")
     )
    (bind-map pp/ledger-map
      :evil-keys (",")
      :major-modes (ledger-mode)))
  :mode (("\\.dat" . ledger-mode)))

(provide 'pp-ledger)
