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
      :major-modes (ledger-mode))

    (setq ledger-reports
      (quote
       (("expenses monthly" "ledger -f %(ledger-file) reg expenses -M")
        ("incomes monthly"  "ledger -f %(ledger-file) reg incomes -M")
        ("current balance"  "ledger -f %(ledger-file) bal assets")
        ("bal"              "ledger -f %(ledger-file) bal")
        ("reg"              "ledger -f %(ledger-file) reg")
        ("payee"            "ledger -f %(ledger-file) payee")
        ("account"          "ledger -f %(ledger-file) account")
        )))
    )
  :mode (("\\.dat" . ledger-mode)))

(provide 'pp-ledger)
