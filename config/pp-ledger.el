;; ledger mode configuration

(use-package ledger-mode
  :defer t
  :config
  (progn
    (def-popup! "*Ledger Report*" :align below :size 14 :select t :regexp nil :popup t)
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

    (define-key ledger-report-mode-map "\C-h" 'evil-window-left)
    (define-key ledger-report-mode-map "\C-j" 'evil-window-down)
    (define-key ledger-report-mode-map "\C-k" 'evil-window-up)
    (define-key ledger-report-mode-map "\C-l" 'evil-window-right)

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
