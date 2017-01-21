(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports
   (quote
    (("current balance" "ledger -f /Users/ppiotrowicz/finance/ledger.dat bal assets --cleared")
     (#("expenses monthly" 0 1
        (idx 0))
      "ledger -f %(ledger-file) reg expenses -M")
     (#("incomes monthly" 0 1
        (idx 1))
      "ledger -f %(ledger-file) reg incomes -M")
     (#("bal" 0 1
        (idx 3))
      "ledger -f %(ledger-file) bal")
     (#("reg" 0 1
        (idx 4))
      "ledger -f %(ledger-file) reg")
     (#("payee" 0 1
        (idx 5))
      "ledger -f %(ledger-file) payee")
     (#("account" 0 1
        (idx 6))
      "ledger -f %(ledger-file) account")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
