(use-package elixir-mode
  :config
  (progn
    (use-package exunit)

    (defvar pp/elixir-map (make-sparse-keymap) "Elixir keymap.")
    (general-define-key
     :keymaps 'pp/elixir-map
     "="  '(elixir-format                           :which-key "format")
     "t"  '(:ignore t                               :which-key "test")
     "ta" '(exunit-verify-all                       :which-key "run all")
     "tb" '(exunit-verify                           :which-key "run buffer")
     "tr" '(exunit-rerun                            :which-key "rerun")
     "tt" '(exunit-verify-single                    :which-key "run")
     "h"  '(:ignore t                               :which-key "help")
     "hh" '(eglot-help-at-point                     :which-key "show doc")
     "hH" '(display-local-help                      :which-key "local help")
     "f"  '(:ignore t                               :which-key "find")
     "ff" '(xref-find-definitions                   :which-key "definitions")
     "fF" '(xref-find-definitions-other-window      :which-key "def in other window")
     "fr" '(xref-find-references                    :which-key "references")
     )

    (bind-map pp/elixir-map
      :evil-keys (",")
      :major-modes (elixir-mode))
    ))

(add-hook 'elixir-mode-hook 'flycheck-mode)

(use-package flycheck-credo
  :init
  (flycheck-credo-setup)
  :config
  (setq flycheck-elixir-credo-strict t))

(add-hook 'elixir-mode-hook
          (lambda ()
            (general-nmap "C-]" 'xref-find-definitions)
            ))

(provide 'pp-elixir)
