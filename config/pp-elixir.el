(use-package elixir-mode
  :config
  (progn
    (use-package exunit)

    (defvar pp/elixir-map (make-sparse-keymap) "Elixir keymap.")
    (general-define-key
     :keymaps 'pp/elixir-map
     "t"  '(:ignore t                               :which-key "test")
     "ta" '(exunit-verify-all                       :which-key "run all")
     "tb" '(exunit-verify                           :which-key "run buffer")
     "tr" '(exunit-rerun                            :which-key "rerun")
     "tt" '(exunit-verify-single                    :which-key "run")
     "h"  '(:ignore t                               :which-key "help")
     "hh" '(lsp-ui-doc-show                         :which-key "show doc")
     "hH" '(lsp-ui-doc-hide                         :which-key "hide doc")
     )

    (bind-map pp/elixir-map
      :evil-keys (",")
      :major-modes (elixir-mode))
    ))

(use-package flycheck-credo
  :init
  (flycheck-credo-setup)
  :config
  (setq flycheck-elixir-credo-strict t))

;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs `(elixir-mode "/Users/ppiotrowicz/code/elixir-ls/release")))

;; (add-hook 'elixir-mode-hook
;;           (lambda ()
;;             (define-key evil-normal-state-local-map "C-]" 'alchemist-goto-definition-at-point)
;;             (define-key evil-normal-state-local-map "C-[" 'alchemist-goto-jump-back)
;;             ))

(provide 'pp-elixir)
