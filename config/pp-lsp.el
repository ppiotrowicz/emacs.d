(use-package lsp-mode
  :commands lsp
  :ensure t
  :hook
  (elixir-mode . lsp)
  (rjsx-mode   . lsp)
  :init
  (add-to-list 'exec-path "/Users/ppiotrowicz/code/elixir-ls/release"))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-peek-enable nil))

(use-package company-lsp :commands company-lsp)

(add-hook 'elixir-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-checkers 'lsp-ui t)

(provide 'pp-lsp)
