;; pp-typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode -1)
  (company-mode +1)
  (setq tide-format-options '(:indentSize 2))
  (setq typescript-indent-level 2
        typescript-expr-indent-offset 2)

  (defvar pp/typescript-map (make-sparse-keymap) "Typescript keymap.")
  (general-define-key
   :keymaps 'pp/typescript-map
   "]"  '(tide-jump-to-definition     :which-key "Jump to definition")
   "["  '(tide-jump-back                   :which-key "Jump to definition")

   "ss" '(tide-start-server           :which-key "Start server")
   "sr" '(tide-restart-server         :which-key "Restart server")
   "t"  '(tide-references             :whick-key "Show references")
   "="  '(tide-format                 :which-key "Format")
   "i"  '(tide-organize-imports       :which-key "Organize imports")
   ","  '(tide-fix                    :which-key "Fix")
   "."  '(tide-refactor               :which-key "Refactor")
   "r"  '(tide-rename-symbol          :which-key "Rename")
   "?"  '(tide-documentation-at-point :which-key "Show docs")
   )
  (bind-map pp/typescript-map
    :evil-keys (",")
    :major-modes (typescript-mode))
  )

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(provide 'pp-typescript)
