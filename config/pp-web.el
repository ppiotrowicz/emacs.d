;; packages for web development

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode))
  :config
  (progn
    (defvar pp/restclient-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "s") 'restclient-http-send-current)
        (define-key map (kbd "c") 'restclient-copy-curl-command)
        map)
      "Restclient keymap.")

    (bind-map pp/restclient-map
      :evil-keys (",")
      :major-modes (restclient-mode))))

(use-package coffee-mode
  :config
  (progn
    (setq coffee-tab-width 2)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode
  :config
  (setq js-indent-level 2))

(use-package csv-mode)

(use-package emmet-mode
  :config
  (setq emmet-expand-jsx-className? t)
  (setq emmet-self-closing-tag-style "/"))
(add-hook! 'rjsx-mode-hook 'emmet-mode)

(use-package js-import
  :config
  (setq js-import-quote "'"))

(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode))
  :config
  (progn
    (defvar pp/js-map (make-sparse-keymap) "Js/React keymap.")
    (general-define-key        :keymaps 'pp/js-map
     "i" '(js-import           :which-key "import")
     "I" '(js-import-dev       :which-key "import dev")
     )
    (bind-map pp/js-map
      :evil-keys (",")
      :major-modes (rjsx-mode))
    ))

(add-hook 'rjsx-mode-hook #'add-node-modules-path)
(add-hook 'rjsx-mode-hook #'prettier-js-mode)
(setq css-indent-offset 2)

(provide 'pp-web)
