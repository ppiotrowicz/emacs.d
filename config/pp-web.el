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

(provide 'pp-web)
