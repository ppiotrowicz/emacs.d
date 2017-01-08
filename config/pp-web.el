;; pacakges for web development

(use-package restclient
  :ensure t
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
  :ensure t
  :config
  (progn
    (setq coffee-tab-width 2)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

(provide 'pp-web)
