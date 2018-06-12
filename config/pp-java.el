(use-package lsp-javacomp
  :commands lsp-javacomp-enable
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              ;; Load company-lsp before enabling lsp-javacomp, so that function
              ;; parameter snippet works.
              (require 'company-lsp)
              (lsp-javacomp-enable)
              ;; Use company-lsp as the company completion backend
              (set (make-variable-buffer-local 'company-backends) '(company-lsp))
              ;; Optional company-mode settings
              (set (make-variable-buffer-local 'company-idle-delay) 0.1)
              (set (make-variable-buffer-local 'company-minimum-prefix-length) 1)))
  ;; Optional, make sure JavaComp is installed. See below.
  :config
  (lsp-javacomp-install-server)))

(provide 'pp-java)
