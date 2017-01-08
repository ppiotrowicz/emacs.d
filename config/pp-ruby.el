(use-package ruby-mode
  :ensure t
  :defer t
  :config
   (progn
     (defvar pp/ruby-map (make-sparse-keymap) "Ruby keymap.")
     (general-define-key
      :keymaps 'pp/ruby-map
      ;; bundle
      "b"  '(:ignore t               :which-key "bundle")
      "bi" '(bundle-install          :which-key "bundle install")
      "bo" '(bundle-open             :which-key "bundle open")
      "be" '(bundle-exec             :which-key "bundle exec")
      "bc" '(bundle-console          :which-key "bundle console")
      "bu" '(bundle-update           :which-key "bundle update")
      ;; testing
      "t"  '(:ignore t               :which-key "rspec")
      "ta" '(rspec-verify-all        :which-key "run all")
      "tb" '(rspec-verify            :which-key "run buffer")
      "tl" '(rspec-run-last-failed   :which-key "last failed")
      "tr" '(rspec-rerun             :which-key "rerun")
      "tt" '(rspec-verify-single     :which-key "run")
      "tk" '((lambda () (interactive) (kill-buffer "*rspec-compilation*")) :which-key "stop")
      ;; rbenv
      "v"  '(:ignore t               :which-key "rbenv")
      "vc" '(rbenv-use-corresponding :which-key "use local")
      "vg" '(rbenv-use-global        :which-key "use global")
      )
    (bind-map pp/ruby-map
      :evil-keys (",")
      :major-modes (ruby-mode))
    (use-package inf-ruby
      :ensure t)
    (use-package rbenv
      :ensure t
      :config
      (progn
        (global-rbenv-mode)
        (set-face-attribute 'rbenv-active-ruby-face nil
                            :inherit 'mode-line-face
                            :foreground "#eab700")
        (setq rspec-autosave-buffer t)
        (setq rspec-spec-command "rspec --format progress --no-profile")
        (add-hook 'projectile-after-switch-project-hook 'rbenv-use-corresponding)))
    (use-package rspec-mode
      :ensure t
      :config
      (progn
        (setq compilation-scroll-output t)
        (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)))
    (use-package bundler
      :ensure t)))

(provide 'pp-ruby)
