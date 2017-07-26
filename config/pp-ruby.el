(use-package ruby-mode
  :defer t
  :config
  (progn
    (defvar pp/ruby-map (make-sparse-keymap) "Ruby keymap.")
    (general-define-key
     :keymaps 'pp/ruby-map
     ":"  '(pp/ruby-19-hash             :which-key "ruby 1.9 hash")
     "'"  '(pp/ruby-string-to-symbol    :which-key "string to sym")
     ;; bundle
     "b"  '(:ignore t                   :which-key "bundle")
     "bi" '(bundle-install              :which-key "bundle install")
     "bo" '(bundle-open                 :which-key "bundle open")
     "be" '(bundle-exec                 :which-key "bundle exec")
     "bc" '(bundle-console              :which-key "bundle console")
     "bu" '(bundle-update               :which-key "bundle update")
     ;; rubocop
     "r"  '(:ignore t                   :which-key "rubocop")
     "rp" '(rubocop-check-project       :which-key "check project")
     "rr" '(rubocop-check-current-file  :which-key "check file")
     "ra" '(rubocop-autocorrect-current-file :which-key "autocorrect file")
     ;; testing
     "t"  '(:ignore t                   :which-key "rspec")
     "ta" '(rspec-verify-all            :which-key "run all")
     "tb" '(rspec-verify                :which-key "run buffer")
     "tl" '(rspec-run-last-failed       :which-key "last failed")
     "tr" '(rspec-rerun                 :which-key "rerun")
     "tt" '(rspec-verify-single         :which-key "run")
     "tk" '((lambda () (interactive) (kill-buffer "*rspec-compilation*")) :which-key "stop")
     ;; rbenv
     "v"  '(:ignore t                   :which-key "rbenv")
     "vc" '(rbenv-use-corresponding     :which-key "use local")
     "vg" '(rbenv-use-global            :which-key "use global")
     )
    (setq ruby-insert-encoding-magic-comment nil)
    (bind-map pp/ruby-map
      :evil-keys (",")
      :major-modes (ruby-mode))
    (use-package inf-ruby)
    (use-package rbenv
      :config
      (progn
        (global-rbenv-mode)
        (set-face-attribute 'rbenv-active-ruby-face nil
                            :inherit 'mode-line-face
                            :foreground "#eab700")
        (add-hook 'projectile-after-switch-project-hook 'rbenv-use-corresponding)))
    (use-package rspec-mode
      :config
      (progn
        (def-popup! "\\*rspec-compilation\\*" :align below :size 14 :noselect t :regexp t :popup t)
        (setq compilation-scroll-output t)
        (setq rspec-autosave-buffer t)
        (setq rspec-spec-command "rspec --format progress --no-profile")
        (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)))
    (use-package bundler
      :config
      (def-popup! "\\*Bundler\\*" :align below :size 14 :noselect t :regexp t :popup t))
    (use-package rubocop
      :config
      (def-popup! "\\*RuboCop.+\\*" :align below :size 14 :noselect t :regexp t))))

(add-hook! rspec-compilation-mode
  (toggle-truncate-lines -1))

;;; refactorings
(defun pp/ruby-19-hash ()
  "Convert old hashrocket syntax to ruby 1.9 hash"
  (interactive)
  (let (symbol)
    (save-excursion
      (save-restriction
        (narrow-to-region (line-beginning-position) (line-end-position))
        (if (looking-at-p ":")
            (forward-char 1))
        (skip-chars-backward "^:")
        (if (looking-at-p "[a-z_]+ =>")
            (progn
              (delete-backward-char 1)
              (search-forward " =>" nil t)
              (replace-match ":")))))))

(defun pp/ruby-string-to-symbol ()
  "Convert string to symbol"
  (interactive)
  (if (looking-at-p "\w*\"")
      (evil-surround-delete ?\")
    (evil-surround-delete ?\'))
  (insert ":"))


(provide 'pp-ruby)
