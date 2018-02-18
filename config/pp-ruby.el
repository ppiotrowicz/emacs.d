(use-package ruby-mode
  :defer t
  :config
  (progn
    (defvar pp/ruby-map (make-sparse-keymap) "Ruby keymap.")
    (general-define-key
     :keymaps 'pp/ruby-map
     ":"  '(pp/ruby-19-hash             :which-key "ruby 1.9 hash")
     "'"  '(pp/ruby-string-to-symbol    :which-key "string to sym")
     "a"  '(projectile-find-implementation-or-test-other-window :which-key "implementation or test")
     "p"  '(pp/insert-pry               :which-key "insert binding.pry")
     "ym" '(pp/copy-module              :which-key "copy module name")
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
     "t"  '(:ignore t                   :which-key "spec")
     "ta" '(pp/spec-verify-all          :which-key "run all")
     "tb" '(pp/spec-verify              :which-key "run buffer")
     "tl" '(pp/spec-run-last-failed     :which-key "last failed")
     "tr" '(pp/spec-rerun               :which-key "rerun")
     "tt" '(pp/spec-verify-single       :which-key "run")
     "tk" '(pp/spec-stop-spec           :which-key "stop")
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

        (defadvice rspec-compile
            (before rspec-save-before-compile (A-FILE-OR-DIR &optional opts) activate)
          "Save current buffer before running spec.  This remove the annoying save confirmation."
          (save-some-buffers (lambda () (string-match "\\.rb" (buffer-name (current-buffer))))))
        ))
    (use-package minitest
      :config
      (progn
        (def-popup! "\\*minitest .*" :align below :size 14 :noselect t :regexp t :popup t)
        (setenv "EMACS" "t") ; makes default reporter output colorful
        (defun minitest--post-command (cmd str)
          (format "%s" (replace-regexp-in-string "[\s#:]" " " str)))))
    (use-package bundler
      :config
      (def-popup! "\\*Bundler\\*" :align below :size 14 :noselect t :regexp t :popup t))
    (use-package rubocop
      :config
      (def-popup! "\\*RuboCop.+\\*" :align below :size 14 :noselect t :regexp t))))

(add-hook! rspec-compilation-mode
  (toggle-truncate-lines -1))

(add-hook! 'compilation-filter-hook 'inf-ruby-auto-enter)

(add-hook! 'ruby-mode-hook (setq-local flycheck-command-wrapper-function
                                 (lambda (command)
                                   (append '("bundle" "exec") command))))

(add-hook! 'ruby-mode-hook
  (whitespace-mode +1))

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

(defcustom pp/ruby-test-framework "rspec"
  "Specifies test framework for ruby"
  :safe (lambda (x) t))

(defun pp/spec-verify-single ()
  "Runs rspec or minitest on single spec"
  (interactive)
  (cond ((string= pp/ruby-test-framework "rspec") (rspec-verify-single))
        ((string= pp/ruby-test-framework "minitest") (minitest-verify-single))
      ))

(defun pp/spec-verify ()
  "Runs rspec or minitest on entire buffer"
  (interactive)
  (cond ((string= pp/ruby-test-framework "rspec") (rspec-verify))
        ((string= pp/ruby-test-framework "minitest") (minitest-verify))
      ))

(defun pp/spec-verify-all ()
  "Runs rspec or minitest on entire spec suite"
  (interactive)
  (cond ((string= pp/ruby-test-framework "rspec") (rspec-verify-all))
        ((string= pp/ruby-test-framework "minitest") (minitest-verify-all))
      ))

(defun pp/spec-rerun ()
  "Reruns rspec or minitest"
  (interactive)
  (cond ((string= pp/ruby-test-framework "rspec") (rspec-rerun))
        ((string= pp/ruby-test-framework "minitest") (minitest-rerun))
      ))

(defun pp/spec-run-last-failed ()
  "Runs rspec or minitest on failed tests"
  (interactive)
  (cond ((string= pp/ruby-test-framework "rspec") (rspec-run-last-failed))
        ((string= pp/ruby-test-framework "minitest") (minitest-rerun))
      ))

(defun pp/spec-stop-spec ()
  "Kills rspec or minitest compilation buffers"
  (interactive)
  (cond ((string= pp/ruby-test-framework "rspec") (kill-buffer "*rspec-compilation*"))
        ((string= pp/ruby-test-framework "minitest") (kill-buffer "*minitest "))
      ))

(defun pp/pry-jump-to-source ()
  "Jumps to source location given debugger output"
  (interactive)
  (delete-other-windows)
  (when (save-excursion (search-backward-regexp "From: \\(.*\.rb\\) @ line \\([0-9]+\\)")))
  (let ((file (match-string 1))
        (line (string-to-number (match-string 2))))
    (find-file-other-window file)
    (goto-line line)))

(defun pp/insert-pry ()
  "Inserts require 'pry'; binding.pry"
  (interactive)
  (insert "require 'pry'; binding.pry")
  (c-indent-command))

(defun pp/copy-module ()
  (interactive)
  (let* ((class_name '())
         (module_regex "\\bmodule \\w+\\b")
         (class_regex "\\bclass \\w+\\b"))

    (save-excursion
      (goto-char (point-max))

      (while (re-search-backward class_regex nil t)
        (add-to-list 'class_name (substring (match-string-no-properties 0) 6)))

      (while (re-search-backward module_regex nil t)
        (add-to-list 'class_name (substring (match-string-no-properties 0) 7))))
    (kill-new (s-join "::" class_name))
  ))

(provide 'pp-ruby)
