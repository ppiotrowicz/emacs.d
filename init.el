;;; init.el

(load (concat user-emacs-directory "config/pp-core.el"))

(pp-emacs pp-core-defuns
          pp-core-ui
          pp-core-vcs
          pp-settings
          pp-editor
          pp-dired
          pp-theme
          pp-interface
          pp-dashboard
          pp-evil
          pp-development
          pp-scratch
          pp-org-mode
          pp-ruby
          pp-js
          pp-web
          pp-ledger
          pp-modeline
          pp-hydras
          pp-keybindings
          pp-funcs
          pp-elfeed)
