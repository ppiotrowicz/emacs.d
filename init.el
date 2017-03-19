;;; init.el

(load (concat user-emacs-directory "config/pp-core.el"))

(pp-emacs pp-packages
          pp-core-defuns
          pp-core-ui
          pp-core-vcs
          pp-settings
          pp-editor
          pp-theme
          pp-interface
          pp-evil
          pp-scratch
          pp-development
          pp-org-mode
          pp-ruby
          pp-web
          pp-ledger
          pp-modeline
          pp-hydras
          pp-keybindings
          pp-funcs)
