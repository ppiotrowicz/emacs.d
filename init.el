;;; init.el

(load (concat user-emacs-directory "config/pp-core.el"))

(pp-emacs pp-core-defuns
          pp-core-ui
          pp-core-vcs
          pp-defuns
          pp-settings
          pp-shell
          pp-editor
          pp-dired
          pp-theme
          pp-interface
          pp-perspective
          pp-evil
          pp-development
          pp-scratch
          pp-org-mode
          pp-ruby
          pp-lsp
          pp-elixir
          pp-web
          pp-modeline
          pp-hydras
          pp-keybindings
          pp-funcs)
