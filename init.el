;;; init.el

(load (concat user-emacs-directory "config/pp-core.el"))

(pp-emacs pp-core-defuns
          pp-core-ui
          pp-core-vcs
          pp-defuns
          pp-settings
          pp-editor
          pp-dired
          pp-theme
          pp-interface
          pp-evil
          pp-development
          pp-scratch
          pp-org-mode
          pp-ruby
          pp-elixir
          pp-java
          pp-web
          pp-beancount
          pp-modeline
          pp-hydras
          pp-keybindings
          pp-funcs)
