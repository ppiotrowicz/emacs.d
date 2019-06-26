;;; ui/doom-modeline/config.el -*- lexical-binding: t; -*-

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :config
      (setq doom-modeline-buffer-file-name-style 'relative-from-project)
      (setq doom-modeline-buffer-encoding nil)
      (setq doom-modeline-mu4e nil)
      (setq doom-modeline-irc nil)
      (setq doom-modeline-checker-simple-format nil)
      (setq doom-modeline-buffer-state-icon t)
      (setq doom-modeline-enable-word-count t)
)

(provide 'pp-modeline)
