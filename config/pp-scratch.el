;; pp-scratch.el

(setq initial-scratch-message
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; " ; comment each line
        (replace-regexp-in-string
         "\n$" ""    ; remove trailing linebreak
         (shell-command-to-string "fortune")))))


;; (setq initial-major-mode 'fundamental-mode
;;       initial-scratch-message "\n  Loading..."
;;       inhibit-startup-screen t
;;       ;; shuts up emacs at startup
;;       inhibit-startup-echo-area-message user-login-name)

(provide 'pp-scratch)
