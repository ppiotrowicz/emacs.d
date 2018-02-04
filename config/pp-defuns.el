(defun pp/open-eshell ()
  "Opens eshell in current project or current dir"
  (interactive)
  (let ((directory (or (ignore-errors (projectile-project-root))
                       (ignore-errors (file-name-directory (buffer-file-name)))
                       (getenv "HOME")
                       )))
        (message directory)
        (setq default-directory directory)
        (eshell)))

(provide 'pp-defuns)
