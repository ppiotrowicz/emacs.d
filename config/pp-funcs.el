;; custom functions

(defun pp/highlight-symbol-hydra () "Highlights symbol and begins a search hydra."
  (interactive)
  (highlight-symbol)
  (hydra-search/body))

(defun pp/edit-emacs-config ()
  "Open emacs config file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun pp/find-in-project ()
  "Searches in current project."
  (interactive)
  (counsel-ag nil (projectile-project-root)))

(defun pp/find-symbol-at-point ()
  "Searches for symbol under cursor in current project."
  (interactive)
  (counsel-ag (thing-at-point 'symbol) (projectile-project-root)))

(defun pp/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun pp/toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun pp/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun pp/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun pp/swiper-at-point ()
  (interactive)
  (swiper (thing-at-point 'symbol)))

(defun pp/terminal-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iterm\"\n"
   ))

(defun pp/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun pp/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun pp/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)

(setq gc-cons-threshold 800000)

(fset 'ruby_toggle_symbol
      (lambda (&optional arg)
        "Keyboard macro for changing :symbol => '' into symbol: ''."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([66 120 69 97 58 kp-delete kp-delete kp-delete escape] 0 "%d")) arg)))

(provide 'pp-funcs)
