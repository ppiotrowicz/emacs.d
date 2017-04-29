(require 'recentf)
(require 'calendar)
(require 'org-agenda)

(defconst pp-dashboard-buffer-name "*dashboard*"
  "Dashboard's buffer name.") 
(defconst pp-dashboard-width 80
  "Dashboard's width.")

(defun pp/dashboard-create ()
  "Create dashboard if it doesn't exist yet"
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer pp-dashboard-buffer-name)))
        (save-line nil))
    (when (not buffer-exists)
      (pp/dashboard-redraw))))

(defun pp/dashboard-redraw ()
  "Redraw dashboard"
  (interactive)
  (with-current-buffer (get-buffer-create pp-dashboard-buffer-name)
    (erase-buffer)
    (let* ((buffer-width (window-width (get-buffer-window pp-dashboard-buffer-name)))
           (margin-left (/ (- buffer-width pp-dashboard-width) 2))
           (margin (s-repeat margin-left " ")))
      (pp/dashboard-insert-banner (concat user-emacs-directory "banner.png"))
      (pp/dashboard-insert-title "Recent files" margin)
      (pp/dashboard-insert-recents 5 margin)
      (pp/dashboard-insert-title "Projects" margin)
      (pp/dashboard-insert-projects 5 margin)
      (pp/dashboard-insert-title "Today" margin)
      (pp/dashboard-insert-agenda 5 margin)
      (pp/dashboard-insert-break margin))
    (goto-char (point-min))
  ))

(defun pp/dashboard-refresh-buffer ()
  "Refresh buffer."
  (interactive)
  (kill-buffer pp-dashboard-buffer-name)
  (pp/dashboard-create)
  (switch-to-buffer pp-dashboard-buffer-name))

;;;###autoload
(defun pp/dashboard-setup-startup-hook ()
  "Setup post initialization hooks. If a command line argument is provided, assume a filename and skip displaying Dashboard"
  (if (< (length command-line-args) 2 )
      (progn
        (add-hook 'after-init-hook (lambda () (pp/dashboard-create)))
        (add-hook 'emacs-startup-hook '(lambda ()
                                         (switch-to-buffer "*dashboard*")
                                         (goto-char (point-min))
                                         (redisplay))))))

;; widgets

(defun pp/dashboard-subseq (seq start end)
  "Return the subsequence of SEQ from START to END..
Uses `cl-subseq`, but accounts for end points greater than the size of the
list.
Return entire list if `END' is omitted."
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defun pp/dashboard-insert-banner (banner)
  "Display an image BANNER."
  (when (file-exists-p banner)
    (let* ((spec (create-image banner))
           (size (image-size spec))
           (width (car size))
           (left-margin (max 0 (floor (- (window-width) width) 2))))
      (goto-char (point-min))
      (insert "\n")
      (insert (make-string left-margin ?\ ))
      (insert-image spec)
      (insert "\n\n")
      )))

(defun pp/dashboard-insert-break (margin)
  "Insert a page break line in dashboard buffer."
  (let ((buffer-width (window-width (get-buffer-window pp-dashboard-buffer-name))))
    (insert (concat "\n"
                    margin
                    (propertize (s-repeat pp-dashboard-width "-") 'face 'font-lock-comment-face)
                    "\n\n"))))

(defun pp/dashboard-insert-title (title margin)
  "Insert a title line in dashboard buffer."
  (let* ((title-width (string-width title))
         (separator-width (/ (- pp-dashboard-width title-width 2) 2)))
    (insert (concat "\n"
                    margin
                    (propertize (s-repeat separator-width "-") 'face 'font-lock-comment-face)
                    " "
                    (propertize title 'face 'font-lock-keyword-face)
                    " "
                    (propertize (s-repeat separator-width "-") 'face 'font-lock-comment-face)
                    "\n\n"))))

;;
;; Recentf
;;
(defun pp/dashboard-insert-recentf-list (list margin)
  "Render LIST-DISPLAY-NAME title and items of LIST."
  (when (car list)
    (mapc (lambda (el)
            (insert (concat margin "       "))
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el))
            (insert "\n"))
          list)))

(defun pp/dashboard-insert-recents (list-size margin)
  "Add the list of LIST-SIZE items from recently edited files."
  (recentf-mode)
  (pp/dashboard-insert-recentf-list
	 (pp/dashboard-subseq recentf-list 0 list-size)
     margin))

;;
;; Projectile
;;
(defun pp/dashboard-insert-project-list (list margin)
  "Render LIST-DISPLAY-NAME title and project items of LIST."
  (when (car list)
    (mapc (lambda (el)
            (insert (concat margin "      " ))
            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
				      (projectile-switch-project-by-name ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el))
            (insert "\n"))
          list)))

(defun pp/dashboard-insert-projects (list-size margin)
  "Add the list of LIST-SIZE items of projects."
  (projectile-mode)
  (if (bound-and-true-p projectile-mode)
      (progn
        (projectile-load-known-projects)
        (pp/dashboard-insert-project-list
         (pp/dashboard-subseq (projectile-relevant-known-projects) 0 list-size)
         margin))
    ))

;;
;; Org Agenda
;;
(defun pp/dashboard-insert-agenda-list (list margin)
  "Render LIST-DISPLAY-NAME title and agenda items from LIST."
  (if (car list)
      (mapc (lambda (el)
              (insert (concat margin "    "))
              (let ((filename (nth 4 el))
                    (lineno (nth 3 el))
                    (title (nth 0 el)))
                (widget-create 'push-button
                               :action `(lambda (&rest ignore)
                                          (let ((buffer (find-file-other-window ,filename)))
                                            (with-current-buffer buffer
                                              (goto-char ,lineno)
                                              )
                                            (switch-to-buffer buffer)))
                               :mouse-face 'highlight
                               :follow-link "\C-m"
                               :button-prefix ""
                               :button-suffix ""
                               :format "%[%t%]"
                               (format "%s" title))
                (insert "\n")))
            list)
    (insert (propertize "\n    --- No items ---" 'face 'widget-button))))

(defun pp/dashboard-timestamp-to-gregorian-date (timestamp)
  "Convert TIMESTAMP to a gregorian date.

The result can be used with functions like
`calendar-date-compare'."
  (let ((decoded-timestamp (decode-time timestamp)))
    (list (nth 4 decoded-timestamp)
          (nth 3 decoded-timestamp)
          (nth 5 decoded-timestamp))))

(defun pp/dashboard-date-due-p (timestamp &optional due-date)
  "Check if TIMESTAMP is today or in the past.

If DUE-DATE is nil, compare TIMESTAMP to today; otherwise,
compare to the date in DUE-DATE.

The time part of both TIMESTAMP and DUE-DATE is ignored, only the
date part is considered."
  (unless due-date
    (setq due-date (current-time)))
  (setq due-date (time-add due-date 86400))
  (let* ((gregorian-date (pp/dashboard-timestamp-to-gregorian-date timestamp))
         (gregorian-due-date (pp/dashboard-timestamp-to-gregorian-date due-date)))
    (calendar-date-compare (list gregorian-date)
                           (list gregorian-due-date))))

(defun pp/dashboard-get-agenda ()
  "Get agenda items for today."
  (org-compile-prefix-format 'agenda)
  (let* ((filtered-entries nil))
    (org-map-entries
     (lambda ()
       (let ((schedule-time (org-get-scheduled-time (point)))
             (deadline-time (org-get-deadline-time (point)))
             (item (org-agenda-format-item
                    ""
                    (org-get-heading t t)
                    (org-outline-level)
                    (org-get-category)
                    (org-get-tags)
                    t))
             (loc (point))
             (file (buffer-file-name)))
         (when (and (not (org-entry-is-done-p))
                    (or (and schedule-time (pp/dashboard-date-due-p schedule-time))
                        (and deadline-time (pp/dashboard-date-due-p deadline-time))))
           (setq filtered-entries
                 (append filtered-entries
                         (list (list item schedule-time deadline-time loc file)))))))
     nil
     'agenda)
    filtered-entries))

(defun pp/dashboard-insert-agenda (list-size margin)
  "Add the list of LIST-SIZE items of agenda."
  (pp/dashboard-insert-agenda-list (pp/dashboard-get-agenda) margin))

;; resize

(defun pp/dashboard-resize-on-hook ()
  (let ((space-win (get-buffer-window pp-dashboard-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (pp/dashboard-redraw)))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook 'pp/dashboard-resize-on-hook)
            (pp/dashboard-resize-on-hook)))

(pp/dashboard-setup-startup-hook)
(provide 'pp-dashboard)
