;; modeline config

(defvar mode-line-height 30
  "How tall the mode-line should be. This is only respected in GUI emacs.")

;; Load powerline only when uncompiled, in order to generate the xpm bitmaps for
;; the mode-line. This is the tall blue bar on the left of the mode-line.
;; NOTE Compile this file for a faster startup!
(eval-when-compile (require 'powerline))
;; FIXME Don't hardcode colors in
(defvar mode-line-bar          (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil)))
(defvar mode-line-eldoc-bar    (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil)))
(defvar mode-line-inactive-bar (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil)))

;; Custom faces
(defface mode-line-is-modified nil
  "Face for mode-line modified symbol")

(defface mode-line-is-saved nil
  "Face for mode-line modified symbol")

(defface mode-line-2 nil
  "The alternate color for mode-line text.")

(defface mode-line-highlight nil
  "Face for bright segments of the mode-line.")

(defface mode-line-count-face nil
  "Face for anzu/evil-substitute/evil-search number-of-matches display.")

(defun pp/project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

;; Initialization

;; So the mode-line can keep track of "the current window"
(defvar mode-line-selected-window nil)
(defun pp/set-selected-window (&rest _)
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq mode-line-selected-window window))))
(add-hook 'window-configuration-change-hook #'pp/set-selected-window)
(add-hook 'focus-in-hook #'pp/set-selected-window)
(advice-add 'select-window :after 'pp/set-selected-window)
(advice-add 'select-frame  :after 'pp/set-selected-window)

;;
;; Mode-line segments
;;

(defun *buffer-path ()
  "Displays the buffer's full path relative to the project root (includes the
project root). Excludes the file basename. See `*buffer-name' for that."
  (when buffer-file-name
    (propertize
     (f-dirname
      (let ((buffer-path (file-relative-name buffer-file-name (pp/project-root)))
            (max-length (truncate (/ (window-body-width) 1.75))))
        (concat (projectile-project-name) "/"
                (if (> (length buffer-path) max-length)
                    (let ((path (reverse (split-string buffer-path "/" t)))
                          (output ""))
                      (when (and path (equal "" (car path)))
                        (setq path (cdr path)))
                      (while (and path (<= (length output) (- max-length 4)))
                        (setq output (concat (car path) "/" output))
                        (setq path (cdr path)))
                      (when path
                        (setq output (concat "../" output)))
                      (when (string-suffix-p "/" output)
                        (setq output (substring output 0 -1)))
                      output)
                  buffer-path))))
     'face (if active 'mode-line-2))))


(defun *buffer-name ()
  "The buffer's base name or id."
  (s-trim-left (format-mode-line "%b")))

(defun *buffer-pwd ()
  "Displays `default-directory', for special buffers like the scratch buffer."
  (propertize
   (concat "[" (abbreviate-file-name default-directory) "]")
   'face 'mode-line-2))

(defun *buffer-state ()
  "Displays symbols representing the buffer's state
(non-existent/modified/read-only)"
  (when buffer-file-name
     (concat (if (not (file-exists-p buffer-file-name))
                 (propertize (all-the-icons-faicon "ban" :height 1.3 :v-adjust 0.0) 'face 'mode-line-is-modified))
               (if (buffer-modified-p)
                   (propertize (all-the-icons-faicon "circle" :height 1.3 :v-adjust 0.0) 'face 'mode-line-is-modified)
                   (propertize (all-the-icons-faicon "check-circle" :height 1.3 :v-adjust 0.0) 'face 'mode-line-is-saved))
             (if buffer-read-only
                 (propertize (all-the-icons-faicon "lock" :height 1.3 :v-adjust 0.0) 'face 'mode-line-is-modified)))))

(defun *buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
      ""
    (symbol-name buffer-file-coding-system)))

(defun *ruby-version ()
  "Currently active ruby version"
  (when (string-equal mode-name "Ruby")
    (concat " [" (rbenv--active-ruby-version) "]")))

(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (format-mode-line mode-name)
          (if (stringp mode-line-process) mode-line-process)
          (and (featurep 'face-remap)
               (/= text-scale-mode-amount 0)
               (format " (%+d)" text-scale-mode-amount))))

(defun *major-mode-icon ()
    (propertize (format "%s" (all-the-icons-icon-for-buffer)
                'help-echo (format "Major-mode: `%s`" major-mode)
                'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

(defun *org-timer ()
  "Displays org timers"
  (if (and (boundp 'org-mode-line-string) (stringp org-mode-line-string))
      (propertize
       (format " %s " (s-match "\\[.*\\]" org-mode-line-string))
       'face 'mode-line-2)))

(add-hook 'org-clock-out-hook
          '(lambda ()
             (setq org-mode-line-string nil)
             (force-mode-line-update)))

(defun *git-vc ()
  (when vc-mode
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                   'display '(raise -0.1))
       (propertize (format " %s" branch) 'face `(:height 0.9))
       " · "))))

(defun *selection-info ()
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and active (evil-visual-state-p))
    (propertize
     (let ((reg-beg (region-beginning))
           (reg-end (region-end))
           (evil (eq 'visual evil-state)))
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
             (chars (- (1+ reg-end) reg-beg))
             (cols (1+ (abs (- (evil-column reg-end)
                               (evil-column reg-beg))))))
         (cond
          ;; rectangle selection
          ((or (bound-and-true-p rectangle-mark-mode)
               (and evil (eq 'block evil-visual-selection)))
           (format " %dx%dB " lines (if evil cols (1- cols))))
          ;; line selection
          ((or (> lines 1) (eq 'line evil-visual-selection))
           (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
               (format " %dL " lines)
             (format " %dC %dL " chars lines)))
          (t (format " %dC " (if evil chars (1- chars)))))))
     'face 'mode-line-highlight)))

(defun *macro-recording ()
  "Display current macro being recorded."
  (when (and active defining-kbd-macro)
    (propertize
     (format " %s ▶ " (char-to-string evil-this-macro))
     'face 'mode-line-highlight)))

(defun *evil-substitute ()
  "Show number of :s matches in real time."
  (when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches "
                   (count-matches pattern (car range) (cdr range))
                   evil-ex-argument)
         " ... "))
     'face (if active 'mode-line-count-face))))

(defun *buffer-position ()
  "A more vim-like buffer position."
  (let ((start (window-start))
        (end (window-end))
        (pend (point-max)))
    (format "%d%%%%" (/ end 0.01 pend))))

(defun *time ()
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
    (concat
     (propertize (format-time-string "%H:%M ") 'face `(:height 0.9))
     (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))

(defun *dot-separator ()
    (propertize " · " 'face `(:height 0.9)))

(defun pp/mode-line (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) mode-line-selected-window))
           (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
                      (*macro-recording)
                      (*selection-info)
                      " "
                      (*buffer-path)
                      (*buffer-name)
                      " "
                      (*buffer-state)
                      ,(if (eq id 'scratch) '(*buffer-pwd))))
           (rhs (list
                      ;; (*org-timer)
                      (*git-vc)
                      (*major-mode-icon)
                      (*dot-separator)
                      (propertize
                       "(%l,%c)"
                       'face (if active 'mode-line-2))
                      (*dot-separator)
                      (*time)))
           (middle (propertize
                    " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                       ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs middle rhs))))

(setq-default mode-line-format (pp/mode-line))

(provide 'pp-modeline)
