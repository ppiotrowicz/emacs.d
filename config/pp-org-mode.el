(use-package org
  :config
  (progn
    (setq org-export-coding-system 'utf-8)
    (setq org-indent-mode-turns-on-hiding-stars t)
    (setq org-adapt-indentation nil)
    (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . auto)))
    (setq org-cycle-separator-lines 1)
    (setq org-cycle-include-plain-lists t)
    (setq org-entities-user '(("flat" "\\flat" nil "" "" "266D" "♭")
                              ("sharp" "\\sharp" nil "" "" "266F" "♯")))
    (setq org-fontify-done-headline t)
    (setq org-fontify-quote-and-verse-blocks t)
    (setq org-fontify-whole-heading-line t)
    (setq org-footnote-auto-label 'plain)
    (setq org-hide-emphasis-markers t)
    (setq org-image-actual-width nil)
    (setq org-pretty-entities t)
    (setq org-pretty-entities-include-sub-superscripts t)
    (setq org-startup-folded t)
    (setq org-startup-indented t)
    (setq org-startup-with-inline-images nil)
    (setq org-use-sub-superscripts '{})
    (setq org-src-fontify-natively t)
    (setq org-startup-indented t)
    (setq org-hide-leading-stars t)
    (setq org-directory "~/org")
    (setq org-link-abbrev-alist
          '(("SD"   . "https://getbase.atlassian.net/browse/SD-")
            ("jira" . "https://getbase.atlassian.net/browse/")
            ("conf" . "https://getbase.atlassian.net/wiki/display/%h")))
    (setq org-agenda-files (list "~/org/home.org" "~/org/work.org"))
    (setq org-log-into-drawer "LOGBOOK")
    (setq org-clock-into-drawer "CLOCKING")
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))
    (setq org-refile-use-outline-path nil)
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    (setq org-tags-column -90)
    (setq org-export-html-postamble nil)


    (setq org-enforce-todo-dependencies t)
    (setq org-log-done (quote time))
    (setq org-log-redeadline (quote time))
    (setq org-log-reschedule (quote time))

    ;; mobile
    (setq org-mobile-inbox-for-pull "~/org/mobile-notes.org")
    (setq org-mobile-directory "~/Dropbox/Aplikacje/MobileOrg")

  ;;; Custom fontification
    (defun +org--tag-face (n)
      (let ((kwd (match-string n)))
        (or (and (equal kwd "#") 'org-tag)
            (and (equal kwd "@") 'org-special-keyword))))

    (defun +org|adjust-faces ()
      "Correct (and improve) org-mode's font-lock keywords.

  1. Re-set `org-todo' & `org-headline-done' faces, to make them respect
     underlying faces.
  2. Fontify item bullets
  3. Fontify item checkboxes (and when they're marked done)"
      (let ((org-todo (format org-heading-keyword-regexp-format
                              org-todo-regexp))
            (org-done (format org-heading-keyword-regexp-format
                              (concat "\\(?:" (mapconcat #'regexp-quote org-done-keywords "\\|") "\\)"))))
        (setq
         org-font-lock-extra-keywords
         (append (org-delete-all
                  `(("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                     (0 (org-get-checkbox-statistics-face) t))
                    (,org-todo (2 (org-get-todo-face 2) t))
                    (,org-done (2 'org-headline-done t)))
                  org-font-lock-extra-keywords)
                 `((,org-todo (2 (org-get-todo-face 2) prepend))
                   (,org-done (2 'org-headline-done prepend))
                   ;; Make checkbox statistic cookies respect underlying faces
                   ("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                    (0 (org-get-checkbox-statistics-face) prepend))
                   ;; I like how org-mode fontifies checked TODOs and want this to extend to
                   ;; checked checkbox items:
                   ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                    1 'org-headline-done prepend)
                   ;; make plain list bullets stand out
                   ("^ *\\([-+]\\|[0-9]+[).]\\) " 1 'org-list-dt append)
                   ;; and separators/dividers
                   ("^ *\\(-----+\\)$" 1 'org-meta-line)
                   ;; custom #hashtags & @at-tags for another level of organization
                   ;; TODO refactor this into a single rule
                   ("\\s-\\(\\([#@]\\)[^ \n]+\\)" 1 (+org--tag-face 2)))))))
    (add-hook 'org-font-lock-set-keywords-hook #'+org|adjust-faces)

    (setq org-capture-templates
          (quote
           (("w" "Work TODO" entry
             (file+headline "~/org/work.org" "INBOX")
             "* TODO %?")
            ("h" "Home TODO" entry
             (file+headline "~/org/home.org" "INBOX")
             "* TODO %?")
            ("l" "TIL" entry
             (file+datetree "~/org/til.org")
             "* %? %^g")
            ("t" "Today" entry
             (file+datetree "~/org/today.org")
             "* TODO %?")
            ("b" "A link, for reading later." entry
             (file+headline "~/org/bookmarks.org" "INBOX")
             "* %:description\n%u\n%c\n\n%i")
            )))

    ;; fix level 1 heading colors
    (set-face-attribute 'org-level-1 nil
                        :background "#262c34"
                        :foreground "#00B3EF"
                        :box nil
                        :height 1.0)
    (set-face-attribute 'org-level-2 nil :height 1.0)
    (set-face-attribute 'org-level-3 nil :height 1.0)
    (set-face-attribute 'org-level-4 nil :height 1.0)
    (set-face-attribute 'org-level-5 nil :height 1.0)

    ;; Keybindings
    (evil-set-initial-state 'org-agenda-mode 'normal)
    (defvar org-agenda-mode-map)
    (evil-define-key 'normal org-agenda-mode-map
      "l" 'org-agenda-later
      "h" 'org-agenda-earlier
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      "\C-n" 'org-agenda-next-line
      "\C-p" 'org-agenda-previous-line
      (kbd "RET") 'org-agenda-switch-to
      [escape] 'org-agenda-quit
      "q" 'org-agenda-quit
      "s" 'org-save-all-org-buffers
      "t" 'org-agenda-todo
      (kbd "SPC") 'org-agenda-show-and-scroll-up
      "+" 'org-agenda-priority-up
      "-" 'org-agenda-priority-down
      "e" 'org-agenda-set-effort
      "r" 'org-agenda-redo
      "<" 'org-agenda-do-date-earlier
      ">" 'org-agenda-do-date-later
      )


    (defun pp/org-skip-subtree-if-priority (priority)
      "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (pri-value (* 1000 (- org-lowest-priority priority)))
            (pri-current (org-get-priority (thing-at-point 'line t))))
        (if (= pri-value pri-current)
            subtree-end
          nil)))

    ;; Agenda views
    (setq org-agenda-custom-commands
          '(("c" "Custom agenda view"
             ((tags "PRIORITY=\"A\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "High-priority unfinished tasks:")))
              (agenda "" ((org-agenda-span 1)))
              (alltodo ""
                       ((org-agenda-skip-function
                         '(or (pp/org-skip-subtree-if-priority ?A)
                              (org-agenda-skip-if nil '(scheduled deadline))))))))))


    ;; other
    (require 'org-protocol)

    (use-package ob-restclient)
    (use-package ox-gfm)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t)))
    ))

(provide 'pp-org-mode)
