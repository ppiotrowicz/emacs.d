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
    (setq org-M-RET-may-split-line '((default . nil)))
    (setq org-fontify-done-headline t)
    (setq org-fontify-quote-and-verse-blocks t)
    (setq org-fontify-whole-heading-line t)
    (setq org-footnote-auto-label 'plain)
    (setq org-hide-emphasis-markers t)
    (setq org-image-actual-width nil)
    (setq org-pretty-entities t)
    (setq org-pretty-entities-include-sub-superscripts t)
    (setq org-startup-folded 'content)
    (setq org-startup-indented t)
    (setq org-startup-with-inline-images nil)
    (setq org-use-sub-superscripts '{})
    (setq org-src-fontify-natively t)
    (setq org-hide-leading-stars t)
    (setq org-directory "~/org")
    (setq org-todo-keywords
          '((sequence "TODO" "|" "DONE" "CANCELLED")))

    (setq org-log-into-drawer nil)
    (setq org-clock-into-drawer "CLOCKING")

    (setq org-agenda-files (list "~/org/current.org"))
    ;;                              "~/org/reminders.org"
    ;;                              "~/org/someday.org"))
    (setq org-refile-targets '(("~/org/current.org" :maxlevel . 2)
                               ("~/org/today.org" :level . 1)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    (setq org-tags-column -90)
    (setq org-export-html-postamble nil)


    (setq org-enforce-todo-dependencies t)
    (setq org-log-done (quote time))
    (setq org-log-redeadline (quote time))
    (setq org-log-reschedule (quote time))

    (setq org-capture-templates
          (quote
           (("f" "file reference" entry (entry "~/org/current.org" "INBOX")
             "* TODO %?\n%i\n%a")
            ("t" "todo TODO" entry
             (file+headline "~/org/current.org" "INBOX")
             "* TODO %?")
            )))

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
              ))))

    ;; other
    (require 'org-protocol)

    (use-package ob-restclient)
    (use-package ox-gfm)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t)
       (shell . t)
       (ruby . t))
    )))

(provide 'pp-org-mode)
