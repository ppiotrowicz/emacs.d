(use-package org
  :ensure t
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
    (setq org-hide-leading-stars t)
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


    ;; Fontify checkboxes and dividers
    (defface org-list-bullet '((t ())) "Face for list bullets")
    (font-lock-add-keywords
     'org-mode '(("^ *\\([-+]\\|[0-9]+[).]\\) "
                  (1 'org-list-bullet))
                 ("^ *\\(-----+\\)$"
                  (1 'org-meta-line))))
    (setq org-capture-templates
          (quote
           (("w" "Work")
            ("wt" "Todo" entry
             (file+headline "~/org/work.org" "INBOX")
             "* TODO %?")
            ("wf" "Todo with link" entry
             (file+headline "~/org/work.org" "INBOX")
             "* TODO %?\n  %i\n  %a")
            ("h" "Home")
            ("ht" "Todo" entry
             (file+headline "~/org/home.org" "INBOX")
             "* TODO %?")
            ("l" "TIL" entry
             (file+datetree "~/org/til.org")
             "* %? %^g")
            ("b" "A link, for reading later." entry
             (file+headline "~/org/bookmarks.org" "INBOX")
             "* %:description\n%u\n%c\n\n%i")
            )))

    ;; fix level 1 heading colors
    (set-face-attribute 'org-level-1 nil
                        :background "#262c34"
                        :foreground "#00B3EF"
                        :box nil
                        :height 1.2)

  ;; Keybindings
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (defvar org-agenda-mode-map)
  (evil-define-key 'normal org-agenda-mode-map
    "l" 'org-agenda-later
    "h" 'org-agenda-earlier
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "RET") 'org-agenda-switch-to
    [escape] 'org-agenda-quit
    "q" 'org-agenda-quit
    "s" 'org-agenda-save-all-org-buffers
    "t" 'org-agenda-todo
    (kbd "SPC") 'org-agenda-show-and-scroll-up
    )

  ;; other
  (require 'org-protocol)
  ))

(provide 'pp-org-mode)
