(setq elfeed-db-directory "~/Dropbox/Elfeed/elfeeddb")

(use-package elfeed
  :config
  (setf url-queue-timeout 100))

(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-position 'bottom
        elfeed-goodies/powerline-default-separator nil))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/Elfeed/elfeed.org")))

(general-define-key :keymaps 'elfeed-search-mode-map
                    :states '(normal)
                    "RET" 'elfeed-search-show-entry
                    "+"   'elfeed-search-tag-all
                    "-"   'elfeed-search-untag-all
                    "G"   'elfeed-search-fetch
                    "S"   'elfeed-search-set-filter
                    "b"   'elfeed-search-browse-url
                    "g"   'elfeed-search-update--force
                    "q"   'quit-window
                    "r"   'elfeed-search-untag-all-unread
                    "s"   'elfeed-search-live-filter
                    "u"   'elfeed-search-tag-all-unread
                    "y"   'elfeed-search-yank
                    "?"   'elfeed-goodies/toggle-logs)

(general-define-key :keymaps 'elfeed-show-mode-map
                    :states '(normal)
                    "+" 'elfeed-show-tag
                    "-" 'elfeed-show-untag
                    "b" 'elfeed-show-visit
                    "g" 'elfeed-show-refresh
                    "n" 'elfeed-goodies/split-show-next
                    "p" 'elfeed-goodies/split-show-prev
                    "q" 'elfeed-kill-buffer
                    "s" 'elfeed-show-new-live-search
                    "y" 'elfeed-show-yank)

(provide 'pp-elfeed)
