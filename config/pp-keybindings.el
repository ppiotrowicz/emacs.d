;; Keybindings config

(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "'"   '(pp/terminal-focus                   :which-key "iTerm")
 ":"   '(counsel-M-x                         :which-key "M-x")
 "!"   '(pp/open-vterm                       :which-key "vterm")
 "SPC" '(avy-goto-char                       :which-key "avy char")
 "a"   '(org-agenda                          :which-key "agenda")
 "t"   '(neotree-projectile-find             :which-key "neotree find")
 "T"   '(neotree-projectile                  :which-key "neotree")
 ;; Buffers
 "b"   '(:ignore t                           :which-key "buffers")
 "bb"  '(ivy-switch-buffer                   :which-key "switch buffer")
 "bd"  '(kill-this-buffer                    :which-key "kill buffer")
 "bn"  '(evil-buffer-new                     :which-key "new buffer")
 "TAB" '(pp/switch-to-previous-buffer        :which-key "previous buffer")
 ;; Help
 "h"   '(:ignore t                           :which-key "help")
 "hc"  '(pp/edit-emacs-config                :which-key "edit config")
 "hd"  '(pp/edit-dotfiles                    :which-key "edit dotfiles")
 "hf"  '(describe-function                   :which-key "describe function")
 "hk"  '(describe-key                        :which-key "describe key")
 "hm"  '(describe-mode                       :which-key "describe mode")
 "hp"  '(paradox-list-packages               :which-key "paradox")
 "hv"  '(describe-variable                   :which-key "describe variable")
 ;; Errors
 "e"   '(:ignore t                           :which-key "errors")
 "ed"  '(flycheck-disable-checker            :which-key "disable checker")
 "ee"  '(flycheck-list-errors                :which-key "list")
 "em"  '(flycheck-mode                       :which-key "mode")
 "en"  '(flycheck-next-error                 :which-key "next error")
 "ep"  '(flycheck-previous-error             :which-key "next error")
 "eb"  '(flycheck-buffer                     :which-key "check buffer")
 ;; Files
 "f"   '(:ignore t                           :which-key "files")
 "fd"  '(pp/delete-file-and-buffer           :which-key "delete file")
 "ff"  '(counsel-find-file                   :which-key "find file")
 "fr"  '(pp/rename-file-and-buffer           :which-key "rename file")
 "fy"  '(pp/copy-file-name-to-clipboard      :which-key "copy filename")
 "fw"  '(delete-trailing-whitespace          :which-key "clear trailing whitespace")
 ;; Git
 "g"   '(:ignore t                           :which-key "magit")
 "gs"  '(magit-status                        :which-key "status")
 "gb"  '(magit-blame                         :which-key "blame")
 "go"  '(github-browse-file                  :which-key "github browse")
 "m"   '(:ignore t                           :which-key "modes")
 "ma"  '(pp/display-ansi-colors              :which-key "ansi colors")
 "mf"  '(auto-fill-mode                      :which-key "fill")
 "mj"  '(json-mode                           :which-key "json")
 "mt"  '(toggle-truncate-lines               :which-key "truncate")
 "mw"  '(whitespace-mode                     :which-key "whitespace")
 ;; Open
 "o"   '(:ignore t                           :which-key "open")
 "ob"  '((lambda () (interactive) (find-file "~/org/bookmarks.org"))  :which-key "bookmarks")
 "oe"  '((lambda () (interactive) (find-file "~/org/emacs.org"))      :which-key "emacs tasks")
 "oi"  '((lambda () (interactive) (find-file "~/org/inbox.org"))      :which-key "inbox")
 "oc"  '((lambda () (interactive) (find-file "~/org/current.org"))    :which-key "current tasks")
 "os"  '((lambda () (interactive) (find-file "~/org/someday.org"))    :which-key "someday/maybe")
 "or"  '((lambda () (interactive) (find-file "~/org/reminders.org"))  :which-key "reminders")
 "ot"  '((lambda () (interactive) (find-file "~/org/today.org"))      :which-key "today tasks")
 "ol"  '((lambda () (interactive) (find-file "~/org/til.org"))        :which-key "today I learned")
 "of"  '((lambda () (interactive) (find-file "~/finance/2018.beancount")) :which-key "ledger")
 ;;; Capture
 "c"   '(:ignore t                           :which-key "capture")
 "cc"  '(org-capture                         :which-key "org capture")
 "cl"  '(org-store-link                      :which-key "store link")
 "ci"  '(org-insert-link                     :which-key "insert link")
 ;; Project
 "p"   '(:ignore t                           :which-key "project")
 "pp"  '(counsel-projectile-switch-project   :which-key "switch project")
 "pf"  '(counsel-projectile-find-file        :which-key "find file")
 "pb"  '(counsel-projectile-switch-to-buffer :which-key "find buffer")
 "pk"  '(projectile-kill-buffers             :which-key "kill buffers")
 "ps"  '(projectile-run-eshell               :which-key "run shell")
 "p/"  '(pp/find-in-project                  :which-key "find string")
 "/"   '(pp/find-in-project                  :which-key "find string")
 ;; Spotify
 "s"   '(:ignore t                           :which-key "spotify")
 "sn"  '(counsel-spotify-next                :which-key "next")
 "sp"  '(counsel-spotify-previous            :which-key "previous")
 "ss"  '(counsel-spotify-search-track        :which-key "search")
 "s SPC"  '(counsel-spotify-toggle-play-pause :which-key "play/pause")
 ;; Windows
 "w"   '(:ignore t                           :which-key "windows")
 "ws"  '(split-window-vertically             :which-key "split -")
 "wS"  '(pp/split-window-below-and-focus     :which-key "split - and focus")
 "wv"  '(split-window-horizontally           :which-key "split |")
 "wV"  '(pp/split-window-right-and-focus     :which-key "split | and focus")
 "wc"  '(delete-window                       :which-key "delete window")
 "w="  '(balance-windows                     :which-key "balance windows")
 "wr"  '(hydra-window/body                   :which-key "resize")
 "ww"  '(ace-window                          :which-key "ace window")
 "wf"  '(pp/toggle-fullscreen                :which-key "fullscreen")
 "w <left>" '(winner-undo                    :which-key "winner undo")
 "w <right>" '(winner-redo                   :which-key "winner redo")
 "wm" '(ivy-push-view                        :which-key "push view")
 "wM" '(ivy-pop-view                         :which-key "push view")
)

(global-set-key (kbd "M-x") 'counsel-M-x)

(general-nmap "*"   'pp/highlight-symbol-hydra)
(general-nmap "C-y" 'counsel-yank-pop)
(general-imap "C-y" 'counsel-yank-pop)
(general-nmap "gc"  'evil-commentary-line)
(general-nmap "%"   'evilmi-jump-items)
;; window movement
(general-nmap "C-h" 'evil-window-left)
(general-nmap "C-j" 'evil-window-down)
(general-nmap "C-k" 'evil-window-up)
(general-nmap "C-l" 'evil-window-right)
(general-imap "C-h" 'evil-window-left)
(general-imap "C-j" 'evil-window-down)
(general-imap "C-k" 'evil-window-up)
(general-imap "C-l" 'evil-window-right)
;; folding
(general-nmap "zm"  'yafolding-toggle-all)
(general-nmap "zc"  'yafolding-hide-parent-element)
(general-nmap "za"  'yafolding-toggle-element)
;; narrowring
(general-nmap "zz"  'eos/narrow-or-widen-dwim)

(general-define-key "M-w" 'quit-window)

(general-nmap "C-t" 'counsel-imenu)

;; Help mode
(evil-set-initial-state 'help-mode 'normal)
(general-evil-define-key 'normal help-mode-map
   "q" 'quit-window)

;; compilation mode
(define-key compilation-mode-map "\C-h" 'evil-window-left)
(define-key compilation-mode-map "\C-j" 'evil-window-down)
(define-key compilation-mode-map "\C-k" 'evil-window-up)
(define-key compilation-mode-map "\C-l" 'evil-window-right)

;; compilation mode
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map "\C-h" 'evil-window-left)
            (define-key eshell-mode-map "\C-j" 'evil-window-down)
            (define-key eshell-mode-map "\C-k" 'evil-window-up)
            (define-key eshell-mode-map "\C-l" 'evil-window-right)))

;; epa key list mode
(general-evil-define-key 'normal epa-key-list-mode-map "m" 'epa-mark-key)
(general-evil-define-key 'normal epa-key-list-mode-map "q" 'epa-exit-buffer)

;; rspec-compilation-mode
(general-evil-define-key 'normal rspec-compilation-mode-map "?" 'pp/toggle-window-height)

;; clear C-d from evil
(define-key evil-insert-state-map (kbd "C-d") nil)

(provide 'pp-keybindings)
