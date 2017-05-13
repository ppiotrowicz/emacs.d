;; Keybindings config

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "'"   '(pp/terminal-focus                   :which-key "iTerm")
 ":"   '(execute-extended-command            :which-key "M-x")
 "!"   '(eshell                              :which-key "eshell")
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
 "d"   '(pp/show-dashboard                   :which-key "dashboard")
 ;; Help
 "h"   '(:ignore t                           :which-key "help")
 "hc"  '(pp/edit-emacs-config                :which-key "edit config")
 "hf"  '(describe-function                   :which-key "describe function")
 "hk"  '(describe-key                        :which-key "describe key")
 "hm"  '(describe-mode                       :which-key "describe mode")
 "hp"  '(paradox-list-packages               :which-key "paradox")
 "hv"  '(describe-variable                   :which-key "describe variable")
 ;; Errors
 "e"   '(:ignore t                           :which-key "errors")
 "ee"  '(flycheck-list-errors                :which-key "list")
 "en"  '(flycheck-next-error                 :which-key "next error")
 "ep"  '(flycheck-previous-error             :which-key "next error")
 "eb"  '(flycheck-buffer                     :which-key "check buffer")
 ;; Files
 "f"   '(:ignore t                           :which-key "files")
 "fd"  '(pp/delete-file-and-buffer           :which-key "delete file")
 "ff"  '(counsel-find-file                   :which-key "find file")
 "fr"  '(pp/rename-file-and-buffer           :which-key "rename file")
 ;; Git
 "g"   '(:ignore t                           :which-key "magit")
 "gs"  '(magit-status                        :which-key "status")
 "gb"  '(magit-blame                         :which-key "blame")
 "go"  '(github-browse-file                  :which-key "github browse")
 "m"   '(:ignore t                           :which-key "modes")
 "mf"  '(auto-fill-mode                      :which-key "fill")
 "mj"  '(json-mode                           :which-key "json")
 "mt"  '(toggle-truncate-lines               :which-key "truncate")
 "mw"  '(whitespace-mode                     :which-key "whitespace")
 ;; Open
 "o"   '(:ignore t                           :which-key "open")
 "oc"  '(org-capture                         :which-key "org capture")
 "ob"  '((lambda () (interactive) (find-file "~/org/bookmarks.org"))  :which-key "bookmarks")
 "oe"  '((lambda () (interactive) (find-file "~/org/emacs.org"))      :which-key "emacs tasks")
 "oh"  '((lambda () (interactive) (find-file "~/org/home.org"))       :which-key "home tasks")
 "ot"  '((lambda () (interactive) (find-file "~/org/today.org"))      :which-key "today tasks")
 "ol"  '((lambda () (interactive) (find-file "~/org/til.org"))        :which-key "today I learned")
 "ow"  '((lambda () (interactive) (find-file "~/org/work.org"))       :which-key "work tasks")
 "of"  '((lambda () (interactive) (find-file "~/finance/ledger.dat")) :which-key "ledger")
 ;;; Capture
 "c"   '(:ignore t                           :which-key "capture")
 "cc"  '(org-capture                         :which-key "org capture")
 ;; Project
 "p"   '(:ignore t                           :which-key "project")
 "pp"  '(counsel-projectile-switch-project   :which-key "switch project")
 "pf"  '(counsel-projectile-find-file        :which-key "find file")
 "pb"  '(counsel-projectile-switch-to-buffer :which-key "find buffer")
 "pk"  '(projectile-kill-buffers             :which-key "kill buffers")
 "p/"  '(pp/find-in-project                  :which-key "find string")
 "/"   '(pp/find-in-project                  :which-key "find string")
 ;; Windows
 "w"   '(:ignore t                           :which-key "windows")
 "ws"  '(split-window-vertically             :which-key "split -")
 "wS"  '(pp/split-window-below-and-focus     :which-key "split - and focus")
 "wv"  '(split-window-horizontally           :which-key "split |")
 "wV"  '(pp/split-window-right-and-focus     :which-key "split | and focus")
 "wc"  '(delete-window                       :which-key "delete window")
 "w="  '(balance-windows                     :which-key "balance windows")
 "ww"  '(ace-window                          :which-key "ace window")
 "wf"  '(pp/toggle-fullscreen                :which-key "fullscreen")
)

;;(define-key ruby-mode-map (kbd "C-c :") 'ruby_toggle_symbol)

(general-nmap "*"   'pp/highlight-symbol-hydra)
(general-nmap "C-y" 'counsel-yank-pop)
(general-nmap "gc"  'evilnc-comment-or-uncomment-lines)
(general-nmap "%"   'evilmi-jump-items)
;; window movement
(general-nmap "C-h" 'evil-window-left)
(general-nmap "C-j" 'evil-window-down)
(general-nmap "C-k" 'evil-window-up)
(general-nmap "C-l" 'evil-window-right)
;; folding
(general-nmap "zm"  'yafolding-toggle-all)
(general-nmap "zc"  'yafolding-hide-parent-element)
(general-nmap "za"  'yafolding-toggle-element)

(general-define-key "M-w" 'quit-window)

;; Help mode
(evil-set-initial-state 'help-mode 'normal)
(general-evil-define-key 'normal help-mode-map
   "q" 'quit-window)

;; compilation mode
(define-key compilation-mode-map "\C-h" 'evil-window-left)
(define-key compilation-mode-map "\C-j" 'evil-window-down)
(define-key compilation-mode-map "\C-k" 'evil-window-up)
(define-key compilation-mode-map "\C-l" 'evil-window-right)

(provide 'pp-keybindings)
