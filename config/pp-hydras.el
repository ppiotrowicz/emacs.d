;; random hydras

(use-package hydra)

(defhydra hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

(defhydra hydra-org (:color red :columns 3)
  "Org Mode Movements"
  ("n" outline-next-visible-heading "next heading")
  ("p" outline-previous-visible-heading "prev heading")
  ("N" org-forward-heading-same-level "next heading at same level")
  ("P" org-backward-heading-same-level "prev heading at same level")
  ("u" outline-up-heading "up heading")
  ("g" org-goto "goto" :exit t))

(defhydra hydra-search (:post highlight-symbol-remove-all)
  "Search"
  ("n" highlight-symbol-next "next")
  ("p" highlight-symbol-prev "prev")
  ("e" evil-multiedit-match-all "edit")
  ("/" pp/find-symbol-at-point "in project")
  ("s" pp/swiper-at-point "swiper")
  ("q" highlight-symbol-remove-all "quit" :exit t))


(defhydra hydra-window (:color red
                        :hint nil)
  "
   _k_
 _h_   _l_
   _j_
 _=_:balance
  "
  ("h" shrink-window-horizontally)
  ("l" enlarge-window-horizontally)
  ("k" enlarge-window)
  ("j" shrink-window)
  ("=" balance-windows))

(provide 'pp-hydras)
