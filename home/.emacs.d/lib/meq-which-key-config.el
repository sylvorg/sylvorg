(meq/up which-key :deino (deino/which-key (:color blue :columns 4) "w"
        ("`" nil "cancel")
        ("a" cosmoem-any-popup-showing-p "any popup showing")
        ("h" meq/which-key--hide-popup "hide-popup")
        ("s" meq/which-key--show-popup "show-popup")
        ("r" meq/which-key--refresh-popup "refresh-popup")
        ("t" meq/toggle-which-key "toggle")
        ("l" meq/which-key-show-top-level "meq/toplevel")
        ("L" which-key-show-top-level "toplevel"))
    :gsetq
        (which-key-enable-extended-define-key t)
        (which-key-idle-delay 0.1)
        (which-key-idle-secondary-delay nil)
        (which-key-allow-evil-operators t)

        ;; NOTE: This will cause the which-key maps for the operator states to show up,
        ;; breaking functionality such as `d 13 <arrow-down>', etc.
        ;; (which-key-show-operator-state-maps t)

        ;; TODO: Choose a fun one!
        (which-key-separator " × ")
        ;; (which-key-separator " |-> ")

        (which-key-popup-type 'side-window)
        (which-key-side-window-location '(right bottom left top))

        ;; If this percentage is too small, the keybindings frame will appear at the bottom
        (which-key-side-window-max-width 0.5)

        (which-key-side-window-max-height 0.25))
