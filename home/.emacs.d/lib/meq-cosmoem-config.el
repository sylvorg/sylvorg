(meq/upnsd cosmoem
    :upnsd-postconfig (meta)
    :config (prime ", m" map-of-infinity/body "map-of-infinity")
    :which-key-change-ryo ("," "damascus")
    :deino (map-of-infinity nil ", m"
            ("`" nil "cancel")
            ("w" deino/which-key/body "which-key")
            ("h" deino/cosmoem/body "cosmoem")
            ("d" meq/disable-all-modal-modes "disable all modal modes" :color blue)
            ("t" toggles/body "toggles")
            ("k" all-keymaps/body "all keymaps"))
        (deino/cosmoem (:color blue) ", c"
            ("`" nil "cancel")
            ("h" cosmoem-hide-all-modal-modes "hide all modal modes"))
        (toggles (:color blue) ", t" ("`" nil "cancel"))
        (all-keymaps (:color blue) ", k" ("`" nil "cancel")))
