(meq/upnsd sorrow
    :primer+ ("t" "toggles")
    :demon ((alloy-chord "kk") 'meq/sorrow-execute-with-current-bindings)
    :config ;; From: https://github.com/shadowrylander/sorrow#which-key-integration
        (push '((nil . "sorrow:.*:") . (nil . "")) which-key-replacement-alist))
