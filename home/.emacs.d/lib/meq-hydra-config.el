(meq/up hydra
    :custom (hydra-hint-display-type 'lv)
    :bind (:map hydra-base-map ("~" . hydra--universal-argument))
    :upnsd-preconfig (janus)
    :use-package-preconfig (use-package-hydra)
    :upnsd-postconfig (use-package-deino) (deino :custom (deino-hint-display-type 'lv)))
