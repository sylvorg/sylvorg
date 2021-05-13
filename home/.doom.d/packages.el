;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(setq straight-disable-byte-compilation t)

;; From: https://github.com/clemera/helm-ido-like-guide
(package! helm-swoop)
(package! helm-flx)
(package! helm-fuzzier)
(package! helm-smex)
(package! smex)
(package! dash)
(package! s)

(package! help-macro+)
(package! help-fns+)
(package! help-mode+)

(package! use-package-chords)
(package! rainbow-delimiters)
(package! xah-fly-keys)
(package! multi-term)
(package! exwm)
(package! leaf)
(package! manage-minor-mode)
(package! kakoune)
(package! modalka)
(package! dockerfile-mode)
(package! ox-pandoc)

;; From: https://github.com/mohsenil85/evil-evilified-state and https://github.com/syl20bnr/spacemacs
(package! bind-map)

(package! hercules
    :recipe (:host gitlab :repo "jjzmajic/hercules.el" :branch "master"))
(package! vimrc-mode
    :recipe (:host github :repo "mcandre/vimrc-mode" :branch "master"))
(package! xonsh-mode
    :recipe (:host github :repo "seanfarley/xonsh-mode" :branch "master"))
(package! evil-evilified-state
    :recipe (:host github :repo "shadowrylander/evil-evilified-state" :branch "master"))
(package! helm-ido-like
    :recipe (:host github :repo "shadowrylander/helm-ido-like-guide" :branch "master"))
(package! ryo-modal
    :recipe (:host github :repo "kungsgeten/ryo-modal" :branch "master"))
(package! emux
    :recipe (:host github :repo "re5et/emux" :branch "master"))
(package! elscreen
    :recipe (:host github :repo "knu/elscreen" :branch "master"))
