;; Keywords Next Door

;; This loop sets up all the necessary [[https://github.com/jwiegley/use-package][use-package]] keywords for the
;; [[https://github.com/ch11ng/exwm][Emacs X Window Manager]] by
;; [[https://github.com/ch11ng][Chris Feng]]; it includes the settings for the following:

;; - [[https://github.com/abo-abo/hydra][hydra]] by [[https://github.com/abo-abo][Oleh Krehel]]
;; - [[https://github.com/shadowrylander/alloy][alloy]], forked from
;; [[https://github.com/noctuid/general.el][general.el]] by [[https://github.com/noctuid][Fox Kiester]]
;; - [[https://github.com/justbur/emacs-which-key][emacs-which-key]] by [[https://github.com/justbur][Justin Burkett]]
;; - [[https://gitlab.com/shadowrylander/cosmoem][cosmoem]], forked from
;; [[https://gitlab.com/jjzmajic/hercules.el][hercules.el]] by [[https://gitlab.com/jjzmajic][jjzmajic]]
;; - [[https://github.com/shadowrylander/sorrow][sorrow]], forked from
;; [[https://github.com/Kungsgeten/ryo-modal][ryo-modal]] by [[https://github.com/Kungsgeten][Erik Sjöstrand]]


;; [[file:FEEDME.org::*Keywords Next Door][Keywords Next Door:1]]
(mapc #'(lambda (config) (interactive)
            (load (meq/ued-lib (concat "meq-" (symbol-name config) "-config"))))
    '(hydra alloy which-key cosmoem sorrow))
;; Keywords Next Door:1 ends here

;; It's fun to stay at the E.X.W.M.

;; This will set up ~EXWM~ for all profiles:


;; [[file:FEEDME.org::*It's fun to stay at the E.X.W.M.][It's fun to stay at the E.X.W.M.:1]]
(load (meq/ued-lib "meq-exwm-config"))
;; It's fun to stay at the E.X.W.M.:1 ends here

;; Let 'er rip!

;; And adapted from the ~README~:

;; #+begin_quote
;; This bit here will load an alternate ~init~ for a profile if ~--profile-init~ is passed on the
;; command-line, and otherwise, load the profile's usual ~init~ if it exists.
;; #+end_quote


;; [[file:FEEDME.org::*Let 'er rip!][Let 'er rip!:1]]
(meq/if-item-in-cla "--profile-init"
    (meq/load-from-cla "--profile-init")
    (if meq/var/literate-config
        (meq/cl (if (f-exists? (meq/ued "FEEDME.org"))
                                (meq/ued "FEEDME.org")
                                (meq/ued "README.org")))
        (meq/cl "init.el")))
;; Let 'er rip!:1 ends here

;; A quieter place

;; Also adapted from the ~README~:

;; #+begin_quote
;; If there's an alternate library directory or file the user wants to load,
;; ~--profile-lib~ will do the trick:
;; #+end_quote


;; [[file:FEEDME.org::*A quieter place][A quieter place:1]]
(meq/load-from-cla "--profile-lib" t)
;; A quieter place:1 ends here
