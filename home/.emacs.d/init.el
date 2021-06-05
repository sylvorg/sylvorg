;;; $EMACSDIR/config.el -*- lexical-binding: t; -*-
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(load-file "../.doom.d/help+20.el")
(setq confirm-kill-emacs nil)

(use-package rainbow-delimiters :straight t)

(with-eval-after-load 'rainbow-delimiters (load-file "../.doom.d/aiern/aiern.el")
    ;; From: https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
    (if (and (fbatp 'native-comp-available-p) (native-comp-available-p))
        (message "Native compilation is available")
        (message "Native complation is *not* available"))
    (if (fbatp 'json-serialize)
        (message "Native JSON is available")
        (message "Native JSON is *not* available")))

;; Adapted From:
;; From: https://emacs.stackexchange.com/a/19507
;; User: https://emacs.stackexchange.com/users/50/malabarba
;; (setq byte-compile-warnings (not t))
;; (setq byte-compile warnings (not obsolete))

;; From: https://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
;; (setq initial-major-mode 'org-mode)

;; (add-to-list 'org-src-lang-modes '("nix-repl" . nix-mode))
;; (org-babel-do-load-languages 'org-babel-load-languages '((nix-mode . t)))
;; (json (if (assoc :json params) (nth (+ (cl-position :json params) 1) params) nil))
;; (optargs (if (assoc '-- params) (nthcdr (+ (cl-position '-- params) 1) params) nil))
;; (if (or (eq json nil) (<= json 0)) "" "--json")
;; (if optargs (format "%s" optargs) "")
;; (format "%s" (cdr params))

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Use Python Syntax Highlighting for ".xonshrc" files
;; (setq auto-mode-alist 
;;       (append '(".*\\.xonshrc\\'" . python-mode)
;;               auto-mode-alist))
;; (setq auto-mode-alist 
;;       (append '(".*\\.xsh\\'" . python-mode)
;;              auto-mode-alist))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; Adapted From: https://www.reddit.com/r/emacs/comments/8fz6x2/relative_number_with_line_folding/dy7lmh7?utm_source=share&utm_medium=web2x&context=3
;; (display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Adapted From:
;; Answer: https://unix.stackexchange.com/a/152151
;; User: https://unix.stackexchange.com/users/72170/ole
;; No more typing the whole yes or no. Just y or n will do.
;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun aiern/remove-scratch-buffer nil
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'aiern/remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda nil
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

(fset 'yes-or-no-p 'y-or-n-p)

;; From: https://kundeveloper.com/blog/autorevert/
;; Auto revert files when they change
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; (add-hook #'find-file-hook #'aiern/set-buffer-save-without-query)

;; The following avoids being ask to allow the file local
;; setting of `buffer-save-without-query'.
;; IMHO it is not a big risk:
;; The malicious code that must not be saved
;; should never be allowed to enter Emacs in the first place.
;; (put 'buffer-save-without-query 'safe-local-variable #'booleanp)

(setq user-full-name "Jeet Ray"
      user-mail-address "aiern@protonmail.com")

(load-theme 'exo-ui-red-dark)

;; use-package
;; (setq use-package-always-defer t)

;; From: https://github.com/hartzell/straight.el/commit/882649137f73998d60741c7c8c993c7ebbe0f77a#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5R1649
;; (setq straight-disable-byte-compilation t)

;; Adapted From: https://github.com/jwiegley/use-package#use-package-chords
;; Important: https://github.com/noctuid/general.el/issues/53#issuecomment-307262154
(use-package use-package-chords
    :demand t
    :hook (after-init . key-chord-mode)
    :straight t)
(use-package hydra
    :straight t
    :demand t
    :custom (hydra-hint-display-type 'lv))
(use-package use-package-hydra
    :demand t
    :straight (use-package-hydra :type git :host gitlab :repo "picotech/use-package-hydra" :branch "master"))
(use-package use-package-hydra+
    :demand t
    :straight (use-package-hydra+ :type git :host gitlab :repo "picotech/use-package-hydra-plus" :branch "master"))
(use-package use-package-hercules
    :demand t
    :straight (use-package-hercules :type git :host gitlab :repo "shadowrylander/use-package-hercules" :branch "master"))

;; keys
(load-file "../.doom.d/naked.el")
(use-package general
    :demand t
    :config
        (general-auto-unbind-keys)
        (general-def :keymaps '(
            minibuffer-local-keymap
            counsel-describe-map
            helm-buffer-map) "M-x" 'exit-minibuffer)
    :custom
        (general-implicit-kbd t)
    :straight t)

;; (general-def :keymaps 'override
;;     (general-chord "zz") '+zen/toggle-fullscreen)

;; modal-modes
;; (add-hook! after-init 'aiern/disable-all-modal-modes)

;; hercules
(use-package hercules
    :straight (hercules :type git :host gitlab :repo "jjzmajic/hercules.el" :branch "master")
    :demand t
    :general (:keymaps 'override
        (general-chord "\\\\") 'aiern/toggle-which-key
        (general-chord "\\]") 'map-of-infinity/body)
    :hydra (map-of-infinity (:color blue :pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup))))
            ("`" nil "cancel")
            ("w" hydra/which-key/body "which-key")
            ("h" hydra/hercules/body "hercules")
            ("d" aiern/disable-all-modal-modes "disable all modal modes")
            ("t" toggles/body "toggles")
            ("k" all-keymaps/body "all keymaps"))
        (hydra/which-key (:color blue :pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup))))
            ("`" nil "cancel")
            ("a" aiern/any-popup-showing-p "any popup showing")
            ("h" aiern/which-key--hide-popup "hide-popup")
            ("s" aiern/which-key--show-popup "show-popup")
            ("r" aiern/which-key--refresh-popup "refresh-popup")
            ("t" aiern/toggle-which-key "toggle")
            ("l" aiern/which-key-show-top-level "aiern/toplevel")
            ("L" which-key-show-top-level "toplevel"))
        (hydra/hercules (:color blue :pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup))))
            ("`" nil "cancel")
            ("h" aiern/hercules-hide-all-modal-modes "hide all modal modes"))
        (toggles (:color blue :pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("`" nil "cancel"))
        (all-keymaps (:color blue :pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("`" nil "cancel"))
    :init
        (setq which-key-enable-extended-define-key t)
        (setq which-key-idle-delay 0.1)
        (setq which-key-idle-secondary-delay nil)
    :custom
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

;; ryo modal
(use-package ryo-modal
    :straight (ryo-modal :type git :host github :repo "kungsgeten/ryo-modal" :branch "master")
    :demand t
    :general (:keymaps 'override (general-chord "  ") 'aiern/toggle-ryo-hercules)
    :hydra+
      (toggles (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("r" aiern/toggle-ryo "ryo"))
        (all-keymaps (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("r" (progn (setq all-keymaps-map 'ryo-modal-mode) (aiern/ryo-show-top-level)) "ryo"))
    :hercules
        (:show-funs #'aiern/ryo-hercules-show
        :hide-funs #'aiern/ryo-hercules-hide
        :toggle-funs #'aiern/ryo-hercules-toggle
        :keymap 'ryo-modal-mode-map
        ;; :transient t
        )
    :config
        (defun aiern/ryo-hercules-toggle nil (interactive))
        (defun aiern/ryo-show-top-level nil (interactive)
            (aiern/which-key-show-top-level 'ryo-modal-mode-map))
        (add-to-list 'modal-modes 'ryo-modal-mode)
        (add-to-list 'modal-prefixes "ryo")
    
        (defun aiern/toggle-ryo nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "ryo" (fbatp ryo-modal-mode) 'ryo-modal-mode-map))
        (defun aiern/toggle-ryo-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "ryo" (fbatp ryo-modal-mode) 'ryo-modal-mode-map t))
        ;; From: https://github.com/Kungsgeten/ryo-modal#which-key-integration
        (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

;; evil
(use-package bind-map :straight t)
(use-package evil
    :demand t
    :straight t
    :init (setq-default evil-escape-key-sequence nil)
    :general (:keymaps 'override
        (general-chord "kk") 'aiern/toggle-evil
        (general-chord ",,") 'evil-ex)
    :hydra+
      (toggles (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("e" aiern/toggle-evil "evil"))
        (all-keymaps (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("e" (progn (setq all-keymaps-map 'evil-mode) (aiern/evil-show-top-level)) "evil"))
    :hercules
        (:show-funs #'aiern/evil-hercules-show
        :hide-funs #'aiern/evil-hercules-hide
        :toggle-funs #'aiern/evil-hercules-toggle
        :keymap 'evil-normal-state-map
        ;; :transient t
        )
    :config
        (defun aiern/evil-hercules-toggle nil (interactive))
        (defun aiern/evil-show-top-level nil (interactive)
            (aiern/which-key-show-top-level 'evil-normal-state-map))
        (add-to-list 'modal-modes 'evil-mode)
        (add-to-list 'modal-prefixes "evil")
    
        (defun aiern/toggle-evil nil (interactive)
            (funcall 'aiern/toggle-inner 'evil-mode "evil" (fbatp evil-mode) 'evil-normal-state-map))
        (defun aiern/toggle-evil-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'evil-mode "evil" (fbatp evil-mode) 'evil-normal-state-map t))
        ;; From: https://www.reddit.com/r/emacs/comments/lp45zd/help_requested_in_configuring_ryomodal/gp3rfx9?utm_source=share&utm_medium=web2x&context=3
        ;; Kept for documentation porpoises
        ;; (eval
        ;;       `(ryo-modal-keys
        ;;             ("l l" ,(general-simulate-key ":wq <RET>") :first '(evil-normal-state) :name "wq")
        ;;             ("l p" ,(general-simulate-key ":q <RET>") :first '(evil-normal-state) :name "q")
        ;;             ("l o" ,(general-simulate-key ":w <RET>") :first '(evil-normal-state) :name "w")
        ;;             ("l q" ,(general-simulate-key ":q! <RET>") :first '(evil-normal-state) :name "q!")))

        ;; Use to get command name:
        ;; Eg: (cdr (assoc "q" evil-ex-commands))
        ;; Then "C-x C-e" (eval-last-sexp)
    :ryo
        ("l" :hydra
                '(evil-exits (:color blue :pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup))))
                    ;; From: https://gist.github.com/shadowrylander/46b81297d1d3edfbf1e2d72d5e29171e
                    "A hydra for getting the fuck outta' here!"
                    ("`" nil "cancel")
                    ("l" evil-save-and-quit ":wq")
                    ("p" evil-quit ":q")
                    ("o" evil-write ":w")
                    ("O" evil-write-all ":wa")
                    ;; ("q" (funcall (general-simulate-key ":q! <RET>")) ":q!"))
                    ("q" (funcall (evil-quit t)) ":q!"))
                :name "evil exits"))

;; Adapted From: https://github.com/mohsenil85/evil-evilified-state and https://github.com/syl20bnr/spacemacs
(use-package evil-evilified-state
    :after evil
    :straight (evil-evilified-state
        :type git
        :host github
        :repo "shadowrylander/evil-evilified-state"
        :branch "master"))

;; god mode
(use-package god-mode
    :straight t
    :general
        (:keymaps 'override
            (general-chord "jj") 'aiern/toggle-god
            (general-chord "';") 'god-execute-with-current-bindings)
    :hydra+
      (toggles (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("g" aiern/toggle-god "god"))
        (all-keymaps (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("g" (progn (setq all-keymaps-map 'god-local-mode) (aiern/god-show-top-level)) "god"))
    :hercules
        (:show-funs #'aiern/god-hercules-show
        :hide-funs #'aiern/god-hercules-hide
        :toggle-funs #'aiern/god-hercules-toggle
        :keymap 'global-map
        ;; :transient t
        )
    :config
        (defun aiern/god-hercules-toggle nil (interactive))
        (defun aiern/god-show-top-level nil (interactive)
            (aiern/which-key-show-top-level 'global-map))
        (add-to-list 'modal-modes 'god-local-mode)
        (add-to-list 'modal-prefixes "god")
    
        (defun aiern/toggle-god nil (interactive)
            (funcall 'aiern/toggle-inner 'god-local-mode "god" (fbatp god-local-mode) 'global-map))
        (defun aiern/toggle-god-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'god-local-mode "god" (fbatp god-local-mode) 'global-map t))
        (which-key-enable-god-mode-support))

;; xah-fly-keys
(use-package xah-fly-keys
    :straight t
    :ryo
        ("m" :hydra
            '(modal-modes (:color blue :pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup))))
                "A modal hydra!"
                ("`" nil "cancel")
                ("x" aiern/toggle-xah "xah-fly-keys")) :name "modal modes")
    :hydra+
      (toggles (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("x" aiern/toggle-xah "xah"))
        (all-keymaps (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("x" (progn (setq all-keymaps-map 'xah-fly-keys) (aiern/xah-show-top-level)) "xah"))
    :hercules
        (:show-funs #'aiern/xah-hercules-show
        :hide-funs #'aiern/xah-hercules-hide
        :toggle-funs #'aiern/xah-hercules-toggle
        :keymap 'xah-fly-command-map
        ;; :transient t
        )
    :config
        (defun aiern/xah-hercules-toggle nil (interactive))
        (defun aiern/xah-show-top-level nil (interactive)
            (aiern/which-key-show-top-level 'xah-fly-command-map))
        (add-to-list 'modal-modes 'xah-fly-keys)
        (add-to-list 'modal-prefixes "xah")
    
        (defun aiern/toggle-xah nil (interactive)
            (funcall 'aiern/toggle-inner 'xah-fly-keys "xah" (fbatp xah-fly-keys) 'xah-fly-command-map))
        (defun aiern/toggle-xah-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'xah-fly-keys "xah" (fbatp xah-fly-keys) 'xah-fly-command-map t)))

;; objed
(use-package objed
    :straight t
    :general (:keymaps 'override (general-chord "ii") 'aiern/toggle-objed)
    :hydra+
      (toggles (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("o" aiern/toggle-objed "objed"))
        (all-keymaps (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("o" (progn (setq all-keymaps-map 'objed-mode) (aiern/objed-show-top-level)) "objed"))
    :hercules
        (:show-funs #'aiern/objed-hercules-show
        :hide-funs #'aiern/objed-hercules-hide
        :toggle-funs #'aiern/objed-hercules-toggle
        :keymap 'objed-map
        ;; :transient t
        )
    :config
        (defun aiern/objed-hercules-toggle nil (interactive))
        (defun aiern/objed-show-top-level nil (interactive)
            (aiern/which-key-show-top-level 'objed-map))
        (add-to-list 'modal-modes 'objed-mode)
        (add-to-list 'modal-prefixes "objed")
    
        (defun aiern/toggle-objed nil (interactive)
            (funcall 'aiern/toggle-inner 'objed-mode "objed" (fbatp objed-mode) 'objed-map))
        (defun aiern/toggle-objed-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'objed-mode "objed" (fbatp objed-mode) 'objed-map t)))

;; kakoune
(use-package kakoune
    :straight t
    :hydra+
        (modal-modes (:color blue) ("k" aiern/toggle-kakoune-hercules "kakoune"))
      (toggles (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("k" aiern/toggle-kakoune "kakoune"))
        (all-keymaps (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("k" (progn (setq all-keymaps-map 'ryo-modal-mode) (aiern/kakoune-show-top-level)) "kakoune"))
    :hercules
        (:show-funs #'aiern/kakoune-hercules-show
        :hide-funs #'aiern/kakoune-hercules-hide
        :toggle-funs #'aiern/kakoune-hercules-toggle
        :keymap 'ryo-modal-mode-map
        ;; :transient t
        )
    :config
        (defun aiern/kakoune-hercules-toggle nil (interactive))
        (defun aiern/kakoune-show-top-level nil (interactive)
            (aiern/which-key-show-top-level 'ryo-modal-mode-map))
        (add-to-list 'modal-modes 'ryo-modal-mode)
        (add-to-list 'modal-prefixes "kakoune")
    
        (defun aiern/toggle-kakoune nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "kakoune" (fbatp ryo-modal-mode) 'ryo-modal-mode-map))
        (defun aiern/toggle-kakoune-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "kakoune" (fbatp ryo-modal-mode) 'ryo-modal-mode-map t)))

;; modalka
(use-package modalka
    :straight t
    ;; :general (:keymaps 'override (general-chord "::") 'aiern/toggle-modalka-hercules)
    :hydra+
      (toggles (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("m" aiern/toggle-modalka "modalka"))
        (all-keymaps (:color blue :pre (progn
                    (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup)))) ("m" (progn (setq all-keymaps-map 'modalka-mode) (aiern/modalka-show-top-level)) "modalka"))
    :hercules
        (:show-funs #'aiern/modalka-hercules-show
        :hide-funs #'aiern/modalka-hercules-hide
        :toggle-funs #'aiern/modalka-hercules-toggle
        :keymap 'modalka-mode-map
        ;; :transient t
        )
    :config
        (defun aiern/modalka-hercules-toggle nil (interactive))
        (defun aiern/modalka-show-top-level nil (interactive)
            (aiern/which-key-show-top-level 'modalka-mode-map))
        (add-to-list 'modal-modes 'modalka-mode)
        (add-to-list 'modal-prefixes "modalka")
    
        (defun aiern/toggle-modalka nil (interactive)
            (funcall 'aiern/toggle-inner 'modalka-mode "modalka" (fbatp modalka-mode) 'modalka-mode-map))
        (defun aiern/toggle-modalka-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'modalka-mode "modalka" (fbatp modalka-mode) 'modalka-mode-map t)))

;; org-mode
(use-package org
    :straight t
    :init
        ;; I'm using ox-pandoc
        ;; (setq org-export-backends '(md gfm latex odt org))
        (setq org-directory "/tmp")
        (setq org-roam-directory org-directory)
    :config
        (org-babel-lob-ingest "./README.org")
        
    :general
        (:keymaps 'override
            (naked "backtab") 'aiern/evil-close-fold)
    :ryo ("o" :hydra
        '(hydra-org (:color blue :pre (progn
            (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup))))
                "A hydra for org-mode!"
                ("o" org-babel-tangle "tangle")
                ("a" aiern/org-babel-tangle-append "tangle append")
                ("f" org-babel-tangle-file "tangle file")
                ("n" aiern/narrow-or-widen-dwim "narrow")
                ("s" org-edit-special "org edit special")
                ("q" nil "cancel")))
    :custom
        (org-descriptive-links t)
        (org-confirm-babel-evaluate nil)
        (org-startup-folded t)
        (org-src-fontify-natively t)
        ;; (org-src-window-setup 'current-window)
        (org-cycle-emulate-tab 'whitestart))

(with-eval-after-load 'org (load-file "../.doom.d/emacs-bankruptcy/site-lisp/org-numbers-overlay.el"))
(use-package nix-mode
    :straight t
    :commands (org-babel-execute:nix)
    :mode ("\\.nix\\'")
    :after org)
(use-package xonsh-mode
    :straight (xonsh-mode :type git :host github :repo "seanfarley/xonsh-mode" :branch "master")
    :commands (org-babel-execute:xonsh org-babel-expand-body:xonsh)
    :mode ("\\.xonshrc\\'" "\\.xsh\\'")
    :after org)
(use-package dockerfile-mode
    :straight t
    :config
        (org-babel-do-load-languages 'org-babel-load-languages
            (append org-babel-load-languages
                '((Dockerfile . t))))
    :mode ("\\Dockerfile\\'")
    :after org)
(use-package vimrc-mode
    :straight (vimrc-mode :type git :host github :repo "mcandre/vimrc-mode" :branch "master")
    :commands
        (org-babel-execute:vimrc
        org-babel-expand-body:vimrc)
    :mode "\\.vim\\(rc\\)?\\'"
    :after org)

;; minibuffer


;; TODO: Split this into multiple `use-package!' instances using my new `hydra+' keyword
(with-eval-after-load 'ryo-modal (ryo-modal-key "x" :hydra
      '(hydra-execute (:color blue :pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup))))
            "A hydra for launching stuff!"
            ("c" counsel-M-x "counsel")
            ("h" helm-smex-major-mode-commands "helm smex major mode")
            ("s" helm-smex "helm smex")
            ("e" execute-extended-command "M-x")
            ("q" nil "cancel"))
            :name "execute order 65"))

;; git
(use-package git-gutter
    :straight t
    :ryo ("g" :hydra
        '(hydra-git (:pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup))))
            "A hydra for git!"
            ("`" nil "cancel" :color blue)
            ("j" git-gutter:next-hunk "next")
            ("k" git-gutter:previous-hunk "previous")
            ("d" git-gutter:popup-hunk "diff")
            ("s" git-gutter:stage-hunk "stage")
            ("r" git-gutter:revert-hunk "revert")
            ("m" git-gutter:mark-hunk "mark"))))
(use-package magit
    :straight t
    :ryo ("g" :hydra+
        '(hydra-git (:pre (progn
                (when (aiern/any-popup-showing-p) (aiern/which-key--hide-popup))) :post (progn (unless hydra-curr-map (aiern/which-key--show-popup))))
            "A hydra for git!"
            ("g" magit-status "magit" :color blue))))
;; (use-package! gitattributes-mode)

;; buffer
;; (remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(defun display-startup-echo-area-message nil (aiern/which-key-show-top-level))

(use-package writeroom-mode
    :straight t
    :hook after-init
    :general (:keymaps 'override (general-chord "zz") 'writeroom-mode)
    :custom (writeroom-fullscreen-effect t))

(use-package focus
    :straight t
    ;; :hook (doom-init-ui . focus-mode)
    :custom
        (focus-mode-to-thing '(
            ;; (prog-mode . defun)
            (prog-mode . line)
            ;; (text-mode . sentence)
            (text-mode . line)
            (outline-mode . line))))

;; (when (featurep! :editor parinfer) (use-package! parinfer-rust-mode
;;     :hook emacs-lisp-mode
;;     :init (setq parinfer-rust-auto-download t)
;;     :custom (parinfer-rust-check-before-enable nil)))

(use-package projectile :straight t)
(use-package yasnippet :straight t)
(use-package company :straight t)
(use-package yankpad
    :straight t
    :after (projectile company yasnippet)
    :init
        (setq yankpad-file "./yankpad.org")
        (defun aiern/yankpad-hercules-toggle nil (interactive))
    :general (:keymap 'override
        (general-chord "[[") 'aiern/yankpad-hercules-toggle
        (general-chord "]]") 'yankpad-expand)
    :config (yankpad-map)
    :hercules
        (:show-funs #'aiern/yankpad-hercules-show
            :hide-funs #'aiern/yankpad-hercules-hide
            :toggle-funs #'aiern/yankpad-hercules-toggle
            :keymap 'yankpad-keymap
            ;; :transient t
        ))

(use-package vlf
    :straight (vlf :type git :host github :repo "m00natic/vlfi" :branch "master")
    :demand t
    :custom (vlf-application 'always))

;; !!! THE ORDER HERE MATTERS! !!!
;; (add-hook! doom-init-ui
;;     (load-file "fit-frame.el")
;;     (load-file "autofit-frame.el")
;;     ;; (load-file "buff-menu+.el")
;;     (load-file "compile-.el")
;;     (load-file "compile+.el")
;;     (load-file "grep+.el")
;;     (load-file "dired+.el")
;;     (load-file "dired-details.el")
;;     (load-file "dired-details+.el")
;;     (load-file "doremi.el")
;;     (load-file "hexrgb.el")
;;     (load-file "frame-fns.el")
;;     (load-file "faces+.el")
;;     (load-file "doremi-frm.el")
;;     (load-file "eyedropper.el")
;;     (load-file "facemenu+.el")
;;     (load-file "frame+.el")
;;     (load-file "help+.el")
;;     (load-file "info+.el")
;;     (load-file "menu-bar+.el")
;;     (load-file "mouse+.el")
;;     (load-file "setup-keys.el")
;;     (load-file "strings.el")
;;     ;; (load-file "simple+.el")
;;     (load-file "frame-cmds.el")
;;     (load-file "thumb-frm.el")
;;     (load-file "window+.el")
;;     (load-file "zoom-frm.el")
;;     (load-file "oneonone.el")
;;     (use-package! oneonone
;;         :demand t
;;         :hook (after-init . 1on1-emacs)
;;         :custom
;;             (1on1-minibuffer-frame-width 10000)
;;             (1on1-minibuffer-frame-height 10000)))

;; terminal


;; window manager


;; system
;; (eval `(let ((mypaths
;;     '(
;;         ,(concat "/home/" (getenv "USER") "/.nix-profile/bin")
;;         "/home/linuxbrew/.linuxbrew/bin"
;;         "/usr/bin"
;;         "/usr/sbin"
;;         ,(concat "/home/" (getenv "USER") "/.emacs.d/bin")
;;         ,(concat "/home/" (getenv "USER") "/.doom.d"))))
;;     ;; (setenv "PATH" (mapconcat 'identity mypaths ";") )
;;     (setq exec-path (append mypaths (list "." exec-directory)) )
;; ))
(use-package exec-path-from-shell
    :demand t
    :straight (exec-path-from-shell
        :type git
        :host github
        :repo "purcell/exec-path-from-shell"
        :branch "master"))


;; etc
(setq-default indent-tabs-mode nil)
;; (when (featurep! :private spacemacs) (use-package! spacemacs
;;     :init (remove-hook 'org-load-hook #'+org-init-keybinds-h)
;;     :hook (doom-init-ui . spacemacs/home)))
