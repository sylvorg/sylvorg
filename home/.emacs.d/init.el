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
(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)

;; Adapted From: https://www.reddit.com/r/emacs/comments/caifq4/package_updates_with_straight/et99epi?utm_source=share&utm_medium=web2x&context=3
;; And: https://github.com/raxod502/straight.el#updating-recipe-repositories
(when (member "--update" command-line-args) (straight-pull-all)
(straight-merge-all)
(straight-freeze-versions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use-package
;; (setq use-package-always-defer t)

;; Adapted From: https://github.com/jwiegley/use-package#use-package-chords
;; Important: https://github.com/noctuid/general.el/issues/53#issuecomment-307262154
(use-package use-package-chords :demand t)

(use-package use-package-hydra
    :demand t
    :straight (use-package-hydra
        :type git
        :host gitlab
        :repo "picotech/use-package-hydra"
        :branch "master")
    :init (use-package hydra :demand t :custom (hydra-hint-display-type 'lv)))
(use-package use-package-deino
    :demand t
    :straight nil
    :load-path "lib/use-package-deino"
    :init (use-package use-package-extras :demand t :straight nil :load-path "lib/use-package-extras"))

;; keys
(use-package alloy
    :straight nil
    :load-path "lib/alloy"
    :demand t
    :use-package-preconfig (command-log-mode)
    :load-emacs-file-preconfig ("naked")
    :config
        (alloy-auto-unbind-keys)
        (alloy-def :keymaps '(override
            aiern-insert-state-map
            aiern-normal-state-map
            evil-insert-state-map
            evil-normal-state-map)
            ;; Adapted From:
            ;; Answer: https://stackoverflow.com/a/4557027/10827766
            ;; User: https://stackoverflow.com/users/387076/gilles-so-stop-being-evil
            "\eOA" [up]
            "\e[A" [up]
            "\eOB" [down]
            "\e[B" [down]
            "\eOD" [left]
            "\e[D" [left]
            "\eOC" [right]
            "\e[C" [right])
        (alloy-def :keymaps '(
                minibuffer-local-map
                counsel-describe-map
                helm-buffer-map)
            "M-x" 'exit-minibuffer)
    :custom (alloy-implicit-kbd t))

;; hercules
(use-package hercules
    :straight (hercules :type git :host gitlab :repo "jjzmajic/hercules.el" :branch "master")
    :use-package-postconfig (dash) (s)
        (meq :straight nil :load-path "lib/meq" :demand t)
        (deino :demand t :straight nil :load-path "lib/deino" :gsetq (deino-hint-display-type 'lv))
        (lode :demand t :straight nil :load-path "lib/lode")
    :demand t
    :demon
        ((alloy-chord "\\\\") 'meq/toggle-which-key)
        ((alloy-chord "\\]") 'map-of-infinity/body)
    :deino (map-of-infinity (:color blue)
            ("`" nil "cancel")
            ("w" deino/which-key/body "which-key")
            ("h" deino/hercules/body "hercules")
            ("d" meq/disable-all-modal-modes "disable all modal modes")
            ("t" toggles/body "toggles")
            ("k" all-keymaps/body "all keymaps"))
        (deino/which-key (:color blue)
            ("`" nil "cancel")
            ("a" meq/any-popup-showing-p "any popup showing")
            ("h" meq/which-key--hide-popup "hide-popup")
            ("s" meq/which-key--show-popup "show-popup")
            ("r" meq/which-key--refresh-popup "refresh-popup")
            ("t" meq/toggle-which-key "toggle")
            ("l" meq/which-key-show-top-level "meq/toplevel")
            ("L" which-key-show-top-level "toplevel"))
        (deino/hercules (:color blue)
            ("`" nil "cancel")
            ("h" meq/hercules-hide-all-modal-modes "hide all modal modes"))
        (toggles (:color blue) ("`" nil "cancel"))
        (all-keymaps (:color blue) ("`" nil "cancel"))
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

;; alamode
(use-package alamode :demand t :straight nil :load-path "lib/alamode")

;; ryo modal
(use-package ryo-modal
    :straight (ryo-modal :type git :host github :repo "kungsgeten/ryo-modal" :branch "master")
    :demand t
    :demon
        ((alloy-chord "  ") 'meq/toggle-ryo-hercules)
        ((alloy-chord " ,") 'meq/ryo-execute-with-current-bindings)
    :config ;; From: https://github.com/Kungsgeten/ryo-modal#which-key-integration
        (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

;; sorrow


;; damascus
(use-package damascus
    :demand t
    :straight nil
    :load-emacs-file-preconfig ("damascus")
    :load-emacs-file-postconfig ("help+20")
    ;; :hook
        ;; (find-file . meq/set-buffer-save-without-query)
    :gsetq
        (indent-tabs-mode nil
            confirm-kill-emacs nil)

        ;; Adapted From:
        ;; From: https://emacs.stackexchange.com/a/19507
        ;; User: https://emacs.stackexchange.com/users/50/malabarba
        ;; (byte-compile-warnings (not t))
        ;; (byte-compile warnings (not obsolete))
        
        ;; From: https://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
        (initial-major-mode 'org-mode)

        ;; Follow symlinks
        (vc-follow-symlinks t)

        ;; Use Python Syntax Highlighting for ".xonshrc" files
        ;; (auto-mode-alist 
        ;;       (append '(".*\\.xonshrc\\'" . python-mode)
        ;;               auto-mode-alist))
        ;; (auto-mode-alist 
        ;;       (append '(".*\\.xsh\\'" . python-mode)
        ;;              auto-mode-alist))

        (user-full-name "Jeet Ray"
            user-mail-address "aiern@protonmail.com")
    :config/defun*
        ;; Answer: https://emacs.stackexchange.com/a/51829
        ;; User: https://emacs.stackexchange.com/users/2370/tobias
        (meq/set-buffer-save-without-query nil
            "Set `buffer-save-without-query' to t."
            (unless (variable-binding-locus 'buffer-save-without-query)
                (setq buffer-save-without-query t)))
    :init
        ;; From: https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
        (if (and (meq/fbatp 'native-comp-available-p) (native-comp-available-p))
            (message "Native compilation is available")
            (message "Native complation is *not* available"))
        (if (meq/fbatp 'json-serialize)
            (message "Native JSON is available")
            (message "Native JSON is *not* available"))

        ;; (add-to-list 'org-src-lang-modes '("nix-repl" . nix-mode))
        ;; (org-babel-do-load-languages 'org-babel-load-languages '((nix-mode . t)))
        ;; (json (if (assoc :json params) (nth (+ (cl-position :json params) 1) params) nil))
        ;; (optargs (if (assoc '-- params) (nthcdr (+ (cl-position '-- params) 1) params) nil))
        ;; (if (or (eq json nil) (<= json 0)) "" "--json")
        ;; (if optargs (format "%s" optargs) "")
        ;; (format "%s" (cdr params))

        ;; This determines the style of line numbers in effect. If set to `nil', line
        ;; numbers are disabled. For relative line numbers, set this to `relative'.
        ;; Adapted From: https://www.reddit.com/r/emacs/comments/8fz6x2/relative_number_with_line_folding/dy7lmh7?utm_source=share&utm_medium=web2x&context=3
        ;; (display-line-numbers-mode 1)
        (setq display-line-numbers-type 'relative)

        ;; Adapted From:
        ;; Answer: https://stackoverflow.com/a/50716229/10827766
        ;; User: https://stackoverflow.com/users/1482346/muro
        (global-display-line-numbers-mode t)

        ;; Adapted From:
        ;; Answer: https://unix.stackexchange.com/a/152151
        ;; User: https://unix.stackexchange.com/users/72170/ole
        ;; No more typing the whole yes or no. Just y or n will do.
        ;; Makes *scratch* empty.
        (setq initial-scratch-message "")

        ;; Removes *scratch* from buffer after the mode has been set.
        (defun meq/remove-scratch-buffer nil
        (if (get-buffer "*scratch*")
            (kill-buffer "*scratch*")))
        (add-hook 'after-change-major-mode-hook 'meq/remove-scratch-buffer)

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

        ;; The following avoids being ask to allow the file local
        ;; setting of `buffer-save-without-query'.
        ;; IMHO it is not a big risk:
        ;; The malicious code that must not be saved
        ;; should never be allowed to enter Emacs in the first place.
        ;; (put 'buffer-save-without-query 'safe-local-variable #'booleanp)

        ;; (toggle-debug-on-error)

        (unless (meq/which-theme) (cond
            ((member "--purple" command-line-args) (load-theme 'dracula-purple-dark))
            ((member "--orange" command-line-args) (load-theme 'dracula-orange-dark))
            ((member "--red" command-line-args) (load-theme 'exo-ui-red-dark))
            ((member "--flamingo" command-line-args) (load-theme 'herschel-flamingo-pink-dark))
            ((member "--blue" command-line-args) (load-theme 'st-giles-blue-dark))
            (t (load-theme 'ghostfreak-green-dark)))))

;; modal-modes

;; aiern
(use-package aiern
    :demand t
    :use-package-preconfig (bind-map)
    :straight nil
    :load-path "lib/aiern"
    :demon
        ((alloy-chord "uu") 'meq/toggle-aiern)
        ((alloy-chord "UU") 'meq/toggle-aiern-force)

        ;; TODO
        ;; ((alloy-chord "") 'meq/toggle-aiern-ex-hercules)
        ;; ((alloy-chord "") 'meq/toggle-aiern-ex-hercules-force)

        ((alloy-chord ",.") 'aiern-ex)
        ((alloy-chord ",/") 'meq/aiern-execute-with-current-bindings)
    :config
        (alloy-def :keymaps '(override aiern-insert-state-map)
            (naked "RET") 'newline-and-indent
            (alloy-chord "]\\") 'meq/end-of-line-and-indented-new-line)
        ;; TODO: How do I create a keymap `aiern-ex-keymap' out of the `aiern-ex-commands' alist?

        ;; (hercules-def :show-funs #'meq/aiern-ex-hercules-show
        ;;     :hide-funs #'meq/aiern-ex-hercules-hide
        ;;     :toggle-funs #'meq/aiern-ex-hercules-toggle
        ;;     :keymap 'aiern-ex-keymap
        ;;     ;; :transient t
        ;; )

        ;; (defun meq/aiern-ex-hercules-toggle nil (interactive))
        ;; (defun meq/aiern-ex-show-top-level nil (interactive)
        ;;     (meq/which-key-show-top-level 'aiern-ex-keymap))

        ;; (defun meq/toggle-aiern-ex nil (interactive)
        ;;     (funcall 'meq/toggle-inner 'aiern-mode "aiern-ex" (meq/fbatp aiern-mode) 'aiern-ex-keymap))
        ;; (defun meq/toggle-aiern-ex-force nil (interactive)
        ;;     (funcall 'meq/toggle-inner 'aiern-mode "aiern-ex" (meq/fbatp aiern-mode) 'aiern-ex-keymap nil t))
        ;; (defun meq/toggle-aiern-ex-hercules nil (interactive)
        ;;     (funcall 'meq/toggle-inner 'aiern-mode "aiern-ex" (meq/fbatp aiern-mode) 'aiern-ex-keymap t))
        ;; (defun meq/toggle-aiern-ex-hercules-force nil (interactive)
        ;;     (funcall 'meq/toggle-inner 'aiern-mode "aiern-ex" (meq/fbatp aiern-mode) 'aiern-ex-keymap t t))
        )

;; evil
(use-package evil
    :demand t
    :use-package-preconfig (bind-map)
    :use-package-postconfig
        ;; Adapted From: https://github.com/mohsenil85/evil-evilified-state and
        ;; https://github.com/syl20bnr/spacemacs
        (evil-evilified-state
            :after evil
            :straight (evil-evilified-state
                :type git
                :host github
                :repo "shadowrylander/evil-evilified-state"
                :branch "master"))
    :gsetq (evil-escape-key-sequence nil)
    :demon
        ((alloy-chord "kk") 'meq/toggle-evil)
        ((alloy-chord "KK") 'meq/toggle-evil-force)
        
        ;; TODO
        ;; ((alloy-chord "") 'meq/toggle-evil-ex-hercules)
        ;; ((alloy-chord "") 'meq/toggle-evil-ex-hercules-force)
        
        ((alloy-chord ",,") 'evil-ex)
    :leaf (evil :advice
        (:override evil-insert-state (lambda (&optional _) (interactive)
            (meq/disable-all-modal-modes))))
    :config
        ;; From: https://www.reddit.com/r/emacs/comments/lp45zd/help_requested_in_configuring_ryomodal/gp3rfx9?utm_source=share&utm_medium=web2x&context=3
        ;; Kept for documentation porpoises
        ;; (eval
        ;;       `(ryo-modal-keys
        ;;             ("l l" ,(alloy-simulate-key ":wq <RET>") :first '(evil-normal-state) :name "wq")
        ;;             ("l p" ,(alloy-simulate-key ":q <RET>") :first '(evil-normal-state) :name "q")
        ;;             ("l o" ,(alloy-simulate-key ":w <RET>") :first '(evil-normal-state) :name "w")
        ;;             ("l q" ,(alloy-simulate-key ":q! <RET>") :first '(evil-normal-state) :name "q!")))

        ;; Use to get command name:
        ;; Eg: (cdr (assoc "q" evil-ex-commands))
        ;; Then "C-x C-e" (eval-last-sexp)

        ;; TODO: How do I create a keymap `evil-ex-keymap' out of the `evil-ex-commands' alist?

        ;; (hercules-def :show-funs #'meq/evil-ex-hercules-show
        ;;     :hide-funs #'meq/evil-ex-hercules-hide
        ;;     :toggle-funs #'meq/evil-ex-hercules-toggle
        ;;     :keymap 'evil-ex-keymap
        ;;     ;; :transient t
        ;; )

        ;; (defun meq/evil-ex-hercules-toggle nil (interactive))
        ;; (defun meq/evil-ex-show-top-level nil (interactive)
        ;;     (meq/which-key-show-top-level 'evil-ex-keymap))

        ;; (defun meq/toggle-evil-ex nil (interactive)
        ;;     (funcall 'meq/toggle-inner 'evil-mode "evil-ex" (meq/fbatp evil-mode) 'evil-ex-keymap))
        ;; (defun meq/toggle-evil-ex-force nil (interactive)
        ;;     (funcall 'meq/toggle-inner 'evil-mode "evil-ex" (meq/fbatp evil-mode) 'evil-ex-keymap nil t))
        ;; (defun meq/toggle-evil-ex-hercules nil (interactive)
        ;;     (funcall 'meq/toggle-inner 'evil-mode "evil-ex" (meq/fbatp evil-mode) 'evil-ex-keymap t))
        ;; (defun meq/toggle-evil-ex-hercules-force nil (interactive)
        ;;     (funcall 'meq/toggle-inner 'evil-mode "evil-ex" (meq/fbatp evil-mode) 'evil-ex-keymap t t))
    :ryo
        ("l" :hydra
                '(evil-exits (:color blue)
                    ;; From: https://github.com/emacs-evil/evil/blob/master/evil-maps.el#L449
                    "A deino for getting the fuck outta' here!"
                    ("`" nil "cancel")
                    ("l" evil-save-and-quit ":wq")
                    ("p" evil-quit ":q")
                    ("o" evil-write ":w")
                    ("O" evil-write-all ":wa")
                    ;; ("q" (funcall (alloy-simulate-key ":q! <RET>")) ":q!"))
                    ("q" (funcall (evil-quit t)) ":q!"))
                :name "evil exits"))

;; god mode
(use-package god-mode
    :demand t
    :use-package-postconfig
        (aiern-god-state :straight nil :load-path "lib/aiern-god-state" :demand t)
        (evil-god-state :demand t :straight (evil-god-state
            :type git
            :host github
            :repo "gridaphobe/evil-god-state"
            :branch "master"))
    :demon
        ((alloy-chord "jj") 'meq/toggle-god)
        ((alloy-chord ";'") 'god-execute-with-current-bindings)
    :config (which-key-enable-god-mode-support))

;; xah-fly-keys
(use-package xah-fly-keys
    :ryo
        ("m" :hydra
            '(modal-modes (:color blue)
                "A modal deino!"
                ("`" nil "cancel")
                ("x" meq/toggle-xah "xah-fly-keys")) :name "modal modes"))

;; objed
(use-package objed
    :demon ((alloy-chord "ii") 'meq/toggle-objed))

;; kakoune
(use-package kakoune :deino+ (modal-modes (:color blue) ("k" meq/toggle-kakoune-hercules "kakoune")))

;; modalka
(use-package modalka :demon ((alloy-chord "::") 'meq/toggle-modalka-hercules))

;; org-mode
(use-package outshine :hook ((outline-mode prog-mode text-mode org-mode) . outshine-mode))
(use-package org
    :use-package-postconfig
        (nix-mode
            :demand t
           
            :commands (org-babel-execute:nix)
            :mode ("\\.nix\\'")
            :init/defun*
                ;; Adapted From:
                ;; Answer: https://emacs.stackexchange.com/a/61442
                ;; User: https://emacs.stackexchange.com/users/20061/zeta
                (org-babel-execute:nix (body params)
                    "Execute a block of Nix code with org-babel."
                    (message "executing Nix source code block")
                    (let ((in-file (org-babel-temp-file "n" ".nix"))
                        (json (or (cdr (assoc :json params)) nil))
                        (opts (or (cdr (assoc :opts params)) nil))
                        (args (or (cdr (assoc :args params)) nil))
                        (read-write-mode (or (cdr (assoc :read-write-mode params)) nil))
                        (eval (or (cdr (assoc :eval params)) nil))
                        (show-trace (or (cdr (assoc :show-trace params)) nil)))
                    (with-temp-file in-file
                        (insert body))
                    (org-babel-eval
                        (format "nix-instantiate %s %s %s %s %s %s %s"
                            (if (xor (eq json nil) (<= json 0)) "" "--json")
                            (if (xor (eq show-trace nil) (<= show-trace 0)) "" "--show-trace")
                            (if (xor (eq read-write-mode nil) (<= read-write-mode 0)) "" "--read-write-mode")
                            (if (xor (eq eval nil) (<= eval 0)) "" "--eval")
                            (if (eq opts nil) "" opts)
                            (if (eq args nil) "" args)
                            (org-babel-process-file-name in-file))
                    ""))))
        (xonsh-mode
            :demand t
            :straight (xonsh-mode :type git :host github :repo "seanfarley/xonsh-mode" :branch "master")
            :commands (org-babel-execute:xonsh org-babel-expand-body:xonsh)
            :mode ("\\.xonshrc\\'" "\\.xsh\\'")
            :init/defun*
                ;; Adapted From:
                ;; Answer: https://emacs.stackexchange.com/a/61442
                ;; User: https://emacs.stackexchange.com/users/20061/zeta
                (org-babel-execute:xonsh (body params)
                    "Execute a block of Xonsh code with org-babel."
                    (message "executing Xonsh source code block")
                    (let ((in-file (org-babel-temp-file "x" ".xsh"))
                        (opts (or (cdr (assoc :opts params)) nil))
                        (args (or (cdr (assoc :args params)) nil)))
                    (with-temp-file in-file
                        (insert body))
                    (org-babel-eval
                        (format "xonsh %s %s %s"
                            (if (eq opts nil) "" opts)
                            (if (eq args nil) "" args)
                            (org-babel-process-file-name in-file))
                    ""))))
        (dockerfile-mode :demand t :mode ("\\Dockerfile\\'"))
        (vimrc-mode
            :demand t
            :straight (vimrc-mode :type git :host github :repo "mcandre/vimrc-mode" :branch "master")
            :commands
                (org-babel-execute:vimrc
                org-babel-expand-body:vimrc)
            :mode "\\.vim\\(rc\\)?\\'")
    :config
        (org-babel-do-load-languages 'org-babel-load-languages
            (append org-babel-load-languages
            '((python . t)
            (shell . t))))
        (org-babel-lob-ingest "./README.org")

        (when (file-exists-p "~/shadowrylander/README.org")
            (org-babel-lob-ingest "~/shadowrylander/README.org"))
        (when (file-exists-p "~/shadowrylander/strange.aiern.org")
            (org-babel-lob-ingest "~/shadowrylander/strange.aiern.org"))
        
        (defun meq/get-header nil (interactive)
            (nth 4 (org-heading-components)))
        (defun meq/tangle-path nil (interactive)
            (string-remove-prefix "/" (concat
                (org-format-outline-path (org-get-outline-path)) "/"
                    (meq/get-header))))
        (defun meq/get-theme-from-header nil (interactive)
            (string-remove-suffix "-theme.el" (meq/get-header)))
    ;; :demon ((naked "backtab") 'evil-close-fold)
    :ryo ("o" :hydra
        '(deino-org (:color blue)
                "A deino for org-mode!"
                ("o" org-babel-tangle "tangle")
                ("a" meq/org-babel-tangle-append "tangle append")
                ("f" org-babel-tangle-file "tangle file")
                ("n" meq/narrow-or-widen-dwim "narrow")
                ("s" org-edit-special "org edit special")
                ("q" nil "cancel")))
    :gsetq
        ;; I'm using ox-pandoc
        ;; (org-export-backends '(md gfm latex odt org))
        (org-directory "/tmp")
        (org-roam-directory org-directory)
        (org-descriptive-links t)
        (org-confirm-babel-evaluate nil)
        (org-startup-folded t)
        (org-src-fontify-natively t)
        ;; (org-src-window-setup 'current-window)
        (org-cycle-emulate-tab 'whitestart))
(use-package org-pandoc-import
    :use-package-preconfig (ox-gfm) (ox-pandoc)
    :hook (after-init . org-pandoc-import-transient-mode)
    :straight (org-pandoc-import
        :type git
        :host github
        :repo "tecosaur/org-pandoc-import"
        :files ("*.el" "filters" "preprocessors")))

;; minibuffer
(use-package ivy :hook ((emacs-startup . counsel-mode) (emacs-startup . ivy-mode)))

(use-package helm
    :use-package-postconfig ;; Adapted From: https://github.com/clemera/helm-ido-like-guide
        (helm-swoop)
        (helm-flx)
        (smex)
        (helm-smex)
        (helm-ido-like
            :straight (helm-ido-like
                :type git
                :host github
                :repo "shadowrylander/helm-ido-like-guide"
                :branch "master")
            :hook after-init
            :after
                (helm-swoop
                helm-flx
                helm-fuzzier
                helm-smex
                smex
                dash)))

;; TODO: Split this into multiple `use-package!' instances using my new `deino+' keyword
(with-eval-after-load 'ryo-modal (ryo-modal-key "x" :hydra
      '(deino-execute (:color blue)
            "A deino for launching stuff!"
            ("c" counsel-M-x "counsel")
            ("h" helm-smex-major-mode-commands "helm smex major mode")
            ("s" helm-smex "helm smex")
            ("e" execute-extended-command "M-x")
            ("q" nil "cancel"))
            :name "execute order 65"))

;; git
(use-package git-gutter
    :ryo ("g" :hydra
        '(deino-git nil
            "A deino for git!"
            ("`" nil "cancel" :color blue)
            ("j" git-gutter:next-hunk "next")
            ("k" git-gutter:previous-hunk "previous")
            ("d" git-gutter:popup-hunk "diff")
            ("s" git-gutter:stage-hunk "stage")
            ("r" git-gutter:revert-hunk "revert")
            ("m" git-gutter:mark-hunk "mark"))))
(use-package magit
    :ryo ("g" :hydra+
        '(deino-git nil
            "A deino for git!"
            ("g" magit-status "magit" :color blue))))
;; (use-package! gitattributes-mode)

;; buffer
(defun display-startup-echo-area-message nil (meq/which-key-show-top-level))

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;; Adapted From: https://github.com/seagle0128/doom-aiern-modeline#customize
(use-package doom-aiern-modeline
    :straight nil
    :load-path "lib/doom-aiern-modeline"
    :hook (after-init . doom-aiern-modeline-mode)
    :use-package-preconfig
        (all-the-icons)
        (shrink-path)
    :gsetq
        ;; How tall the mode-line should be. It's only respected in GUI.
        ;; If the actual char height is larger, it respects the actual height.
        (doom-aiern-modeline-height 25)

        ;; How wide the mode-line bar should be. It's only respected in GUI.
        (doom-aiern-modeline-bar-width 3)

        ;; The limit of the window width.
        ;; If `window-width' is smaller than the limit, some information won't be displayed.
        (doom-aiern-modeline-window-width-limit fill-column)

        ;; How to detect the project root.
        ;; The default priority of detection is `ffip' > `projectile' > `project'.
        ;; nil means to use `default-directory'.
        ;; The project management packages have some issues on detecting project root.
        ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
        ;; to hanle sub-projects.
        ;; You can specify one if you encounter the issue.
        (doom-aiern-modeline-project-detection 'project)

        ;; Determines the style used by `doom-aiern-modeline-buffer-file-name'.
        ;;
        ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
        ;;   auto => emacs/lisp/comint.el (in a project) or comint.el
        ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
        ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
        ;;   truncate-with-project => emacs/l/comint.el
        ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
        ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
        ;;   truncate-all => ~/P/F/e/l/comint.el
        ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
        ;;   relative-from-project => emacs/lisp/comint.el
        ;;   relative-to-project => lisp/comint.el
        ;;   file-name => comint.el
        ;;   buffer-name => comint.el<2> (uniquify buffer name)
        ;;
        ;; If you are experiencing the laggy issue, especially while editing remote files
        ;; with tramp, please try `file-name' style.
        ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
        (doom-aiern-modeline-buffer-file-name-style 'auto)

        ;; Whether display icons in the mode-line.
        ;; While using the server mode in GUI, should set the value explicitly.
        (doom-aiern-modeline-icon (display-graphic-p))

        ;; Whether display the icon for `major-mode'. It respects `doom-aiern-modeline-icon'.
        (doom-aiern-modeline-major-mode-icon t)

        ;; Whether display the colorful icon for `major-mode'.
        ;; It respects `all-the-icons-color-icons'.
        (doom-aiern-modeline-major-mode-color-icon t)

        ;; Whether display the icon for the buffer state. It respects `doom-aiern-modeline-icon'.
        (doom-aiern-modeline-buffer-state-icon t)

        ;; Whether display the modification icon for the buffer.
        ;; It respects `doom-aiern-modeline-icon' and `doom-aiern-modeline-buffer-state-icon'.
        (doom-aiern-modeline-buffer-modification-icon t)

        ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
        (doom-aiern-modeline-unicode-fallback nil)

        ;; Whether display the minor modes in the mode-line.
        (doom-aiern-modeline-minor-modes nil)

        ;; If non-nil, a word count will be added to the selection-info modeline segment.
        (doom-aiern-modeline-enable-word-count nil)

        ;; Major modes in which to display word count continuously.
        ;; Also applies to any derived modes. Respects `doom-aiern-modeline-enable-word-count'.
        ;; If it brings the sluggish issue, disable `doom-aiern-modeline-enable-word-count' or
        ;; remove the modes from `doom-aiern-modeline-continuous-word-count-modes'.
        (doom-aiern-modeline-continuous-word-count-modes '(
            markdown-mode
            gfm-mode
            org-mode
            outline-mode))

        ;; Whether display the buffer encoding.
        (doom-aiern-modeline-buffer-encoding t)

        ;; Whether display the indentation information.
        (doom-aiern-modeline-indent-info nil)

        ;; If non-nil, only display one number for checker information if applicable.
        (doom-aiern-modeline-checker-simple-format t)

        ;; The maximum number displayed for notifications.
        (doom-aiern-modeline-number-limit 99)

        ;; The maximum displayed length of the branch name of version control.
        (doom-aiern-modeline-vcs-max-length 12)

        ;; Whether display the workspace name. Non-nil to display in the mode-line.
        (doom-aiern-modeline-workspace-name t)

        ;; Whether display the perspective name. Non-nil to display in the mode-line.
        (doom-aiern-modeline-persp-name t)

        ;; If non nil the default perspective name is displayed in the mode-line.
        (doom-aiern-modeline-display-default-persp-name nil)

        ;; If non nil the perspective name is displayed alongside a folder icon.
        (doom-aiern-modeline-persp-icon t)

        ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
        (doom-aiern-modeline-lsp t)

        ;; Whether display the GitHub notifications. It requires `ghub' package.
        (doom-aiern-modeline-github nil)

        ;; The interval of checking GitHub.
        (doom-aiern-modeline-github-interval (* 30 60))

        ;; Whether display the modal state icon.
        ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
        (doom-aiern-modeline-modal-icon t)

        ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
        (doom-aiern-modeline-mu4e nil)

        ;; Whether display the gnus notifications.
        (doom-aiern-modeline-gnus t)

        ;; Wheter gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
        (doom-aiern-modeline-gnus-timer 2)

        ;; Wheter groups should be excludede when gnus automatically being updated.
        (doom-aiern-modeline-gnus-excluded-groups '("dummy.group"))

        ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
        (doom-aiern-modeline-irc t)

        ;; Function to stylize the irc buffer names.
        (doom-aiern-modeline-irc-stylize 'identity)

        ;; Whether display the environment version.
        (doom-aiern-modeline-env-version t)
        ;; Or for individual languages
        (doom-aiern-modeline-env-enable-python t)
        (doom-aiern-modeline-env-enable-ruby t)
        (doom-aiern-modeline-env-enable-perl t)
        (doom-aiern-modeline-env-enable-go t)
        (doom-aiern-modeline-env-enable-elixir t)
        (doom-aiern-modeline-env-enable-rust t)

        ;; Change the executables to use for the language version string
        (doom-aiern-modeline-env-python-executable "python") ; or `python-shell-interpreter'
        (doom-aiern-modeline-env-ruby-executable "ruby")
        (doom-aiern-modeline-env-perl-executable "perl")
        (doom-aiern-modeline-env-go-executable "go")
        (doom-aiern-modeline-env-elixir-executable "iex")
        (doom-aiern-modeline-env-rust-executable "rustc")

        ;; What to dispaly as the version while a new one is being loaded
        (doom-aiern-modeline-env-load-string "...")

        ;; Hooks that run before/after the modeline version string is updated
        (doom-aiern-modeline-before-update-env-hook nil)
        (doom-aiern-modeline-after-update-env-hook nil))

(use-package writeroom-mode
    :hook emacs-startup
    :demon ((alloy-chord "zz") 'writeroom-mode)
    :gsetq
        (writeroom-fullscreen-effect t)
        (writeroom-fringes-outside-margins t)
        (writeroom-width 0.75)
        (writeroom-mode-line t))

(use-package focus
    :hook (emacs-startup . focus-mode)
    :gsetq
        (focus-mode-to-thing '(
            ;; (prog-mode . defun)
            (prog-mode . line)
            ;; (text-mode . sentence)
            (text-mode . line)
            (outline-mode . line))))

(use-package yankpad
    :use-package-preconfig
        (projectile)
        (yasnippet)
        (company)
    :init/defun* (meq/yankpad-hercules-toggle nil (interactive))
    :gsetq (yankpad-file "./yankpad.org")
    :demon
        ((alloy-chord "[[") 'meq/yankpad-hercules-toggle)
        ((alloy-chord "]]") 'yankpad-expand)
    :config (yankpad-map)
    :hercules
        (:show-funs #'meq/yankpad-hercules-show
            :hide-funs #'meq/yankpad-hercules-hide
            :toggle-funs #'meq/yankpad-hercules-toggle
            :keymap 'yankpad-keymap
            ;; :transient t
        ))

(use-package vlf
    :straight (vlf :type git :host github :repo "m00natic/vlfi" :branch "master")
    :demand t
    :gsetq (vlf-application 'always))

;; !!! THE ORDER HERE MATTERS! !!!
;; (add-hook 'emacs-startup '(lambda nil (interactive)
;;     (use-package oneonone
;;         :demand t
;;         :load-emacs-file-preconfig
;;             ("fit-frame")
;;             ("autofit-frame")
;;             ;; ("buff-menu+")
;;             ("compile-")
;;             ("compile+")
;;             ("grep+")
;;             ("dired+")
;;             ("dired-details")
;;             ("dired-details+")
;;             ("doremi")
;;             ("hexrgb")
;;             ("frame-fns")
;;             ("faces+")
;;             ("doremi-frm")
;;             ("eyedropper")
;;             ("facemenu+")
;;             ("frame+")
;;             ("help+")
;;             ("info+")
;;             ("menu-bar+")
;;             ("mouse+")
;;             ("setup-keys")
;;             ("strings")
;;             ;; ("simple+")
;;             ("frame-cmds")
;;             ("thumb-frm")
;;             ("window+")
;;             ("zoom-frm")
;;             ("oneonone")
;;         :gsetq
;;             (1on1-minibuffer-frame-width 10000)
;;             (1on1-minibuffer-frame-height 10000))))

;; terminal
;; (use-package term
;;     :config/defun* (meq/term-hercules-toggle nil (interactive))
;;     :alloy
;;         (:keymaps 'term-mode-map
;;             "C-c C-c" 'term-interrupt-subjob
;;             "C-m"     'term-send-raw
;;             "C-S-c"   'term-interrupt-subjob
;;             "M-,"     'term-send-input
;;             "M-b"     'term-send-backward-word
;;             "M-d"     'term-send-forward-kill-word
;;             "M-DEL"   'term-send-backward-kill-word
;;             "M-f"     'term-send-forward-word
;;             "M-o"     'term-send-backspace)
;;     :gsetq
;;         (term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-l" "<ESC>"))
;;         (term-buffer-maximum-size 16384)
;;         (term-default-bg-color "#000000") '(term-default-fg-color "#AAAAAA")
;;     :hercules
;;         (:show-funs #'meq/term-hercules-show
;;         :hide-funs #'meq/term-hercules-hide
;;         :toggle-funs #'meq/term-hercules-toggle
;;         :keymap 'term-raw-map
;;         ;; :transient t
;;         ))

;; (ansi-term-color-vector [unspecified "white" "red" "green" "yellow" "royal blue" "magenta" "cyan" "white"] t)
;; (ansi-color-names-vector [unspecified "white" "red" "green" "yellow" "royal blue" "magenta" "cyan" "white"] t)
(load-emacs-file "fringe")
;; (with-eval-after-load 'fringe-mode (fringe-mode (quote (1 . 1)) nil (fringe)))
(with-eval-after-load 'fringe-mode (fringe-mode 'none))

;; (use-package vterm
;;     :gsetq
;;         (vterm-shell "/usr/bin/env xonsh")
;;         (vterm-always-compile-module t)
;;         (vterm-kill-buffer-on-exit t))

(use-package multi-term
    :gsetq
        (multi-term-program "/usr/bin/env xonsh")
        (multi-term-scroll-show-maximum-output t))

;; NOTE: Not working
(use-package emux
    ;; :straight (emux :type git :host github :repo "re5et/emux" :branch "master")
    :load-emacs-file-preconfig
        ("emux/emux-base")
        ("emux/emux-term")
        ("emux/emux-screen")
        ("emux/emux-session")
    :config/defun*
        (meq/make-frame nil (interactive) (modify-frame-parameters (make-frame) ((name . "emux"))))
        (meq/select-emux nil (interactive) (select-frame-by-name "emux"))
    :config (emux-completing-read-command (quote ido-completing-read))
    :demon
        ;; (""          'meq/make-frame)
        ;; (""          'meq/select-emux)
        ("C-x c"     'emux-term-create)
        ("C-x P"     'emux-session-load-template)
    :alloy
        (:keymaps 'term-mode-map
            "C-S-p"     'previous-line
            "C-S-r"     'isearch-backward
            "C-S-s"     'isearch-forward
            "C-S-y"     'emux-term-yank
            "C-x -"     'emux-term-vsplit
            "C-x |"     'emux-term-hsplit
            "C-x B"     'emux-jump-to-buffer
            "C-x C-S-k" 'emux-session-destroy
            "C-x C"     'emux-screen-create
            "C-x c"     'emux-term-create
            "C-x K"     'emux-term-destroy
            "C-x M-s"   'emux-jump-to-screen
            "C-x P"     'emux-session-load-template
            "C-x R"     'emux-screen-rename
            "C-x r"     'emux-term-rename
            "C-x s"     'emux-screen-switch
            "C-x S"     'emux-session-switch
            "M-."       'comint-dynamic-complete)
    :hercules
        (:show-funs #'meq/emux-hercules-show
        :hide-funs #'meq/emux-hercules-hide
        :toggle-funs #'meq/emux-hercules-toggle
        :keymap 'term-mode-map
        ;; :transient t
        ))

;; (use-package! elscreen
;;     :straight (elscreen :type git :host github :repo "knu/elscreen" :branch "master")
;;     :gsetq
;;         ;; NOTE: Remember to escape the backslash
;;         (elscreen-prefix-key "C-S-\\")
;;     :config/defun* (meq/elscreen-hercules-toggle nil (interactive))
;;     :hercules
;;         (:show-funs #'meq/elscreen-hercules-show
;;         :hide-funs #'meq/elscreen-hercules-hide
;;         :toggle-funs #'meq/elscreen-hercules-toggle
;;         :keymap 'elscreen-map
;;         ;; :transient t
;;         ))

(use-package escreen
    :load-emacs-file-preconfig ("escreen")
    :init/defun* (meq/escreen-hercules-toggle nil (interactive))
    :demon
        ;; ((naked "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)
        ;; ((naked "M-[") 'dim:escreen-goto-prev-screen)
        ;; ((naked "M-]") 'dim:escreen-goto-next-screen)
        ;; ((naked "C-\\ DEL") 'dim:escreen-goto-prev-screen)
        ;; ((naked "C-\\ SPC") 'dim:escreen-goto-next-screen)

        ;; ([s-mouse-4] 'dim:escreen-goto-prev-screen)
        ;; ([s-mouse-5] 'dim:escreen-goto-next-screen)
        ((alloy-chord "||") 'meq/escreen-hercules-toggle)
    ;; :alloy
        ;; (:keymaps 'escreen-map
            ;; escreen-prefix-char 'dim:escreen-goto-last-screen)
        ;; (:keymaps 'term-raw-map
            ;; add support for C-\ from terms
            ;; escreen-prefix-char escreen-map

            ;; (naked "M-[") 'dim:escreen-goto-prev-screen
            ;; (naked "M-]") 'dim:escreen-goto-next-screen)
    :config/defun*
        ;; add C-\ l to list screens with emphase for current one
        (escreen-get-active-screen-numbers-with-emphasis nil
            "what the name says"
            (interactive)
                (let ((escreens (escreen-get-active-screen-numbers))
                    (emphased ""))

                    (dolist (s escreens)
                        (setq emphased
                            (concat emphased (if (= escreen-current-screen-number s)
                                (propertize (number-to-string s)
                                    ;;'face 'custom-variable-tag) " ")
                                    'face 'info-title-3)
                                    ;;'face 'font-lock-warning-face)
                                    ;;'face 'secondary-selection)
                                (number-to-string s))
                            " ")))
                    (message "escreen: active screens: %s" emphased)))

        (dim:escreen-goto-last-screen nil (interactive)
            (escreen-goto-last-screen)
            (escreen-get-active-screen-numbers-with-emphasis))

        (dim:escreen-goto-prev-screen (&optional n) (interactive "p")
            (escreen-goto-prev-screen n)
            (escreen-get-active-screen-numbers-with-emphasis))

        (dim:escreen-goto-next-screen (&optional n) (interactive "p")
            (escreen-goto-next-screen n)
            (escreen-get-active-screen-numbers-with-emphasis))
    :config
        (require 'term)
    :hercules
        (:show-funs #'meq/escreen-hercules-show
        :hide-funs #'meq/escreen-hercules-hide
        :toggle-funs #'meq/escreen-hercules-toggle
        :keymap 'escreen-map
        ;; :transient t
        ))


;; window manager
(when (member "--exwm" command-line-args) (use-package exwm :demand t
    :config
        (require 'exwm-config)
        (exwm-config-default)
        ;; (exwm-enable)
        ))

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


(when (> (length command-line-args) 2) (find-file (car (last command-line-args))))
