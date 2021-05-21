;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "help+20")
;; (when (eq (find-font (font-spec :family "all-the-icons")) nil) (all-the-icons-install-fonts))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/26840/31428
;; User: https://emacs.stackexchange.com/users/253/dan
;; Adapted From: https://emacsredux.com/blog/2020/06/14/checking-the-major-mode-in-emacs-lisp/
(defun jr/outline-folded-p nil
    "Returns non-nil if point is on a folded headline or plain list
    item."
    (interactive)
    (and (if (eq major-mode 'org-mode)
            (or (org-at-heading-p)
                (org-at-item-p))
            outline-on-heading-p)
        (invisible-p (point-at-eol))))

;; (setq use-package-always-defer t)

;; From: https://github.com/hartzell/straight.el/commit/882649137f73998d60741c7c8c993c7ebbe0f77a#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5R1649
;; (setq straight-disable-byte-compilation t)

;; Adapted From: https://github.com/jwiegley/use-package#use-package-chords
;; Important: https://github.com/noctuid/general.el/issues/53#issuecomment-307262154
(use-package! use-package-chords
        :demand t
        :hook (after-init . key-chord-mode))
(load! "naked")

(use-package! use-package-hydra
        :demand t
        :load-path "use-package-hydra"
        :custom (hydra-hint-display-type 'lv))
(use-package! use-package-hercules
        :demand t
        :load-path "use-package-hercules")

(use-package! hercules
        :demand t
        :init
            (setq which-key-enable-extended-define-key t)
            (setq which-key-idle-delay 0.1)
            (setq which-key-idle-secondary-delay nil)
        :custom

            ;; NOTE: When using the side window, this doesn't matter, apparently;
            ;; only the hercules transient property does
            (which-key-persistent-popup t)

            (which-key-allow-evil-operators t)

            ;; NOTE: This will cause the which-key maps for the operator states to show up,
            ;; breaking functionality such as `d 13 <arrow-down>', etc.
            ;; (which-key-show-operator-state-maps t)

            ;; TODO: Choose a fun one!
            (which-key-separator " | ")

            (which-key-popup-type 'side-window)
            (which-key-side-window-location '(right bottom left top))

            ;; If this percentage is too small, the keybindings frame will appear at the bottom
            (which-key-side-window-max-width 0.5)

            (which-key-side-window-max-height 0.25))

(use-package! ryo-modal
        :demand t
        :preface
            ;; (setq modal-modes '(evil-mode))
            (defvar modal-modes '())

            ;; Adapted From:
            ;; Answer: https://emacs.stackexchange.com/a/42240
            ;; User: user12563
            (defun jr/disable-all-modal-modes nil
                (interactive)
                (mapc
                    (lambda (mode-symbol)
                        (when (functionp mode-symbol)
                        ;; some symbols are functions which aren't normal mode functions
                            (ignore-errors 
                                (funcall mode-symbol -1))))
                        modal-modes))

            ;; Answer: https://stackoverflow.com/a/14490054/10827766
            ;; User: https://stackoverflow.com/users/1600898/user4815162342
            (defun jr/keymap-symbol (keymap)
                "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
                (catch 'gotit
                        (mapatoms (lambda (sym)
                            (and (boundp sym)
                                    (eq (symbol-value sym) keymap)
                                    (not (eq sym 'keymap))
                                    (throw 'gotit sym))))))

            ;; Adapted From: https://gitlab.com/jjzmajic/hercules.el/-/blob/master/hercules.el#L83
            (defun jr/toggle-inner (mode prefix mode-on map) (interactive)
                (unless mode-on (setq evil-mode-on (bound-and-true-p evil-mode)))
                (funcall (intern (concat "jr/" prefix "-hercules-hide")))
                (jr/evil-show)
                (if mode-on
                    (unless evil-mode-on (jr/evil-hide))
                    (funcall mode 1)
                    ;; (internal-push-keymap (symbol-value map) 'overriding-terminal-local-map)
                    (funcall (intern (concat "jr/" prefix "-hercules-show")))))

        :general
            (:keymaps 'override
                  (general-chord "  ") 'jr/toggle-ryo
                  "M-w M-w" 'jr/ryo-hercules-toggle)
        :hercules
            (:show-funs #'jr/ryo-hercules-show
            :hide-funs #'jr/ryo-hercules-hide
            :toggle-funs #'jr/ryo-hercules-toggle
            :keymap 'ryo-modal-mode-map
            ;; :transient t
            )
        :config
            (defun jr/ryo-hercules-toggle nil (interactive))
            (add-to-list 'modal-modes 'ryo-modal-mode)
        
            (defun jr/toggle-ryo nil (interactive)
                (funcall 'jr/toggle-inner 'ryo-modal-mode "ryo" (bound-and-true-p ryo-modal-mode) 'ryo-modal-mode-map))

            ;; From: https://github.com/Kungsgeten/ryo-modal#which-key-integration
            (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

(use-package! evil
        :hook
            (doom-init-ui . which-key-show-top-level)
        :init
            ;; From:
            ;; Answer: https://stackoverflow.com/questions/25542097/emacs-evil-mode-how-to-change-insert-state-to-emacs-state-automatically/56206909#56206909
            ;; User: https://stackoverflow.com/users/1259257/mshohayeb
            ;; (setq evil-disable-insert-state-bindings t)

            (setq-default evil-escape-key-sequence nil)

            (defun jr/evil-hercules-toggle nil (interactive))
            (defun jr/evil-quit nil (interactive) (funcall (general-simulate-key ":q! <RET>")))
            (defun jr/evil-show nil (interactive)
                (jr/disable-all-modal-modes)
                (evil-mode 1)
                (evil-normal-state)
                (which-key-show-top-level))
            (defun jr/evil-hide (&rest args) (interactive)
                (evil-mode -1)
                (which-key-show-top-level))
            (defun jr/evil-toggle nil (interactive)
                (if
                    (bound-and-true-p evil-mode)
                    (jr/evil-hide)
                    (jr/evil-show)))
            ;; (setq evil-want-keybinding nil)
            ;; (setq evil-want-integration t)
        :general
            (:keymaps 'override

                ;; Used on Android
                "∅" 'evil-save-and-close
                "æ" 'jr/evil-quit

                (general-chord ";'") 'evil-execute-in-emacs-state

                ;; NOTE: Will not toggle if hercules is transient
                (naked "escape") 'jr/evil-toggle
                (general-chord "kk") 'jr/evil-toggle)
        :config
            (advice-add #'evil-insert-state :override #'jr/evil-hide)

            (use-package! god-mode
                    :general
                        (:keymaps 'override
                              (general-chord "jj") 'jr/toggle-god
                              (general-chord "';") 'god-execute-with-current-bindings
            
                              ;; NOTE: Will not toggle if hercules is transient
                              "C-;" 'jr/god-hercules-toggle)
                    :hook
                        (doom-first-buffer-hook . (lambda nil (interactive)
                            (god-mode -1)
                            (god-local-mode -1)))
                    :hercules
                        (:show-funs #'jr/god-hercules-show
                        :hide-funs #'jr/god-hercules-hide
                        :toggle-funs #'jr/god-hercules-toggle
                        :keymap 'global-map
                        ;; :transient t
                        )
                    :config
                        (defun jr/god-hercules-toggle nil (interactive))
                        (add-to-list 'modal-modes 'god-local-mode)
                    
                        (defun jr/toggle-god nil (interactive)
                            (funcall 'jr/toggle-inner 'god-local-mode "god" (bound-and-true-p god-local-mode) 'global-map))
                        (which-key-enable-god-mode-support))

            (use-package! xah-fly-keys
                    :general
                        (:keymaps 'xah-fly-command-map
                            ";" 'jr/xah-hercules-toggle)
                    :ryo
                        ("m" :hydra
                              '(modal-modes nil
                                    "A modal hydra!"
                                    ("x" jr/toggle-xah "xah-fly-keys")
                                    ("`" nil "cancel" :color blue)) :name "modal modes")
                    :hercules
                        (:show-funs #'jr/xah-hercules-show
                        :hide-funs #'jr/xah-hercules-hide
                        :toggle-funs #'jr/xah-hercules-toggle
                        :keymap 'xah-fly-command-map
                        ;; :transient t
                        )
                    :config
                        (defun jr/xah-hercules-toggle nil (interactive))
                        (add-to-list 'modal-modes 'xah-fly-keys)
                    
                        (defun jr/toggle-xah nil (interactive)
                            (funcall 'jr/toggle-inner 'xah-fly-keys "xah" (bound-and-true-p xah-fly-keys) 'xah-fly-command-map)))

            (use-package! objed
                    :general
                        (:keymaps 'override
                            (general-chord "ii") 'jr/toggle-objed)
                        (:keymaps 'objed-map
                            ";" 'jr/objed-hercules-toggle)
                    :hook
                        (after-init . (lambda nil (interactive)
                            (objed-mode -1)))
                    :hercules
                        (:show-funs #'jr/objed-hercules-show
                        :hide-funs #'jr/objed-hercules-hide
                        :toggle-funs #'jr/objed-hercules-toggle
                        :keymap 'objed-map
                        ;; :transient t
                        )
                    :config
                        (defun jr/objed-hercules-toggle nil (interactive))
                        (add-to-list 'modal-modes 'objed-mode)
                    
                        (defun jr/toggle-objed nil (interactive)
                            (funcall 'jr/toggle-inner 'objed-mode "objed" (bound-and-true-p objed-mode) 'objed-map)))

            ;; Use `ryo-modal'
            (use-package! kakoune)

            ;; Assign to `xah-fly-keys' mode's previous general keybinding, i.e. `[['
            (use-package! modalka)

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
                  '(evil-exits (:color blue)
                        ;; From: https://gist.github.com/shadowrylander/46b81297d1d3edfbf1e2d72d5e29171e
                        "A hydra for getting the fuck outta' here!"
                        ("l" evil-save-and-quit ":wq")
                        ("p" evil-quit ":q")
                        ("o" evil-write ":w")
                        ("O" evil-write-all ":wa")
                        ("q" (funcall (general-simulate-key ":q! <RET>")) ":q!")
                        ("`" nil "cancel"))
                    ;; :first '(evil-normal-state)
                    :name "evil exits")
            (";" jr/ryo-hercules-toggle :name "toggle keymap"))

;; Adapted From: https://github.com/mohsenil85/evil-evilified-state and https://github.com/syl20bnr/spacemacs
(use-package! evil-evilified-state :after evil)

(use-package! org
        :init
            ;; I'm using ox-pandoc
            ;; (setq org-export-backends '(md gfm latex odt org))
            (setq org-directory "/tmp")
            (setq org-roam-directory org-directory)
        :hook
            ((org-mode . 'jr/org-babel-tangle-append-setup)
            ;; (kill-emacs . 'org-babel-tangle)

            ;; Adapted From: https://www.reddit.com/r/emacs/comments/6klewl/org_cyclingto_go_from_folded_to_children_skipping/djniygy?utm_source=share&utm_medium=web2x&context=3
            (org-cycle . (lambda (state) (interactive) (when (eq state 'children) (setq org-cycle-subtree-status 'subtree)))))
        :config
            (org-babel-lob-ingest "/README.org")

            (defun jr/org-babel-tangle-append nil
              "Append source code block at point to its tangle file.
            The command works like `org-babel-tangle' with prefix arg
            but `delete-file' is ignored."
              (interactive)
              (cl-letf (((symbol-function 'delete-file) #'ignore))
                (org-babel-tangle '(4))))
            
            (defun jr/org-babel-tangle-append-setup nil
              "Add key-binding C-c C-v C-t for `jr/org-babel-tangle-append'."
              (org-defkey org-mode-map (kbd "C-c C-v +") 'jr/org-babel-tangle-append))

            (defun org-babel-tangle-collect-blocks-handle-tangle-list (&optional language tangle-file)
              "Can be used as :override advice for `org-babel-tangle-collect-blocks'.
            Handles lists of :tangle files."
              (let ((counter 0) last-heading-pos blocks)
                (org-babel-map-src-blocks (buffer-file-name)
                  (let ((current-heading-pos
                     (org-with-wide-buffer
                      (org-with-limited-levels (outline-previous-heading)))))
                (if (eq last-heading-pos current-heading-pos) (cl-incf counter)
                  (setq counter 1)
                  (setq last-heading-pos current-heading-pos)))
                  (unless (org-in-commented-heading-p)
                (let* ((info (org-babel-get-src-block-info 'light))
                       (src-lang (nth 0 info))
                       (src-tfiles (cdr (assq :tangle (nth 2 info))))) ; Tobias: accept list for :tangle
                  (unless (consp src-tfiles) ; Tobias: unify handling of strings and lists for :tangle
                    (setq src-tfiles (list src-tfiles))) ; Tobias: unify handling
                  (dolist (src-tfile src-tfiles) ; Tobias: iterate over list
                    (unless (or (string= src-tfile "no")
                        (and tangle-file (not (equal tangle-file src-tfile)))
                        (and language (not (string= language src-lang))))
                      ;; Add the spec for this block to blocks under its
                      ;; language.
                      (let ((by-lang (assoc src-lang blocks))
                        (block (org-babel-tangle-single-block counter)))
                    (setcdr (assoc :tangle (nth 4 block)) src-tfile) ; Tobias: 
                    (if by-lang (setcdr by-lang (cons block (cdr by-lang)))
                      (push (cons src-lang (list block)) blocks)))))))) ; Tobias: just ()
                ;; Ensure blocks are in the correct order.
                (mapcar (lambda (b) (cons (car b) (nreverse (cdr b)))) blocks)))
            
            (defun org-babel-tangle-single-block-handle-tangle-list (oldfun block-counter &optional only-this-block)
              "Can be used as :around advice for `org-babel-tangle-single-block'.
            If the :tangle header arg is a list of files. Handle all files"
              (let* ((info (org-babel-get-src-block-info))
                 (params (nth 2 info))
                 (tfiles (cdr (assoc :tangle params))))
                (if (null (and only-this-block (consp tfiles)))
                (funcall oldfun block-counter only-this-block)
                  (cl-assert (listp tfiles) nil
                     ":tangle only allows a tangle file name or a list of tangle file names")
                  (let ((ret (mapcar
                      (lambda (tfile)
                        (let (old-get-info)
                          (cl-letf* (((symbol-function 'old-get-info) (symbol-function 'org-babel-get-src-block-info))
                             ((symbol-function 'org-babel-get-src-block-info)
                              `(lambda (&rest get-info-args)
                                 (let* ((info (apply 'old-get-info get-info-args))
                                    (params (nth 2 info))
                                    (tfile-cons (assoc :tangle params)))
                                   (setcdr tfile-cons ,tfile)
                                   info))))
                        (funcall oldfun block-counter only-this-block))))
                      tfiles)))
                (if only-this-block
                    (list (cons (cl-caaar ret) (mapcar #'cadar ret)))
                  ret)))))
            
            (advice-add 'org-babel-tangle-collect-blocks :override #'org-babel-tangle-collect-blocks-handle-tangle-list)
            (advice-add 'org-babel-tangle-single-block :around #'org-babel-tangle-single-block-handle-tangle-list)

            (use-package! nix-mode
                :commands (org-babel-execute:nix)
                :mode ("\\.nix\\'")
                :config
                    ;; Adapted From:
                    ;; Answer: https://emacs.stackexchange.com/a/61442
                    ;; User: https://emacs.stackexchange.com/users/20061/zeta
                    (defun org-babel-execute:nix (body params)
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
            
            (use-package! xonsh-mode
                :commands
                    (org-babel-execute:xonsh
                    org-babel-expand-body:xonsh)
                :mode ("\\.xonshrc\\'" "\\.xsh\\'")
                :config
                    ;; Adapted From:
                    ;; Answer: https://emacs.stackexchange.com/a/61442
                    ;; User: https://emacs.stackexchange.com/users/20061/zeta
                    (defun org-babel-execute:xonsh (body params)
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
            
            (use-package! dockerfile-mode
                :config
                    (org-babel-do-load-languages 'org-babel-load-languages
                        (append org-babel-load-languages
                            '((Dockerfile . t))))
                :mode ("\\Dockerfile\\'"))
            
            (use-package! vimrc-mode
                :commands
                    (org-babel-execute:vimrc
                    org-babel-expand-body:vimrc)
                :mode "\\.vim\\(rc\\)?\\'")

            ;; Adapted From:
            ;; Answer: https://emacs.stackexchange.com/a/37791/31428
            ;; User: https://emacs.stackexchange.com/users/12497/toothrot
            (defun jr/go-to-parent nil (interactive)
                (outline-up-heading (if (and (or (org-at-heading-p) (invisible-p (point))) (invisible-p (point-at-eol))
                        (>= (org-current-level) 2))
                    1 0)))

            (defun jr/evil-close-fold nil (interactive) (jr/go-to-parent) (evil-close-fold))

            (defun jr/org-cycle nil (interactive)
                (if (jr/outline-folded-p) (org-cycle) (jr/evil-close-fold)))

            (advice-add #'org-edit-special :before #'jr/src-mode-entry)
            (advice-add #'org-edit-special :after #'jr/src-mode-settings)

            (defun jr/get-header nil (interactive)
                (nth 4 (org-heading-components)))
            (defun jr/tangle-path nil (interactive)
                (string-remove-prefix "/" (concat
                    (org-format-outline-path (org-get-outline-path)) "/"
                        (jr/get-header))))
            (defun jr/tangle-oreo nil (interactive)
                (org-babel-lob-ingest "./strange.aiern.org")
                (jr/tangle-path))
            (defun jr/get-theme-from-header nil (interactive)
                (string-remove-suffix "-theme.el" (jr/get-header)))
        :general
            (:keymaps 'org-roam-mode-map
                  "C-c n" '(:ignore t :which-key "Org-Roam")
                  "C-c n l" 'org-roam
                  "C-c n f" 'org-roam-find-file
                  "C-c n g" 'org-roam-graph)
            (:keymaps 'org-mode-map
                  "C-c n i" 'org-roam-insert
                  "C-c n I" 'org-roam-insert-immediate)
            (:keymaps 'override
                (naked "backtab") 'jr/evil-close-fold)
        :ryo ("o" :hydra
            '(hydra-org (:color blue)
                  "A hydra for org-mode!"
                  ("o" org-babel-tangle "tangle")
                  ("a" jr/org-babel-tangle-append "tangle append")
                  ("f" org-babel-tangle-file "tangle file")
                  ("n" jr/narrow-or-widen-dwim "narrow")
                  ("s" org-edit-special "org edit special")
                  ("q" nil "cancel")))
        :custom
            (org-descriptive-links t)
            (org-confirm-babel-evaluate nil)
            (org-startup-folded t)
            (org-src-fontify-natively t)
            ;; (org-src-window-setup 'current-window)
            (org-cycle-emulate-tab 'whitestart))

(use-package! org-numbers-overlay
        :load-path "emacs-bankruptcy/site-lisp"
        :hook (org-mode . org-numbers-overlay-mode))



(ryo-modal-key "x" :hydra
      '(hydra-execute nil
            "A hydra for launching stuff!"
            ("c" counsel-M-x "counsel")
            ("h" helm-smex-major-mode-commands "helm smex major mode")
            ("s" helm-smex "helm smex")
            ("e" execute-extended-command "M-x")
            ("q" nil "cancel" :color blue)) :first '(evil-insert-state) :name "execute order 65")

;; TODO: Does this work? Or do I have to pull the hydra out?
(general-def
    :keymaps 'override
    "C-S-p" 'hydra-execute)

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/54921/31428
;; User: https://emacs.stackexchange.com/users/160/jordon-biondo
(general-def minibuffer-local-map "M-x" 'doom-escape)

(use-package! git-gutter
      :ryo ("g" :hydra
            '(hydra-git nil
                  "A hydra for git!"
                  ("g" magit-status "magit" :color blue)
                  ("j" git-gutter:next-hunk "next")
                  ("k" git-gutter:previous-hunk "previous")
                  ("d" git-gutter:popup-hunk "diff")
                  ("s" git-gutter:stage-hunk "stage")
                  ("r" git-gutter:revert-hunk "revert")
                  ("m" git-gutter:mark-hunk "mark")
                  ("q" nil "cancel" :color blue))))
;; (use-package! gitattributes-mode)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(defun jr/src-mode-entry nil (interactive)
  (setq evil-mode-on-before-narrow (bound-and-true-p evil-mode)))

(defun jr/src-mode-settings nil (interactive)
    (jr/disable-all-modal-modes)
    (jr/evil-hide)
    (focus-mode 1)
    (which-key-show-top-level))

(defun jr/src-mode-exit nil (interactive)
    (winner-undo)
    (if evil-mode-on-before-narrow (jr/evil-show) (jr/evil-hide))
    (which-key-show-top-level))

;; Adapted From: http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
;; NOTE: For some reason, I can't advise this function properly with `jr/src-mode-entry' and `-settings'
(defun jr/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (jr/src-mode-entry)
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun)))
    (jr/src-mode-settings))

;; Adapted From: https://github.com/syl20bnr/spacemacs/issues/13058#issuecomment-565741009
(advice-add #'org-edit-src-exit :after #'jr/src-mode-exit)
(advice-add #'org-edit-src-abort :after #'jr/src-mode-exit)

;; (use-package! writeroom-mode
;;     :general (:keymaps 'override (general-chord "zz") 'writeroom-mode)
;;     :custom (writeroom-fullscreen-effect t)
;;     :hook after-init)

(general-def
    :keymaps 'override
    (general-chord "zz") '+zen/toggle-fullscreen)

(use-package! focus
    ;; :hook (doom-init-ui . focus-mode)
    :custom
        (focus-mode-to-thing '(
            ;; (prog-mode . defun)
            (prog-mode . line)
            ;; (text-mode . sentence)
            (text-mode . line)
            (outline-mode . line))))

(use-package! rainbow-delimiters
      :hook ((prog-mode . rainbow-delimiters-mode)

            ;; Add more modes here
            ))

(use-package! parinfer-rust-mode
    :hook emacs-lisp-mode
    :init (setq parinfer-rust-auto-download t)
    :custom (parinfer-rust-check-before-enable nil))

(after! org
    (load! "fit-frame")
    (load! "autofit-frame")
    ;; (load! "buff-menu+")
    (load! "compile-")
    (load! "compile+")
    (load! "grep+")
    (load! "dired+")
    (load! "dired-details")
    (load! "dired-details+")
    (load! "doremi-frm")
    (load! "facemenu+")
    (load! "fname+")
    (load! "help+")
    (load! "info+")
    (load! "menu-bar+")
    (load! "mouse+")
    (load! "setup-keys")
    (load! "simple+")
    (load! "thumb-frm")
    (load! "window+")
    (load! "frame-fns")
    (load! "frame-cmds")
    (load! "zoom-frm")
    (load! "hexrgb")
    (use-package! oneonone
        :load-path "oneonone"
        :hook (after-init . 1on1-emacs)))

;; (use-package! term
;;     :general
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
;;     :custom
;;         (term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-l" "<ESC>"))
;;         (term-buffer-maximum-size 16384)
;;         (term-default-bg-color "#000000") '(term-default-fg-color "#AAAAAA"))

;; (ansi-term-color-vector [unspecified "white" "red" "green" "yellow" "royal blue" "magenta" "cyan" "white"] t)
;; (ansi-color-names-vector [unspecified "white" "red" "green" "yellow" "royal blue" "magenta" "cyan" "white"] t)
;; (fringe-mode (quote (1 . 1)) nil (fringe))

;; (use-package! vterm
;;     :custom
;;         (vterm-shell "/usr/bin/env xonsh")
;;         (vterm-always-compile-module t)
;;         (vterm-kill-buffer-on-exit t))

(use-package! multi-term
    :custom
        (multi-term-program "/usr/bin/env xonsh")
        (multi-term-scroll-show-maximum-output t))

;; NOTE: Not working
;; (use-package! emux-session
;;     :config
;;         (emux-completing-read-command (quote ido-completing-read))
;; 
;;         (defun jr/make-frame nil (interactive) (modify-frame-parameters (make-frame) ((name . "emux"))))
;;         (defun jr/select-emux nil (interactive) (select-frame-by-name "emux"))
;;     :general
;;         (:keymaps 'override
;;             ;; ""          'jr/make-frame
;;             ;; ""          'jr/select-emux
;;             "C-x c"     'emux-term-create
;;             "C-x P"     'emux-session-load-template)
;;         (:keymaps 'term-mode-map
;;             "C-S-p"     'previous-line
;;             "C-S-r"     'isearch-backward
;;             "C-S-s"     'isearch-forward
;;             "C-S-y"     'emux-term-yank
;;             "C-x -"     'emux-term-vsplit
;;             "C-x |"     'emux-term-hsplit
;;             "C-x B"     'emux-jump-to-buffer
;;             "C-x C-S-k" 'emux-session-destroy
;;             "C-x C"     'emux-screen-create
;;             "C-x c"     'emux-term-create
;;             "C-x K"     'emux-term-destroy
;;             "C-x M-s"   'emux-jump-to-screen
;;             "C-x P"     'emux-session-load-template
;;             "C-x R"     'emux-screen-rename
;;             "C-x r"     'emux-term-rename
;;             "C-x s"     'emux-screen-switch
;;             "C-x S"     'emux-session-switch
;;             "M-."       'comint-dynamic-complete
;; 
;;             ";" 'jr/emux-hercules-toggle)
;;     :hercules
;;         (:show-funs #'jr/emux-hercules-show
;;         :hide-funs #'jr/emux-hercules-hide
;;         :toggle-funs #'jr/emux-hercules-toggle
;;         :keymap 'term-mode-map
;;         ;; :transient t
;;         )
;;     ;; :hook (after-init . emux-mode)
;;         )

;; (use-package! elscreen
;;     :hook
;;         ;; (emacs-startup . elscreen-start)
;;         (after-init . elscreen-start)
;;     :custom
;;         ;; NOTE: Remember to escape the backslash
;;         (elscreen-prefix-key "C-S-\\")
;;     :hercules
;;         (:show-funs #'jr/elscreen-hercules-show
;;         :hide-funs #'jr/elscreen-hercules-hide
;;         :toggle-funs #'jr/elscreen-hercules-toggle
;;         :keymap 'elscreen-map
;;         ;; :transient t
;;         ))

(load! "escreen")
(use-package! escreen
    :hook
        (after-init . escreen-install)
    :general
        (:keymaps 'override
            (general-chord "\\\\") 'jr/escreen-hercules-toggle)
    :config
        (defun jr/escreen-hercules-toggle nil(interactive))

        ;; Adapted From: https://tapoueh.org/blog/2009/09/escreen-integration/

        ;; add C-\ l to list screens with emphase for current one
        (defun escreen-get-active-screen-numbers-with-emphasis nil
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

        ;; (global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)

        (defun dim:escreen-goto-last-screen nil (interactive)
            (escreen-goto-last-screen)
            (escreen-get-active-screen-numbers-with-emphasis))

        (defun dim:escreen-goto-prev-screen (&optional n) (interactive "p")
            (escreen-goto-prev-screen n)
            (escreen-get-active-screen-numbers-with-emphasis))

        (defun dim:escreen-goto-next-screen (&optional n) (interactive "p")
            (escreen-goto-next-screen n)
            (escreen-get-active-screen-numbers-with-emphasis))

        ;; (define-key escreen-map escreen-prefix-char 'dim:escreen-goto-last-screen)

        ;; (global-set-key (kbd "M-[") 'dim:escreen-goto-prev-screen)
        ;; (global-set-key (kbd "M-]") 'dim:escreen-goto-next-screen)
        ;; (global-set-key (kbd "C-\\ DEL") 'dim:escreen-goto-prev-screen)
        ;; (global-set-key (kbd "C-\\ SPC") 'dim:escreen-goto-next-screen)

        ;; (global-set-key '[s-mouse-4] 'dim:escreen-goto-prev-screen)
        ;; (global-set-key '[s-mouse-5] 'dim:escreen-goto-next-screen)

        ;; add support for C-\ from terms
        ;; (require 'term)
        ;; (define-key term-raw-map escreen-prefix-char escreen-map)
        ;; (define-key term-raw-map (kbd "M-[") 'dim:escreen-goto-prev-screen)
        ;; (define-key term-raw-map (kbd "M-]") 'dim:escreen-goto-next-screen)
    :hercules
        (:show-funs #'jr/escreen-hercules-show
        :hide-funs #'jr/escreen-hercules-hide
        :toggle-funs #'jr/escreen-hercules-toggle
        :keymap 'escreen-map
        ;; :transient t
        ))




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
(use-package! exec-path-from-shell :demand t)


(setq-default indent-tabs-mode nil)

;; From:
;; Answer: https://stackoverflow.com/questions/24832699/emacs-24-untabify-on-save-for-everything-except-makefiles
;; User: https://stackoverflow.com/users/2677392/ryan-m
(defun untabify-everything ()
  (untabify (point-min) (point-max)))
(defun untabify-everything-on-save ()
  (add-hook 'before-save-hook 'untabify-everything)
  nil)

;; Adapted From:
;; Answer: https://stackoverflow.com/a/24857101/10827766
;; User: https://stackoverflow.com/users/936762/dan
(defun untabify-except-makefiles nil
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify-everything)))

(add-hook 'before-save-hook 'untabify-except-makefiles)

(general-auto-unbind-keys)
(when (featurep! :module spacemacs) (use-package! spacemacs
    :init (remove-hook 'org-load-hook #'+org-init-keybinds-h)
    :hook (doom-init-ui . spacemacs/home)))

;; Answer: https://stackoverflow.com/a/57075163
;; User: https://stackoverflow.com/users/2708138/tobias
(defun jr/eval-after-load-all (my-features form)
    "Run FORM after all MY-FEATURES are loaded.
    See `eval-after-load' for the possible formats of FORM."
    (if (null my-features)
        (if (functionp form)
        (funcall form)
    (eval form))
    (eval-after-load (car my-features)
        `(lambda nil
    (eval-after-load-all
        (quote ,(cdr my-features))
        (quote ,form))))))

;; Adapted From:
;; Answer: https://stackoverflow.com/a/57075163
;; User: https://stackoverflow.com/users/2708138/tobias
;; (defun jr/eval-after-load-some (my-features form)
;;     "Run FORM after all MY-FEATURES are loaded.
;;     See `eval-after-load' for the possible formats of FORM."
;;     (if (any my-features)
;;         (if (functionp form)
;;         (funcall form)
;;     (eval form))
;;     (eval-after-load (car my-features)
;;         `(lambda nil
;;     (eval-after-load-all
;;         (quote ,(cdr my-features))
;;         (quote ,form))))))

;; From: https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation

(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))
(if (functionp 'json-serialize)
  (message "Native JSON is available")
(message "Native JSON is *not* available"))

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
(defun jr/remove-scratch-buffer nil
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'jr/remove-scratch-buffer)

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

;; Answer: https://emacs.stackexchange.com/a/51829
;; User: https://emacs.stackexchange.com/users/2370/tobias
;; (defun jr/set-buffer-save-without-query nil
;;     "Set `buffer-save-without-query' to t."
;;     (unless (variable-binding-locus 'buffer-save-without-query)
;;         (setq buffer-save-without-query t)))

;; (add-hook #'find-file-hook #'jr/set-buffer-save-without-query)

;; The following avoids being ask to allow the file local
;; setting of `buffer-save-without-query'.
;; IMHO it is not a big risk:
;; The malicious code that must not be saved
;; should never be allowed to enter Emacs in the first place.
;; (put 'buffer-save-without-query 'safe-local-variable #'booleanp)

;; (setq doom-theme 'exo-ui-red-dark)
(setq doom-theme 'dracula-orange-dark)
;; (setq doom-theme 'dracula-purple-dark)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme nil)
;; From: https://github.com/hlissner/emacs-doom-themes#common-issues
;; (let ((height (face-attribute 'default :height)))
;;   ;; for all linum/nlinum users
;;   (set-face-attribute 'linum nil :height height)
;;   ;; only for `linum-relative' users:
;;   (set-face-attribute 'linum-relative-current-face nil :height height)
;;   ;; only for `nlinum-relative' users:
;;   ;; (set-face-attribute 'nlinum-relative-current-face nil :height height)
;; )

(setq user-full-name "Jeet Ray"
      user-mail-address "aiern@protonmail.com")

(setq doom-font (font-spec :family "Cartograph CF" :size 15 :weight 'light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; (dump-emacs-portable "~/.emacs.d/garboder")
