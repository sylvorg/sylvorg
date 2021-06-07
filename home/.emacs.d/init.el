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

(defun load-emacs-file (path) (interactive)
    (load-file (concat user-emacs-directory path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar modal-modes '())
(defvar modal-prefixes (mapcar (lambda (mode) (interactive) (car (split-string (symbol-name mode) "-"))) modal-modes))
(defvar last-modal-mode nil)
(defvar all-keymaps-map nil)
(defvar novel-map-p nil)

;; Adapted From:
;; Answer: https://stackoverflow.com/a/10088995/10827766
;; User: https://stackoverflow.com/users/324105/phils
(defun fbatp (mode) (interactive)
    (let* ((is-it-bound (boundp mode)))
        (when is-it-bound (and (or (boundp (symbol-value mode))) (or (fboundp mode) (functionp mode))) mode)))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/26840/31428
;; User: https://emacs.stackexchange.com/users/253/dan
;; Adapted From: https://emacsredux.com/blog/2020/06/14/checking-the-major-mode-in-emacs-lisp/
(defun aiern/outline-folded-p nil
    "Returns non-nil if point is on a folded headline or plain list
    item."
    (interactive)
    (and (if (eq major-mode 'org-mode)
            (or (org-at-heading-p)
                (org-at-item-p))
            outline-on-heading-p)
        (invisible-p (point-at-eol))))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/37791/31428
;; User: https://emacs.stackexchange.com/users/12497/toothrot
(defun aiern/go-to-parent nil (interactive)
    (outline-up-heading (if (and (or (org-at-heading-p) (invisible-p (point))) (invisible-p (point-at-eol))
            (>= (org-current-level) 2))
        1 0)))

(load-emacs-file "help+20.el")
(setq confirm-kill-emacs nil)

;; From: https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (and (fbatp 'native-comp-available-p) (native-comp-available-p))
    (message "Native compilation is available")
    (message "Native complation is *not* available"))
(if (fbatp 'json-serialize)
    (message "Native JSON is available")
    (message "Native JSON is *not* available"))

;; Adapted From:
;; From: https://emacs.stackexchange.com/a/19507
;; User: https://emacs.stackexchange.com/users/50/malabarba
;; (setq byte-compile-warnings (not t))
;; (setq byte-compile warnings (not obsolete))

;; From: https://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
(setq initial-major-mode 'org-mode)

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

;; Answer: https://emacs.stackexchange.com/a/51829
;; User: https://emacs.stackexchange.com/users/2370/tobias
(defun aiern/set-buffer-save-without-query nil
    "Set `buffer-save-without-query' to t."
    (unless (variable-binding-locus 'buffer-save-without-query)
        (setq buffer-save-without-query t)))

;; (add-hook #'find-file-hook #'aiern/set-buffer-save-without-query)

;; The following avoids being ask to allow the file local
;; setting of `buffer-save-without-query'.
;; IMHO it is not a big risk:
;; The malicious code that must not be saved
;; should never be allowed to enter Emacs in the first place.
;; (put 'buffer-save-without-query 'safe-local-variable #'booleanp)

(setq user-full-name "Jeet Ray"
      user-mail-address "aiern@protonmail.com")

(setq custom-safe-themes t)
(load-theme 'dracula-purple-dark)

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
(use-package command-log-mode :straight t)
(load-emacs-file "naked.el")
(use-package general
    :demand t
    :config
        (general-auto-unbind-keys)
        (general-def :keymaps '(
            minibuffer-local-keymap
            counsel-describe-map
            helm-buffer-map) "M-x" 'exit-minibuffer)

        ;; Answer: https://stackoverflow.com/a/14490054/10827766
        ;; User: https://stackoverflow.com/users/1600898/user4815162342
        (defun aiern/keymap-symbol (keymap)
            "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
            (catch 'gotit
                (mapatoms (lambda (sym)
                    (and (boundp sym)
                        (eq (symbol-value sym) keymap)
                        (not (eq sym 'keymap))
                        (throw 'gotit sym))))))

        ;; Adapted From:
        ;; Answer: https://emacs.stackexchange.com/a/3883/31428
        ;; User: https://emacs.stackexchange.com/users/2454/alexander-shukaev
        (defun aiern/enable-novel-map nil (interactive)
            (use-global-map (make-sparse-keymap))

            (global-set-key [t] #'self-insert-command)
            (let ((c ?\s))
            (while (< c ?\d)
                (global-set-key (vector c) #'self-insert-command)
                (setq c (1+ c)))
            (when (eq system-type 'ms-dos)
                (setq c 128)
                (while (< c 160)
                (global-set-key (vector c) #'self-insert-command)
                (setq c (1+ c))))
            (setq c 160)
            (while (< c 256)
                (global-set-key (vector c) #'self-insert-command)
                (setq c (1+ c))))

            (global-set-key (kbd "C-x C-c") 'evil-quit)

            ;; Adapted From:
            ;; Answer: https://superuser.com/a/331662/1154755
            ;; User: https://superuser.com/users/656734/phimuemue
            (defun aiern/end-of-line-and-indented-new-line nil (interactive)
                (end-of-line)
                (newline-and-indent))

            ;; TODO: Add delete, backspace, etc.
            (general-def :keymaps 'override
                ;; NOTE: When I couldn't find this, I used `command-log-mode':
                ;; http://ergoemacs.org/emacs/emacs_show_key_and_command.html
                (naked "deletechar") 'delete-char

                ;; NOTE: This is actually backspace...
                (naked "DEL") 'delete-backward-char

                (naked "up") 'previous-line
                (naked "down") 'next-line
                (naked "left") 'backward-char
                (naked "right") 'forward-char
                (general-chord "uu") 'aiern/toggle-novel-map
                (naked "RET") 'newline-and-indent
                (general-chord "./") 'aiern/end-of-line-and-indented-new-line)

            (setq novel-map-p t)
            (aiern/update-global-mode-string))

        (general-def :keymaps 'override
            (general-chord "uu") 'aiern/toggle-novel-map
            (naked "RET") 'newline-and-indent
            (general-chord "./") 'aiern/end-of-line-and-indented-new-line)

        (defun aiern/disable-novel-map nil (interactive)
            (use-global-map global-map)
            (setq novel-map-p nil)
            (aiern/update-global-mode-string))
        (defun aiern/toggle-novel-map nil (interactive)
            (funcall (intern (concat "aiern/" (if novel-map-p "disable" "enable") "-novel-map"))))
    :custom
        (general-implicit-kbd t)
    :straight t)

;; modal-modes

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/42240
;; User: user12563
(defun aiern/disable-all-modal-modes (&optional keymap) (interactive)
    (mapc
        (lambda (mode-symbol)
            ;; some symbols are functions which aren't normal mode functions
            (when (fbatp mode-symbol)
                (message (format "Disabling %s" (symbol-name mode-symbol)))
                (ignore-errors
                    (funcall mode-symbol -1))))
            modal-modes)
    (aiern/hercules-hide-all-modal-modes keymap))

;; Adapted From: https://gitlab.com/jjzmajic/hercules.el/-/blob/master/hercules.el#L83
(defun aiern/toggle-inner (mode prefix mode-on map &optional use-hercules force) (interactive)
    (aiern/disable-all-modal-modes)
    (if mode-on
        (when force (aiern/which-key--hide-popup))
        (when force (aiern/which-key--show-popup))
        (funcall mode 1)
        (if use-hercules (ignore-errors (funcall (intern (concat "aiern/" prefix "-hercules-show"))))
            (aiern/which-key-show-top-level map))
        (setq last-modal-mode prefix)))

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
    :config
        (defun aiern/any-popup-showing-p nil (interactive) (or hercules--popup-showing-p (which-key--popup-showing-p)))
        
        ;; Adapted From:
        ;; Answer: https://emacs.stackexchange.com/questions/12997/how-do-i-use-nadvice/14827#14827
        ;; User: https://emacs.stackexchange.com/users/2308/kdb
        (defun aiern/which-key--hide-popup nil (interactive)
            (aiern/disable-all-modal-modes)
            (setq which-key-persistent-popup nil) (which-key--hide-popup)
            (which-key-mode -1))

        (defun aiern/which-key--show-popup (&optional keymap force) (interactive)
            (when (or (not which-key-persistent-popup) force)
                (aiern/disable-all-modal-modes keymap)
                (which-key-mode 1)
                (setq which-key-persistent-popup t)))

        (defun aiern/which-key--refresh-popup (&optional keymap) (interactive)
            (aiern/which-key--hide-popup)
            (aiern/which-key--show-popup keymap t))

        (defun aiern/toggle-which-key (&optional keymap) (interactive)

            ;; TODO: How does this bit work again...?
            (setq which-key-persistent-popup (not (aiern/any-popup-showing-p)))

            (if (aiern/any-popup-showing-p)
                (aiern/which-key--hide-popup)
                (aiern/which-key--show-popup keymap t)))

        (defun aiern/hercules--hide-advice (&optional keymap flatten &rest _)
                "Dismiss hercules.el.
            Pop KEYMAP from `overriding-terminal-local-map' when it is not
            nil.  If FLATTEN is t, `hercules--show' was called with the same
            argument.  Restore `which-key--update' after such a call."
                (setq hercules--popup-showing-p nil)
                (setq overriding-terminal-local-map nil)
                (when flatten (advice-remove #'which-key--update #'ignore))
                (aiern/which-key-show-top-level))
        (advice-add #'hercules--hide :override #'aiern/hercules--hide-advice)

        
        (defun aiern/hercules--show-advice (&optional keymap flatten transient &rest _)
        "Summon hercules.el showing KEYMAP.
        Push KEYMAP onto `overriding-terminal-local-map' when TRANSIENT
        is nil.  Otherwise use `set-transient-map'.  If FLATTEN is t,
        show full keymap \(including sub-maps\), and prevent redrawing on
        prefix-key press by overriding `which-key--update'."
        (when which-key-persistent-popup
            (setq hercules--popup-showing-p t)
            (when keymap
                (let ((which-key-show-prefix hercules-show-prefix))
                (if flatten
                    (progn
                        (which-key--show-keymap
                        (symbol-name keymap) (symbol-value keymap) nil t t)
                        (advice-add #'which-key--update :override #'ignore))
                    (which-key--show-keymap
                    (symbol-name keymap) (symbol-value keymap) nil nil t)))
                (if transient
                    (set-transient-map (symbol-value keymap)
                                    t #'hercules--hide)
                (internal-push-keymap (symbol-value keymap)
                                        'overriding-terminal-local-map)))))
        (advice-add #'hercules--show :override #'aiern/hercules--show-advice)

        (defun aiern/which-key-show-top-level (&optional keymap) (interactive)
            (let* ((current-map (or (symbol-value keymap) (or overriding-terminal-local-map global-map)))
                (which-key-function
                    ;; #'which-key-show-top-level
                    ;; #'(lambda nil (interactive) (which-key-show-full-keymap 'global-map))
                    ;; #'which-key-show-full-major-mode
                    ;; #'which-key-show-major-mode

                    ;; Adapted From:
                    ;; https://github.com/justbur/emacs-which-key/blob/master/which-key.el#L2359
                    ;; https://github.com/justbur/emacs-which-key/blob/master/which-key.el#L2666
                    #'(lambda nil (interactive)
                        (when which-key-persistent-popup (which-key--create-buffer-and-show nil current-map nil "Current bindings")))))
                (if (which-key--popup-showing-p)
                    (when (or (member last-modal-mode modal-prefixes) keymap)
                        (funcall which-key-function) (setq last-modal-mode nil))
                    (funcall which-key-function))))

        ;; Adapted From:
        ;; Answer: https://emacs.stackexchange.com/a/42240
        ;; User: user12563
        (defun aiern/hercules-hide-all-modal-modes (&optional keymap) (interactive)
            (when overriding-terminal-local-map (mapc #'(lambda (prefix) (interactive)
                (message (format "Hiding %s" prefix))
                (ignore-errors (funcall (intern (concat "aiern/" prefix "-hercules-hide"))))
                ;; (internal-push-keymap 'global-map 'overriding-terminal-local-map)
                ;; (internal-push-keymap nil 'overriding-terminal-local-map)
                (setq overriding-terminal-local-map nil)) modal-prefixes))
            (aiern/which-key-show-top-level keymap))
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
        (defun aiern/toggle-ryo-force nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "ryo" (fbatp ryo-modal-mode) 'ryo-modal-mode-map nil t))
        (defun aiern/toggle-ryo-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "ryo" (fbatp ryo-modal-mode) 'ryo-modal-mode-map t))
        (defun aiern/toggle-ryo-hercules-force nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "ryo" (fbatp ryo-modal-mode) 'ryo-modal-mode-map t t))
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
        (general-chord "KK") 'aiern/toggle-evil-force
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
        (defun aiern/toggle-evil-force nil (interactive)
            (funcall 'aiern/toggle-inner 'evil-mode "evil" (fbatp evil-mode) 'evil-normal-state-map nil t))
        (defun aiern/toggle-evil-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'evil-mode "evil" (fbatp evil-mode) 'evil-normal-state-map t))
        (defun aiern/toggle-evil-hercules-force nil (interactive)
            (funcall 'aiern/toggle-inner 'evil-mode "evil" (fbatp evil-mode) 'evil-normal-state-map t t))
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

        (advice-add #'evil-close-fold :before #'aiern/go-to-parent)
        (advice-add #'evil-insert-state :override #'aiern/disable-all-modal-modes)

        (defun aiern/evil-ex-advice (func &rest arguments) (aiern/which-key--hide-popup) (apply func arguments) (aiern/which-key--show-popup))
        (advice-add #'evil-ex :around #'aiern/evil-ex-advice)
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
        (defun aiern/toggle-god-force nil (interactive)
            (funcall 'aiern/toggle-inner 'god-local-mode "god" (fbatp god-local-mode) 'global-map nil t))
        (defun aiern/toggle-god-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'god-local-mode "god" (fbatp god-local-mode) 'global-map t))
        (defun aiern/toggle-god-hercules-force nil (interactive)
            (funcall 'aiern/toggle-inner 'god-local-mode "god" (fbatp god-local-mode) 'global-map t t))
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
        (defun aiern/toggle-xah-force nil (interactive)
            (funcall 'aiern/toggle-inner 'xah-fly-keys "xah" (fbatp xah-fly-keys) 'xah-fly-command-map nil t))
        (defun aiern/toggle-xah-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'xah-fly-keys "xah" (fbatp xah-fly-keys) 'xah-fly-command-map t))
        (defun aiern/toggle-xah-hercules-force nil (interactive)
            (funcall 'aiern/toggle-inner 'xah-fly-keys "xah" (fbatp xah-fly-keys) 'xah-fly-command-map t t)))

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
        (defun aiern/toggle-objed-force nil (interactive)
            (funcall 'aiern/toggle-inner 'objed-mode "objed" (fbatp objed-mode) 'objed-map nil t))
        (defun aiern/toggle-objed-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'objed-mode "objed" (fbatp objed-mode) 'objed-map t))
        (defun aiern/toggle-objed-hercules-force nil (interactive)
            (funcall 'aiern/toggle-inner 'objed-mode "objed" (fbatp objed-mode) 'objed-map t t)))

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
        (defun aiern/toggle-kakoune-force nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "kakoune" (fbatp ryo-modal-mode) 'ryo-modal-mode-map nil t))
        (defun aiern/toggle-kakoune-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "kakoune" (fbatp ryo-modal-mode) 'ryo-modal-mode-map t))
        (defun aiern/toggle-kakoune-hercules-force nil (interactive)
            (funcall 'aiern/toggle-inner 'ryo-modal-mode "kakoune" (fbatp ryo-modal-mode) 'ryo-modal-mode-map t t)))

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
        (defun aiern/toggle-modalka-force nil (interactive)
            (funcall 'aiern/toggle-inner 'modalka-mode "modalka" (fbatp modalka-mode) 'modalka-mode-map nil t))
        (defun aiern/toggle-modalka-hercules nil (interactive)
            (funcall 'aiern/toggle-inner 'modalka-mode "modalka" (fbatp modalka-mode) 'modalka-mode-map t))
        (defun aiern/toggle-modalka-hercules-force nil (interactive)
            (funcall 'aiern/toggle-inner 'modalka-mode "modalka" (fbatp modalka-mode) 'modalka-mode-map t t)))

;; org-mode
(use-package org
    :straight t
    :hook (
        (kill-emacs-hook . org-babel-tangle)
        (org-mode-hook . aiern/org-babel-tangle-append-setup))
    :init
        ;; I'm using ox-pandoc
        ;; (setq org-export-backends '(md gfm latex odt org))
        (setq org-directory "/tmp")
        (setq org-roam-directory org-directory)
    :config
        (use-package nix-mode
            :demand t
            :straight t
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

        (use-package xonsh-mode
            :demand t
            :straight (xonsh-mode :type git :host github :repo "seanfarley/xonsh-mode" :branch "master")
            :commands (org-babel-execute:xonsh org-babel-expand-body:xonsh)
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

        (use-package dockerfile-mode
            :demand t
            :straight t
            :mode ("\\Dockerfile\\'"))

        (use-package vimrc-mode
            :demand t
            :straight (vimrc-mode :type git :host github :repo "mcandre/vimrc-mode" :branch "master")
            :commands
                (org-babel-execute:vimrc
                org-babel-expand-body:vimrc)
            :mode "\\.vim\\(rc\\)?\\'")

        (org-babel-do-load-languages 'org-babel-load-languages
            (append org-babel-load-languages
            '((python . t)
            (shell . t))))

        (org-babel-lob-ingest "./README.org")

        ;; Adapted From: https://www.reddit.com/r/emacs/comments/6klewl/org_cyclingto_go_from_folded_to_children_skipping/djniygy?utm_source=share&utm_medium=web2x&context=3
        (defun aiern/org-cycle nil (interactive)
            (if (aiern/outline-folded-p) (org-cycle) (evil-close-fold)))

        ;; Adapted From:
        ;; Answer: https://emacs.stackexchange.com/questions/28098/how-to-change-org-mode-babel-tangle-write-to-file-way-as-append-instead-of-overr/38898#38898
        ;; User: https://emacs.stackexchange.com/users/2370/tobias
        (defun aiern/org-babel-tangle-append nil
            "Append source code block at point to its tangle file.
            The command works like `org-babel-tangle' with prefix arg
            but `delete-file' is ignored."
            (interactive)
            (cl-letf (((symbol-function 'delete-file) #'ignore))
                (org-babel-tangle '(4))))

        ;; Adapted From:
        ;; Answer: https://emacs.stackexchange.com/questions/28098/how-to-change-org-mode-babel-tangle-write-to-file-way-as-append-instead-of-overr/38898#38898
        ;; User: https://emacs.stackexchange.com/users/2370/tobias
        (defun aiern/org-babel-tangle-append-setup nil
            "Add key-binding C-c C-v C-t for `aiern/org-babel-tangle-append'."
            (org-defkey org-mode-map (kbd "C-c C-v +") 'aiern/org-babel-tangle-append))

        
        ;; Adapted From:
        ;; Answer: https://emacs.stackexchange.com/questions/39032/tangle-the-same-src-block-to-different-files/39039#39039
        ;; User: https://emacs.stackexchange.com/users/2370/tobias
        (defun aiern/org-babel-tangle-collect-blocks-handle-tangle-list (&optional language tangle-file)
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

        ;; Adapted From:
        ;; Answer: https://emacs.stackexchange.com/questions/39032/tangle-the-same-src-block-to-different-files/39039#39039
        ;; User: https://emacs.stackexchange.com/users/2370/tobias
        (defun aiern/org-babel-tangle-single-block-handle-tangle-list (oldfun block-counter &optional only-this-block)
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
        
        (defun aiern/src-mode-settings nil (interactive) (aiern/disable-all-modal-modes) (focus-mode 1))
        (defun aiern/src-mode-exit nil (interactive) (winner-undo) (aiern/disable-all-modal-modes))

        ;; Adapted From: http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
        (defun aiern/narrow-or-widen-dwim (p)
        "Widen if buffer is narrowed, narrow-dwim otherwise.
        Dwim means: region, org-src-block, org-subtree, or
        defun, whichever applies first. Narrowing to
        org-src-block actually calls `org-edit-src-code'.

        With prefix P, don't widen, just narrow even if buffer
        is already narrowed."
        (interactive "P")
        (declare (interactive-only))
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
            (aiern/src-mode-settings))

        ;; Adapted From: https://github.com/syl20bnr/spacemacs/issues/13058#issuecomment-565741009
        (advice-add #'org-edit-src-exit :after #'aiern/src-mode-exit)
        (advice-add #'org-edit-src-abort :after #'aiern/src-mode-exit)
        (advice-add #'org-edit-special :after #'aiern/src-mode-settings)
        (advice-add #'org-babel-tangle-collect-blocks :override #'aiern/org-babel-tangle-collect-blocks-handle-tangle-list)
        (advice-add #'org-babel-tangle-single-block :around #'aiern/org-babel-tangle-single-block-handle-tangle-list)
        (advice-add #'org-cycle :after #'(lambda (state) (interactive) (when (eq state 'children) (setq org-cycle-subtree-status 'subtree))))
        ;; (add-hook 'org-cycle-hook (lambda (state) (interactive) (when (eq state 'children) (setq org-cycle-subtree-status 'subtree))))

        (defun aiern/get-header nil (interactive)
            (nth 4 (org-heading-components)))
        (defun aiern/tangle-path nil (interactive)
            (org-babel-lob-ingest "./README.org")
            (string-remove-prefix "/" (concat
                (org-format-outline-path (org-get-outline-path)) "/"
                    (aiern/get-header))))
        (defun aiern/tangle-oreo nil (interactive)
            (org-babel-lob-ingest "./strange.aiern.org")
            (aiern/tangle-path))
        (defun aiern/get-theme-from-header nil (interactive)
            (string-remove-suffix "-theme.el" (aiern/get-header)))
    ;; :general
        ;; (:keymaps 'override
        ;;     (naked "backtab") 'aiern/evil-close-fold)
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

(advice-add #'counsel-M-x :before #'aiern/which-key--hide-popup)
(advice-add #'helm-smex-major-mode-commands :before #'aiern/which-key--hide-popup)
(advice-add #'helm-smex :before #'aiern/which-key--hide-popup)

;; TODO
;; (advice-add #'execute-extended-command :before #'aiern/which-key--hide-popup)

(advice-add #'doom-escape :after #'aiern/which-key--show-popup)
(advice-add #'keyboard-escape-quit :after #'aiern/which-key--show-popup)
(advice-add #'keyboard-quit :after #'aiern/which-key--show-popup)
(advice-add #'exit-minibuffer :after #'aiern/which-key--show-popup)

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
(defun display-startup-echo-area-message nil (aiern/which-key-show-top-level))

(use-package rainbow-delimiters
    :straight t
    :hook (prog-mode-hook . rainbow-delimiters-mode))

;; From:
;; Answer: https://stackoverflow.com/questions/24832699/emacs-24-untabify-on-save-for-everything-except-makefiles
;; User: https://stackoverflow.com/users/2677392/ryan-m
(defun aiern/untabify-everything nil (untabify (point-min) (point-max)))

;; From:
;; Answer: https://stackoverflow.com/questions/24832699/emacs-24-untabify-on-save-for-everything-except-makefiles
;; User: https://stackoverflow.com/users/2677392/ryan-m
(defun aiern/untabify-everything-on-save nil (add-hook 'before-save-hook 'untabify-everything) nil)

;; Adapted From:
;; Answer: https://stackoverflow.com/a/24857101/10827766
;; User: https://stackoverflow.com/users/936762/dan
(defun aiern/untabify-except-makefiles nil
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (aiern/untabify-everything)))

(add-hook 'before-save-hook 'aiern/untabify-except-makefiles)

;; Adapted From: https://github.com/seagle0128/doom-modeline#customize
(use-package doom-modeline
    :straight t
    :hook (after-init . doom-modeline-mode)
    :init
        (defun aiern/update-global-mode-string nil (interactive)
            (setq global-mode-string
                (list
                "%n "
                "Novel Keymap: "
                (if novel-map-p "True" "False"))))
        (aiern/update-global-mode-string)
    :custom
        ;; How tall the mode-line should be. It's only respected in GUI.
        ;; If the actual char height is larger, it respects the actual height.
        (doom-modeline-height 25)

        ;; How wide the mode-line bar should be. It's only respected in GUI.
        (doom-modeline-bar-width 3)

        ;; The limit of the window width.
        ;; If `window-width' is smaller than the limit, some information won't be displayed.
        (doom-modeline-window-width-limit fill-column)

        ;; How to detect the project root.
        ;; The default priority of detection is `ffip' > `projectile' > `project'.
        ;; nil means to use `default-directory'.
        ;; The project management packages have some issues on detecting project root.
        ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
        ;; to hanle sub-projects.
        ;; You can specify one if you encounter the issue.
        (doom-modeline-project-detection 'project)

        ;; Determines the style used by `doom-modeline-buffer-file-name'.
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
        (doom-modeline-buffer-file-name-style 'auto)

        ;; Whether display icons in the mode-line.
        ;; While using the server mode in GUI, should set the value explicitly.
        (doom-modeline-icon (display-graphic-p))

        ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
        (doom-modeline-major-mode-icon t)

        ;; Whether display the colorful icon for `major-mode'.
        ;; It respects `all-the-icons-color-icons'.
        (doom-modeline-major-mode-color-icon t)

        ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
        (doom-modeline-buffer-state-icon t)

        ;; Whether display the modification icon for the buffer.
        ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
        (doom-modeline-buffer-modification-icon t)

        ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
        (doom-modeline-unicode-fallback nil)

        ;; Whether display the minor modes in the mode-line.
        (doom-modeline-minor-modes nil)

        ;; If non-nil, a word count will be added to the selection-info modeline segment.
        (doom-modeline-enable-word-count nil)

        ;; Major modes in which to display word count continuously.
        ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
        ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
        ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
        (doom-modeline-continuous-word-count-modes '(
            markdown-mode
            gfm-mode
            org-mode
            outline-mode))

        ;; Whether display the buffer encoding.
        (doom-modeline-buffer-encoding t)

        ;; Whether display the indentation information.
        (doom-modeline-indent-info nil)

        ;; If non-nil, only display one number for checker information if applicable.
        (doom-modeline-checker-simple-format t)

        ;; The maximum number displayed for notifications.
        (doom-modeline-number-limit 99)

        ;; The maximum displayed length of the branch name of version control.
        (doom-modeline-vcs-max-length 12)

        ;; Whether display the workspace name. Non-nil to display in the mode-line.
        (doom-modeline-workspace-name t)

        ;; Whether display the perspective name. Non-nil to display in the mode-line.
        (doom-modeline-persp-name t)

        ;; If non nil the default perspective name is displayed in the mode-line.
        (doom-modeline-display-default-persp-name nil)

        ;; If non nil the perspective name is displayed alongside a folder icon.
        (doom-modeline-persp-icon t)

        ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
        (doom-modeline-lsp t)

        ;; Whether display the GitHub notifications. It requires `ghub' package.
        (doom-modeline-github nil)

        ;; The interval of checking GitHub.
        (doom-modeline-github-interval (* 30 60))

        ;; Whether display the modal state icon.
        ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
        (doom-modeline-modal-icon t)

        ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
        (doom-modeline-mu4e nil)

        ;; Whether display the gnus notifications.
        (doom-modeline-gnus t)

        ;; Wheter gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
        (doom-modeline-gnus-timer 2)

        ;; Wheter groups should be excludede when gnus automatically being updated.
        (doom-modeline-gnus-excluded-groups '("dummy.group"))

        ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
        (doom-modeline-irc t)

        ;; Function to stylize the irc buffer names.
        (doom-modeline-irc-stylize 'identity)

        ;; Whether display the environment version.
        (doom-modeline-env-version t)
        ;; Or for individual languages
        (doom-modeline-env-enable-python t)
        (doom-modeline-env-enable-ruby t)
        (doom-modeline-env-enable-perl t)
        (doom-modeline-env-enable-go t)
        (doom-modeline-env-enable-elixir t)
        (doom-modeline-env-enable-rust t)

        ;; Change the executables to use for the language version string
        (doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
        (doom-modeline-env-ruby-executable "ruby")
        (doom-modeline-env-perl-executable "perl")
        (doom-modeline-env-go-executable "go")
        (doom-modeline-env-elixir-executable "iex")
        (doom-modeline-env-rust-executable "rustc")

        ;; What to dispaly as the version while a new one is being loaded
        (doom-modeline-env-load-string "...")

        ;; Hooks that run before/after the modeline version string is updated
        (doom-modeline-before-update-env-hook nil)
        (doom-modeline-after-update-env-hook nil))

(use-package writeroom-mode
    :straight t
    :hook emacs-startup
    :general (:keymaps 'override (general-chord "zz") 'writeroom-mode)
    :custom
        (writeroom-fullscreen-effect t)
        (writeroom-fringes-outside-margins t)
        (writeroom-width 0.75)
        (writeroom-mode-line t))

(use-package focus
    :straight t
    :hook (emacs-startup . focus-mode)
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
;; (add-hook 'emacs-startup '(lambda nil (interactive)
;;     (load-emacs-file "fit-frame.el")
;;     (load-emacs-file "autofit-frame.el")
;;     ;; (load-emacs-file "buff-menu+.el")
;;     (load-emacs-file "compile-.el")
;;     (load-emacs-file "compile+.el")
;;     (load-emacs-file "grep+.el")
;;     (load-emacs-file "dired+.el")
;;     (load-emacs-file "dired-details.el")
;;     (load-emacs-file "dired-details+.el")
;;     (load-emacs-file "doremi.el")
;;     (load-emacs-file "hexrgb.el")
;;     (load-emacs-file "frame-fns.el")
;;     (load-emacs-file "faces+.el")
;;     (load-emacs-file "doremi-frm.el")
;;     (load-emacs-file "eyedropper.el")
;;     (load-emacs-file "facemenu+.el")
;;     (load-emacs-file "frame+.el")
;;     (load-emacs-file "help+.el")
;;     (load-emacs-file "info+.el")
;;     (load-emacs-file "menu-bar+.el")
;;     (load-emacs-file "mouse+.el")
;;     (load-emacs-file "setup-keys.el")
;;     (load-emacs-file "strings.el")
;;     ;; (load-emacs-file "simple+.el")
;;     (load-emacs-file "frame-cmds.el")
;;     (load-emacs-file "thumb-frm.el")
;;     (load-emacs-file "window+.el")
;;     (load-emacs-file "zoom-frm.el")
;;     (load-emacs-file "oneonone.el")
;;     (use-package! oneonone
;;         :demand t
;;         :hook (after-init . 1on1-emacs)
;;         :custom
;;             (1on1-minibuffer-frame-width 10000)
;;             (1on1-minibuffer-frame-height 10000))))

;; terminal
;; (use-package term
;;     :straight t
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
(load-emacs-file "fringe.el")
;; (with-eval-after-load 'fringe-mode (fringe-mode (quote (1 . 1)) nil (fringe)))
(with-eval-after-load 'fringe-mode (fringe-mode 'none))

;; (use-package vterm
;;     :straight t
;;     :custom
;;         (vterm-shell "/usr/bin/env xonsh")
;;         (vterm-always-compile-module t)
;;         (vterm-kill-buffer-on-exit t))

(use-package multi-term
    :straight t
    :custom
        (multi-term-program "/usr/bin/env xonsh")
        (multi-term-scroll-show-maximum-output t))

;; NOTE: Not working
(load-emacs-file "emux/emux-base.el")
(load-emacs-file "emux/emux-term.el")
(load-emacs-file "emux/emux-screen.el")
(load-emacs-file "emux/emux-session.el")
(use-package emux
    ;; :straight (emux :type git :host github :repo "re5et/emux" :branch "master")
    ;; :hook (emacs-startup . emux-mode)
    :config
        (emux-completing-read-command (quote ido-completing-read))

        (defun aiern/make-frame nil (interactive) (modify-frame-parameters (make-frame) ((name . "emux"))))
        (defun aiern/select-emux nil (interactive) (select-frame-by-name "emux"))
    :general
        (:keymaps 'override
            ;; ""          'aiern/make-frame
            ;; ""          'aiern/select-emux
            "C-x c"     'emux-term-create
            "C-x P"     'emux-session-load-template)
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
        (:show-funs #'aiern/emux-hercules-show
        :hide-funs #'aiern/emux-hercules-hide
        :toggle-funs #'aiern/emux-hercules-toggle
        :keymap 'term-mode-map
        ;; :transient t
        ))

;; (use-package! elscreen
;;     :straight (elscreen :type git :host github :repo "knu/elscreen" :branch "master")
;;     ;; :hook (after-init . elscreen-start)
;;     ;; :hook (emacs-startup . elscreen-start)
;;     :custom
;;         ;; NOTE: Remember to escape the backslash
;;         (elscreen-prefix-key "C-S-\\")
;;     :hercules
;;         (:show-funs #'aiern/elscreen-hercules-show
;;         :hide-funs #'aiern/elscreen-hercules-hide
;;         :toggle-funs #'aiern/elscreen-hercules-toggle
;;         :keymap 'elscreen-map
;;         ;; :transient t
;;         ))

(load-emacs-file "escreen.el")
(use-package escreen
    ;; :hook (after-init . escreen-install)
    :general
        (:keymaps 'override
            (general-chord "||") 'aiern/escreen-hercules-toggle)
    :config
        (defun aiern/escreen-hercules-toggle nil(interactive))

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
        (:show-funs #'aiern/escreen-hercules-show
        :hide-funs #'aiern/escreen-hercules-hide
        :toggle-funs #'aiern/escreen-hercules-toggle
        :keymap 'escreen-map
        ;; :transient t
        ))


;; window manager
(use-package exwm :straight t)

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
