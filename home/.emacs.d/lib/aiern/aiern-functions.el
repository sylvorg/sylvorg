;;; aiern-functions.el --- Random functions -*- lexical-binding: t -*-

;; Author: Jeet Ray <aiern@protonmail.com>
;; Maintainer: Jeet Ray <aiern@protonmail.com>

;; Version: 1.14.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of aiern.
;;
;; aiern is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; aiern is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with aiern.  If not, see <http://www.gnu.org/licenses/>.

(require 'aiern-core)
(require 'hercules)
(require 'naked)

(defvar modal-modes nil)
(defvar modal-prefixes (mapcar (lambda (mode) (interactive) (car (split-string (symbol-name mode) "-"))) modal-modes))
(defvar last-modal-mode nil)
(defvar current-modal-mode nil)
(defvar dont-disable-modal-modes nil)
(defvar all-keymaps-map nil)
(defvar last-global-map nil)
(defvar which-key-really-dont-show t)

;; Adapted From:
;; Answer: https://stackoverflow.com/a/10088995/10827766
;; User: https://stackoverflow.com/users/324105/phils
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun fbatp (mode) (interactive)
    (let* ((is-it-bound (boundp mode)))
        (when is-it-bound (and (or (boundp (symbol-value mode))) (or (fboundp mode) (functionp mode))) mode)))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/26840/31428
;; User: https://emacs.stackexchange.com/users/253/dan
;; Adapted From: https://emacsredux.com/blog/2020/06/14/checking-the-major-mode-in-emacs-lisp/
(with-eval-after-load 'org
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
    (with-eval-after-load 'evil (advice-add #'evil-close-fold :before #'aiern/go-to-parent))
    (advice-add #'aiern-close-fold :before #'aiern/go-to-parent)

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
        (org-defkey org-mode-map (naked "C-c C-v +") 'aiern/org-babel-tangle-append))

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

    (defun aiern/src-mode-settings nil (interactive)
        (aiern/disable-all-modal-modes) (when (featurep 'focus) (focus-mode 1)))
    (defun aiern/src-mode-exit nil (interactive)
        (when (featurep 'winner-mode) (winner-undo)) (aiern/disable-all-modal-modes))

    ;; Adapted From: https://github.com/syl20bnr/spacemacs/issues/13058#issuecomment-565741009
    (advice-add #'org-edit-src-exit :after #'aiern/src-mode-exit)
    (advice-add #'org-edit-src-abort :after #'aiern/src-mode-exit)
    (advice-add #'org-edit-special :after #'aiern/src-mode-settings)
    (advice-add #'org-babel-tangle-collect-blocks :override #'aiern/org-babel-tangle-collect-blocks-handle-tangle-list)
    (advice-add #'org-babel-tangle-single-block :around #'aiern/org-babel-tangle-single-block-handle-tangle-list)
    (add-hook 'org-mode-hook 'aiern/org-babel-tangle-append-setup)
    (add-hook 'org-cycle-hook '(lambda (state) (interactive) (when (eq state 'children) (setq org-cycle-subtree-status 'subtree))))

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
            (aiern/src-mode-settings)))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/42240
;; User: user12563
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
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

;; Answer: https://stackoverflow.com/a/14490054/10827766
;; User: https://stackoverflow.com/users/1600898/user4815162342
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/keymap-symbol (keymap)
    "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
    (interactive)
    (catch 'gotit
        (mapatoms (lambda (sym)
            (and (boundp sym)
                (eq (symbol-value sym) keymap)
                (not (eq sym 'keymap))
                (throw 'gotit sym))))))

;; Adapted From:
;; Answer: https://superuser.com/a/331662/1154755
;; User: https://superuser.com/users/656734/phimuemue
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/end-of-line-and-indented-new-line nil (interactive)
    (end-of-line)
    (newline-and-indent))

(defun aiern/any-popup-showing-p nil (interactive)
    (or hercules--popup-showing-p (which-key--popup-showing-p)))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/questions/12997/how-do-i-use-nadvice/14827#14827
;; User: https://emacs.stackexchange.com/users/2308/kdb
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/which-key--hide-popup (&optional force) (interactive)
        (when force (setq which-key-really-dont-show t))
        (unless dont-disable-modal-modes
            (setq dont-disable-modal-modes nil)
            (aiern/disable-all-modal-modes))
        (setq which-key-persistent-popup nil)
        (which-key--hide-popup)
        (which-key-mode -1))

;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/which-key--show-popup (&optional keymap force) (interactive)
    (let ((show-popup #'(lambda (keymap) (interactive)
            (aiern/disable-all-modal-modes keymap)
            (which-key-mode 1)
            (setq which-key-persistent-popup t))))
        (if which-key-really-dont-show
            (when force (setq which-key-really-dont-show nil) (funcall show-popup keymap))
            (funcall show-popup keymap))))

(mapc #'(lambda (state) (interactive)
    (add-hook (intern (concat "aiern-" (symbol-name (car state)) "-state-entry-hook"))
        #'(lambda nil (interactive)
            (aiern/which-key--show-popup (intern (concat "aiern-" (symbol-name (car state)) "-state-map")))))
    (add-hook (intern (concat "aiern-" (symbol-name (car state)) "-state-exit-hook"))
        #'(lambda nil (interactive)
            (setq dont-disable-modal-modes t)
            (aiern/which-key--hide-popup)))
    (add-hook (intern (concat "evil-" (symbol-name (car state)) "-state-entry-hook"))
        #'(lambda nil (interactive)
            (aiern/which-key--show-popup (intern (concat "evil-" (symbol-name (car state)) "-state-map")))))
    (add-hook (intern (concat "evil-" (symbol-name (car state)) "-state-exit-hook"))
        #'(lambda nil (interactive)
            (setq dont-disable-modal-modes t)
            (aiern/which-key--hide-popup))))
    aiern-state-properties)

;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/which-key--refresh-popup (&optional keymap) (interactive)
    (aiern/which-key--hide-popup t)
    (aiern/which-key--show-popup keymap t))

;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/toggle-which-key (&optional keymap) (interactive)
    (if (aiern/any-popup-showing-p)
        (aiern/which-key--hide-popup t)
        (aiern/which-key--show-popup keymap t)))

;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/hercules--hide-advice (&optional keymap flatten &rest _)
        "Dismiss hercules.el.
    Pop KEYMAP from `overriding-terminal-local-map' when it is not
    nil.  If FLATTEN is t, `hercules--show' was called with the same
    argument.  Restore `which-key--update' after such a call."
        (interactive)
        (setq hercules--popup-showing-p nil)
        (setq overriding-terminal-local-map nil)
        (when flatten (advice-remove #'which-key--update #'ignore))
        (aiern/which-key-show-top-level))
(advice-add #'hercules--hide :override #'aiern/hercules--hide-advice)

;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/hercules--show-advice (&optional keymap flatten transient &rest _)
    "Summon hercules.el showing KEYMAP.
    Push KEYMAP onto `overriding-terminal-local-map' when TRANSIENT
    is nil.  Otherwise use `set-transient-map'.  If FLATTEN is t,
    show full keymap \(including sub-maps\), and prevent redrawing on
    prefix-key press by overriding `which-key--update'."
    (interactive)
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

;;;###autoload (autoload 'aiern-mode "aiern" nil t)
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
            (when (or (member current-modal-mode modal-modes) keymap)
                (funcall which-key-function) (setq current-modal-mode nil))
            (funcall which-key-function))))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/42240
;; User: user12563
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/hercules-hide-all-modal-modes (&optional keymap) (interactive)
    (when overriding-terminal-local-map (mapc #'(lambda (prefix) (interactive)
        (message (format "Hiding %s" prefix))
        (ignore-errors (funcall (intern (concat "aiern/" prefix "-hercules-hide"))))
        ;; (internal-push-keymap 'global-map 'overriding-terminal-local-map)
        ;; (internal-push-keymap nil 'overriding-terminal-local-map)
        (setq overriding-terminal-local-map nil)) modal-prefixes))
    (aiern/which-key-show-top-level keymap))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/14956/31428
;; User: https://emacs.stackexchange.com/users/25/gilles-so-stop-being-evil
;; (with-eval-after-load 'evil (defun aiern/newline-and-indent-advice (func &rest arguments)
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/newline-and-indent-advice (func &rest arguments)
    (if (window-minibuffer-p)
        (cond
            ((evil-ex-p) (evil-ex-execute (minibuffer-contents)))
            ((aiern-ex-p) (aiern-ex-execute (minibuffer-contents)))
            (t (progn (minibuffer-complete-and-exit) (minibuffer-complete-and-exit))))
        (apply func arguments)))
        ;; )

;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/pre-post-command-hook-command nil (interactive)
    (if (window-minibuffer-p)
        (tag-def :keymaps 'override (naked "RET") nil)
        (tag-def :keymaps 'override (naked "RET") 'newline-and-indent)))
(add-hook 'pre-command-hook 'aiern/pre-post-command-hook-command)
(add-hook 'post-command-hook 'aiern/pre-post-command-hook-command)

;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/evil-ex-advice (func &rest arguments)
    (setq dont-disable-modal-modes t)
    (aiern/which-key--hide-popup)
    (setq last-global-map (current-global-map))
    (use-global-map global-map)

    (apply func arguments)

    (use-global-map last-global-map)
    (setq last-global-map nil)
    (aiern/which-key--show-popup))
(advice-add #'aiern-ex :around #'aiern/evil-ex-advice)
(with-eval-after-load 'evil (advice-add #'evil-ex :around #'aiern/evil-ex-advice))

;; From:
;; Answer: https://stackoverflow.com/questions/24832699/emacs-24-untabify-on-save-for-everything-except-makefiles
;; User: https://stackoverflow.com/users/2677392/ryan-m
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/untabify-everything nil (untabify (point-min) (point-max)))

;; Adapted From:
;; Answer: https://stackoverflow.com/a/24857101/10827766
;; User: https://stackoverflow.com/users/936762/dan
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/untabify-except-makefiles nil
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (aiern/untabify-everything)))

(add-hook 'before-save-hook 'aiern/untabify-except-makefiles)

;; Adapted From: https://github.com/emacsorphanage/god-mode/blob/master/god-mode.el#L454
;;;###autoload (autoload 'aiern-mode "aiern" nil t)
(defun aiern/god-prefix-command-p nil
  "Return non-nil if the current command is a \"prefix\" command.
This includes prefix arguments and any other command that should
be ignored by `god-execute-with-current-bindings'."
  (memq this-command '((when (featurep 'god-mode) god-mode-self-insert)
                       digit-argument
                       negative-argument
                       universal-argument
                       universal-argument-more)))

(with-eval-after-load 'evil (defun aiern/both-ex-define-cmd (cmd function) (interactive)
    (evil-ex-define-cmd cmd function)
    (aiern-ex-define-cmd cmd function)))

(with-eval-after-load 'counsel (advice-add #'counsel-M-x :before #'aiern/which-key--hide-popup))
(with-eval-after-load 'helm
    (advice-add #'helm-smex-major-mode-commands :before #'aiern/which-key--hide-popup)
    (advice-add #'helm-smex :before #'aiern/which-key--hide-popup))

;; TODO
;; (advice-add #'execute-extended-command :before #'aiern/which-key--hide-popup)

(advice-add #'keyboard-escape-quit :after #'aiern/which-key--show-popup)
(advice-add #'keyboard-quit :after #'aiern/which-key--show-popup)
(advice-add #'exit-minibuffer :after #'aiern/which-key--show-popup)

(add-hook 'after-init-hook 'key-chord-mode)

(provide 'aiern-functions)

;;; aiern-functions.el ends here