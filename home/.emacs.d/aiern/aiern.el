;;; aiern.el --- a bunch of useful functions and advices                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jeet Ray

;; Author: Jeet Ray <aiern@protonmail.com>
;; Keywords: lisp aiern doom
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Credit given where applicable.

;;; Code:
(defvar modal-modes '())
(defvar modal-prefixes (mapcar (lambda (mode) (interactive) (car (split-string (symbol-name mode) "-"))) modal-modes))
(defvar last-modal-mode nil)
(defvar all-keymaps-map nil)
(defvar novel-map-p nil)

;;;###autoload
(defun aiern/outline-folded-p nil
    "Returns non-nil if point is on a folded headline or plain list
    item."
    (interactive)
    (and (if (eq major-mode 'org-mode)
            (or (org-at-heading-p)
                (org-at-item-p))
            outline-on-heading-p)
        (invisible-p (point-at-eol))))

;;;###autoload
(defun aiern/set-buffer-save-without-query nil
    "Set `buffer-save-without-query' to t."
    (unless (variable-binding-locus 'buffer-save-without-query)
        (setq buffer-save-without-query t)))

;;;###autoload
(defun aiern/go-to-parent nil (interactive)
    (outline-up-heading (if (and (or (org-at-heading-p) (invisible-p (point))) (invisible-p (point-at-eol))
            (>= (org-current-level) 2))
        1 0)))

;;;###autoload
(defun aiern/evil-close-fold nil (interactive) (aiern/go-to-parent) (evil-close-fold))

;;;###autoload
(defun aiern/org-cycle nil (interactive) (if (aiern/outline-folded-p) (org-cycle) (aiern/evil-close-fold)))

;;;###autoload
(defun aiern/org-babel-tangle-append nil
  "Append source code block at point to its tangle file.
The command works like `org-babel-tangle' with prefix arg
but `delete-file' is ignored."
  (interactive)
  (cl-letf (((symbol-function 'delete-file) #'ignore))
    (org-babel-tangle '(4))))

;;;###autoload
(defun aiern/org-babel-tangle-append-setup nil
  "Add key-binding C-c C-v C-t for `aiern/org-babel-tangle-append'."
  (org-defkey org-mode-map (kbd "C-c C-v +") 'aiern/org-babel-tangle-append))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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
    "")))

;;;###autoload
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
    "")))

;;;###autoload
(defun aiern/any-popup-showing-p nil (interactive) (or hercules--popup-showing-p (which-key--popup-showing-p)))

;;;###autoload
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

;;;###autoload
(defun aiern/hercules-hide-all-modal-modes (&optional keymap) (interactive)
    (when overriding-terminal-local-map (mapc #'(lambda (prefix) (interactive)
        (message (format "Hiding %s" prefix))
        (ignore-errors (funcall (intern (concat "aiern/" prefix "-hercules-hide"))))
        ;; (internal-push-keymap 'global-map 'overriding-terminal-local-map)
        ;; (internal-push-keymap nil 'overriding-terminal-local-map)
        (setq overriding-terminal-local-map nil)) modal-prefixes))
    (aiern/which-key-show-top-level keymap))

;;;###autoload
(defun fbatp (mode) (interactive)
    (let* ((is-it-bound (boundp mode)))
        (when is-it-bound (and (or (boundp (symbol-value mode))) (or (fboundp mode) (functionp mode))) mode)))

;;;###autoload
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

;;;###autoload
(defun aiern/keymap-symbol (keymap)
    "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
    (catch 'gotit
        (mapatoms (lambda (sym)
            (and (boundp sym)
                (eq (symbol-value sym) keymap)
                (not (eq sym 'keymap))
                (throw 'gotit sym))))))

;;;###autoload
(defun aiern/toggle-inner (mode prefix mode-on map &optional use-hercules force) (interactive)
    (aiern/disable-all-modal-modes)
    (if mode-on
        (when force (aiern/which-key--hide-popup))
        (when force (aiern/which-key--show-popup))
        (funcall mode 1)
        (if use-hercules (ignore-errors (funcall (intern (concat "aiern/" prefix "-hercules-show"))))
            (aiern/which-key-show-top-level map))
        (setq last-modal-mode prefix)))

;;;###autoload
(defun aiern/which-key--hide-popup nil (interactive)
    (aiern/disable-all-modal-modes)
    (setq which-key-persistent-popup nil) (which-key--hide-popup)
    (which-key-mode -1))

;;;###autoload
(defun aiern/which-key--show-popup (&optional keymap force) (interactive)
    (when (or (not which-key-persistent-popup) force)
        (aiern/disable-all-modal-modes keymap)
        (which-key-mode 1)
        (setq which-key-persistent-popup t)))

;;;###autoload
(defun aiern/which-key--refresh-popup (&optional keymap) (interactive)
    (aiern/which-key--hide-popup)
    (aiern/which-key--show-popup keymap t))

;;;###autoload
(defun aiern/toggle-which-key (&optional keymap) (interactive)

    ;; TODO: How does this bit work again...?
    (setq which-key-persistent-popup (not (aiern/any-popup-showing-p)))

    (if (aiern/any-popup-showing-p)
        (aiern/which-key--hide-popup)
        (aiern/which-key--show-popup keymap t)))

;;;###autoload
(defun aiern/hercules--hide-advice (&optional keymap flatten &rest _)
        "Dismiss hercules.el.
    Pop KEYMAP from `overriding-terminal-local-map' when it is not
    nil.  If FLATTEN is t, `hercules--show' was called with the same
    argument.  Restore `which-key--update' after such a call."
        (setq hercules--popup-showing-p nil)
        (setq overriding-terminal-local-map nil)
        (when flatten (advice-remove #'which-key--update #'ignore))
        (aiern/which-key-show-top-level))

;;;###autoload
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

;;;###autoload
(defun aiern/src-mode-settings nil (interactive) (aiern/disable-all-modal-modes) (focus-mode 1))

;;;###autoload
(defun aiern/src-mode-exit nil (interactive) (winner-undo) (aiern/disable-all-modal-modes))

;;;###autoload
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

;;;###autoload
(defun aiern/untabify-everything nil (untabify (point-min) (point-max)))

;;;###autoload
(defun aiern/untabify-everything-on-save nil (add-hook 'before-save-hook 'untabify-everything) nil)

;;;###autoload
(defun aiern/untabify-except-makefiles nil
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (aiern/untabify-everything)))

;;;###autoload
(defun aiern/evil-ex-advice (func &rest arguments) (aiern/which-key--hide-popup) (apply func arguments) (aiern/which-key--show-popup))

;;;###autoload
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
        (naked "right") 'forward-char)

    (setq novel-map-p t))

;;;###autoload
(defun aiern/disable-novel-map nil (interactive)
    (use-global-map global-map)
    (setq novel-map-p nil))

;;;###autoload
(defun aiern/toggle-novel-map nil (interactive)
    (intern (concat "aiern/" (if novel-map-p "enable" "disable") "-novel-map")))

;;;###autoload
(advice-add #'org-edit-special :after #'aiern/src-mode-settings)
;;;###autoload
(advice-add #'org-babel-tangle-collect-blocks :override #'aiern/org-babel-tangle-collect-blocks-handle-tangle-list)
;;;###autoload
(advice-add #'org-babel-tangle-single-block :around #'aiern/org-babel-tangle-single-block-handle-tangle-list)
;;;###autoload
(advice-add #'org-cycle :after #'(lambda (state) (interactive) (when (eq state 'children) (setq org-cycle-subtree-status 'subtree))))
;; (add-hook 'org-cycle-hook (lambda (state) (interactive) (when (eq state 'children) (setq org-cycle-subtree-status 'subtree))))

;; Adapted From: https://github.com/syl20bnr/spacemacs/issues/13058#issuecomment-565741009
;;;###autoload
(advice-add #'org-edit-src-exit :after #'aiern/src-mode-exit)
;;;###autoload
(advice-add #'org-edit-src-abort :after #'aiern/src-mode-exit)
;;;###autoload
(advice-add #'hercules--hide :override #'aiern/hercules--hide-advice)
;;;###autoload
(advice-add #'hercules--show :override #'aiern/hercules--show-advice)
;;;###autoload
(advice-add #'evil-insert-state :override #'aiern/disable-all-modal-modes)
;;;###autoload
(advice-add #'evil-ex :around #'aiern/evil-ex-advice)

;; TODO
;; ;;;###autoload
;; (advice-add #'counsel-M-x :before #'aiern/which-key--hide-popup)
;; ;;;###autoload
;; (advice-add #'helm-smex-major-mode-commands :before #'aiern/which-key--hide-popup)
;; ;;;###autoload
;; (advice-add #'helm-smex :before #'aiern/which-key--hide-popup)
;; ;;;###autoload
;; (advice-add #'execute-extended-command :before #'aiern/which-key--hide-popup)

;;;###autoload
(advice-add #'doom-escape :after #'aiern/which-key--show-popup)
;;;###autoload
(advice-add #'keyboard-escape-quit :after #'aiern/which-key--show-popup)
;;;###autoload
(advice-add #'keyboard-quit :after #'aiern/which-key--show-popup)
;;;###autoload
(advice-add #'exit-minibuffer :after #'aiern/which-key--show-popup)

(provide 'aiern)
;;; aiern.el ends here
