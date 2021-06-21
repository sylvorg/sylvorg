;;; aiern-integration.el --- Integrate aiern with other modules -*- lexical-binding: t -*-

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

;;; Commentary:

;; This provides aiern integration for various emacs modes.
;; Additional keybindings (or default state) should go into aiern-keybindings.el.

;;; Code:

(require 'aiern-maps)
(require 'aiern-core)
(require 'aiern-macros)
(require 'aiern-types)
(require 'aiern-repeat)

;;; aiernize some commands

;; unbound keys should be ignored
(aiern-declare-ignore-repeat 'undefined)

(mapc #'(lambda (cmd)
          (aiern-set-command-property cmd :keep-visual t)
          (aiern-declare-not-repeat cmd))
      '(digit-argument
        negative-argument
        universal-argument
        universal-argument-minus
        universal-argument-more
        universal-argument-other-key))
(mapc #'aiern-declare-not-repeat
      '(what-cursor-position))
(mapc #'aiern-declare-change-repeat
      '(dabbrev-expand
        hippie-expand
        quoted-insert))
(mapc #'aiern-declare-abort-repeat
      '(balance-windows
        eval-expression
        execute-extended-command
        exit-minibuffer
        compile
        delete-window
        delete-other-windows
        find-file-at-point
        ffap-other-window
        recompile
        redo
        save-buffer
        split-window
        split-window-horizontally
        split-window-vertically
        undo
        undo-tree-redo
        undo-tree-undo))

(aiern-set-type #'previous-line 'line)
(aiern-set-type #'next-line 'line)

(dolist (cmd '(keyboard-quit keyboard-escape-quit))
  (aiern-set-command-property cmd :suppress-operator t))

;;; Mouse
(aiern-declare-insert-at-point-repeat 'mouse-yank-primary)
(aiern-declare-insert-at-point-repeat 'mouse-yank-secondary)

;;; key-binding

;; Calling `keyboard-quit' should cancel repeat
(defadvice keyboard-quit (before aiern activate)
  (when (fboundp 'aiern-repeat-abort)
    (aiern-repeat-abort)))

(eval-after-load 'wdired
  '(progn
     (add-hook 'wdired-mode-hook #'aiern-change-to-initial-state)
     (defadvice wdired-change-to-dired-mode (after aiern activate)
       (aiern-change-to-initial-state nil t))))

;;; Parentheses

(defadvice show-paren-function (around aiern disable)
  "Match parentheses in Normal state."
  (if (if (memq 'not aiern-highlight-closing-paren-at-point-states)
          (memq aiern-state aiern-highlight-closing-paren-at-point-states)
        (not (memq aiern-state aiern-highlight-closing-paren-at-point-states)))
      ad-do-it
    (let ((pos (point)) syntax narrow)
      (setq pos
            (catch 'end
              (dotimes (var (1+ (* 2 aiern-show-paren-range)))
                (if (zerop (mod var 2))
                    (setq pos (+ pos var))
                  (setq pos (- pos var)))
                (setq syntax (syntax-class (syntax-after pos)))
                (cond
                 ((eq syntax 4)
                  (setq narrow pos)
                  (throw 'end pos))
                 ((eq syntax 5)
                  (throw 'end (1+ pos)))))))
      (if pos
          (save-excursion
            (goto-char pos)
            (save-restriction
              (when narrow
                (narrow-to-region narrow (point-max)))
              ad-do-it))
        ;; prevent the preceding pair from being highlighted
        (dolist (ov '(show-paren--overlay
                      show-paren--overlay-1
                      show-paren-overlay
                      show-paren-overlay-1))
          (let ((ov (and (boundp ov) (symbol-value ov))))
            (when (overlayp ov) (delete-overlay ov))))))))

;;; Undo tree
(eval-after-load 'undo-tree
  '(with-no-warnings
     (defadvice undo-tree-visualize (after aiern activate)
       "Initialize aiern in the visualization buffer."
       (when aiern-local-mode
         (aiern-initialize-state)))

     (when (fboundp 'undo-tree-visualize)
       (aiern-ex-define-cmd "undol[ist]" 'undo-tree-visualize)
       (aiern-ex-define-cmd "ul" 'undo-tree-visualize))

     (when (boundp 'undo-tree-visualizer-mode-map)
       (define-key undo-tree-visualizer-mode-map
         [remap aiern-backward-char] 'undo-tree-visualize-switch-branch-left)
       (define-key undo-tree-visualizer-mode-map
         [remap aiern-forward-char] 'undo-tree-visualize-switch-branch-right)
       (define-key undo-tree-visualizer-mode-map
         [remap aiern-next-line] 'undo-tree-visualize-redo)
       (define-key undo-tree-visualizer-mode-map
         [remap aiern-previous-line] 'undo-tree-visualize-undo)
       (define-key undo-tree-visualizer-mode-map
         [remap aiern-ret] 'undo-tree-visualizer-set))

     (when (boundp 'undo-tree-visualizer-selection-mode-map)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap aiern-backward-char] 'undo-tree-visualizer-select-left)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap aiern-forward-char] 'undo-tree-visualizer-select-right)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap aiern-next-line] 'undo-tree-visualizer-select-next)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap aiern-previous-line] 'undo-tree-visualizer-select-previous)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap aiern-ret] 'undo-tree-visualizer-set))))

;;; Auto-complete
(eval-after-load 'auto-complete
  '(progn
     (aiern-add-command-properties 'auto-complete :repeat 'aiern-ac-repeat)
     (aiern-add-command-properties 'ac-complete :repeat 'aiern-ac-repeat)
     (aiern-add-command-properties 'ac-expand :repeat 'aiern-ac-repeat)
     (aiern-add-command-properties 'ac-next :repeat 'ignore)
     (aiern-add-command-properties 'ac-previous :repeat 'ignore)

     (defvar aiern-ac-prefix-len nil
       "The length of the prefix of the current item to be completed.")

     (defvar ac-prefix)
     (defun aiern-ac-repeat (flag)
       "Record the changes for auto-completion."
       (cond
        ((eq flag 'pre)
         (setq aiern-ac-prefix-len (length ac-prefix))
         (aiern-repeat-start-record-changes))
        ((eq flag 'post)
         ;; Add change to remove the prefix
         (aiern-repeat-record-change (- aiern-ac-prefix-len)
                                    ""
                                    aiern-ac-prefix-len)
         ;; Add change to insert the full completed text
         (aiern-repeat-record-change
          (- aiern-ac-prefix-len)
          (buffer-substring-no-properties (- aiern-repeat-pos
                                             aiern-ac-prefix-len)
                                          (point))
          0)
         ;; Finish repeation
         (aiern-repeat-finish-record-changes))))))

;;; Company
(eval-after-load 'company
  '(progn
     (mapc #'aiern-declare-change-repeat
           '(company-complete-mouse
             company-complete-number
             company-complete-selection
             company-complete-common))

     (mapc #'aiern-declare-ignore-repeat
           '(company-abort
             company-select-next
             company-select-previous
             company-select-next-or-abort
             company-select-previous-or-abort
             company-select-mouse
             company-show-doc-buffer
             company-show-location
             company-search-candidates
             company-filter-candidates))))

;; Eval last sexp
(cond
 ((version< emacs-version "25")
  (defadvice preceding-sexp (around aiern activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not aiern-move-beyond-eol)
             (or (aiern-normal-state-p) (aiern-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          ad-do-it)
      ad-do-it))

  (defadvice pp-last-sexp (around aiern activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not aiern-move-beyond-eol)
             (or (aiern-normal-state-p) (aiern-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          ad-do-it)
      ad-do-it)))
 (t
  (defun aiern--preceding-sexp (command &rest args)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not aiern-move-beyond-eol)
             (or (aiern-normal-state-p) (aiern-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          (apply command args))
      (apply command args)))

  (advice-add 'elisp--preceding-sexp :around 'aiern--preceding-sexp '((name . aiern)))
  (advice-add 'pp-last-sexp          :around 'aiern--preceding-sexp '((name . aiern)))))

;; Show key
(defadvice quail-show-key (around aiern activate)
  "Temporarily go to Emacs state"
  (aiern-with-state emacs ad-do-it))

(defadvice describe-char (around aiern activate)
  "Temporarily go to Emacs state"
  (aiern-with-state emacs ad-do-it))

;; ace-jump-mode
(declare-function ace-jump-char-mode "ext:ace-jump-mode")
(declare-function ace-jump-word-mode "ext:ace-jump-mode")
(declare-function ace-jump-line-mode "ext:ace-jump-mode")
(defvar ace-jump-mode-scope)

(defvar aiern-ace-jump-active nil)

(defmacro aiern-enclose-ace-jump-for-motion (&rest body)
  "Enclose ace-jump to make it suitable for motions.
This includes restricting `ace-jump-mode' to the current window
in visual and operator state, deactivating visual updates, saving
the mark and entering `recursive-edit'."
  (declare (indent defun)
           (debug t))
  `(let ((old-mark (mark))
         (ace-jump-mode-scope
          (if (and (not (memq aiern-state '(visual operator)))
                   (boundp 'ace-jump-mode-scope))
              ace-jump-mode-scope
            'window)))
     (remove-hook 'pre-command-hook #'aiern-visual-pre-command t)
     (remove-hook 'post-command-hook #'aiern-visual-post-command t)
     (unwind-protect
         (let ((aiern-ace-jump-active 'prepare))
           (add-hook 'ace-jump-mode-end-hook
                     #'aiern-ace-jump-exit-recursive-edit)
           ,@body
           (when aiern-ace-jump-active
             (setq aiern-ace-jump-active t)
             (recursive-edit)))
       (remove-hook 'post-command-hook
                    #'aiern-ace-jump-exit-recursive-edit)
       (remove-hook 'ace-jump-mode-end-hook
                    #'aiern-ace-jump-exit-recursive-edit)
       (if (aiern-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'aiern-visual-pre-command nil t)
             (add-hook 'post-command-hook #'aiern-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(eval-after-load 'ace-jump-mode
  `(defadvice ace-jump-done (after aiern activate)
     (when aiern-ace-jump-active
       (add-hook 'post-command-hook #'aiern-ace-jump-exit-recursive-edit))))

(defun aiern-ace-jump-exit-recursive-edit ()
  "Exit a recursive edit caused by an aiern jump."
  (cond
   ((eq aiern-ace-jump-active 'prepare)
    (setq aiern-ace-jump-active nil))
   (aiern-ace-jump-active
    (remove-hook 'post-command-hook #'aiern-ace-jump-exit-recursive-edit)
    (exit-recursive-edit))))

(aiern-define-motion aiern-ace-jump-char-mode (count)
  "Jump visually directly to a char using ace-jump."
  :type inclusive
  (aiern-without-repeat
    (let ((pnt (point))
          (buf (current-buffer)))
      (aiern-enclose-ace-jump-for-motion
        (call-interactively 'ace-jump-char-mode))
      ;; if we jump backwards, motion type is exclusive, analogously
      ;; to `aiern-find-char-backward'
      (when (and (equal buf (current-buffer))
                 (< (point) pnt))
        (setq aiern-this-type
              (cond
               ((eq aiern-this-type 'exclusive) 'inclusive)
               ((eq aiern-this-type 'inclusive) 'exclusive)))))))

(aiern-define-motion aiern-ace-jump-char-to-mode (count)
  "Jump visually to the char in front of a char using ace-jump."
  :type inclusive
  (aiern-without-repeat
    (let ((pnt (point))
          (buf (current-buffer)))
      (aiern-enclose-ace-jump-for-motion
        (call-interactively 'ace-jump-char-mode))
      (if (and (equal buf (current-buffer))
               (< (point) pnt))
          (progn
            (or (eobp) (forward-char))
            (setq aiern-this-type
                  (cond
                   ((eq aiern-this-type 'exclusive) 'inclusive)
                   ((eq aiern-this-type 'inclusive) 'exclusive))))
        (backward-char)))))

(aiern-define-motion aiern-ace-jump-line-mode (count)
  "Jump visually to the beginning of a line using ace-jump."
  :type line
  :repeat abort
  (aiern-without-repeat
    (aiern-enclose-ace-jump-for-motion
      (call-interactively 'ace-jump-line-mode))))

(aiern-define-motion aiern-ace-jump-word-mode (count)
  "Jump visually to the beginning of a word using ace-jump."
  :type exclusive
  :repeat abort
  (aiern-without-repeat
    (aiern-enclose-ace-jump-for-motion
      (call-interactively 'ace-jump-word-mode))))

(define-key aiern-motion-state-map [remap ace-jump-char-mode] #'aiern-ace-jump-char-mode)
(define-key aiern-motion-state-map [remap ace-jump-line-mode] #'aiern-ace-jump-line-mode)
(define-key aiern-motion-state-map [remap ace-jump-word-mode] #'aiern-ace-jump-word-mode)

;;; avy
(declare-function avy-goto-word-or-subword-1 "ext:avy")
(declare-function avy-goto-line "ext:avy")
(declare-function avy-goto-char "ext:avy")
(declare-function avy-goto-char-2 "ext:avy")
(declare-function avy-goto-char-2-above "ext:avy")
(declare-function avy-goto-char-2-below "ext:avy")
(declare-function avy-goto-char-in-line "ext:avy")
(declare-function avy-goto-word-0 "ext:avy")
(declare-function avy-goto-word-1 "ext:avy")
(declare-function avy-goto-word-1-above "ext:avy")
(declare-function avy-goto-word-1-below "ext:avy")
(declare-function avy-goto-subword-0 "ext:avy")
(declare-function avy-goto-subword-1 "ext:avy")
(declare-function avy-goto-char-timer "ext:avy")
(defvar avy-all-windows)

(defmacro aiern-enclose-avy-for-motion (&rest body)
  "Enclose avy to make it suitable for motions.
Based on `aiern-enclose-ace-jump-for-motion'."
  (declare (indent defun)
           (debug t))
  `(let ((avy-all-windows
          (if (and (not (memq aiern-state '(visual operator)))
                   (boundp 'avy-all-windows))
              avy-all-windows
            nil)))
     ,@body))

(defmacro aiern-define-avy-motion (command type)
  (declare (indent defun)
           (debug t))
  (let ((name (intern (format "aiern-%s" command))))
    `(aiern-define-motion ,name (count)
       ,(format "aiern motion for `%s'." command)
       :type ,type
       :jump t
       :repeat abort
       (aiern-without-repeat
         (aiern-enclose-avy-for-motion
           (call-interactively ',command))))))

;; define aiern-avy-* motion commands for avy-* commands
(aiern-define-avy-motion avy-goto-char inclusive)
(aiern-define-avy-motion avy-goto-char-2 inclusive)
(aiern-define-avy-motion avy-goto-char-2-above inclusive)
(aiern-define-avy-motion avy-goto-char-2-below inclusive)
(aiern-define-avy-motion avy-goto-char-in-line inclusive)
(aiern-define-avy-motion avy-goto-char-timer inclusive)
(aiern-define-avy-motion avy-goto-line line)
(aiern-define-avy-motion avy-goto-line-above line)
(aiern-define-avy-motion avy-goto-line-below line)
(aiern-define-avy-motion avy-goto-subword-0 exclusive)
(aiern-define-avy-motion avy-goto-subword-1 exclusive)
(aiern-define-avy-motion avy-goto-symbol-1 exclusive)
(aiern-define-avy-motion avy-goto-symbol-1-above exclusive)
(aiern-define-avy-motion avy-goto-symbol-1-below exclusive)
(aiern-define-avy-motion avy-goto-word-0 exclusive)
(aiern-define-avy-motion avy-goto-word-1 exclusive)
(aiern-define-avy-motion avy-goto-word-1-above exclusive)
(aiern-define-avy-motion avy-goto-word-1-below exclusive)
(aiern-define-avy-motion avy-goto-word-or-subword-1 exclusive)

;; remap avy-* commands to aiern-avy-* commands
(dolist (command '(avy-goto-char
                   avy-goto-char-2
                   avy-goto-char-2-above
                   avy-goto-char-2-below
                   avy-goto-char-in-line
                   avy-goto-char-timer
                   avy-goto-line
                   avy-goto-line-above
                   avy-goto-line-below
                   avy-goto-subword-0
                   avy-goto-subword-1
                   avy-goto-symbol-1
                   avy-goto-symbol-1-above
                   avy-goto-symbol-1-below
                   avy-goto-word-0
                   avy-goto-word-1
                   avy-goto-word-1-above
                   avy-goto-word-1-below
                   avy-goto-word-or-subword-1))
  (define-key aiern-motion-state-map
    (vector 'remap command) (intern-soft (format "aiern-%s" command))))

;;; nXhtml/mumamo
;; ensure that mumamo does not toggle aiern through its globalized mode
(eval-after-load 'mumamo
  '(with-no-warnings
     (push 'aiern-mode-cmhh mumamo-change-major-mode-no-nos)))

;; visual-line-mode integration
(when aiern-respect-visual-line-mode
  (aiern-define-command aiern-digit-argument-or-aiern-beginning-of-visual-line ()
    :digit-argument-redirection aiern-beginning-of-visual-line
    :keep-visual t
    :repeat nil
    (interactive)
    (cond
     (current-prefix-arg
      (setq this-command #'digit-argument)
      (call-interactively #'digit-argument))
     (t
      (setq this-command 'aiern-beginning-of-visual-line)
      (call-interactively 'aiern-beginning-of-visual-line))))

  (aiern-define-minor-mode-key 'motion 'visual-line-mode
    "j" 'aiern-next-visual-line
    "gj" 'aiern-next-line
    "k" 'aiern-previous-visual-line
    "gk" 'aiern-previous-line
    "0" 'aiern-digit-argument-or-aiern-beginning-of-visual-line
    "g0" 'aiern-beginning-of-line
    "$" 'aiern-end-of-visual-line
    "g$" 'aiern-end-of-line
    "V" 'aiern-visual-screen-line))

;;; abbrev.el
(defun aiern-maybe-expand-abbrev ()
  (when (and abbrev-mode aiern-want-abbrev-expand-on-insert-exit)
    (expand-abbrev)))

(eval-after-load 'abbrev
  '(add-hook 'aiern-insert-state-exit-hook 'aiern-maybe-expand-abbrev))

;;; ElDoc
(eval-after-load 'eldoc
  '(when (fboundp 'eldoc-add-command-completions)
     (eldoc-add-command-completions "aiern-window-")))

;;; XRef
(eval-after-load 'xref
  '(progn
     (aiern-set-command-property 'xref-find-definitions :jump t)
     (aiern-set-command-property 'xref-find-references :jump t)))

(provide 'aiern-integration)

;;; aiern-integration.el ends here
