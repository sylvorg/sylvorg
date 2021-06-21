;;; aiern-vars.el --- Settings and variables -*- lexical-binding: t -*-

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

;;; Code:

(declare-function aiern-add-command-properties "aiern-common"
                  (command &rest properties))
(declare-function aiern-update-insert-state-bindings "aiern-maps"
                  (&optional _option-name remove force))

;;; Hooks

(defvar aiern-after-load-hook nil
  "Functions to be run when loading of aiern is finished.
This hook can be used the execute some initialization routines
when aiern is completely loaded.")

(defcustom aiern-goto-definition-functions
  '(aiern-goto-definition-imenu
    aiern-goto-definition-semantic
    aiern-goto-definition-xref
    aiern-goto-definition-search)
  "List of functions run until success by `aiern-goto-definition'."
  :type 'hook
  :group 'aiern)

;;; Initialization

(defvar aiern-pending-custom-initialize nil
  "A list of pending initializations for custom variables.
Each element is a triple (FUNC VAR VALUE). When aiern is
completely loaded then the functions (funcall FUNC VAR VALUE) is
called for each element. FUNC should be a function suitable for
the :initialize property of `defcustom'.")

(defun aiern-custom-initialize-pending-reset (var value)
  "Add a pending customization with `custom-initialize-reset'."
  (push (list 'custom-initialize-reset var value)
        aiern-pending-custom-initialize))

(defun aiern-run-pending-custom-initialize ()
  "Executes the pending initializations.
See `aiern-pending-custom-initialize'."
  (dolist (init aiern-pending-custom-initialize)
    (apply (car init) (cdr init)))
  (remove-hook 'aiern-after-load-hook 'aiern-run-pending-custom-initialize))
(add-hook 'aiern-after-load-hook 'aiern-run-pending-custom-initialize)

;;; Setters

(defun aiern-set-toggle-key (key)
  "Set `aiern-toggle-key' to KEY.
KEY must be readable by `read-kbd-macro'."
  (let ((old-key (read-kbd-macro
                  (if (boundp 'aiern-toggle-key)
                      aiern-toggle-key
                    "C-z")))
        (key (read-kbd-macro key)))
    (with-no-warnings
      (dolist (pair '((aiern-motion-state-map aiern-emacs-state)
                      (aiern-insert-state-map aiern-emacs-state)
                      (aiern-emacs-state-map aiern-exit-emacs-state)))
        (when (boundp (car pair))
          (let ((map (symbol-value (car pair)))
                (fun (cadr pair)))
            (when (keymapp map)
              (define-key map key fun)
              (define-key map old-key nil))))))))

(defun aiern-set-custom-state-maps (var pending-var key _make newlist)
  "Changes the list of special keymaps.
VAR         is the variable containing the list of keymaps.
PENDING-VAR is the variable containing the list of the currently pending
            keymaps.
KEY         the special symbol to be stored in the keymaps.
MAKE        the creation function of the special keymaps.
NEWLIST     the list of new special keymaps."
  (set-default pending-var newlist)
  (when (default-boundp var)
    (dolist (map (default-value var))
      (when (and (boundp (car map))
                 (keymapp (default-value (car map))))
        (define-key (default-value (car map)) (vector key) nil))))
  (set-default var newlist)
  (aiern-update-pending-maps))

(defun aiern-update-pending-maps (&optional _file)
  "Tries to set pending special keymaps.
This function should be called from an `after-load-functions'
hook."
  (let ((maps '((aiern-make-overriding-map . aiern-pending-overriding-maps)
                (aiern-make-intercept-map . aiern-pending-intercept-maps))))
    (while maps
      (let* ((map (pop maps))
             (make (car map))
             (pending-var (cdr map))
             (pending (symbol-value pending-var))
             newlist)
        (while pending
          (let* ((map (pop pending))
                 (kmap (and (boundp (car map))
                            (keymapp (symbol-value (car map)))
                            (symbol-value (car map))))
                 (state (cdr map)))
            (if kmap
                (funcall make kmap state)
              (push map newlist))))
        (set-default pending-var newlist)))))

(defun aiern-set-visual-newline-commands (var value)
  "Set the value of `aiern-visual-newline-commands'.
Setting this variable changes the properties of the appropriate
commands."
  (with-no-warnings
    (when (default-boundp var)
      (dolist (cmd (default-value var))
        (aiern-set-command-property cmd :exclude-newline nil)))
    (set-default var value)
    (dolist (cmd (default-value var))
      (aiern-set-command-property cmd :exclude-newline t))))

(defun aiern-set-custom-motions (var values)
  "Sets the list of motion commands."
  (with-no-warnings
    (when (default-boundp var)
      (dolist (motion (default-value var))
        (aiern-add-command-properties motion :keep-visual nil :repeat nil)))
    (set-default var values)
    (mapc #'aiern-declare-motion (default-value var))))

;;; Customization group

(defgroup aiern nil
  "Extensible vi layer."
  :group 'emulations
  :prefix 'aiern-)

(defcustom aiern-auto-indent t
  "\\<aiern-normal-state-map>
Whether to auto-indent when opening lines with \\[aiern-open-below] \
and \\[aiern-open-above]."
  :type  'boolean
  :group 'aiern)
(make-variable-buffer-local 'aiern-auto-indent)

(defcustom aiern-shift-width 4
  "\\<aiern-normal-state-map>
The number of columns by which a line is shifted.
This applies to the shifting operators \\[aiern-shift-right] and \
\\[aiern-shift-left]."
  :type 'integer
  :group 'aiern)
(make-variable-buffer-local 'aiern-shift-width)

(defcustom aiern-shift-round t
  "\\<aiern-normal-state-map>
Whether shifting rounds to the nearest multiple.
If non-nil, \\[aiern-shift-right] and \\[aiern-shift-left] adjust line
indentation to the nearest multiple of `aiern-shift-width'."
  :type 'boolean
  :group 'aiern)
(make-variable-buffer-local 'aiern-shift-round)

(defcustom aiern-indent-convert-tabs t
  "\\<aiern-normal-state-map>
If non-nil, the \\[aiern-indent] operator converts between leading tabs and spaces.
Whether tabs are converted to spaces or vice versa depends on the
value of `indent-tabs-mode'."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-default-cursor t
  "The default cursor.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'aiern)

(defvar aiern-force-cursor nil
  "Overwrite the current states default cursor.")

(defcustom aiern-repeat-move-cursor t
  "\\<aiern-normal-state-map>
Whether repeating commands with \\[aiern-repeat] may move the cursor.
If nil, the original cursor position is preserved, even if the command
normally would have moved the cursor."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-cross-lines nil
  "\\<aiern-motion-state-map>
Whether horizontal motions may move to other lines.  If non-nil,
certain motions that conventionally operate in a single line may move
the cursor to other lines.  Otherwise, they are restricted to the
current line.  This applies to \\[aiern-backward-char], \
\\[aiern-forward-char], \\[aiern-find-char], \
\\[aiern-find-char-backward], \\[aiern-find-char-to], \
\\[aiern-find-char-to-backward], \
\\<aiern-normal-state-map>\\[aiern-invert-char]."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-backspace-join-lines t
  "Whether backward delete in insert state may join lines."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-move-cursor-back t
  "Whether the cursor is moved backwards when exiting insert state.
If non-nil, the cursor moves \"backwards\" when exiting insert state,
so that it ends up on the character to the left.  Otherwise it remains
in place, on the character to the right."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-move-beyond-eol nil
  "Whether the cursor can move past the end of the line.
If non-nil, the cursor is allowed to move one character past the
end of the line, as in Emacs."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-respect-visual-line-mode nil
  "\\<aiern-motion-state-map>
Whether movement commands respect `visual-line-mode'.
If non-nil, `visual-line-mode' is generally respected when it is
on.  In this case, motions such as \\[aiern-next-line] and
\\[aiern-previous-line] navigate by visual lines (on the screen) rather
than \"physical\" lines (defined by newline characters).  If nil,
the setting of `visual-line-mode' is ignored.

This variable must be set before aiern is loaded."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-repeat-find-to-skip-next t
  "Whether a repeat of t or T should skip an adjacent character."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-kbd-macro-suppress-motion-error nil
  "\\<aiern-motion-state-map>
Whether left/right motions signal errors in keyboard macros.
This variable only affects beginning-of-line or end-of-line errors
regarding the motions \\[aiern-backward-char] and \\[aiern-forward-char]
respectively.  This may be desired since such errors cause macro
definition or execution to be terminated.  There are four
possibilities:

- `record': errors are suppressed when recording macros, but not when
  replaying them.
- `replay': errors are suppressed when replaying macros, but not when
  recording them.
- `t': errors are suppressed in both cases.
- `nil': errors are never suppressed."
  :type '(radio (const :tag "No" :value nil)
                (const :tag "Record" :value record)
                (const :tag "Replay" :value replay)
                (const :tag "Both" :value t))
  :group 'aiern)

(defcustom aiern-track-eol t
  "\\<aiern-motion-state-map>
Whether \\[aiern-end-of-line] \"sticks\" the cursor to the end of the line.
If non-nil, vertical motions after \\[aiern-end-of-line] maintain the cursor at the
end of the line, even if the target line is longer.  This is analogous
to `track-eol', but respects aiern's interpretation of end-of-line."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-mode-line-format 'before
  "The position of the state tag in the mode line.
If set to `before' or `after', the tag is placed at the beginning
or the end of the mode-line, respectively.  If nil, there is no
tag.  Otherwise it should be a cons cell (WHERE . WHICH), where
WHERE is either `before' or `after', and WHICH is a symbol in
`mode-line-format'.  The tag is then placed before or after that
symbol, respectively."
  :type '(radio :value 'before
                (const before)
                (const after)
                (cons :tag "Next to symbol"
                      (choice :value after
                              (const before)
                              (const after))
                      symbol))
  :group 'aiern)

(defcustom aiern-mouse-word 'aiern-word
  "The thing-at-point symbol for double click selection.
The double-click starts visual state in a special word selection
mode. This symbol is used to determine the words to be
selected. Possible values are `aiern-word' or `aiern-WORD'."
  :type 'symbol
  :group 'aiern)

(defcustom aiern-bigword "^ \t\r\n"
  "The set of characters to be interpreted as WORD boundaries.
This is enclosed with square brackets and used as a regular
expression.  By default, whitespace characters are considered
WORD boundaries."
  :type 'string
  :group 'aiern)
(make-variable-buffer-local 'aiern-bigword)

(defcustom aiern-want-fine-undo nil
  "Whether actions are undone in several steps.
There are two possible choices: nil (\"no\") means that all
changes made during insert state, including a possible delete
after a change operation, are collected in a single undo step.
Non-nil (\"yes\") means that undo steps are determined according
to Emacs heuristics, and no attempt is made to aggregate changes.

For backward compatibility purposes, the value `fine' is
interpreted as `nil'.  This option was removed because it did not
work consistently."
  :type '(radio (const :tag "No" :value nil)
                (const :tag "Fine (obsolete)" :value fine)
                (const :tag "Yes" :value t))
  :group 'aiern)

(defcustom aiern-regexp-search t
  "\\<aiern-motion-state-map>
Whether to use regular expressions for searching in \
\\[aiern-search-forward] and \\[aiern-search-backward]."
  :type  'boolean
  :group 'aiern)

(defcustom aiern-search-wrap t
  "\\<aiern-motion-state-map>
Whether search with \\[aiern-search-forward] and \
\\[aiern-search-backward] wraps around the buffer.
If this is non-nil, search stops at the buffer boundaries."
  :type  'boolean
  :group 'aiern)

(defcustom aiern-flash-delay 2
  "\\<aiern-motion-state-map>
Time in seconds to flash search matches after \\[aiern-search-next] and \
\\[aiern-search-previous]."
  :type  'number
  :group 'aiern)

(defcustom aiern-auto-balance-windows t
  "If non-nil window creation and deletion trigger rebalancing."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-split-window-below nil
  "If non-nil split windows are created below."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-vsplit-window-right nil
  "If non-nil vertically split windows with are created to the right."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-esc-delay 0.01
  "The time, in seconds, to wait for another key after escape.
If no further event arrives during this time, the event is
translated to `ESC'.  Otherwise, it is translated according to
`input-decode-map'.  This does not apply in Emacs state, and may
also be inhibited by setting `aiern-inhibit-esc'."
  :type 'number
  :group 'aiern)

(defvar aiern-esc-mode nil
  "Non-nil if `aiern-esc-mode' is enabled.")

(defvar aiern-esc-map nil
  "Original ESC prefix map in `input-decode-map'.
Used by `aiern-esc-mode'.")

(defvar aiern-inhibit-esc nil
  "If non-nil, the \\e event will never be translated to 'escape.")

(defcustom aiern-intercept-esc 'always
  "Whether aiern should intercept the escape key.
In the terminal, escape and a meta key sequence both generate the
same event.  In order to distingush these, aiern uses
`input-decode-map'.  It is not necessary to do this in a graphical
Emacs session.  However, if you prefer to use `C-[' as escape (which
is identical to the terminal escape key code), this interception must
also happen in graphical Emacs sessions.  Set this variable to
`always', t (only in the terminal) or nil (never intercept)."
  :type '(radio (const :tag "Never" :value nil)
                (const :tag "In terminal only" :value t)
                (const :tag "Always" :value always))
  :group 'aiern)

(defcustom aiern-show-paren-range 0
  "The minimal distance between point and a parenthesis
which causes the parenthesis to be highlighted."
  :type 'integer
  :group 'aiern)

(defcustom aiern-ex-hl-update-delay 0.02
  "Time in seconds of idle before updating search highlighting.
Setting this to a period shorter than that of keyboard's repeat
rate allows highlights to update while scrolling."
  :type 'number
  :group 'aiern)

(defcustom aiern-highlight-closing-paren-at-point-states
  '(not emacs insert replace)
  "The states in which the closing parenthesis at point should be highlighted.
All states listed here highlight the closing parenthesis at
point (which is Vim's default behavior).  All others highlight the
parenthesis before point (which is Emacs default behavior). If
this list contains the symbol `not' then its meaning is inverted,
i.e. all states listed here highlight the closing parenthesis
before point."
  :type '(repeat symbol)
  :group 'aiern)

(defcustom aiern-kill-on-visual-paste t
  "Whether pasting in visual state adds the replaced text to the
kill ring, making it the default for the next paste. The default,
replicates the default Vim behavior."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-want-C-i-jump t
  "Whether `C-i' jumps forward in the jump list (like Vim).
Otherwise, `C-i' inserts a tab character."
  :type 'boolean
  :group 'aiern
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'aiern-motion-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key aiern-motion-state-map (kbd "C-i"))
                        'aiern-jump-forward))
               (define-key aiern-motion-state-map (kbd "C-i") nil))
              ((and value
                    (not (lookup-key aiern-motion-state-map (kbd "C-i"))))
               (define-key aiern-motion-state-map (kbd "C-i") 'aiern-jump-forward))))))

(defcustom aiern-want-C-u-scroll nil
  "Whether `C-u' scrolls up (like Vim).
Otherwise, `C-u' applies a prefix argument.  The binding of
`C-u' mirrors Emacs behaviour by default due to the relative
ubiquity of prefix arguments."
  :type 'boolean
  :group 'aiern
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'aiern-motion-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key aiern-motion-state-map (kbd "C-u"))
                        'aiern-scroll-up))
               (define-key aiern-motion-state-map (kbd "C-u") nil))
              ((and value
                    (not (lookup-key aiern-motion-state-map (kbd "C-u"))))
               (define-key aiern-motion-state-map (kbd "C-u") 'aiern-scroll-up))))))

(defcustom aiern-want-C-d-scroll t
  "Whether `C-d' scrolls down (like Vim)."
  :type 'boolean
  :group 'aiern
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'aiern-motion-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key aiern-motion-state-map (kbd "C-d"))
                        'aiern-scroll-down))
               (define-key aiern-motion-state-map (kbd "C-d") nil))
              ((and value
                    (not (lookup-key aiern-motion-state-map (kbd "C-d"))))
               (define-key aiern-motion-state-map (kbd "C-d") 'aiern-scroll-down))))))

(defcustom aiern-want-C-u-delete nil
  "Whether `C-u' deletes back to indentation in insert state.
Otherwise, `C-u' applies a prefix argument.  The binding of
`C-u' mirrors Emacs behaviour by default due to the relative
ubiquity of prefix arguments."
  :type 'boolean
  :group 'aiern
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'aiern-insert-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key aiern-insert-state-map (kbd "C-u"))
                        'aiern-delete-back-to-indentation))
               (define-key aiern-insert-state-map (kbd "C-u") nil))
              ((and value
                    (not (lookup-key aiern-insert-state-map (kbd "C-u"))))
               (define-key aiern-insert-state-map (kbd "C-u") 'aiern-delete-back-to-indentation))))))

(defcustom aiern-want-C-w-delete t
  "Whether `C-w' deletes a word in Insert state."
  :type 'boolean
  :group 'aiern
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'aiern-insert-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key aiern-insert-state-map (kbd "C-w"))
                        'aiern-delete-backward-word))
               (define-key aiern-insert-state-map (kbd "C-w") 'aiern-window-map))
              ((and value
                    (eq (lookup-key aiern-insert-state-map (kbd "C-w"))
                        'aiern-window-map))
               (define-key aiern-insert-state-map (kbd "C-w") 'aiern-delete-backward-word))))))

(defcustom aiern-want-C-g-bindings nil
  "Whether `C-g' postfix can be used in bindings."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-want-C-w-in-emacs-state nil
  "Whether `C-w' prefixes windows commands in Emacs state."
  :type 'boolean
  :group 'aiern
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'aiern-emacs-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key aiern-emacs-state-map (kbd "C-w"))
                        'aiern-window-map))
               (define-key aiern-emacs-state-map (kbd "C-w") nil))
              ((and value
                    (not (lookup-key aiern-emacs-state-map (kbd "C-w"))))
               (define-key aiern-emacs-state-map (kbd "C-w") 'aiern-window-map))))))

(defcustom aiern-want-change-word-to-end t
  "Whether `cw' behaves like `ce'."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-want-Y-yank-to-eol nil
  "Whether `Y' yanks to the end of the line.
The default behavior is to yank the whole line, like Vim."
  :group 'aiern
  :type 'boolean
  :initialize #'aiern-custom-initialize-pending-reset
  :set #'(lambda (sym value)
           (set-default sym value)
           (aiern-add-command-properties
            'aiern-yank-line
            :motion (if value
                        'aiern-end-of-line-or-visual-line
                      'aiern-line-or-visual-line))))

(defcustom aiern-disable-insert-state-bindings nil
  "Whether insert state bindings should be used.
Bindings for escape, delete and `aiern-toggle-key' are always
available. If this is non-nil, default Emacs bindings are by and
large accessible in insert state."
  :group 'aiern
  :type 'boolean
  :initialize #'aiern-custom-initialize-pending-reset
  :set #'(lambda (sym value)
           (set-default sym value)
           (aiern-update-insert-state-bindings sym value)))

(defcustom aiern-echo-state t
  "Whether to signal the current state in the echo area."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-complete-all-buffers t
  "\\<aiern-insert-state-map>
Whether completion looks for matches in all buffers.
This applies to \\[aiern-complete-next] and \\[aiern-complete-previous] \
in insert state."
  :type 'boolean
  :group 'aiern)

(defvar dabbrev-search-these-buffers-only)
(defvar dabbrev-case-distinction)
(defcustom aiern-complete-next-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless aiern-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (condition-case nil
            (if (eq last-command this-command)
                (dabbrev-expand nil)
              (dabbrev-expand (- (abs (or arg 1)))))
          (error (dabbrev-expand nil)))))
  "Completion function used by \
\\<aiern-insert-state-map>\\[aiern-complete-next]."
  :type 'function
  :group 'aiern)

(defcustom aiern-complete-previous-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless aiern-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (dabbrev-expand arg)))
  "Completion function used by \
\\<aiern-insert-state-map>\\[aiern-complete-previous]."
  :type 'function
  :group 'aiern)

(defcustom aiern-complete-next-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<aiern-insert-state-map>\\[aiern-complete-next]."
  :type 'function
  :group 'aiern)

(defcustom aiern-complete-previous-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<aiern-insert-state-map>\\[aiern-complete-previous]."
  :type 'function
  :group 'aiern)

(defcustom aiern-complete-next-line-func
  #'(lambda (arg)
      (let ((hippie-expand-try-functions-list
             '(try-expand-line
               try-expand-line-all-buffers)))
        (hippie-expand arg)))
  "Minibuffer completion function used by \
\\<aiern-insert-state-map>\\[aiern-complete-next-line]."
  :type 'function
  :group 'aiern)

(defcustom aiern-complete-previous-line-func
  aiern-complete-next-line-func
  "Minibuffer completion function used by \
\\<aiern-insert-state-map>\\[aiern-complete-previous-line]."
  :type 'function
  :group 'aiern)

(defcustom aiern-lookup-func #'woman
  "Lookup function used by \
\"\\<aiern-motion-state-map>\\[aiern-lookup]\"."
  :type 'function
  :group 'aiern)

(defcustom aiern-toggle-key "C-z"
  "The key used to change to and from Emacs state.
Must be readable by `read-kbd-macro'. For example: \"C-z\"."
  :type 'string
  :group 'aiern
  :set #'(lambda (sym value)
           (aiern-set-toggle-key value)
           (set-default sym value)))

(defcustom aiern-default-state 'normal
  "The default aiern state.
This is the state a buffer starts in when it is not otherwise
configured (see `aiern-set-initial-state' and
`aiern-buffer-regexps').  The value may be one of `normal',
`insert', `visual', `replace', `operator', `motion' and `emacs'."
  :type  'symbol
  :group 'aiern)

(defcustom aiern-buffer-regexps
  '(("^ \\*load\\*" . nil))
  "Regular expressions determining the initial state for a buffer.
Entries have the form (REGEXP . STATE), where REGEXP is a regular
expression matching the buffer's name and STATE is one of `normal',
`insert', `visual', `replace', `operator', `motion', `emacs' and
`nil'.  If STATE is `nil', aiern is disabled in the buffer."
  :type '(alist :key-type string :value-type symbol)
  :group 'aiern)

(defcustom aiern-emacs-state-modes
  '(5x5-mode
    archive-mode
    bbdb-mode
    biblio-selection-mode
    blackbox-mode
    bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    browse-kill-ring-mode
    bs-mode
    bubbles-mode
    bzr-annotate-mode
    calc-mode
    cfw:calendar-mode
    completion-list-mode
    Custom-mode
    custom-theme-choose-mode
    debugger-mode
    delicious-search-mode
    desktop-menu-blist-mode
    desktop-menu-mode
    doc-view-mode
    dun-mode
    dvc-bookmarks-mode
    dvc-diff-mode
    dvc-info-buffer-mode
    dvc-log-buffer-mode
    dvc-revlist-mode
    dvc-revlog-mode
    dvc-status-mode
    dvc-tips-mode
    ediff-mode
    ediff-meta-mode
    efs-mode
    Electric-buffer-menu-mode
    emms-browser-mode
    emms-mark-mode
    emms-metaplaylist-mode
    emms-playlist-mode
    ess-help-mode
    etags-select-mode
    fj-mode
    gc-issues-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    gist-list-mode
    git-rebase-mode
    gnus-article-mode
    gnus-browse-mode
    gnus-group-mode
    gnus-server-mode
    gnus-summary-mode
    gomoku-mode
    google-maps-static-mode
    ibuffer-mode
    jde-javadoc-checker-report-mode
    magit-cherry-mode
    magit-diff-mode
    magit-log-mode
    magit-log-select-mode
    magit-popup-mode
    magit-popup-sequence-mode
    magit-process-mode
    magit-reflog-mode
    magit-refs-mode
    magit-revision-mode
    magit-stash-mode
    magit-stashes-mode
    magit-status-mode
    mh-folder-mode
    monky-mode
    mpuz-mode
    mu4e-main-mode
    mu4e-headers-mode
    mu4e-view-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    notmuch-tree-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    pdf-outline-buffer-mode
    pdf-view-mode
    proced-mode
    rcirc-mode
    rebase-mode
    recentf-dialog-mode
    reftex-select-bib-mode
    reftex-select-label-mode
    reftex-toc-mode
    sldb-mode
    slime-inspector-mode
    slime-thread-control-mode
    slime-xref-mode
    snake-mode
    solitaire-mode
    sr-buttons-mode
    sr-mode
    sr-tree-mode
    sr-virtual-mode
    tar-mode
    tetris-mode
    tla-annotate-mode
    tla-archive-list-mode
    tla-bconfig-mode
    tla-bookmarks-mode
    tla-branch-list-mode
    tla-browse-mode
    tla-category-list-mode
    tla-changelog-mode
    tla-follow-symlinks-mode
    tla-inventory-file-mode
    tla-inventory-mode
    tla-lint-mode
    tla-logs-mode
    tla-revision-list-mode
    tla-revlog-mode
    tla-tree-lint-mode
    tla-version-list-mode
    twittering-mode
    urlview-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vc-hg-log-view-mode
    vc-svn-log-view-mode
    vm-mode
    vm-summary-mode
    w3m-mode
    wab-compilation-mode
    xgit-annotate-mode
    xgit-changelog-mode
    xgit-diff-mode
    xgit-revlog-mode
    xhg-annotate-mode
    xhg-log-mode
    xhg-mode
    xhg-mq-mode
    xhg-mq-sub-mode
    xhg-status-extra-mode)
  "Modes that should come up in Emacs state."
  :type  '(repeat symbol)
  :group 'aiern)

(defcustom aiern-insert-state-modes
  '(comint-mode
    erc-mode
    eshell-mode
    geiser-repl-mode
    gud-mode
    inferior-apl-mode
    inferior-caml-mode
    inferior-emacs-lisp-mode
    inferior-j-mode
    inferior-python-mode
    inferior-scheme-mode
    inferior-sml-mode
    internal-ange-ftp-mode
    prolog-inferior-mode
    reb-mode
    shell-mode
    slime-repl-mode
    term-mode
    wdired-mode)
  "Modes that should come up in Insert state."
  :type  '(repeat symbol)
  :group 'aiern)

(defcustom aiern-motion-state-modes
  '(apropos-mode
    Buffer-menu-mode
    calendar-mode
    color-theme-mode
    command-history-mode
    compilation-mode
    dictionary-mode
    ert-results-mode
    help-mode
    Info-mode
    Man-mode
    speedbar-mode
    undo-tree-visualizer-mode
    woman-mode)
  "Modes that should come up in Motion state."
  :type  '(repeat symbol)
  :group 'aiern)

(defvar aiern-pending-overriding-maps nil
  "An alist of pending overriding maps.")

(defvar aiern-pending-intercept-maps nil
  "An alist of pending intercept maps.")

(defcustom aiern-overriding-maps
  '((Buffer-menu-mode-map . nil)
    (color-theme-mode-map . nil)
    (comint-mode-map . nil)
    (compilation-mode-map . nil)
    (grep-mode-map . nil)
    (dictionary-mode-map . nil)
    (ert-results-mode-map . motion)
    (Info-mode-map . motion)
    (speedbar-key-map . nil)
    (speedbar-file-key-map . nil)
    (speedbar-buffers-key-map . nil))
  "Keymaps that should override aiern maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be overridden. If STATE is nil, all states are
overridden."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'aiern
  :set #'(lambda (var values)
           (set-default var values)
           (aiern-set-custom-state-maps 'aiern-overriding-maps
                                       'aiern-pending-overriding-maps
                                       'override-state
                                       'aiern-make-overriding-map
                                       values))
  :initialize 'aiern-custom-initialize-pending-reset)

(add-hook 'after-load-functions #'aiern-update-pending-maps)

(defcustom aiern-intercept-maps
  '((edebug-mode-map . nil))
  "Keymaps that should intercept aiern maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be intercepted. If STATE is nil, all states are
intercepted."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'aiern
  :set #'(lambda (var values)
           (set-default var values)
           (aiern-set-custom-state-maps 'aiern-intercept-maps
                                       'aiern-pending-intercept-maps
                                       'intercept-state
                                       'aiern-make-intercept-map
                                       values))
  :initialize 'aiern-custom-initialize-pending-reset)

(defcustom aiern-motions
  '(back-to-indentation
    backward-char
    backward-list
    backward-paragraph
    backward-sentence
    backward-sexp
    backward-up-list
    backward-word
    beginning-of-buffer
    beginning-of-defun
    beginning-of-line
    beginning-of-visual-line
    c-beginning-of-defun
    c-end-of-defun
    diff-file-next
    diff-file-prev
    diff-hunk-next
    diff-hunk-prev
    down-list
    end-of-buffer
    end-of-defun
    end-of-line
    end-of-visual-line
    exchange-point-and-mark
    forward-char
    forward-list
    forward-paragraph
    forward-sentence
    forward-sexp
    forward-word
    goto-last-change
    ibuffer-backward-line
    ibuffer-forward-line
    isearch-abort
    isearch-cancel
    isearch-complete
    isearch-del-char
    isearch-delete-char
    isearch-edit-string
    isearch-exit
    isearch-highlight-regexp
    isearch-occur
    isearch-other-control-char
    isearch-other-meta-char
    isearch-printing-char
    isearch-query-replace
    isearch-query-replace-regexp
    isearch-quote-char
    isearch-repeat-backward
    isearch-repeat-forward
    isearch-ring-advance
    isearch-ring-retreat
    isearch-toggle-case-fold
    isearch-toggle-input-method
    isearch-toggle-regexp
    isearch-toggle-specified-input-method
    isearch-toggle-word
    isearch-yank-char
    isearch-yank-kill
    isearch-yank-line
    isearch-yank-word-or-char
    keyboard-quit
    left-char
    left-word
    mouse-drag-region
    mouse-save-then-kill
    mouse-set-point
    mouse-set-region
    mwheel-scroll
    move-beginning-of-line
    move-end-of-line
    next-error
    next-line
    paredit-backward
    paredit-backward-down
    paredit-backward-up
    paredit-forward
    paredit-forward-down
    paredit-forward-up
    pop-global-mark
    pop-tag-mark
    pop-to-mark-command
    previous-error
    previous-line
    right-char
    right-word
    scroll-down
    scroll-down-command
    scroll-up
    scroll-up-command
    sgml-skip-tag-backward
    sgml-skip-tag-forward
    up-list)
  "Non-aiern commands to initialize to motions."
  :type  '(repeat symbol)
  :group 'aiern
  :set 'aiern-set-custom-motions
  :initialize 'aiern-custom-initialize-pending-reset)

(defcustom aiern-visual-newline-commands
  '(LaTeX-section
    TeX-font)
  "Commands excluding the trailing newline of a Visual Line selection.
These commands work better without this newline."
  :type  '(repeat symbol)
  :group 'aiern
  :set 'aiern-set-visual-newline-commands
  :initialize 'aiern-custom-initialize-pending-reset)

(defcustom aiern-want-visual-char-semi-exclusive nil
  "Visual character selection to beginning/end of line is exclusive.
If non nil then an inclusive visual character selection which
ends at the beginning or end of a line is turned into an
exclusive selection. Thus if the selected (inclusive) range ends
at the beginning of a line it is changed to not include the first
character of that line, and if the selected range ends at the end
of a line it is changed to not include the newline character of
that line."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-text-object-change-visual-type t
  "Text objects change the current visual state type.
If non-nil then a text-object changes the type of the visual state to
its default selection type (e.g. a word object always changes to
charwise visual state). Otherwise the current visual state type is
preserved."
  :type 'boolean
  :group 'aiern)

(defgroup aiern-cjk nil
  "CJK support"
  :prefix "aiern-cjk-"
  :group 'aiern)

(defcustom aiern-cjk-emacs-word-boundary nil
  "Determine word boundary exactly the same way as Emacs does."
  :type 'boolean
  :group 'aiern-cjk)

(defcustom aiern-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `aiern-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'aiern-cjk)

(defcustom aiern-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `aiern-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'aiern-cjk)

(defcustom aiern-ex-complete-emacs-commands 'in-turn
  "TAB-completion for Emacs commands in ex command line.
This variable determines when Emacs commands are considered for
completion, always, never, or only if no aiern ex command is
available for completion."
  :group 'aiern
  :type '(radio (const :tag "Only if no ex-command." :value in-turn)
                (const :tag "Never" :value nil)
                (const :tag "Always" :value t)))

(defface aiern-ex-commands '(( nil
                              :underline t
                              :slant italic))
  "Face for the aiern command in completion in ex mode."
  :group 'aiern)

(defface aiern-ex-info '(( ((supports :slant))
                          :slant italic
                          :foreground "red"))
  "Face for the info message in ex mode."
  :group 'aiern)

(defcustom aiern-ex-visual-char-range nil
  "Type of default ex range in visual char state.
If non-nil the default range when starting an ex command from
character visual state is `<,`> otherwise it is '<,'>. In the
first case the ex command will be passed a region covering only
the visual selection. In the second case the passed region will
be extended to contain full lines."
  :group 'aiern
  :type 'boolean)

;; Searching
(defcustom aiern-symbol-word-search nil
  "If nil then * and # search for words otherwise for symbols."
  :group 'aiern
  :type 'boolean)
(make-variable-buffer-local 'aiern-symbol-word-search)

(defcustom aiern-magic t
  "Meaning which characters in a pattern are magic.
The meaning of those values is the same as in Vim. Note that it
only has influence if the aiern search module is chosen in
`aiern-search-module'."
  :group 'aiern
  :type '(radio (const :tag "Very magic." :value very-magic)
                (const :tag "Magic" :value t)
                (const :tag "Nomagic" :value nil)
                (const :tag "Very nomagic" :value very-nomagic)))

(defcustom aiern-ex-search-vim-style-regexp nil
  "If non-nil Vim-style backslash codes are supported in search patterns.
See `aiern-transform-vim-style-regexp' for the supported backslash
codes.  Note that this only affects the search command if
`aiern-search-module' is set to 'aiern-search. The isearch module
always uses plain Emacs regular expressions."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-ex-interactive-search-highlight 'all-windows
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
                (const :tag "Selected window." selected-window)
                (const :tag "Disable highlighting." nil))
  :group 'aiern)

(defcustom aiern-ex-search-persistent-highlight t
  "If non-nil matches remain highlighted when the search ends."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-ex-search-case 'smart
  "The case behaviour of the search command.
Smart case means that the pattern is case sensitive if and only
if it contains an upper case letter, otherwise it is case
insensitive."
  :type '(radio (const :tag "Case sensitive." sensitive)
                (const :tag "Case insensitive." insensitive)
                (const :tag "Smart case." smart))
  :group 'aiern)

(defcustom aiern-ex-substitute-case nil
  "The case behaviour of the search command.
Smart case means that the pattern is case sensitive if and only
if it contains an upper case letter, otherwise it is case
insensitive. If nil then the setting of `aiern-ex-search-case' is
used."
  :type '(radio (const :tag "Same as interactive search." nil)
                (const :tag "Case sensitive." sensitive)
                (const :tag "Case insensitive." insensitive)
                (const :tag "Smart case." smart))
  :group 'aiern)

(defcustom aiern-ex-search-interactive t
  "If t search is interactive."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-ex-search-highlight-all t
  "If t and interactive search is enabled, all matches are
highlighted."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-ex-substitute-highlight-all t
  "If t all matches for the substitute pattern are highlighted."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-ex-substitute-interactive-replace t
  "If t and substitute patterns are highlighted,
the replacement is shown interactively."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-ex-substitute-global nil
  "If non-nil substitute patterns are global by default.
Usually (if this variable is nil) a substitution works only on
the first match of a pattern in a line unless the 'g' flag is
given, in which case the substitution happens on all matches in a
line. If this option is non-nil, this behaviour is reversed: the
substitution works on all matches unless the 'g' pattern is
specified, then is works only on the first match."
  :type  'boolean
  :group 'aiern)

(defface aiern-ex-search '((t :inherit isearch))
  "Face for interactive search."
  :group 'aiern)

(defface aiern-ex-lazy-highlight '((t :inherit lazy-highlight))
  "Face for highlighting all matches in interactive search."
  :group 'aiern)

(defface aiern-ex-substitute-matches '((t :inherit lazy-highlight))
  "Face for interactive substitute matches."
  :group 'aiern)

(defface aiern-ex-substitute-replacement '((((supports :underline))
                                           :underline t
                                           :foreground "red"))
  "Face for interactive replacement text."
  :group 'aiern)

(defcustom aiern-command-window-height 8
  "Height (in lines) of the command line window.
Set to 0 to use the default height for `split-window'."
  :type 'integer
  :group 'aiern)

(defcustom aiern-display-shell-error-in-message nil
  "Show error output of a shell command in the error buffer.
If this variable is non-nil the error output of a shell command
goes to the messages buffer instead of being mixed with the
regular output. This happens only if the exit status of the
command is non-zero."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-want-abbrev-expand-on-insert-exit t
  "If non-nil abbrevs will be expanded when leaving insert state
like in Vim, if `abbrev-mode' is on."
  :type 'boolean
  :group 'aiern)

;;; Variables

(defmacro aiern-define-local-var (symbol &optional initvalue docstring)
  "Define SYMBOL as permanent buffer local variable, and return SYMBOL.
The parameters are the same as for `defvar', but the variable
SYMBOL is made permanent buffer local."
  (declare (indent defun)
           (doc-string 3)
           (debug (symbolp &optional form stringp)))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(aiern-define-local-var aiern-scroll-count 0
  "Holds last used prefix for `aiern-scroll-up'
and `aiern-scroll-down'.
Determines how many lines should be scrolled.
Default value is 0 - scroll half the screen.")

(aiern-define-local-var aiern-state nil
  "The current aiern state.
To change the state, use `aiern-change-state'
or call the state function (e.g., `aiern-normal-state').")

;; these may be used inside `aiern-define-state'
(aiern-define-local-var aiern-next-state nil
  "The aiern state being switched to.")

(aiern-define-local-var aiern-previous-state-alist nil
  "For Each aiern state the aiern state being switched from.")

(aiern-define-local-var aiern-previous-state nil
  "The aiern state being switched from.")

(defvar aiern-execute-in-emacs-state-buffer nil
  "The buffer of the latest `aiern-execute-in-emacs-state'.
When this command is being executed the current buffer is stored
in this variable. This is necessary in case the Emacs-command to
be called changes the current buffer.")

(aiern-define-local-var aiern-mode-line-tag nil
  "Mode-Line indicator for the current state.")
(put 'aiern-mode-line-tag 'risky-local-variable t)

(defvar aiern-global-keymaps-alist nil
  "Association list of keymap variables.
Entries have the form (MODE . KEYMAP), where KEYMAP
is the variable containing the keymap for MODE.")

(defvar aiern-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defvar aiern-minor-mode-keymaps-alist nil
  "Association list of aiern states to minor-mode keymap alists.
Entries have the form (STATE . MODE-MAP-ALIST), where
MODE-MAP-ALIST is an alist taking the form of
`minor-mode-map-alist'.")

(defvar aiern-state-properties nil
  "Specifications made by `aiern-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `aiern-state-property'.")

(aiern-define-local-var aiern-mode-map-alist nil
  "Association list of keymaps to use for aiern modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")

(defvar aiern-command-properties nil
  "Specifications made by `aiern-define-command'.")

(defvar aiern-change-commands '(aiern-change)
  "Commands that wrap or replace `aiern-change'.
This list exists to apply an inconsistency with vim's change command
to commands that wrap or redefine it. See emacs-aiern/aiern#916.")

(defvar aiern-transient-vars '(cua-mode transient-mark-mode select-active-regions)
  "List of variables pertaining to Transient Mark mode.")

(defvar aiern-transient-vals nil
  "Association list of old values for Transient Mark mode variables.
Entries have the form (VARIABLE VALUE LOCAL), where LOCAL is
whether the variable was previously buffer-local.")

(aiern-define-local-var aiern-no-display nil
  "If non-nil, various aiern displays are inhibited.
Use the macro `aiern-without-display' to set this variable.")

(defvar aiern-type-properties nil
  "Specifications made by `aiern-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(defvar aiern-interactive-alist nil
  "Association list of aiern-specific interactive codes.")

(aiern-define-local-var aiern-motion-marker nil
  "Marker for storing the starting position of a motion.")

(aiern-define-local-var aiern-this-type nil
  "Current motion type.")

(aiern-define-local-var aiern-this-type-modified nil
  "Non-nil iff current motion type has been modified by the user.
If the type has been modified, this variable contains the new
type.")

(aiern-define-local-var aiern-this-register nil
  "Current register.")

(defvar aiern-last-=-register-input nil
  "Most recent input from the `=' register. A string.")

(defvar aiern-this-macro nil
  "Current macro register.")

(aiern-define-local-var aiern-this-operator nil
  "Current operator.")

(aiern-define-local-var aiern-this-motion nil
  "Current motion.")

(aiern-define-local-var aiern-this-motion-count nil
  "Current motion count.")

(defvar aiern-last-register nil
  "The last executed register.")

(defvar aiern-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defvar aiern-inhibit-operator-value nil
  "This variable is used to transfer the value
of `aiern-inhibit-operator' from one local scope to another.")

;; used by `aiern-define-operator'
(defvar aiern-operator-range-beginning nil
  "Beginning of `aiern-operator-range'.")

(defvar aiern-operator-range-end nil
  "End of `aiern-operator-range'.")

(defvar aiern-operator-range-type nil
  "Type of `aiern-operator-range'.")

(defvar aiern-operator-range-motion nil
  "Motion of `aiern-operator-range'.")

(defvar aiern-restriction-stack nil
  "List of previous restrictions.
Using `aiern-with-restriction' stores the previous values of
`point-min' and `point-max' as a pair in this list.")

(aiern-define-local-var aiern-markers-alist
  '((?\( . aiern-backward-sentence)
    (?\) . aiern-forward-sentence)
    (?{ . aiern-backward-paragraph)
    (?} . aiern-forward-paragraph)
    (?' . aiern-jump-backward-swap)
    (?` . aiern-jump-backward-swap)
    (?< . aiern-visual-beginning)
    (?> . aiern-visual-goto-end)
    (?. . (lambda ()
            (let (last-command)
              (goto-last-change nil)))))
  "Association list for markers.
Entries have the form (CHAR . DATA), where CHAR is the marker's
name and DATA is either a marker object as returned by `make-marker',
a variable, a movement function, or a cons cell (STRING NUMBER),
where STRING is a file path and NUMBER is a buffer position.
The global value of this variable holds markers available from
every buffer, while the buffer-local value holds markers available
only in the current buffer.")

(defconst aiern-suppress-map (make-keymap)
  "Full keymap disabling default bindings to `self-insert-command'.")
(suppress-keymap aiern-suppress-map t)

(defvar aiern-read-key-map (make-sparse-keymap)
  "Keymap active during `aiern-read-key'.
This keymap can be used to bind some commands during the
execution of `aiern-read-key' which is usually used to read a
character argument for some commands, e.g. `aiern-replace'.")

;; TODO: customize size of ring
(defvar aiern-repeat-ring (make-ring 10)
  "A ring of repeat-informations to repeat the last command.")

(defvar aiern-repeat-types
  '((t . aiern-repeat-keystrokes)
    (change . aiern-repeat-changes)
    (motion . aiern-repeat-motion)
    (insert-at-point . aiern-repeat-insert-at-point)
    (ignore . nil))
  "An alist of defined repeat-types.")

(defvar aiern-recording-repeat nil
  "Whether we are recording a repeat.")

(defvar aiern-recording-current-command nil
  "Whether we are recording the current command for repeat.")

(defvar aiern-repeat-changes nil
  "Accumulated buffer changes for changed-based commands.")

(defvar aiern-repeat-info nil
  "Information accumulated during current repeat.")

(defvar aiern-repeat-buffer nil
  "The buffer in which the repeat started.
If the buffer is changed, the repeat is cancelled.")

(defvar aiern-repeat-pos nil
  "The position of point at the beginning of an change-tracking
  editing command.")

(defvar aiern-repeat-keys nil
  "The keys that invoked the current command.")

(defvar aiern-last-repeat nil
  "Information about the latest repeat command.
This is a list of three elements (POINT COUNT UNDO-POINTER),
where POINT is the position of point before the latest repeat,
COUNT the count-argument of the latest repeat command and
UNDO-POINTER the head of the undo-list before the last command
has been repeated.")

(defvar aiern-repeat-count nil
  "The explicit count when repeating a command.")

(defvar aiern-maybe-remove-spaces nil
  "Flag to determine if newly inserted spaces should be removed.
See the function `aiern-maybe-remove-spaces'.")

(aiern-define-local-var aiern-insert-count nil
  "The explicit count passed to an command starting Insert state.")

(aiern-define-local-var aiern-insert-vcount nil
  "The information about the number of following lines the
insertion should be repeated. This is list (LINE COLUMN COUNT)
where LINE is the line-number where the original insertion
started and COLUMN is either a number or function determining the
column where the repeated insertions should take place. COUNT is
number of repeats (including the original insertion).")

(defvar aiern-insert-skip-empty-lines nil
  "Non-nil of the current insertion should not take place on
  lines at which the insertion point is behind the end of the
  line.")

(aiern-define-local-var aiern-insert-lines nil
  "Non-nil if the current insertion command is a line-insertion
command o or O.")

(aiern-define-local-var aiern-insert-repeat-info nil
  "Repeat information accumulated during an insertion.")

(aiern-define-local-var aiern-replace-alist nil
  "Association list of characters overwritten in Replace state.
The format is (POS . CHAR).")

(aiern-define-local-var aiern-echo-area-message nil
  "Previous value of `current-message'.")

(defvar aiern-write-echo-area nil
  "If set to t inside `aiern-save-echo-area', then the echo area
is not restored.")

(defvar aiern-last-find nil
  "A pair (FUNCTION . CHAR) describing the lastest character
  search command.")

(defvar aiern-last-paste nil
  "Information about the latest paste.
This should be a list (CMD COUNT POINT BEG END FIRSTVISUAL) where
CMD is the last paste-command (`aiern-paste-before',
`aiern-paste-after' or `aiern-visual-paste'), COUNT is the repeat
count of the paste, POINT is the position of point before the
paste, BEG end END are the region of the inserted
text. FIRSTVISUAL is t if and only if the previous command was
the first visual paste (i.e. before any paste-pop).")

(aiern-define-local-var aiern-last-undo-entry nil
  "Information about the latest undo entry in the buffer.
This should be a pair (OBJ . CONS) where OBJ is the entry as an
object, and CONS is a copy of the entry.")

(aiern-define-local-var aiern-current-insertion nil
  "Information about the latest insertion in insert state.
This should be a pair (BEG . END) that describes the
buffer-region of the newly inserted text.")

(defvar aiern-last-insertion nil
  "The last piece of inserted text.")

(defvar aiern-last-small-deletion nil
  "The last piece of deleted text.
The text should be less than a line.")

(defvar aiern-was-yanked-without-register t
  "Whether text being saved to the numbered-register ring was
not deleted and not yanked to a specific register.")

(defvar aiern-paste-count nil
  "The count argument of the current paste command.")

(defvar aiern-temporary-undo nil
  "When undo is disabled in current buffer.
Certain commands depending on undo use this variable
instead of `buffer-undo-list'.")

(aiern-define-local-var aiern-undo-list-pointer nil
  "Everything up to this mark is united in the undo-list.")

(defvar aiern-in-single-undo nil
  "Set to non-nil if the current undo steps are connected.")

(defvar aiern-flash-timer nil
  "Timer for flashing search results.")

(defvar aiern-search-prompt nil
  "String to use for search prompt.")

(defvar aiern-search-forward-history nil
  "History of forward searches.")

(defvar aiern-search-backward-history nil
  "History of backward searches.")

(defvar aiern-inner-text-objects-map (make-sparse-keymap)
  "Keymap for inner text objects.")

(defvar aiern-outer-text-objects-map (make-sparse-keymap)
  "Keymap for outer text objects.")

(defvar aiern-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(aiern-define-local-var aiern-input-method nil
  "Input method used in Insert state and Emacs state.")

;;; Visual state

(aiern-define-local-var aiern-visual-beginning nil
  "The beginning of the Visual selection, a marker.")

(aiern-define-local-var aiern-visual-end nil
  "The end of the Visual selection, a marker.")

(aiern-define-local-var aiern-visual-point nil
  "The position of point in Visual state, a marker.")

(aiern-define-local-var aiern-visual-mark nil
  "The position of mark in Visual state, a marker.")

(aiern-define-local-var aiern-visual-previous-mark nil
  "The position of mark before Visual state, a marker.")

(aiern-define-local-var aiern-visual-selection nil
  "The kind of Visual selection.
This is a selection as defined by `aiern-define-visual-selection'.")

;; we could infer the direction by comparing `aiern-visual-mark'
;; and `aiern-visual-point', but destructive operations may
;; displace the markers
(aiern-define-local-var aiern-visual-direction 0
  "Whether point follows mark in Visual state.
Negative if point precedes mark, otherwise positive.
See also the function `aiern-visual-direction'.")

(aiern-define-local-var aiern-visual-properties nil
  "Property list of miscellaneous Visual properties.")

(aiern-define-local-var aiern-visual-region-expanded nil
  "Whether the region matches the Visual selection.
That is, whether the positions of point and mark have been
expanded to coincide with the selection's boundaries.
This makes the selection available to functions acting
on Emacs' region.")

(aiern-define-local-var aiern-visual-overlay nil
  "Overlay for highlighting the Visual selection.
Not used for blockwise selections, in which case
see `aiern-visual-block-overlays'.")

(aiern-define-local-var aiern-visual-block-overlays nil
  "Overlays for Visual Block selection, one for each line.
They are reused to minimize flicker.")

(defvar aiern-visual-alist nil
  "Association list of Visual selection functions.
Elements have the form (NAME . FUNCTION).")

(aiern-define-local-var aiern-visual-x-select-timer nil
  "Timer for updating the X selection in visual state.")

(defvar aiern-visual-x-select-timeout 0.1
  "Time in seconds for the update of the X selection.")

(declare-function origami-open-all-nodes "ext:origami.el")
(declare-function origami-close-all-nodes "ext:origami.el")
(declare-function origami-toggle-node "ext:origami.el")
(declare-function origami-open-node "ext:origami.el")
(declare-function origami-open-node-recursively "ext:origami.el")
(declare-function origami-close-node "ext:origami.el")

(defvar aiern-fold-list
  `(((vdiff-mode)
     :open-all   vdiff-open-all-folds
     :close-all  vdiff-close-all-folds
     :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
     :open       ,(lambda () (call-interactively 'vdiff-open-fold))
     :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
     :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
    ((vdiff-3way-mode)
     :open-all   vdiff-open-all-folds
     :close-all  vdiff-close-all-folds
     :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
     :open       ,(lambda () (call-interactively 'vdiff-open-fold))
     :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
     :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
    ((hs-minor-mode)
     :open-all   hs-show-all
     :close-all  hs-hide-all
     :toggle     hs-toggle-hiding
     :open       hs-show-block
     :open-rec   nil
     :close      hs-hide-block)
    ((hide-ifdef-mode)
     :open-all   show-ifdefs
     :close-all  hide-ifdefs
     :toggle     nil
     :open       show-ifdef-block
     :open-rec   nil
     :close      hide-ifdef-block)
    ((outline-mode
      outline-minor-mode
      org-mode
      markdown-mode)
     :open-all   show-all
     :close-all  ,(lambda ()
                    (with-no-warnings (hide-sublevels 1)))
     :toggle     outline-toggle-children
     :open       ,(lambda ()
                    (with-no-warnings
                      (show-entry)
                      (show-children)))
     :open-rec   show-subtree
     :close      hide-subtree)
    ((origami-mode)
     :open-all   ,(lambda () (origami-open-all-nodes (current-buffer)))
     :close-all  ,(lambda () (origami-close-all-nodes (current-buffer)))
     :toggle     ,(lambda () (origami-toggle-node (current-buffer) (point)))
     :open       ,(lambda () (origami-open-node (current-buffer) (point)))
     :open-rec   ,(lambda () (origami-open-node-recursively (current-buffer) (point)))
     :close      ,(lambda () (origami-close-node (current-buffer) (point)))))
  "Actions to be performed for various folding operations.

The value should be a list of fold handlers, were a fold handler has
the format:

  ((MODES) PROPERTIES)

MODES acts as a predicate, containing the symbols of all major or
minor modes for which the handler should match.  For example:

  '((outline-minor-mode org-mode) ...)

would match for either outline-minor-mode or org-mode, even though the
former is a minor mode and the latter is a major.

PROPERTIES specifies possible folding actions and the functions to be
applied in the event of a match on one (or more) of the MODES; the
supported properties are:

  - `:open-all'
    Open all folds.
  - `:close-all'
    Close all folds.
  - `:toggle'
    Toggle the display of the fold at point.
  - `:open'
    Open the fold at point.
  - `:open-rec'
    Open the fold at point recursively.
  - `:close'
    Close the fold at point.

Each value must be a function.  A value of `nil' will cause the action
to be ignored for that respective handler.  For example:

  `((org-mode)
     :close-all  nil
     :open       ,(lambda ()
                    (show-entry)
                    (show-children))
     :close      hide-subtree)

would ignore `:close-all' actions and invoke the provided functions on
`:open' or `:close'.")

;;; Ex

(defvar aiern-ex-map (make-sparse-keymap)
  "Keymap for Ex.
Key sequences bound in this map are immediately executed.")

(defvar aiern-ex-completion-map (make-sparse-keymap)
  "Completion keymap for Ex.")

(defvar aiern-ex-initial-input nil
  "Additional initial content of the ex command line.
This content of this variable is appended to the ex command line
if ex is started interactively.")

(defvar aiern-ex-shell-argument-initialized nil
  "This variable is set to t if shell command completion has been initialized.
See `aiern-ex-init-shell-argument-completion'.")

(defvar aiern-ex-commands nil
  "Association list of command bindings and functions.")

(defvar aiern-ex-history nil
  "History of Ex commands.")

(defvar aiern-ex-current-buffer nil
  "The buffer from which Ex was started.")

(defvar aiern-ex-expression nil
  "The evaluation tree.")

(defvar aiern-ex-tree nil
  "The syntax tree.")

(defvar aiern-ex-command nil
  "The current Ex command.")

(defvar aiern-ex-previous-command nil
  "The previously executed Ex command.")

(defvar aiern-ex-cmd nil
  "The current Ex command string.")

(defvar aiern-ex-point nil
  "The position of `point' when the ex command has been called.")

(defvar aiern-ex-range nil
  "The current range of the Ex command.")

(defvar aiern-ex-bang nil
  "The \"!\" argument of the current Ex command.")

(defvar aiern-ex-argument nil
  "The current argument of the Ex command.")

(defvar aiern-ex-argument-handler nil
  "The argument handler for the current Ex command.")

(defvar aiern-ex-argument-types nil
  "Association list of argument handlers.")

(defvar aiern-previous-shell-command nil
  "The last shell command.")

;; Eval
(defvar aiern-eval-history nil
  "History of eval input, from the `=' register.")

(defvar aiern-eval-map (make-sparse-keymap)
  "Keymap for eval input.")

;; Searching
(defvar aiern-ex-search-history nil
  "The history for the search command.")

(defvar aiern-ex-search-direction nil
  "The direction of the current search, either 'forward or 'backward.")

(defvar aiern-ex-search-count nil
  "The count if the current search.")

(defvar aiern-ex-search-start-point nil
  "The point where the search started.")

(defvar aiern-ex-search-overlay nil
  "The overlay for the current search result.")

(defvar aiern-ex-search-pattern nil
  "The last search pattern.")

(defvar aiern-ex-search-offset nil
  "The last search offset.")

(defvar aiern-ex-search-match-beg nil
  "The beginning position of the last match.")

(defvar aiern-ex-search-match-end nil
  "The end position of the last match.")

(defvar aiern-ex-substitute-pattern nil
  "The last substitute pattern.")

(defvar aiern-ex-substitute-replacement nil
  "The last substitute replacement.")

(defvar aiern-ex-substitute-flags nil
  "The last substitute flags.")

(defvar aiern-ex-substitute-current-replacement nil
  "The actual replacement.")

(defvar aiern-ex-last-was-search nil
  "Non-nil if the previous was a search.
Otherwise the previous command is assumed as substitute.")

;;; Command line window

(defvar aiern-command-window-current-buffer nil
  "The buffer from which the command line window was called.")

(aiern-define-local-var aiern-command-window-execute-fn nil
  "The command to execute when exiting the command line window.")

(aiern-define-local-var aiern-command-window-cmd-key nil
  "The key for the command that opened the command line window (:, /, or ?).")

;; The lazy-highlighting framework.
(aiern-define-local-var aiern-ex-active-highlights-alist nil
  "An alist of currently active highlights.")

(aiern-define-local-var aiern-ex-hl-update-timer nil
  "Time used for updating highlights.")

(defvar aiern-ex-search-keymap (make-sparse-keymap)
  "Keymap used in ex-search-mode.")
(define-key aiern-ex-search-keymap [escape] 'abort-recursive-edit)
(set-keymap-parent aiern-ex-search-keymap minibuffer-local-map)

(defconst aiern-version
  (eval-when-compile
    (with-temp-buffer
      (let ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file))))
        ;; git repository
        (if (and (file-exists-p (concat dir "/.git"))
                 (ignore-errors
                   (zerop (call-process "git" nil '(t nil) nil
                                        "rev-parse"
                                        "--short" "HEAD"))))
            (progn
              (goto-char (point-min))
              (concat "aiern-git-"
                      (buffer-substring (point-min)
                                        (line-end-position))))
          ;; no repo, use plain version
          "1.14.0"))))
  "The current version of aiern")

(defcustom aiern-want-integration t
  "Whether to load aiern-integration.el.
This variable must be set before aiern is loaded."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-want-keybinding t
  "Whether to load aiern-keybindings.el.

This loads a set of keybindings for aiern in other modes as well as
setting the initial aiern state in those modes.

This variable must be set before aiern is loaded."
  :type 'boolean
  :group 'aiern)

(defcustom aiern-want-minibuffer nil
  "Whether to enable aiern in minibuffer(s)."
  :type 'boolean
  :group 'aiern
  :set #'(lambda (sym value)
           (set-default sym value)
           (if value
               (add-hook 'minibuffer-setup-hook 'aiern-initialize)
             (remove-hook 'minibuffer-setup-hook 'aiern-initialize))))

(defun aiern--redo-placeholder (_count)
  (user-error "Customize `aiern-undo-system' for redo functionality."))

(defvar aiern-undo-function 'undo
  "Function to be used by `aiern-undo'.
Customized via `aiern-undo-system'.")

(defvar aiern-redo-function 'aiern--redo-placeholder
  "Function to be used by 'aiern-redo'.
Customized via `aiern-undo-system'.")

(defun aiern-set-undo-system (system)
  "Set `aiern-undo-function' and `aiern-redo-function` by SYSTEM."
  (cond
   ((not system)
    (setq aiern-undo-function 'undo
          aiern-redo-function 'aiern--redo-placeholder))
   ((eq system 'undo-redo)
    (setq aiern-undo-function 'undo-only
          aiern-redo-function 'undo-redo))
   ((eq system 'undo-tree)
    (setq aiern-undo-function 'undo-tree-undo
          aiern-redo-function 'undo-tree-redo))
   ((eq system 'undo-fu)
    (setq aiern-undo-function 'undo-fu-only-undo
          aiern-redo-function 'undo-fu-only-redo))
   (t
    (error "Unknown undo system %s" system))))

(defcustom aiern-undo-system nil
  "Undo system aiern should use.  If equal to `undo-tree' or
`undo-fu', those packages must be installed.  If equal to
`undo-tree', `undo-tree-mode' must also be activated.  If equal
to `undo-redo', aiern uses commands natively available in Emacs 28."
  :type '(choice (const :tag "Vanilla undo" nil)
                 (const undo-redo)
                 (const undo-tree)
                 (const undo-fu))
  :group 'aiern
  :set #'(lambda (sym value)
           (aiern-set-undo-system value)
           (set-default sym value)))

(defun aiern-version ()
  (interactive)
  (message "aiern version %s" aiern-version))

(provide 'aiern-vars)

;;; aiern-vars.el ends here
