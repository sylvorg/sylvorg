;;; tag.el --- Convenience wrappers for keybindings. -*- lexical-binding: t -*-

;; Author: Jeet Ray <aiern@protonmail.com>
;; URL: https://github.com/shadowrylander/tag
;; Created: February 17, 2016
;; Keywords: vim, evil, leader, keybindings, keys
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides convenient wrappers for more succinctly defining
;; keybindings. It allows defining multiple keys at once, specifying an
;; arbitrary number of named prefix keys to be used in key definitions,
;; implicitly wrapping key strings with (kbd ...), and more. It provides a
;; single function for standard Emacs key definitions as well as evil key
;; definitions for any evil state and any keymap. It also provides a setup
;; function to generate "nmap", "vmap", etc. keybinding functions for evil.

;; For more information see the README in the online repository.

;;; Code:
(require 'cl-lib)

;; * Settings
(defgroup tag nil
  "Gives convenient wrappers for key definitions."
  :group 'convenience
  :prefix "tag-")

(defcustom tag-implicit-kbd t
  "Whether to implicitly wrap a (kbd) around `tag-define-key' keys.
This applies to the prefix key as well. This option is provided to make it easy
  to transition from other key definers to `tag-define-key'. It does not
  apply to other helpers such as `tag-key', `tag-key-dispatch', and
  `tag-translate-key'. These will always use `kbd' on keys that are
  strings."
  :group 'tag
  :type 'boolean)

(defcustom tag-default-prefix nil
  "The default prefix key sequence to use."
  :group 'tag
  :type 'string)
(make-obsolete-variable 'tag-default-prefix
                        "This functionality will be removed in the future."
                        "2018-01-21")

(defcustom tag-default-non-normal-prefix nil
  "The default prefix key sequence to use for the 'emacs and 'insert states.
Note that this setting is only useful for evil-users and will only have an
effect when binding keys in the 'emacs and/or 'insert states or in the
'evil-insert-state-map and/or 'evil-emacs-state-map keymaps. When this is not
specified, `tag-default-prefix' will be the default prefix for any states
and keymaps. If this is specified `tag-default-prefix' or the arg to :prefix
will not be used when binding keys in the insert and Emacs states."
  :group 'tag
  :type 'string)
(make-obsolete-variable 'tag-default-non-normal-prefix
                        "This functionality will be removed in the future."
                        "2018-01-21")

(defcustom tag-default-global-prefix nil
  "The default prefix key sequence to use for all evil states.
This setting is only useful for evil users. Note that like with
`tag-default-non-normal-prefix', if this or :global-prefix is specified,
`tag-default-prefix' or the arg to :prefix will not be used for binding
keys in the insert and emacs states. If you don't need a different or extra
prefix for one or both state types (insert and Emacs vs. the other states),
just use `tag-default-prefix'/:prefix by itself."
  :group 'tag
  :type 'string)
(make-obsolete-variable 'tag-default-global-prefix
                        "This functionality will be removed in the future."
                        "2018-01-21")

(define-widget 'tag-state 'lazy
  "General's evil state type."
  :type '(choice
          (const :tag "Insert state" insert)
          (const :tag "Emacs state" emacs)
          (const :tag "Normal state" normal)
          (const :tag "Visual state" visual)
          (const :tag "Motion state" motion)
          (const :tag "Operator state" operator)
          (const :tag "Replace state" replace)
          (const :tag "Use define-key not evil-define-key" nil)
          ;; other packages define states
          symbol))

(defcustom tag-default-states nil
  "The default evil state(s) to make mappings in.
Non-evil users should keep this nil."
  :group 'tag
  :type '(choice tag-state
                 (set tag-state)))
(make-obsolete-variable 'tag-default-states
                        "This functionality will be removed in the future."
                        "2018-01-21")

(defcustom tag-non-normal-states '(insert emacs hybrid iedit-insert)
  "List of \"non-normal\" evil states (used with :non-normal-prefix). When
  :states is not specified (only :keymaps), these will automatically be expanded
  to their full global evil keymap equivalents."
  :group 'tag
  :type '(repeat tag-state))

(define-widget 'tag-keymap 'lazy
  "General's keymap type."
  :type '(choice
          (const :tag "Global keymap" global)
          (const :tag "Buffer local keymap" local)
          symbol))

(defcustom tag-default-keymaps 'global
  "The default keymap(s) to bind keys in."
  :group 'tag
  :type '(choice tag-keymap
                 (repeat tag-keymap)))
(make-obsolete-variable 'tag-default-keymaps
                        "This functionality will be removed in the future."
                        "2018-01-21")

(defcustom tag-vim-definer-default nil
  "Whether set the states or keymaps in a `tag-create-vim-definer' function.
If nil, use the default from when the function was created. If 'keymaps, set the
default keymaps. If 'states, set the default states."
  :group 'tag
  :type '(choice
          (const :tag "Default to setting :keymaps" keymaps)
          (const :tag "Default to setting :states" states)
          (const :tag "Use the initial default" nil)))
(make-obsolete-variable 'tag-vim-definer-default
                        "This functionality is no longer necessary."
                        "2018-01-20")

(defvar tag-keybindings nil
  "Holds all the keybindings created with `tag-define-key' (and wrappers).
This is an alist of a keymap to an alist of a state to keybindings.")

(defvar tag-local-keybindings nil
  "Holds all the local keybindings created with `tag-define-key'.
This is an alist of a state to keybindings.")
(make-variable-buffer-local 'tag-local-keybindings)

(define-widget 'tag-alist 'lazy
  "General's alist type."
  :type '(alist :key-type (or symbol (repeat symbol))
                :value-type symbol))

(defcustom tag-keymap-aliases
  '((override . tag-override-mode-map)
    ((i insert) . evil-insert-state-map)
    ((e emacs) . evil-emacs-state-map)
    ((h hybrid) . evil-hybrid-state-map)
    ((n normal) . evil-normal-state-map)
    ((v visual) . evil-visual-state-map)
    ((m motion) . evil-motion-state-map)
    ((o operator) . evil-operator-state-map)
    ((r replace) . evil-replace-state-map)
    ((in inner) . evil-inner-text-objects-map)
    ((out outer) . evil-outer-text-objects-map))
  "An alist for mapping short keymap names to their full names.
Earlier entries have higher precedence."
  :group 'tag
  :type 'tag-alist)

(defcustom tag-state-aliases
  '((i . insert)
    (e . emacs)
    (h . hybrid)
    (n . normal)
    (v . visual)
    (m . motion)
    (o . operator)
    (r . replace))
  "An alist for mapping short state names to their full names.
Earlier entries have higher precedence."
  :group 'tag
  :type 'tag-alist)

;; ** `tag-describe-keybindings' Settings
(defcustom tag-describe-keybinding-sort-function nil
  "Function used to sort keybindings for `tag-describe-keybindings'."
  :group 'tag
  :type '(choice function (const nil)))

(defcustom tag-describe-state-sort-function
  #'tag--sort-evil-state-conses
  "Function used to sort the states conses for `tag-describe-keybindings'."
  :group 'tag
  :type '(choice function (const nil)))

(defcustom tag-describe-keymap-sort-function nil
  "Function used to sort the keymap conses`tag-keybindings' for
`tag-describe-keybindings'."
  :group 'tag
  :type '(choice function (const nil)))

(defcustom tag-describe-priority-keymaps
  '(local
    global
    evil-insert-state-map
    evil-emacs-state-map
    evil-hybrid-state-map
    evil-normal-state-map
    evil-visual-state-map
    evil-motion-state-map
    evil-operator-state-map
    evil-replace-state-map
    evil-inner-text-objects-map
    evil-outer-text-objects-map
    evil-ex-search-keymap
    evil-ex-completion-map
    evil-command-window-mode-map
    evil-window-map)
  "Keymaps to print first for `tag-describe-keybindings'."
  :group 'tag
  :type '(repeat sybmol))

(defcustom tag-describe-update-previous-definition 'on-change
  "Whether to update the previous definition when a key is bound.
When set to 'on-change, the previous definition will only be updated when the
definition changes (e.g. re-evaluating a file with keybindings will not affect
the stored previous definition). When set to nil, it will only be updated when
the key was previously unbound."
  :group 'tag
  ;; can't think of a use case, but add 'always if requested
  ;; t is equivalent of on-change
  :type '(choice
          (const :tag "When definition has changed" on-change)
          (const :tag "When the key was previously unbound" nil)))

;; * Override Minor Modes
(defcustom tag-override-auto-enable t
  "Whether to automatically enable `tag-override-mode'.
If non-nil, enable `tag-override-mode' when binding a key in
`tag-override-mode-map'."
  :group 'tag
  :type 'boolean)

(defvar tag-override-mode-map (make-sparse-keymap)
  "A keymap that will take priority over other minor mode keymaps.
This is only for non-evil keybindings (it won't override keys bound with
`evil-define-key'.")

(define-minor-mode tag-override-mode
  "A global minor mode used for key definitions that should override others."
  :lighter ""
  :global t
  :require 'tag
  :keymap tag-override-mode-map)

(defvar-local tag-override-local-mode-map nil
  "A keymap that will take priority over other minor mode keymaps.
This keymap is buffer-local and will take precedence over
`tag-override-mode-map'. General uses this keymap when creating non-evil
local keybindings.")
(put 'tag-override-local-mode-map 'permanent-local t)

(define-minor-mode tag-override-local-mode
  "A local minor mode used for key definitions that should override others."
  :lighter ""
  :keymap tag-override-local-mode-map)

(defvar-local tag-maps-alist
  `((tag-override-mode . ,tag-override-mode-map))
  "Holds the (mode . keymap) pairs for tag's override modes.")
;; not affected by changing major modes
(put 'tag-maps-alist 'permanent-local t)

(defvar-local tag--maps-alist-updated nil
  "Whether `tag-maps-alist' has been set correctly for the current buffer.")
(put 'tag-maps-alist 'permanent-local t)

(declare-function evil-make-intercept-map "evil-core")
(defun tag-override-make-intercept-maps (_sym states)
  "Make intercept keymaps for STATES in `tag-override-mode-map'.
This means that keys bound in STATES for `tag-override-mode-map' will take
precedence over keys bound in other evil auxiliary maps."
  ;; can't use `tag-with-eval-after-load' here; not available
  (with-eval-after-load 'evil
    ;; TODO eventually use new evil-make-intercept-map arg
    (dolist (state states)
      (evil-make-intercept-map
       (evil-get-auxiliary-keymap tag-override-mode-map state t t)
       state))))

(defcustom tag-override-states
  '(insert
    emacs
    hybrid
    normal
    visual
    motion
    operator
    replace)
  "States to make intercept maps for in `tag-override-mode-map'.
Note that this uses :set, meaning that if you want to change the value, you
should either set it using customize (e.g. `tag-setq' or
`customize-set-variable') or set it before loading tag if using `setq'."
  :group 'tag
  :type '(repeat tag-state)
  :set #'tag-override-make-intercept-maps)

(defun tag--update-maps-alist ()
  "Update `tag-maps-alist' for override modes.
This is necessary to ensure `tag-override-local-mode-map' is the buffer's
local version."
  (setq tag-maps-alist
        `((tag-override-local-mode . ,tag-override-local-mode-map)
          (tag-override-mode . ,tag-override-mode-map))
        tag--maps-alist-updated t))

(cl-pushnew 'tag-maps-alist emulation-mode-map-alists)

(defun tag-local-map ()
  "Return `tag-override-local-mode-map', initializing it if necessary.
Also turn on `tag-override-local-mode' and update `tag-maps-alist'."
  (or tag-override-local-mode (tag-override-local-mode))
  (unless (and tag-override-local-mode-map
               (local-variable-p 'tag-override-local-mode-map))
    (setq tag-override-local-mode-map (make-sparse-keymap)))
  (unless tag--maps-alist-updated
    (tag--update-maps-alist))
  tag-override-local-mode-map)

;; * General Helpers
(defmacro tag-with-eval-after-load (file &rest body)
  "Like `with-eval-after-load' but don't always add to `after-load-alist'.
When FILE has already been loaded, execute BODY immediately without adding it to
`after-load-alist'."
  (declare (indent 1)
           (debug t))
  `(if (if (stringp ,file)
           (load-history-filename-element
            (purecopy (load-history-regexp ,file)))
         (featurep ,file))
       (progn ,@body)
     (eval-after-load ,file (lambda () ,@body))))

(defalias 'tag-after #'tag-with-eval-after-load)

(defun tag--unalias (symbol &optional statep)
  "Return the full keymap or state name associated with SYMBOL.
If STATEP is non-nil, check `tag-state-aliases' instead of
`tag-keymap-aliases'."
  (let ((match
         (cdr (cl-assoc symbol
                        (if statep
                            tag-state-aliases
                          tag-keymap-aliases)
                        ;; test-fn is new to assoc in 26.1
                        :test (lambda (symbol key)
                                (or (eq symbol key)
                                    (ignore-errors (memq symbol key))))))))
    (or match symbol)))

;; don't want to reuse `tag--unalias' since the user can alter
;; `tag-keymap-aliases'
(defun tag--evil-keymap-for-state (state)
  "Return a symbol corresponding to the global evil keymap for STATE."
  (intern (concat "evil-" (symbol-name state) "-state-map")))

(defun tag--kbd (key)
  "Use `kbd' on KEY when it is a string."
  (if (stringp key)
      (kbd key)
    key))

;; TODO refactor to be more straightforward
(defun tag--concat (nokbd &rest keys)
  "Concatenate the strings in KEYS.
If `tag-implicit-kbd' is non-nil, interleave the strings in KEYS with
spaces; unless NOKBD is non-nil, apply (kbd ...) to the result. If
`tag-implicit-kbd' is nil, just concatenate the keys."
  (setq keys (remove nil keys))
  (if tag-implicit-kbd
      (let ((keys (mapconcat (lambda (x)
                               (if (vectorp x)
                                   (key-description x)
                                 x))
                             keys " ")))
        (if nokbd
            keys
          (kbd keys)))
    (apply #'concat keys)))

(defun tag--apply-prefix-and-kbd (prefix maps)
  "Prepend the PREFIX sequence to all the keys that are strings in MAPS.
Also apply (kbd ...) to key and definition strings if `tag-implicit-kbd' is
non-nil."
  (setq prefix (or prefix ""))
  (cl-loop for (key def) on maps by 'cddr
           collect (tag--concat nil prefix key)
           and collect def))

(defun tag--lookup-key (state keymap key &optional minor-mode-p)
  "Return the current definition for STATE, KEYMAP, and KEY."
  (when key
    (let ((keymap (tag--get-keymap state keymap minor-mode-p)))
      (when keymap
        (let ((def (lookup-key keymap key)))
          (if (and (numberp def) (= def 1))
              nil
            def))))))

(defun tag--record-keybindings (keymap state maps &optional minor-mode-p)
  "For KEYMAP and STATE, add MAPS to `tag-keybindings'.
If KEYMAP is \"local\", add MAPS to `tag-local-keybindings.' For non-evil
keybindings, STATE will be nil. Duplicate keys will be replaced with the new
ones. MINOR-MODE-P should be non-nil when keymap corresponds to a minor-mode
name (as used with `evil-define-minor-mode-key') as opposed to a keymap name."
  (if (and state (not (featurep 'evil)))
      (tag-with-eval-after-load 'evil
        (tag--record-keybindings keymap state maps minor-mode-p))
    (let* (keys
           (maps (cl-loop
                  for (key new-def _orig-def) on maps by 'cl-cdddr
                  collect
                  (list key
                        new-def
                        (let* ((current-def (tag--lookup-key
                                             state keymap key minor-mode-p))
                               ;; none of these will fail if nil
                               (keymap-cons (assq keymap tag-keybindings))
                               (state-cons (assq state (cdr keymap-cons)))
                               (mapping (cl-find key (cdr state-cons)
                                                 :test #'equal :key #'car))
                               (previous-def (cl-caddr mapping)))
                          (if (or
                               (and current-def (not previous-def))
                               (and tag-describe-update-previous-definition
                                    (not (equal new-def current-def))))
                              current-def
                            previous-def)))
                  do (push key keys))))
      (cond ((eq keymap 'local)
             (unless (assq state tag-local-keybindings)
               (add-to-list 'tag-local-keybindings (list state)))
             (let ((state-cons (assq state tag-local-keybindings)))
               (setcdr state-cons
                       ;; remove old duplicate keys
                       (cl-remove-duplicates (append (cdr state-cons) maps)
                                             :key #'car
                                             :test #'equal))))
            (t
             (unless (assq keymap tag-keybindings)
               (add-to-list 'tag-keybindings (list keymap)))
             (unless (assq state (assq keymap tag-keybindings))
               (setcdr (assq keymap tag-keybindings)
                       (append (cdr (assq keymap tag-keybindings))
                               (list (list state)))))
             (let ((state-cons (assq state (assq keymap tag-keybindings))))
               (setcdr state-cons
                       (cl-remove-duplicates (append (cdr state-cons) maps)
                                             :key #'car
                                             :test #'equal))))))))

;; don't force non-evil user to require evil for one function
(defun tag--delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK. If APPEND is
non-nil, the entry is appended to the hook. If LOCAL is non-nil,
the buffer-local value of HOOK is modified.

This is `evil-delay'."
  (declare (indent 2))
  (if (and (not (booleanp condition)) (eval condition))
      (eval form)
    (let* ((name (or name (format "tag-delay-form-in-%s" hook)))
           (fun (make-symbol name))
           (condition (or condition t)))
      (fset fun `(lambda (&rest args)
                   (when ,condition
                     (remove-hook ',hook #',fun ',local)
                     ,form)))
      (put fun 'permanent-local-hook t)
      (add-hook hook fun append local))))

(defun tag--getf (def fallback-plist keyword)
  "From DEF or FALLBACK-PLIST get the corresponding value for KEYWORD.
FALLBACK-PLIST will be checked when KEYWORD does not exist in DEF (not in cases
where it is explicitly specified as nil). If DEF isn't a tag extended
definition, only check in FALLBACK-PLIST."
  (if (tag--extended-def-p def)
      (cl-getf def keyword
               (cl-getf fallback-plist keyword))
    (cl-getf fallback-plist keyword)))

(defun tag--getf2 (plist keyword1 keyword2)
  "Check in PLIST for either KEYWORD1 or KEYWORD2."
  (or (cl-getf plist keyword1)
      (cl-getf plist keyword2)))

(declare-function evil-get-minor-mode-keymap "evil-core")
(declare-function evil-state-property "evil-common")
(declare-function evil-get-auxiliary-keymap "evil-core")
(cl-defun tag--get-keymap (state &optional keymap
                                     minor-mode
                                     ignore-special)
  "Transform STATE and the symbol or keymap KEYMAP into the appropriate keymap.
If MINOR-MODE and STATE are non-nil, use `evil-get-minor-mode-keymap'. If
IGNORE-SPECIAL is non-nil, do not try to resolve the \"special\" keymaps 'global
and 'local. In this case, the only thing this function will do is return the
actually keymap if KEYMAP is a symbol besides 'global or 'local. Otherwise the
keymap returned depends on whether STATE is specified. Note that if STATE is
specified, evil needs to be installed and will be required.

STATE nil:
'local  - Run/return `tag-local-map'
'global - Run/return `current-global-map'
else    - Return keymap or (symbol-value keymap)

STATE non-nil:
'local  - Return the corresponding evil local map
'global - Return the corresponding evil global map
else    - Return the corresponding evil auxiliary or minor mode map"
  (when (and (symbolp keymap)
             (not (memq keymap '(global local))))
    (setq keymap (symbol-value keymap)))
  (when ignore-special
    (cl-return-from tag--get-keymap keymap))
  (if state
      (cond ((featurep 'evil)
          (cond ((or (null keymap)
                     (eq keymap 'global))
                 (evil-state-property state :keymap t))
                (minor-mode
                 (evil-get-minor-mode-keymap state keymap))
                ((eq keymap 'local)
                 (evil-state-property state :local-keymap t))
                (t
                 (evil-get-auxiliary-keymap keymap state t t))))
            ((featurep 'aiern)
          (cond ((or (null keymap)
                     (eq keymap 'global))
                 (aiern-state-property state :keymap t))
                (minor-mode
                 (aiern-get-minor-mode-keymap state keymap))
                ((eq keymap 'local)
                 (aiern-state-property state :local-keymap t))
                (t
                 (aiern-get-auxiliary-keymap keymap state t t))))
        (t (error "Evil or Aiern is required if state is specified")))
    (cl-case keymap
      (global (current-global-map))
      (local (tag-local-map))
      (t keymap))))
(define-obsolete-function-alias 'tag--parse-keymap 'tag--get-keymap
  "2018-01-14")

(defun tag--remove-keyword-args (rest)
  "Remove all keyword arguments from the list REST.
Return a list of the altered REST list and a list of the removed keyword
arguments. The order of arguments will be preserved. Note that the length of
REST does not need to be even (i.e. there can be an odd number of positional
arguments)."
  (let (args
        kargs)
    (while rest
      (cond ((keywordp (car rest))
             (push (pop rest) kargs)
             (push (pop rest) kargs))
            (t
             (push (pop rest) args))))
    (list (nreverse args) (nreverse kargs))))

(defmacro tag--ensure-lists (&rest vars)
  "Ensure that all variables in VARS are lists if they are not already.
If any variable is a lambda, it will not be considered to be a list. If a var is
nil, it will be set to (list nil)."
  `(progn
     ,@(mapcar (lambda (var)
                 `(unless (and ,var
                               (listp ,var)
                               ;; lambdas are lists
                               (not (functionp ,var)))
                    (setq ,var (list ,var))))
               vars)))

;; TODO stop using `cl-gensym' for counter functionality
(defvar tag--counter 0
  "Counter to use to prevent name clashes for automatically named functions.")

;; * Extended Key Definition Language
;; ** Variables
(defvar tag-extended-def-keywords
  '(:which-key :wk :properties :repeat :jump)
  "Extra keywords that are valid for extended definitions.

These can work both locally (in extended definitions) and globally (in which
case they apply to all definitions including normal ones). Note that not all
keywords need to make sense/work globally. If the keyword should be ignored when
used globally, add it to `tag-extended-def-global-ignore-keywords' as well.

For each keyword there should be a corresponding function named
tag-extended-def-:<keyword> which will be passed state, keymap (the symbol
not actual keymap), key (the internal representation, i.e. `kbd' already called
if necessary), edef (always a plist; normal definitions will automatically be
converted), and kargs (the original `tag-define-key' keyword argument plist;
useful when the keyword can be used globally or has helper keywords that can be
used globally). This function is only called for side effects; if you actually
need to alter the definition, you should add the keyword to
`tag-rewrite-def-keywords' or `tag-rewrite-def-after-keywords' instead.
The order of those lists matters, but the order of this list does not.

`tag--get-keymap' may be useful for getting the actual keymap from the
keymap symbol. `tag--getf' may be useful for keywords (helper or main) that
can be specified globally (in kargs) and overridden locally (in def).")

(defvar tag-rewrite-def-keywords
  '(:keymap :prefix-command :prefix-keymap)
  "Extended definition keywords that alter the definition.

Each keyword should have a corresponding function named
tag-extended-def-:<keyword> and should return a new extended definition
plist (with an altered :def entry). See `tag-extended-def-keywords' for
information on the arguments this function should take. These functions will be
run in the order they appear in this list, and each will be passed the most
recent version of the extended definition plist.

In contrast to the functions for `tag-rewrite-def-after-keywords', these
functions will alter the definition before any `tag-extended-def-keyword'
functions run. For example, if your function creates a newly named wrapper
command around the user-specified command, you'd want to add the keyword to this
list, so that `tag-extended-def-keywords' functions would have access to new
command name (e.g. for :which-key to work properly). On the other hand, if the
keyword, for example, involves putting the definition in an extended menu item
like with :predicate, you should add to `tag-rewrite-def-after-keywords'
instead.")

(defvar tag-rewrite-def-after-keywords
  '(:predicate)
  "Extended definition keywords that alter the definition.
See `tag-rewrite-def-keywords' for more information.")

(defvar tag-extended-def-global-ignore-keywords
  '(:keymap :prefix-command :prefix-map)
  "Extended definitions that should be ignored when used globally.
For example, :prefix-command and :prefix-map are handled differently when used
globally (they have special interaction with other global keywords). :keymap, on
the other hand, doesn't make sense at all globally.")

;; ** Normal Extended Definition Functions
;; *** Which Key Integration
(defvar which-key-replacement-alist)
(defun tag--add-which-key-replacement (mode replacement)
  (let* ((mode-match (assq mode which-key-replacement-alist))
         (mode-alist (cdr mode-match)))
    (cond (mode
           (push replacement mode-alist)
           (if mode-match
               (setcdr mode-match mode-alist)
             (push (cons mode mode-alist)
                   which-key-replacement-alist)))
          (t
           (push replacement which-key-replacement-alist)))))

(defvar which-key--prefix-title-alist)
(defun tag--add-which-key-title-prefix (mode keys title-prefix)
  (let* ((mode-match (assq mode which-key--prefix-title-alist))
         (title-mode-alist (cdr mode-match))
         (title-cons (cons keys title-prefix)))
    (cond (mode
           (push title-cons title-mode-alist)
           (if mode-match
               (setcdr mode-match
                       title-mode-alist)
             (push (cons mode title-mode-alist)
                   which-key--prefix-title-alist)))
          (t
           (push title-cons which-key--prefix-title-alist)))))

(defun tag--remove-map (keymap)
  "Remove \"-map\" from the symbol KEYMAP." ;
  (intern (replace-regexp-in-string "-map$" "" (symbol-name keymap))))

;; TODO better documentation
(defun tag-extended-def-:which-key (_state keymap key edef kargs)
  "Add a which-key description for KEY.
If :major-modes is specified in EDEF, add the description for the corresponding
major mode. KEY should not be in the kbd format (kbd should have already been
run on it)."
  (tag-with-eval-after-load 'which-key
    (let* ((wk (tag--getf2 edef :which-key :wk))
           (major-modes (tag--getf edef kargs :major-modes))
           (keymaps (plist-get kargs :keymaps))
           ;; index of keymap in :keymaps
           (keymap-index (cl-dotimes (ind (length keymaps))
                           (when (eq (nth ind keymaps) keymap)
                             (cl-return ind))))
           (mode (let ((mode (if (and major-modes (listp major-modes))
                                 (nth keymap-index major-modes)
                               major-modes)))
                   (if (eq mode t)
                       (tag--remove-map keymap)
                     mode)))
           (key (key-description key))
           (key-regexp (concat (when (tag--getf edef kargs :wk-full-keys)
                                 "\\`")
                               (regexp-quote key)
                               "\\'"))
           (prefix (plist-get kargs :prefix))
           (binding (or (when (and (plist-get edef :def)
                                   (not (plist-get edef :keymp)))
                          (plist-get edef :def))
                        (when (and prefix
                                   (string= key prefix))
                          (plist-get kargs :prefix-command))))
           (replacement (cond ((stringp wk)
                               (cons nil wk))
                              (t
                               wk)))
           (match/replacement
            (cons
             (cons (when (tag--getf edef kargs :wk-match-keys)
                     key-regexp)
                   (when (and (tag--getf edef kargs :wk-match-binding)
                              binding
                              (symbolp binding))
                     (regexp-quote (symbol-name binding))))
             replacement)))
      (tag--add-which-key-replacement mode match/replacement)
      (when (and (consp replacement)
                 ;; lambda
                 (not (functionp replacement)))
        (tag--add-which-key-title-prefix
         mode key (cdr replacement))))))

(defalias 'tag-extended-def-:wk #'tag-extended-def-:which-key)

;; *** Evil Integration
(declare-function evil-add-command-properties "evil-common")
(defun tag-extended-def-:properties (_state _keymap _key edef kargs)
  "Use `evil-add-command-properties' to add properties to a command.
The properties should be specified with :properties in either EDEF or KARGS."
  (tag-with-eval-after-load 'evil
    (let ((properties (tag--getf edef kargs :properties))
          (command (cl-getf edef :def)))
      (apply #'evil-add-command-properties command properties))))

(defun tag-extended-def-:repeat (_state _keymap _key edef kargs)
  "Use `evil-add-command-properties' to set the :repeat property for a command.
The repeat property should be specified with :repeat in either EDEF or KARGS."
  (tag-with-eval-after-load 'evil
    (let ((repeat-property (tag--getf edef kargs :repeat))
          (command (cl-getf edef :def)))
      (evil-add-command-properties command :repeat repeat-property))))

(defun tag-extended-def-:jump (_state _keymap _key edef kargs)
  "Use `evil-add-command-properties' to set the :jump property for a command.
The jump property should be specified with :jump in either EDEF or KARGS."
  (tag-with-eval-after-load 'evil
    (let ((jump-property (tag--getf edef kargs :jump))
          (command (cl-getf edef :def)))
      (evil-add-command-properties command :jump jump-property))))

;; ** Extended Defintion Functions That Alter the Definition
(defun tag-extended-def-:keymap (state keymap _key edef kargs)
  "Return an extended definition for a keymap or a \"autoloaded\" keymap.
If the specified keymap does not exist, create a function that binds the keys it
was invoked with in STATE and KEYMAP to the keymap specified in the extended
definition EDEF and then act as if it was originally bound to that
keymap (subsequent keys will be looked up in the keymap). KARGS or EDEF should
contain the package in which the keymap is created (as specified with :package).
If the keymap already exists, it will simply be returned."
  (let ((bind-keymap-sym (plist-get edef :def))
        (package (tag--getf edef kargs :package))
        (definer (tag--getf edef kargs :definer)))
    (if (boundp bind-keymap-sym)
        (setf (cl-getf edef :def) (symbol-value bind-keymap-sym))
      (if package
          (setf (cl-getf edef :def)
                ;; relying on lexical binding here
                (lambda ()
                  (interactive)
                  (unless (or (featurep package)
                              (require package nil t))
                    (error (format "Failed to load package: %s" package)))
                  (unless (and (boundp bind-keymap-sym)
                               (keymapp (symbol-value bind-keymap-sym)))
                    (error (format
                            "A keymap called %s is not defined in the %s package"
                            bind-keymap-sym package)))
                  ;; use `this-command-keys' as `key' may not be the full sequence
                  (let ((keys (this-command-keys))
                        (tag-implicit-kbd nil))
                    (tag-define-key
                     :states state
                     :keymaps keymap
                     :definer definer
                     keys (symbol-value bind-keymap-sym))
                    (setq prefix-arg current-prefix-arg
                          unread-command-events
                          (mapcar (lambda (ev) (cons t ev))
                                  (listify-key-sequence keys))))))
        (error "In order to \"autoload\" a keymap, :package must be specified"))))
  edef)

(defun tag--define-prefix (command-name &optional map-name menu-name)
  "Define a prefix command and/or keymap.
COMMAND-NAME corresponds to the prefix command name. When COMMAND-NAME is
non-nil, `define-prefix-command' will be used and will be passed MAP-NAME and
MENU-NAME. When COMMAND-NAME is nil and MAP-NAME is non-nil, only a prefix
keymap will be created, and its menu name/prompt will be set to MENU-NAME (if
MENU-NAME is non-nil). Existing prefix keymaps/commands will not be
recreated/rebound."
  (if (or (and command-name (fboundp command-name))
          (and map-name (boundp map-name)))
      (or command-name (symbol-value map-name))
    (cond (command-name
           (define-prefix-command command-name map-name menu-name))
          (map-name
           (eval `(defvar ,map-name (make-sparse-keymap ,menu-name)))))))

(defun tag-extended-def-:prefix-command (_state _keymap _key edef _kargs)
  "Create and return a prefix command or map for the extended definition EDEF.
The :prefix-command, :prefix-map, and :prefix-name properties from EDEF are
passed to `tag--define-prefix'."
  ;; NOTE will be called twice if both specified, but doesn't matter because
  ;; won't recreate prefix-command
  (setf (cl-getf edef :def)
        (tag--define-prefix (plist-get edef :prefix-command)
                                (plist-get edef :prefix-map)
                                (plist-get edef :prefix-name)))
  edef)

(defalias 'tag-extended-def-:prefix-map
  #'tag-extended-def-:prefix-command)

;; http://endlessparentheses.com/define-context-aware-keys-in-emacs.html
(defun tag-extended-def-:predicate (_state _keymap _key edef kargs)
  "Return an altered extended definition EDEF with a predicate applied.
The predicate is obtained either from EDEF or KARGS."
  (let ((def (cl-getf edef :def))
        (predicate (tag--getf edef kargs :predicate)))
    (setf (cl-getf edef :def)
          `(menu-item
            "" nil
            :filter (lambda (&optional _)
                      (when ,predicate
                        ',def))))
    edef))

;; ** Parsing Extended Definitions
(defun tag--extended-def-p (def)
  "Return whether DEF is an extended definition."
  (and (listp def)
       (not (keymapp def))
       ;; lambda
       (not (functionp def))
       (not (eq (car def) 'menu-item))
       ;; will error on cons
       (ignore-errors (cl-some #'keywordp def))))

(defun tag--normalize-extended-def (edef)
  "Rewrite the extended definition EDEF to include a :def property.
If EDEF is not an extended defintion, make it into one.

This handles the allowed shorthand syntax. For example, these are the same:

 (some-func)
 (:def some-func)

Some extended definition keywords can be used instead of :def (mainly for
backwards compatibility). For example, these are the same:

 (some-keymap :keymap t)
 (:keymap some-keymap)
 (:def some-keymap :keymap t)"
  ;; NOTE: This is absolutely necessary for plist functions to work
  (if (tag--extended-def-p edef)
      (unless (keywordp (car edef))
        (setq edef (cons :def edef)))
    (setq edef (list :def edef)))
  ;; :keymap checks :def always instead of :keymap, and :which-key also checks
  ;; :def always (instead of :prefix-command)
  ;; note that :keymap and :prefix-map will later rewrite their :def to the
  ;; actual keymap value
  (unless (plist-get edef :def)
    (setf (cl-getf edef :def)
          (cl-getf edef :keymap
                   (cl-getf edef :prefix-command
                            (plist-get edef :prefix-map)))))
  edef)

(defun tag--extract-def (edef)
  "Return the bindable definition from the extended definition EDEF."
  (if (plist-get edef :ignore)
      ;; just for side effects (e.g. which-key description for prefix)
      ;; return something that isn't a valid definition
      :ignore
    (plist-get edef :def)))

(defun tag--run-extended-def-functions (state keymap key edef kargs)
  "Run the extended definition functions for the matched keywords.
Pass each extended definition function STATE, KEYMAP, KEY, EDEF, and KARGS. For
each keyword from `tag-extended-def-keywords',
`tag-rewrite-def-keywords', and `tag-rewrite-def-after-keywords' found
in EDEF or KARGS, call the corresponding function named
tag-extended-def-:<keyword>. The functions for
`tag-rewrite-def-keywords' will rewrite the extended definition plist before
the functions for `tag-extended-def-keywords' are called, and the functions
for `tag-rewrite-def-after-keywords' are called after that. Functions
are called in the order they appear in each list. Finally, return the
potentially altered extended definition plist."
  (cl-flet ((run-edef-functions
             (keywords &optional alter-def)
             (dolist (keyword keywords)
               (when (or (plist-member edef keyword)
                         (and (not
                               (memq
                                keyword
                                tag-extended-def-global-ignore-keywords))
                              (plist-member kargs keyword)))
                 (let ((ret (funcall
                             (intern (format "tag-extended-def-%s" keyword))
                             state keymap key edef kargs)))
                   (when alter-def
                     (setq edef ret)))))))
    (run-edef-functions tag-rewrite-def-keywords t)
    (run-edef-functions tag-extended-def-keywords)
    (run-edef-functions tag-rewrite-def-after-keywords t))
  edef)

(defun tag--parse-def (state keymap key def kargs)
  "Rewrite DEF into a valid/bindable definition.
This function will execute all extended definitions, potentially rewriting the
original definition (e.g. applying a predicate). Pass STATE, KEYMAP, KEY, DEF, and
KARGS to each matched extended definition function. See
`tag--run-extended-def-functions' for more information."
  (setq def (tag--normalize-extended-def def))
  (tag--extract-def
   (tag--run-extended-def-functions state keymap key def kargs)))

(defun tag--parse-maps (state keymap maps kargs)
  "Rewrite MAPS so that the definitions are bindable.
This includes possibly calling `kbd' on keys and parsing extended definitions.
Turn key/binding pairs in MAPS into triples in the form of (key parsed-def
original-def) where parsed-def is the bindable form and original-def is the
original definition as an extended definition plist (turn normal definitions
into extended definition plists and implicitly add \":def\" to the beginning of
extended definitions when necessary)."
  (let (bindable-def)
    (cl-loop for (key def) on maps by 'cddr
             do (setq bindable-def
                      (tag--parse-def state keymap key def kargs))
             unless (eq bindable-def :ignore)
             collect key
             and collect (if tag-implicit-kbd
                             (tag--kbd bindable-def)
                           bindable-def)
             and collect (tag--normalize-extended-def def))))

;; * Helper Key Definers
(declare-function evil-define-minor-mode-key "evil-core")
(defun tag-minor-mode-define-key (state mode key def _orig-def _kargs)
  "A wrapper for `evil-define-minor-mode-key'."
  (tag-with-eval-after-load 'evil
    (evil-define-minor-mode-key state mode key def)))

(declare-function lispy-define-key "lispy")
(defun tag-lispy-define-key (_state keymap key def orig-def kargs)
  "A wrapper for `lispy-define-key'."
  (tag-with-eval-after-load 'lispy
    (let* ((keymap (tag--get-keymap nil keymap))
           (key (key-description key))
           (plist (tag--getf orig-def kargs :lispy-plist)))
      (apply #'lispy-define-key keymap key def plist))))

(declare-function worf-define-key "worf")
(defun tag-worf-define-key (_state keymap key def orig-def kargs)
  "A wrapper for `worf-define-key'."
  (tag-with-eval-after-load 'worf
    (let* ((keymap (tag--get-keymap nil keymap))
           (key (key-description key))
           (plist (tag--getf orig-def kargs :worf-plist)))
      (apply #'worf-define-key keymap key def plist))))

(declare-function lpy-define-key "lpy")
(defun tag-lpy-define-key (_state keymap key def _orig-def _kargs)
  "A wrapper for `lpy-define-key'."
  (tag-with-eval-after-load 'lpy
    (let* ((keymap (tag--get-keymap nil keymap))
           (key (key-description key)))
      (lpy-define-key keymap key def))))

(declare-function evil-define-key* "evil-core")
(defun tag--define-key-dispatch (state keymap maps kargs)
  "In STATE (if non-nil) and KEYMAP, bind MAPS.
MAPS is composed of triplets of (key parsed-def original-def). This function
determines the appropriate base definer function to use based depending on
whether :definer is present in original-def or KARGS or whether STATE is
non-nil if no custom definer is specified."
  (when (and tag-override-auto-enable
             (eq keymap 'tag-override-mode-map)
             (not tag-override-mode))
    (tag-override-mode))
  (while maps
    (let* ((key (pop maps))
           (def (pop maps))
           (orig-def (pop maps))
           (definer (tag--getf orig-def kargs :definer)))
      (if definer
          (funcall (intern (format "tag-%s-define-key"
                                   (symbol-name definer)))
                   state keymap key def orig-def kargs)
        (cond (state
               ;; just to get the symbol-value of the keymap when it is not
               ;; global/local
               (setq keymap (tag--get-keymap nil keymap nil t))
               (tag-with-eval-after-load 'evil
                 (evil-define-key* state keymap key def)))
              (t
               (setq keymap (tag--get-keymap nil keymap))
               (define-key keymap key def)))))))

(defvar tag--definer-p nil
  "Whether the current keybinding is being created with `tag-define-key'.")

(defun tag--define-key
    (state keymap maps non-normal-maps global-maps kargs)
  "A helper function for `tag-define-key'.
Choose based on STATE and KEYMAP which of MAPS, NON-NORMAL-MAPS, and GLOBAL-MAPS
to use for the keybindings. This function will rewrite extended definitions, add
predicates when applicable, and then choose the base function to bind the keys
with by calling `tag--define-key-dispatch'."
  (let ((tag--definer-p t))
    (let* ((non-normal-p (if state
                             (memq state tag-non-normal-states)
                           (memq keymap
                                 (mapcar #'tag--evil-keymap-for-state
                                         tag-non-normal-states))))
           (valid-maps (list (cond ((and non-normal-maps non-normal-p)
                                    non-normal-maps)
                                   ((and global-maps non-normal-p)
                                    nil)
                                   (t
                                    maps))
                             global-maps)))
      (dolist (maps valid-maps)
        (when maps
          (setq maps (tag--parse-maps state keymap maps kargs))
          ;; NOTE: :definer 'minor-mode cannot be specified locally
          (tag--record-keybindings keymap state maps
                                       (eq (cl-getf kargs :definer)
                                           'minor-mode))
          (tag--define-key-dispatch state keymap maps kargs))))))

;; * Functions With Keyword Arguments
;;;###autoload
(cl-defun tag-define-key
    (&rest maps &key
           definer
           (states tag-default-states)
           (keymaps tag-default-keymaps keymaps-specified-p)
           (prefix tag-default-prefix)
           (non-normal-prefix tag-default-non-normal-prefix)
           (global-prefix tag-default-global-prefix)
           infix
           prefix-command
           prefix-map
           prefix-name
           predicate
           ;; related to extended definitions
           package
           properties
           repeat
           jump
           major-modes
           (wk-match-keys t)
           (wk-match-binding t)
           (wk-full-keys t)
           ;; for custom key definers
           lispy-plist
           worf-plist
           &allow-other-keys)
  "The primary key definition function provided by tag.el.

Define MAPS, optionally using DEFINER, in the keymap(s) corresponding to STATES
and KEYMAPS.

MAPS consists of paired keys (vectors or strings; also see
`tag-implicit-kbd') and definitions (those mentioned in `define-key''s
docstring and tag.el's \"extended\" definitions). All pairs (when not
ignored) will be recorded and can be later displayed with
`tag-describe-keybindings'.

If DEFINER is specified, a custom key definer will be used to bind MAPS. See
tag.el's documentation/README for more information.

Unlike with normal key definitions functions, the keymaps in KEYMAPS should be
quoted (this allows using the keymap name for other purposes, e.g. deferring
keybindings if the keymap symbol is not bound, optionally inferring the
corresponding major mode for a symbol by removing \"-map\" for :which-key,
easily storing the keymap name for use with `tag-describe-keybindings',
etc.). Note that tag.el provides other key definer macros that do not
require quoting keymaps.

STATES corresponds to the evil state(s) to bind the keys in. Non-evil users
should not set STATES. When STATES is non-nil, `evil-define-key*' will be
used (the evil auxiliary keymaps corresponding STATES and KEYMAPS will be used);
otherwise `define-key' will be used (unless DEFINER is specified). KEYMAPS
defaults to 'global. There is also 'local, which create buffer-local
keybindings for both evil and non-evil keybindings. There are other special,
user-alterable \"shorthand\" symbols for keymaps and states (see
`tag-keymap-aliases' and `tag-state-aliases').

Note that STATES and KEYMAPS can either be lists or single symbols. If any
keymap does not exist, those keybindings will be deferred until the keymap does
exist, so using `eval-after-load' is not necessary with this function.

PREFIX corresponds to a key to prefix keys in MAPS with and defaults to none. To
bind/unbind a key specified with PREFIX, \"\" can be specified as a key in
MAPS (e.g. ...:prefix \"SPC\" \"\" nil... will unbind space).

The keywords in this paragraph are only useful for evil users. If
NON-NORMAL-PREFIX is specified, this prefix will be used instead of PREFIX for
states in `tag-non-normal-states' (e.g. the emacs and insert states). This
argument will only have an effect if one of these states is in STATES or if
corresponding global keymap (e.g. `evil-insert-state-map') is in KEYMAPS.
Alternatively, GLOBAL-PREFIX can be used with PREFIX and/or NON-NORMAL-PREFIX to
bind keys in all states under the specified prefix. Like with NON-NORMAL-PREFIX,
GLOBAL-PREFIX will prevent PREFIX from applying to `tag-non-normal-states'.
INFIX can be used to append a string to all of the specified prefixes. This is
potentially useful when you are using GLOBAL-PREFIX and/or NON-NORMAL-PREFIX so
that you can sandwich keys in between all the prefixes and the specified keys in
MAPS. This may be particularly useful if you are using default prefixes in a
wrapper function/macro so that you can add to them without needing to re-specify
all of them. If none of the other prefix keyword arguments are specified, INFIX
will have no effect.

If PREFIX-COMMAND or PREFIX-MAP is specified, a prefix command and/or keymap
will be created. PREFIX-NAME can be additionally specified to set the keymap
menu name/prompt. If PREFIX-COMMAND is specified, `define-prefix-command' will
be used. Otherwise, only a prefix keymap will be created. Previously created
prefix commands/keymaps will never be redefined/cleared. All prefixes (including
the INFIX key, if specified) will then be bound to PREFIX-COMMAND or PREFIX-MAP.
If the user did not specify any PREFIX or manually specify any KEYMAPS, tag
will bind all MAPS in the prefix keymap corresponding to either PREFIX-MAP or
PREFIX-COMMAND instead of in the default keymap.

PREDICATE corresponds to a predicate to check to determine whether a definition
should be active (e.g. \":predicate '(eobp)\"). Definitions created with a
predicate will only be active when the predicate is true. When the predicate is
false, key lookup will continue to search for a match in lower-precedence
keymaps.

In addition to the normal definitions supported by `define-key', tag.el also
provides \"extended\" definitions, which are plists containing the normal
definition as well as other keywords. For example, PREDICATE can be specified
globally or locally in an extended definition. New global (~tag-define-key~)
and local (extended definition) keywords can be added by the user. See
`tag-extended-def-keywords' and tag.el's documentation/README for more
information.

PACKAGE is the global version of the extended definition keyword that specifies
the package a keymap is defined in (used for \"autoloading\" keymaps)

PROPERTIES, REPEAT, and JUMP are the global versions of the extended definition
keywords used for adding evil command properties to commands.

MAJOR-MODES, WK-MATCH-KEYS, WK-MATCH-BINDINGS, and WK-FULL-KEYS are the
corresponding global versions of which-key extended definition keywords. They
will only have an effect for extended definitions that specify :which-key or
:wk. See the section on extended definitions in the tag.el
documentation/README for more information.

LISPY-PLIST and WORF-PLIST are the global versions of extended definition
keywords that are used for each corresponding custom DEFINER."
  ;; to silence compiler warning; variables that are later extracted from kargs
  (ignore definer
          predicate
          package
          properties
          repeat
          jump
          major-modes
          lispy-plist
          worf-plist)
  (let ((prefix-def (or prefix-command
                        (when prefix-map
                          (list :keymap prefix-map))))
        non-normal-prefix-maps
        global-prefix-maps
        kargs)
    ;; don't force the user to wrap a single state or keymap in a list
    (tag--ensure-lists states keymaps)
    ;; unalias states and keymaps
    (setq states (mapcar (lambda (state) (tag--unalias state t))
                         states))
    (setq keymaps (mapcar #'tag--unalias keymaps))
    ;; remove keyword arguments from rest var
    (let ((split-maps (tag--remove-keyword-args maps)))
      (setq maps (car split-maps)
            ;; order will be preserved; matters for duplicates
            kargs (append
                   (list
                    ;; should be included even if not manually specified
                    ;; (because have non-nil defaults)
                    :wk-match-keys wk-match-keys
                    :wk-match-binding wk-match-binding
                    :wk-full-keys wk-full-keys
                    ;; so :keymaps and :states are always lists in kargs
                    ;; needed for matching against :major-modes
                    :keymaps keymaps
                    ;; for consistency; may be useful in future or for user
                    :states states)
                   (cadr split-maps))))
    (tag--define-prefix prefix-command prefix-map prefix-name)
    (when (and (or prefix-map prefix-command)
               (not (or prefix keymaps-specified-p)))
      (setq keymaps (list (or prefix-map prefix-command))))
    ;; TODO reduce code duplication here
    (when non-normal-prefix
      (setq non-normal-prefix-maps
            (tag--apply-prefix-and-kbd
             (tag--concat t non-normal-prefix infix)
             (append (when (and prefix prefix-def)
                       (list "" prefix-def))
                     maps))))
    (when global-prefix
      (setq global-prefix-maps
            (tag--apply-prefix-and-kbd
             (tag--concat t global-prefix infix)
             (append (when (and prefix prefix-def)
                       (list "" prefix-def))
                     maps))))
    ;; last so not applying prefix twice
    (setq maps (tag--apply-prefix-and-kbd
                (tag--concat t prefix infix)
                (append (when (and prefix prefix-def)
                          (list "" prefix-def))
                        maps)))
    (dolist (keymap keymaps)
      (dolist (state (or states (list nil)))
        (tag--delay `(or (boundp ',keymap)
                             (and (memq ',keymap '(local global))
                                  (if ',state
                                      ;; this is `evil-state-p'
                                      (and (boundp 'evil-state-properties)
                                           (assq ',state evil-state-properties))
                                    t)))
            `(tag--define-key ',state
                                  ',keymap
                                  ',maps
                                  ',non-normal-prefix-maps
                                  ',global-prefix-maps
                                  ',kargs)
          'after-load-functions t nil
          (symbol-name
           (cl-gensym (format "tag-define-key-in-%s" keymap))))))))

;;;###autoload
(defmacro tag-emacs-define-key (keymaps &rest args)
  "A wrapper for `tag-define-key' that is similar to `define-key'.
It has a positional argument for KEYMAPS (that will not be overridden by a later
:keymaps argument). Besides this, it acts the same as `tag-define-key', and
ARGS can contain keyword arguments in addition to keybindings. This can
basically act as a drop-in replacement for `define-key', and unlike with
`tag-define-key', KEYMAPS does not need to be quoted."
  (declare (indent 1))
  `(tag-define-key
    :keymaps ,(if (and (listp keymaps)
                       (eq (car keymaps) 'quote))
                  `,keymaps
                `',keymaps)
    ,@args))

;;;###autoload
(defmacro tag-evil-define-key (states keymaps &rest args)
  "A wrapper for `tag-define-key' that is similar to `evil-define-key'.
It has positional arguments for STATES and KEYMAPS (that will not be overridden
by a later :keymaps or :states argument). Besides this, it acts the same as
`tag-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for
`evil-define-key', and unlike with `tag-define-key', KEYMAPS does not need
to be quoted."
  (declare (indent 2))
  `(tag-define-key
    :states ,(if (and (listp states)
                      (eq (car states) 'quote))
                 `,states
               `',states)
    :keymaps ,(if (and (listp keymaps)
                       (eq (car keymaps) 'quote))
                  `,keymaps
                `',keymaps)
    ,@args))

(defun tag--positional-arg-p (arg)
  "Return whether ARG is a positional argument for a key definer.
Keyword arguments and strings/vectors are not considered positional arguments."
  (and arg
       (or (symbolp arg) (listp arg))
       (not (keywordp arg))))

;;;###autoload
(defmacro tag-def (&rest args)
  "General definer that takes a variable number of positional arguments in ARGS.
This macro will act as `tag-define-key', `tag-emacs-define-key', or
`tag-evil-define-key' based on how many of the initial arguments do not
correspond to keybindings. All quoted and non-quoted lists and symbols before
the first string, vector, or keyword are considered to be positional arguments.
This means that you cannot use a function or variable for a key that starts
immediately after the positional arguments. If you need to do this, you should
use one of the definers that `tag-def' dispatches to or explicitly separate
the positional arguments from the maps with a bogus keyword pair like
\":start-maps t\""
  (declare (indent defun))
  (let ((pos-args 0))
    (while (tag--positional-arg-p (nth pos-args args))
      (cl-incf pos-args))
    (cl-case pos-args
      (0
       `(tag-define-key ,@args))
      (1
       `(tag-emacs-define-key ,@args))
      (2
       `(tag-evil-define-key ,@args)))))

;;;###autoload
(cl-defmacro tag-create-definer (name &rest defaults &key wrapping
                                          &allow-other-keys)
  "A helper macro to create wrappers for `tag-def'.
This can be used to create key definers that will use a certain keymap, evil
state, prefix key, etc. by default. NAME is the wrapper name and DEFAULTS are
the default arguments. WRAPPING can also be optionally specified to use a
different definer than `tag-def'. It should not be quoted."
  (declare (indent defun))
  (let ((defaults (cl-loop for (key val) on defaults by 'cddr
                           unless (eq key :wrapping)
                           collect key
                           and collect val))
        (definer (or wrapping 'tag-def)))
    `(defmacro ,name (&rest args)
       (declare (indent defun))
       ,(let ((print-quoted t))
          (format
           "A wrapper for `%s'.

It has the following defaults:
%s"
           definer defaults))
       ;; can still override keywords afterwards (first keyword takes precedence)
       `(,',definer
          ,@args ,@',defaults))))

(defun tag--starter-arg-p (arg)
  "Return whether ARG is a keyword or positional argument for a key definer."
  (or (keywordp arg)
      (tag--positional-arg-p arg)))

(defun tag--parse-defs-arglists (args)
  "Parse ARGS to `tag-defs' into a list of `tag-def' arglists.
ARGS is split on \"starter arguments\" as determined by
`tag--starter-arg-p'."
  (let (arglists
        arglist)
    (while args
      (while (and args (tag--starter-arg-p (car args)))
        (when (keywordp (car args))
          (push (pop args) arglist))
        (push (pop args) arglist))
      (while (and args (not (tag--starter-arg-p (car args))))
        (push (pop args) arglist)
        (push (pop args) arglist))
      (push (nreverse arglist) arglists)
      (setq arglist nil))
    (nreverse arglists)))

;;;###autoload
(defmacro tag-defs (&rest args)
  "A wrapper that splits into multiple `tag-def's.
Each consecutive grouping of positional argument followed by keyword/argument
pairs (having only one or the other is fine) marks the start of a new section.
Each section corresponds to one use of `tag-def'. This means that settings
only apply to the keybindings that directly follow.

Since positional arguments can appear at any point, unqouted symbols are always
considered to be positional arguments (e.g. a keymap). This means that variables
can never be used for keys with `tag-defs'. Variables can still be used for
definitions or as arguments to keywords."
  (declare (indent defun)
           (debug [&rest sexp]))
  `(progn
     ,@(mapcar (lambda (arglist)
                 (cons 'tag-def arglist))
               (tag--parse-defs-arglists args))))

;;;###autoload
(cl-defmacro tag-unbind (&rest args)
  "A wrapper for `tag-def' to unbind multiple keys simultaneously.
Insert after all keys in ARGS before passing ARGS to `tag-def.' \":with
 #'func\" can optionally specified to use a custom function instead (e.g.
 `ignore')."
  (declare (indent defun))
  ;; Note: :with can be at an odd position, so must handle internally and not
  ;; with &key
  (let* (with
         (split-args (tag--remove-keyword-args args))
         (kargs (cl-loop for (key val) on (cadr split-args) by 'cddr
                         if (eq key :with)
                         do (setq with val)
                         else
                         collect key
                         and collect val))
         (positional-args-and-maps
          ;; interleave appropriate definitions into maps
          (cl-loop for key in (car split-args)
                   collect key
                   and
                   unless (tag--positional-arg-p key)
                   collect (if (eq with t)
                               nil
                             with)))
         (args (append positional-args-and-maps kargs)))
    `(tag-def ,@args)))

;; * Displaying Keybindings
(defun tag--to-string (x)
  "Convert key vector or symbol X to a string."
  (cond ((vectorp x)
         (key-description x))
        ((symbolp x)
         (symbol-name x))
        (t
         x)))

;; these sorting functions assume x != y (which will hold true for
;; `tag-keybindings')
(defun tag--< (x y)
  "Return t if X is alphabetically less than Y.
Each should be either a string, symbol, or vector. Nil is a special case and is
considered the \"smallest\"."
  (cond ((null x)
         t)
        ((null y)
         nil)
        (t
         (setq x (tag--to-string x)
               y (tag--to-string y))
         (string< x y))))

(defun tag-sort-by-car (list)
  "Sort LIST by comparing the car of each element with `tag--<'."
  (cl-sort list #'tag--< :key #'car))

(defun tag-sort-by-cadr (list)
  "Sort LIST by comparing the cadr of each element with `tag--<'."
  (cl-sort list #'tag--< :key #'cadr))

(defvar tag-describe-evil-states
  '(nil
    insert
    emacs
    hybrid
    normal
    visual
    motion
    operator
    replace)
  "Ordered list of evil states used for `tag--evil-state-<'.")

(defun tag--evil-state-< (x y)
  "Return t if evil state X should come before state Y.
If X and Y are conses, the first element will be compared. Ordering is based on
`tag-describe-evil-states' or the symbol names for states not in the list."
  (let ((xind (cl-position x tag-describe-evil-states))
        (yind (cl-position y tag-describe-evil-states)))
    (cond ((and (null xind)
                (null yind))
           (tag--< x y))
          ((null xind)
           nil)
          ((null yind)
           t)
          (t
           (< xind yind)))))

(defun tag--sort-evil-state-conses (state-conses)
  "Sort STATE-CONSES using `tag--evil-state-<'."
  (cl-sort state-conses #'tag--evil-state-< :key #'car))

(defun tag--print-map (map)
  "Print the keybinding MAP."
  (cl-destructuring-bind (key command previous) map
    (princ (format "|=%.50s=|~%.50s~|~%.50s~|\n"
                   (replace-regexp-in-string "|" "¦" (key-description key))
                   command
                   previous))))

(defun tag--print-maps-table (maps)
  "Print an org table for MAPS."
  (when tag-describe-keybinding-sort-function
    (setq maps (funcall tag-describe-keybinding-sort-function maps)))
  (princ "|key|command|previous|\n|-+-|\n")
  (dolist (map maps)
    (tag--print-map map))
  (princ "\n"))

(defun tag--print-state-heading (state-cons)
  "Print a table and possibly a heading for STATE-CONS."
  (let ((state (car state-cons))
        (maps (cdr state-cons)))
    (unless (null state)
      (princ (capitalize (concat "** " (symbol-name state) " State\n"))))
    (tag--print-maps-table maps)))

(defun tag--print-keymap-heading (keymap-cons)
  "Print headings and tables for KEYMAP-CONS."
  (let ((keymap (car keymap-cons))
        (state-conses (cdr keymap-cons)))
    (princ (capitalize (concat "* " (symbol-name keymap) " Keybindings\n")))
    (when tag-describe-state-sort-function
      (setq state-conses (funcall tag-describe-state-sort-function
                                  state-conses)))
    (dolist (state-cons state-conses)
      (tag--print-state-heading state-cons))))

(declare-function org-at-heading-p "org")
(declare-function org-table-align "org-table")
(declare-function outline-next-heading "outline")
(defvar org-startup-folded)
;;;###autoload
(defun tag-describe-keybindings (&optional arg)
  "Show all keys that have been bound with tag in an org buffer.
Any local keybindings will be shown first followed by global keybindings.
With a non-nil prefix ARG only show bindings in active maps."
  (interactive "P")
  (with-output-to-temp-buffer "*General Keybindings*"
    (let* ((keybindings (append
                         (copy-alist tag-keybindings)
                         (list (cons 'local tag-local-keybindings))))
           (active-maps (current-active-maps)))
      ;; print prioritized keymaps first (if any)
      (dolist (keymap tag-describe-priority-keymaps)
        (let ((keymap-cons (assq keymap keybindings)))
          (when (and keymap-cons
                     (or (null arg)
                         (and (boundp (car keymap-cons))
                              (memq (symbol-value (car keymap-cons))
                                    active-maps))))
            (tag--print-keymap-heading keymap-cons)
            (setq keybindings (assq-delete-all keymap keybindings)))))
      ;; sort the remaining and then print them
      (when tag-describe-keymap-sort-function
        (setq keybindings (funcall tag-describe-keymap-sort-function
                                   keybindings)))
      (dolist (keymap-cons keybindings)
        (when (or (null arg)
                  (and (boundp (car keymap-cons))
                       (memq (symbol-value (car keymap-cons)) active-maps)))
          (tag--print-keymap-heading keymap-cons)))))
  (with-current-buffer "*General Keybindings*"
    (let ((org-startup-folded 'showall))
      (org-mode))
    (read-only-mode -1)
    (while (progn
             (while (progn
                      (forward-line)
                      (org-at-heading-p)))
             (org-table-align)
             (outline-next-heading)))
    (goto-char (point-min))
    (read-only-mode)))

;; * Functions/Macros to Aid Key Definition
;; ** Helpers
(cl-defun tag--call-interactively
    (function &optional (remap t) record-flag keys)
  "Like `call-interactively' but use the remapped FUNCTION if it exists.
If REMAP is specified as nil (it is true by default), this is the same as
`call-interactively'. FUNCTION, RECORD-FLAG, and KEYS are passed to
`call-interactively'."
  (when remap
    (setq function (or (key-binding (kbd (format "<remap> <%s>" function)))
                       function)))
  (call-interactively function record-flag keys))

;; ** Key Simulation
;; https://emacs.stackexchange.com/questions/6037/emacs-bind-key-to-prefix/13432#13432
;; altered to
;; - allow execution in an arbitrary state and keymap
;; - create a named function with a docstring
;; - optionally dynamically lookup the key(s) up in the correct keymap to try to
;;   match a command to execute instead
;; - handle more edge cases like correctly working with macros/repeating

;; TODO
;; - rename keys arguments to key for consistency with builtin functions

(declare-function evil-change-state "evil-core")
(defvar evil-no-display)
(defvar evil-state)
(defvar evil-previous-state)
(defvar evil-previous-state-alist)
(defvar evil-next-state)
(defmacro tag--save-state (&rest body)
  "Save the current state; execute BODY; restore the state.
This is a combination of `evil-without-display' and `evil-save-state'. It is
necessary to define this directly in tag so that it is available when
tag is compiled (as evil is an optional dependency and may not be installed
when tag is compiled)."
  (declare (indent defun)
           (debug t))
  `(let* ((evil-no-display t)
          (evil-state evil-state)
          (evil-previous-state evil-previous-state)
          (evil-previous-state-alist (copy-tree evil-previous-state-alist))
          (evil-next-state evil-next-state)
          (old-state evil-state)
          (inhibit-quit t)
          (buf (current-buffer)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (evil-change-state old-state))))))

;;;###autoload
(cl-defmacro tag-key (key &key
                              state
                              docstring
                              let
                              setup
                              teardown
                              accept-default
                              no-remap
                              position)
  "Act as KEY's definition in the current context.
This uses an extended menu item's capability of dynamically computing a
definition. It is recommended over `tag-simulate-key' wherever possible. See
the docstring of `tag-simulate-key' and the readme for information about the
benefits and downsides of `tag-key'.

KEY should be a string given in `kbd' notation and should correspond to a single
definition (as opposed to a sequence of commands). When STATE is specified, look
up KEY with STATE as the current evil state. When specified, DOCSTRING will be
the menu item's name/description.

Let can be used to bind variables around key lookup. For example:
(tag-key \"some key\"
  :let ((some-var some-val)))

SETUP and TEARDOWN can be used to run certain functions before and after key
lookup. For example, something similar to using :state 'emacs would be:
(tag-key \"some key\"
  :setup (evil-local-mode -1)
  :teardown (evil-local-mode))

ACCEPT-DEFAULT, NO-REMAP, and POSITION are passed to `key-binding'."
  (declare (indent 1))
  `'(menu-item
     ,(or docstring "")
     nil
     :filter
     (lambda (&optional _)
       (let ,let
         ,setup
         (prog1
             ,(if state
                  `(tag--save-state
                     (evil-change-state ,state)
                     (key-binding (tag--kbd ,key) ,accept-default ,no-remap
                                  ,position))
                `(key-binding (tag--kbd ,key) ,accept-default ,no-remap
                              ,position))
           ,teardown)))))

(defvar tag--last-simulated-command nil
  "Holds the last simulated command (or nil for incomplete key sequence).")

(defvar tag--simulate-next-as-is nil
  "Whether to fake keys unconditionally in the next `tag--simulate-keys'.
This is used for testing (but could potentially be useful for a user). Since
`tag--simulate-keys' will normally assume it is being run inside a macro
that was manually recorded, this is needed when executing a keyboard macro that
ends up running `tag--simulate-keys' for the first time.")

(defvar tag--simulate-as-is nil
  "Whether to fake the keys unconditionally in any `tag--simulate-keys'.")

(defun tag--key-binding (keys &optional state keymap)
  "Look up KEYS in the keymap corresponding to STATE and/or KEYMAP.
Continually check whether subsequences of KEYS are bound to a command or keymap
starting with the full KEYS and ending when a match is found or no subsequences
remain. Unlike `lookup-key' if KEYS is not matched, fall back to checking with
`key-binding'. If STATE is specified and KEYMAP is not, temporarily switch to
STATE to look up the keys (this means that keybindings inherited from a
different evil state can still be detected). Return a list of the match and the
leftover keys (or nil if the full KEYS was matched)."
  (let* ((keymap (when keymap
                   (tag--get-keymap state keymap)))
         (len (length keys))
         (ind len)
         match)
    (while (and (> ind 0) (not match))
      (let* ((key (substring keys 0 ind))
             (result (cond (keymap
                            (or (lookup-key keymap key)
                                (key-binding key)))
                           (state
                            ;; this also works fine when evil-local-mode is off
                            (tag--save-state
                              (evil-change-state state)
                              (key-binding key)))
                           (t
                            (key-binding key)))))
        (if (or (commandp result)
                (keymapp result))
            (setq match result)
          (cl-decf ind))))
    (list match
          (if (= ind len)
              nil
            (substring keys ind len)))))

(declare-function evil-echo "evil-common")
(declare-function evil-visual-state-p "evil-common" t t)
(declare-function evil-emacs-state "evil-states" t t)
(defvar evil-move-cursor-back)
(defun tag--execute-in-state (state &optional delay-revert)
  "Execute the next command in STATE.
This is an altered version of `evil-execute-in-normal-state' and
`evil-execute-in-emacs-state'. When calling this in a command, specify
DELAY-REVERT as non-nil to prevent switching the state back until after
`this-command' is finished."
  (interactive)
  (let ((ignore-commands '(evil-use-register
                           digit-argument
                           negative-argument
                           universal-argument
                           universal-argument-minus
                           universal-argument-more
                           universal-argument-other-key)))
    (when delay-revert
      (push this-command ignore-commands))
    (tag--delay `(not (memq this-command ',ignore-commands))
        `(progn
           (with-current-buffer ,(current-buffer)
             (evil-change-state ',evil-state)
             (setq evil-move-cursor-back ',evil-move-cursor-back)))
      'post-command-hook))
  (if (and (eq state 'emacs)
           (evil-visual-state-p))
      (let ((mrk (mark))
            (pnt (point)))
        (evil-emacs-state)
        (set-mark mrk)
        (goto-char pnt))
    (setq evil-move-cursor-back nil)
    (evil-change-state state)))

(cl-defun tag--simulate-keys (command keys &optional state keymap
                                          (lookup t)
                                          (remap t))
  "Simulate COMMAND followed by KEYS in STATE and/or KEYMAP.
If COMMAND is nil, just simulate KEYS. If STATE and KEYMAP are nil, simulate
KEYS in the current context. When COMMAND is non-nil, STATE and KEYMAP will have
no effect. KEYS should be a string that can be passed to `kbd' or nil. If KEYS
is nil, the COMMAND will just be called interactively. If COMMAND is nil and
LOOKUP is non-nil, KEYS will be looked up in the correct context to determine if
any subsequence corresponds to a command or keymap. If a command is matched,
that command will be called followed by the simulation of any leftover keys. To
simulate the keys as-is without any lookup, LOOKUP can be explicitly specified
as nil. When COMMAND has been remapped (i.e. [remap COMMAND] is currently
bound), the remapped version will be used instead of the original command unless
REMAP is specified as nil (it is true by default)."
  (let* ((keys (when keys
                 (tag--kbd keys)))
         ;; TODO remove when get rid of `tag-simulate-keys'
         (state (if (eq state t)
                    'emacs
                  state)))
    (when (and lookup (null command))
      (cl-destructuring-bind (match leftover-keys)
          (tag--key-binding keys state keymap)
        (when (commandp match)
          (setq command match
                keys leftover-keys))))
    (when keys
      ;; only set prefix-arg when there are only keys (otherwise will also
      ;; affect the next command)
      (unless command
        (setq prefix-arg current-prefix-arg)
        (when state
          (tag--execute-in-state state t)))
      (when (or tag--simulate-as-is
                tag--simulate-next-as-is
                (not executing-kbd-macro))
        (setq tag--simulate-next-as-is nil)
        ;; keys are incorrectly saved as this-command-keys when recording macros
        ;; these keys will be played back, so don't simulate them
        (setq unread-command-events
              (nconc
               ;; force keys to be added to this-command-keys
               ;; this happens normally already for macros but it needs to be
               ;; forced for evil-repeat though, which will only include the
               ;; first key otherwise (ideally no keys would ever be added in
               ;; either case)
               (mapcar (lambda (ev) (cons t ev))
                       (listify-key-sequence keys))
               unread-command-events))))
    (when command
      (let ((this-command command))
        (tag--call-interactively command remap)))
    (setq tag--last-simulated-command command)))

;;;###autoload
(cl-defmacro tag-simulate-keys (keys &optional state keymap
                                         (lookup t)
                                         docstring name)
  "Deprecated. Please use `tag-simulate-key' instead."
  (let* ((command (when (listp keys)
                    (car keys)))
         (keys (if (listp keys)
                   (cadr keys)
                 keys))
         (state (if (eq state t)
                    ''emacs
                  state))
         (name (or name
                   (intern (concat
                            (format "tag-simulate-%s"
                                    (if command
                                        (eval command)
                                      ""))
                            (when command
                              "-")
                            (replace-regexp-in-string " " "_" keys)
                            (when state
                              (concat "-in-"
                                      (symbol-name (eval state))
                                      "-state"))
                            (when keymap
                              (concat "-in-"
                                      (symbol-name keymap))))))))
    `(progn
       (eval-after-load 'evil
         '(evil-set-command-property #',name :repeat 'tag--simulate-repeat))
       (defun ,name
           ()
         ,(or docstring
              (concat "Simulate "
                      (when command
                        (concat "`"
                                (symbol-name (eval command))
                                "' then "))
                      "'"
                      keys
                      "' in "
                      (cond ((and state keymap)
                             (concat (symbol-name (eval state))
                                     " state in `"
                                     (symbol-name keymap)
                                     "'."))
                            (keymap
                             (concat (symbol-name keymap)
                                     "."))
                            (state
                             (concat (symbol-name (eval state))
                                     " state."))
                            (t
                             "the current context."))))
         (interactive)
         (tag--simulate-keys ,command ,keys ,state ,keymap ,lookup)))))
(make-obsolete 'tag-simulate-keys 'tag-simulate-key "2018-01-14")

;;;###autoload
(cl-defmacro tag-simulate-key (keys
                                   &key
                                   state keymap
                                   name docstring
                                   (lookup t)
                                   which-key
                                   (remap t))
  "Create and return a command that simulates KEYS in STATE and KEYMAP.

`tag-key' should be prefered over this whenever possible as it is simpler
and has saner functionality in many cases because it does not rely on
`unread-command-events' (e.g. \"C-h k\" will show the docstring of the command
to be simulated ; see the readme for more information). The main downsides of
`tag-key' are that it cannot simulate a command followed by keys or
subsequent commands, and which-key does not currently work well with it when
simulating a prefix key/incomplete key sequence.

KEYS should be a string given in `kbd' notation. It can also be a list of a
single command followed by a string of the key(s) to simulate after calling that
command. STATE should only be specified by evil users and should be a quoted
evil state. KEYMAP should not be quoted. Both STATE and KEYMAP aliases are
supported (but they have to be set when the macro is expanded). When neither
STATE or KEYMAP are specified, the key(s) will be simulated in the current
context.

If NAME is specified, it will replace the automatically generated function name.
NAME should not be quoted. If DOCSTRING is specified, it will replace the
automatically generated docstring.

Normally the generated function will look up KEY in the correct context to try
to match a command. To prevent this lookup, LOOKUP can be specified as nil.
Generally, you will want to keep LOOKUP non-nil because this will allow checking
the evil repeat property of matched commands to determine whether or not they
should be recorded. See the docstring for `tag--simulate-keys' for more
information about LOOKUP.

When a WHICH-KEY description is specified, it will replace the command name in
the which-key popup.

When a command name is specified and that command has been remapped (i.e. [remap
command] is currently bound), the remapped version will be used instead of the
original command unless REMAP is specified as nil (it is true by default).

The advantages of this over a keyboard macro are as follows:
- Prefix arguments are supported
- The user can control the context in which the keys are simulated
- The user can simulate both a named command and keys
- The user can simulate an incomplete key sequence (e.g. for a keymap)"
  (declare (indent defun))
  (let* ((command (when (listp keys)
                    (car keys)))
         (keys (if (listp keys)
                   (cadr keys)
                 keys))
         (state (tag--unalias (eval state) t))
         (keymap (tag--unalias keymap))
         (name (or name
                   (intern (concat
                            (format "tag-simulate-%s"
                                    (if command
                                        (eval command)
                                      ""))
                            (when command
                              "-")
                            (replace-regexp-in-string " " "_" keys)
                            (when state
                              (concat "-in-"
                                      (symbol-name state)
                                      "-state"))
                            (when keymap
                              (concat "-in-"
                                      (symbol-name keymap))))))))
    `(progn
       (eval-after-load 'evil
         '(evil-set-command-property #',name :repeat 'tag--simulate-repeat))
       (when ,which-key
         (tag-with-eval-after-load 'which-key
           (push '((nil . ,(symbol-name name))
                   nil . ,which-key)
                 which-key-replacement-alist)))
       (defun ,name
           ()
         ,(or docstring
              (concat "Simulate "
                      (when command
                        (concat "`"
                                (symbol-name (eval command))
                                "' then "))
                      "'"
                      keys
                      "' in "
                      (cond ((and state keymap)
                             (concat (symbol-name state)
                                     " state in `"
                                     (symbol-name keymap)
                                     "'."))
                            (keymap
                             (concat (symbol-name keymap)
                                     "."))
                            (state
                             (concat (symbol-name state)
                                     " state."))
                            (t
                             "the current context."))))
         (interactive)
         (tag--simulate-keys ,command ,keys ',state ,keymap ,lookup ,remap))
       #',name)))

(defun tag--repeat-abort-p (repeat-prop)
  "Return t if repeat recording should be aborted based on REPEAT-PROP."
  (or (memq repeat-prop (list nil 'abort 'ignore))
      (and (eq repeat-prop 'motion)
           (not (memq evil-state '(insert replace))))))

(declare-function evil-repeat-record "evil-repeat")
(declare-function evil-get-command-property "evil-common")
(declare-function evil-repeat-abort "evil-repeat")
(declare-function evil-this-command-keys "evil-repeat")
(declare-function evil-clear-command-keys "evil-repeat")
(defvar evil-this-register)
(defun tag--simulate-repeat (flag)
  "Modified version of `evil-repeat-keystrokes'.
It behaves as normal but will check the repeat property of a simulated command
to determine whether to abort recording."
  (cond ((eq flag 'pre)
         (when evil-this-register
           (evil-repeat-record
            `(set evil-this-register ,evil-this-register))))
        ((eq flag 'post)
         (let* ((command tag--last-simulated-command)
                (repeat-prop (evil-get-command-property command :repeat t)))
           (if (and command (tag--repeat-abort-p repeat-prop))
               (evil-repeat-abort)
             (evil-repeat-record
              (evil-this-command-keys t))
             (evil-clear-command-keys))))))

;; ** Key Dispatch
(defvar tag--last-dispatch-command nil
  "Holds the last command run from a `tag-key-dispatch' function.")

(defun tag--extend-key-sequence (keys)
  "Read a key and append it to KEYS.
KEYS should be a string given in `kbd' notation."
  (let ((key (read-event)))
    (concat keys
            (when keys
              " ")
            (key-description (if (characterp key)
                                 (char-to-string key)
                               (vector key))))))

;;;###autoload
(cl-defmacro tag-key-dispatch
    (fallback-command &rest maps
                      &key
                      timeout
                      inherit-keymap
                      name docstring
                      which-key
                      (remap t)
                      &allow-other-keys)
  "Create and return a command that runs FALLBACK-COMMAND or a command in MAPS.
MAPS consists of <key> <command> pairs. If a key in MAPS is matched, the
corresponding command will be run. Otherwise FALLBACK-COMMAND will be run with
the unmatched keys. So, for example, if \"ab\" was pressed, and \"ab\" is not
one of the key sequences from MAPS, the FALLBACK-COMMAND will be run followed by
the simulated keypresses of \"ab\". Prefix arguments will still work regardless
of which command is run. This is useful for binding under non-prefix keys. For
example, this can be used to redefine a sequence like \"cw\" or \"cow\" in evil
but still have \"c\" work as `evil-change'. If TIMEOUT is specified,
FALLBACK-COMMAND will also be run in the case that the user does not press the
next key within the TIMEOUT (e.g. 0.5).

NAME and DOCSTRING are optional keyword arguments. They can be used to replace
the automatically generated name and docstring for the created function. By
default, `cl-gensym' is used to prevent name clashes (e.g. allows the user to
create multiple different commands using `self-insert-command' as the
FALLBACK-COMMAND without explicitly specifying NAME to manually prevent
clashes).

When INHERIT-KEYMAP is specified, all the keybindings from that keymap will be
inherited in MAPS.

When a WHICH-KEY description is specified, it will replace the command name in
the which-key popup.

When command to be executed has been remapped (i.e. [remap command] is currently
bound), the remapped version will be used instead of the original command unless
REMAP is specified as nil (it is true by default)."
  (declare (indent 1))
  (let ((name (or name (cl-gensym (format "tag-dispatch-%s-"
                                          (eval fallback-command)))))
        ;; remove keyword arguments from maps
        (maps (car (tag--remove-keyword-args maps))))
    `(progn
       (eval-after-load 'evil
         '(evil-set-command-property #',name :repeat 'tag--dispatch-repeat))
       (when ,which-key
         (tag-with-eval-after-load 'which-key
           (push '((nil . ,(symbol-name name))
                   nil . ,which-key)
                 which-key-replacement-alist)))
       ;; TODO list all of the bound keys in the docstring
       (defun ,name ()
         ,(or docstring (format (concat "Run %s or something else based"
                                        "on the next keypresses.")
                                (eval fallback-command)))
         (interactive)
         (let ((map (make-sparse-keymap))
               (maps (list ,@maps))
               (invoked-keys (this-command-keys))
               (timeout ,timeout)
               (inherit-keymap ,inherit-keymap)
               matched-command
               fallback
               char
               timed-out-p)
           (when inherit-keymap
             (set-keymap-parent map inherit-keymap))
           (while maps
             (define-key map (tag--kbd (pop maps)) (pop maps)))
           (while (progn
                    (if timeout
                        (with-timeout (timeout (setq timed-out-p t))
                          ;; TODO rename char
                          (setq char (tag--extend-key-sequence char)))
                      (setq char (tag--extend-key-sequence char)))
                    (and (not timed-out-p)
                         (keymapp (lookup-key map (kbd char))))))
           (cond
            ((and (not timed-out-p)
                  (setq matched-command (lookup-key map (kbd char))))
             ;; necessary for evil-this-operator checks because
             ;; evil-define-operator sets evil-this-operator to this-command
             (let ((this-command matched-command))
               (tag--call-interactively matched-command ,remap)))
            (t
             (setq matched-command ,fallback-command)
             (tag--simulate-keys ,fallback-command char
                                     nil nil t ,remap)))
           (setq tag--last-dispatch-command matched-command)))
       #',name)))

(defun tag--dispatch-repeat (flag)
  "Modified version of `evil-repeat-keystrokes'.
It behaves as normal but will check the repeat property of a simulated command
to determine whether to abort recording."
  (cond ((eq flag 'pre)
         (when evil-this-register
           (evil-repeat-record
            `(set evil-this-register ,evil-this-register))))
        ((eq flag 'post)
         (let ((repeat-prop (evil-get-command-property
                             tag--last-dispatch-command
                             :repeat t)))
           (if (tag--repeat-abort-p repeat-prop)
               (evil-repeat-abort)
             (evil-repeat-record (evil-this-command-keys t))
             (evil-clear-command-keys))))))

;; ** Predicate Dispatch
;;;###autoload
(cl-defmacro tag-predicate-dispatch
    (fallback-def &rest defs
                  &key docstring
                  &allow-other-keys)
  (declare (indent 1))
  "Create a menu item that will run FALLBACK-DEF or a definition from DEFS.
DEFS consists of <predicate> <definition> pairs. Binding this menu-item to a key
will cause that key to act as the corresponding definition (a command, keymap,
etc) for the first matched predicate. If no predicate is matched FALLBACK-DEF
will be run. When FALLBACK-DEF is nil and no predicates are matched, the keymap
with the next highest precedence for the pressed key will be checked. DOCSTRING
can be specified as a description for the menu item."
  ;; remove keyword arguments from defs and sort defs into pairs
  (let ((defs (cl-loop for (key value) on defs by 'cddr
                       unless (keywordp key)
                       collect (list key value))))
    `'(menu-item
       ,(or docstring "") nil
       :filter (lambda (&optional _)
                 (cond ,@(mapcar (lambda (pred-def)
                                   `(,(car pred-def) ,(cadr pred-def)))
                                 defs)
                       (t ,fallback-def))))))

;; ** Key "Translation"
;;;###autoload
(cl-defun tag-translate-key (states keymaps
                                        &rest maps
                                        &key destructive
                                        &allow-other-keys)
  "Translate keys in the keymap(s) corresponding to STATES and KEYMAPS.
STATES should be the name of an evil state, a list of states, or nil. KEYMAPS
should be a symbol corresponding to the keymap to make the translations in or a
list of keymap names. Keymap and state aliases are supported (as well as 'local
and 'global for KEYMAPS).

MAPS corresponds to a list of translations (key replacement pairs). For example,
specifying \"a\" \"b\" will bind \"a\" to \"b\"'s definition in the keymap.
Specifying nil as a replacement will unbind a key.

If DESTRUCTIVE is non-nil, the keymap will be destructively altered without
creating a backup. If DESTRUCTIVE is nil, store a backup of the keymap on the
initial invocation, and for future invocations always look up keys in the
original/backup keymap. On the other hand, if DESTRUCTIVE is non-nil, calling
this function multiple times with \"a\" \"b\" \"b\" \"a\", for example, would
continue to swap and unswap the definitions of these keys. This means that when
DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation.

If both MAPS and DESCTRUCTIVE are nil, only create the backup keymap."
  (declare (indent defun))
  (tag--ensure-lists states keymaps)
  (dolist (keymap-name keymaps)
    (dolist (state states)
      (setq keymap-name (tag--unalias keymap-name)
            state (tag--unalias state t))
      (let* ((keymap (tag--get-keymap state keymap-name))
             (backup-keymap (intern (format "tag-%s%s-backup-map"
                                            keymap-name
                                            (if state
                                                (format "-%s-state" state)
                                              ""))))
             (lookup-keymap (if (and (not destructive)
                                     (boundp backup-keymap))
                                (symbol-value backup-keymap)
                              (copy-keymap keymap)))
             (maps (cl-loop for (key replacement) on maps by 'cddr
                            ;; :destructive can be in MAPS
                            unless (keywordp key)
                            collect (tag--kbd key)
                            and collect (if replacement
                                            (lookup-key
                                             lookup-keymap
                                             (tag--kbd replacement))
                                          ;; unbind
                                          nil))))
        (unless (or destructive
                    (boundp backup-keymap))
          (set backup-keymap lookup-keymap))
        (apply #'tag-define-key :states state :keymaps keymap-name maps)))))

;;;###autoload
(defmacro tag-swap-key (states keymaps &rest args)
  "Wrapper around `tag-translate-key' for swapping keys.
STATES, KEYMAPS, and ARGS are passed to `tag-translate-key'. ARGS should
consist of key swaps (e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\" \"a\"
with `tag-translate-key') and optionally keyword arguments for
`tag-translate-key'."
  (declare (indent defun))
  (setq args (cl-loop for (key replacement) on args by 'cddr
                      collect key and collect replacement
                      and unless (keywordp key)
                      collect replacement and collect key))
  `(tag-translate-key ,states ,keymaps ,@args))

;; ** Automatic Key Unbinding
(defun tag-unbind-non-prefix-key (define-key keymap key def)
  "Use DEFINE-KEY in KEYMAP to unbind an existing non-prefix subsequence of KEY.
When a tag key definer is in use and a subsequnece of KEY is already bound
in KEYMAP, unbind it using DEFINE-KEY. Always bind KEY to DEF using DEFINE-KEY."
  (when tag--definer-p
    (let ((key (if (stringp key)
                   (string-to-vector key)
                 key)))
      (while (numberp (lookup-key keymap key))
        (setq key (cl-subseq key 0 -1)))
      (funcall define-key keymap key nil)))
  (funcall define-key keymap key def))

;;;###autoload
(defun tag-auto-unbind-keys (&optional undo)
  "Advise `define-key' to automatically unbind keys when necessary.
This will prevent errors when a sub-sequence of a key is already bound (e.g. the
user attempts to bind \"SPC a\" when \"SPC\" is bound, resulting in a \"Key
sequnce starts with non-prefix key\" error). When UNDO is non-nil, remove
advice."
  (if undo
      ;; using tag versions in case start recording advice for later display
      (tag-advice-remove 'define-key #'tag-unbind-non-prefix-key)
    (tag-advice-add 'define-key :around #'tag-unbind-non-prefix-key)))

;; ** Interactive Lambdas
(defmacro tag-lambda (&rest body)
  "Wrap BODY in an interactive lamba"
  (declare (indent defun))
  `(lambda () (interactive)
     ,@body))

(defalias 'tag-l #'tag-lambda)

;; * Functions/Macros to Aid Other Configuration
;; ** Settings
(defmacro tag-setq (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'.
Like `setq', multiple variables can be set at once; SETTINGS should consist of
variable value pairs. Some variables have a custom setter (specified with
`defcustom' and :set) that is used to run code necessary for changes to take
effect (e.g. `auto-revert-interval'). If a package has already been loaded, and
the user uses `setq' to set one of these variables, the :set code will not
run (e.g. in the case of `auto-revert-interval', the timer will not be updated).
Like with `customize-set-variable', `tag-setq' will use the custom :set
setter when it exists. If the package defining the variable has not yet been
loaded, the custom setter will not be known, but it will still be run upon
loading the package. Unlike `customize-set-variable', `tag-setq' does not
attempt to load any dependencies for the variable and does not support giving
variables comments. It also falls back to `set' instead of `set-default', so
that like `setq' it will change the local value of a buffer-local variable
instead of the default value."
  `(progn
     ,@(cl-loop for (var val) on settings by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set)
                                  ',var ,val))))

(defmacro tag-setq-default (&rest settings)
  "An alias for `setq-default'.
In the future, this will automatically record user settings using annalist.el."
  `(setq-default ,@settings))

(defalias 'tag-setq-local #'setq-local
  "An alias for `setq-local'.
In the future, this will automatically record user settings using annalist.el.")

(defmacro tag-pushnew (x place &rest keys)
  "Call `cl-pushnew' with X, PLACE, and KEYS.
:test defaults to `equal'. In the future, this will automatically record user
settings using annalist.el and call a variables :set function."
  (declare (debug
            (form place &rest
                  &or [[&or ":test" ":test-not" ":key"] function-form]
                  [keywordp form])))
  `(cl-pushnew ,x ,place ,@keys :test #'equal))

;; ** Hooks
;; using a function instead of a macro in order to keep the original function
;; name as a prefix (can pass in variable for function)
(defun tag--define-transient-function (function hook &optional advice
                                                    condition)
  "Define and return a modified FUNCTION that removes itself from HOOK.
The new function will automatically remove itself from HOOK after the first time
it is called. If ADVICE is non-nil, HOOK should specify a function to advise
instead. If CONDITION is a function, only remove the function if calling
CONDITION on the return value returns true. For example, if CONDITION is
#'identity, only remove the function if it returns non-nil."
  (let ((name (intern (format "tag--transient-%s%s%s"
                              (if (symbolp function)
                                  (symbol-name function)
                                ;; lambda; name with counter
                                (cl-incf tag--counter))
                              (if advice
                                  "-for-advice"
                                "-for-hook")
                              (if (functionp condition)
                                  (if (symbolp function)
                                      (format "-on-%s" condition)
                                    ;; lambda; name with counter
                                    (format "-on-lambda-%s"
                                            (cl-incf tag--counter)))
                                "")))))
    (defalias name
      (if advice
          (lambda (&rest args)
            (let ((res (apply function args)))
              (when (or (not (functionp condition)) (funcall condition res))
                (advice-remove hook name)
                (fmakunbound name))
              res))
        (lambda (&rest args)
          (let ((res (apply function args)))
            (when (or (not (functionp condition)) (funcall condition res))
              (remove-hook hook name)
              (fmakunbound name))
            res)))
      (format "Call %s with ARGS and then remove it from `%s'%s."
              (if (symbolp function)
                  (format "`%s'" function)
                ;; TODO put full lambda in docstring or use backquote instead of
                ;; relying on lexical-binding (so full lambda is in definition)
                "given lambda")
              hook
              (if (functionp condition)
                  (format " once calling %s on the return value succeeds."
                          (if (symbolp condition)
                              condition
                            "given lambda"))
                "")))
    name))

;;;###autoload
(defun tag-add-hook (hooks functions &optional append local transient)
  "A drop-in replacement for `add-hook'.
Unlike `add-hook', HOOKS and FUNCTIONS can be single items or lists. APPEND and
LOCAL are passed directly to `add-hook'. When TRANSIENT is non-nil, each
function will remove itself from the hook it is in after it is run once. If
TRANSIENT is a function, call it on the return value in order to determine
whether to remove a function from the hook. For example, if TRANSIENT is
#'identity, remove each function only if it returns non-nil. TRANSIENT could
alternatively check something external and ignore the function's return value."
  (tag--ensure-lists hooks functions)
  (dolist (hook hooks)
    (dolist (func functions)
      (when transient
        (setq func (tag--define-transient-function
                    func hook nil transient)))
      (add-hook hook func append local))))

;;;###autoload
(defun tag-remove-hook (hooks functions &optional local)
  "A drop-in replacement for `remove-hook'.
Unlike `remove-hook', HOOKS and FUNCTIONS can be single items or lists. LOCAL is
passed directly to `remove-hook'."
  (tag--ensure-lists hooks functions)
  (dolist (hook hooks)
    (dolist (func functions)
      (remove-hook hook func local))))

;; ** Advice
;;;###autoload
(defun tag-advice-add (symbols where functions &optional props transient)
  "A drop-in replacement for `advice-add'.
SYMBOLS, WHERE, FUNCTIONS, and PROPS correspond to the arguments for
`advice-add'. Unlike `advice-add', SYMBOLS and FUNCTIONS can be single items or
lists. When TRANSIENT is non-nil, each function will remove itself as advice
after it is run once. If TRANSIENT is a function, call it on the return value in
order to determine whether to remove a function as advice. For example, if
TRANSIENT is #'identity, remove each function only if it returns non-nil.
TRANSIENT could alternatively check something external and ignore the function's
return value."
  (tag--ensure-lists symbols functions)
  (dolist (symbol symbols)
    (dolist (func functions)
      (when transient
        (setq func (tag--define-transient-function
                    func symbol t transient)))
      (advice-add symbol where func props))))

;; specify full autoload to prevent function indirection (autoload generation
;; will put a /flipped/ defalias into the autoloads file causing an infinite
;; loop)
;;;###autoload (autoload 'tag-add-advice "tag")
(defalias 'tag-add-advice #'tag-advice-add)

;;;###autoload
(defun tag-advice-remove (symbols functions)
  "A drop-in replacement for `advice-remove'.
Unlike `advice-remove', SYMBOLS and FUNCTIONS can be single items or lists."
  (tag--ensure-lists symbols functions)
  (dolist (symbol symbols)
    (dolist (func functions)
      (advice-remove symbol func))))

;;;###autoload (autoload 'tag-remove-advice "tag")
(defalias 'tag-remove-advice #'tag-advice-remove)

;; ** Packages
(defvar tag-package nil
  "Holds the current package name.
This variable is used to automatically associated recorded keybindings,
settings, etc. with a package.")

(defmacro tag-with-package (package &rest body)
  "After PACKAGE is loaded, run BODY.
This is a small wrapper around `with-eval-after-load' that sets
`tag-package', so that tag commands that record information can
automatically record the package as well. It is meant to be used in addition to
`use-package' in cases where the user has a lot of configuration for a package
and wants to split it up into sections instead of putting it all inside a single
`use-package' statement."
  (declare (indent 1) (debug t))
  `(let ((tag-package ,package))
     (tag-with-eval-after-load ,package
       ,@body)))

(defalias 'tag-with #'tag-with-package)

;; ** Miscellaneous
(defmacro tag-after-gui (&rest body)
  "Run BODY once after the first GUI frame is created."
  (declare (indent 0) (debug t))
  `(if (and (not (daemonp)) (display-graphic-p))
       (progn ,@body)
     (tag-add-hook 'server-after-make-frame-hook
                       (lambda ()
                         (when (display-graphic-p)
                           ,@body
                           t))
                       nil
                       nil
                       #'identity)))

(defmacro tag-after-tty (&rest body)
  "Run BODY once after the first terminal frame is created."
  (declare (indent 0) (debug t))
  `(if (and (not (daemonp)) (not (display-graphic-p)))
       (progn ,@body)
     (tag-add-hook 'server-after-make-frame-hook
                       (lambda ()
                         (unless (display-graphic-p)
                           ,@body
                           t))
                       nil
                       nil
                       #'identity)))

(defmacro tag-after-init (&rest body)
  "Run BODY after emacs initialization.
If after emacs initialization already, run BODY now."
  (declare (indent 0) (debug t))
  `(if after-init-time
       (progn ,@body)
     (tag-add-hook 'after-init-hook (lambda () ,@body))))

;; * Optional Setup

;;;###autoload
(defun tag-evil-setup (&optional short-names _)
  "Set up some basic equivalents for vim mapping functions.
This creates global key definition functions for the evil states.
Specifying SHORT-NAMES as non-nil will create non-prefixed function
aliases such as `nmap' for `tag-nmap'."
  (tag-create-definer tag-imap :states 'insert)
  (tag-create-definer tag-emap :states 'emacs)
  (tag-create-definer tag-nmap :states 'normal)
  (tag-create-definer tag-vmap :states 'visual)
  (tag-create-definer tag-mmap :states 'motion)
  (tag-create-definer tag-omap :states 'operator)
  (tag-create-definer tag-rmap :states 'replace)
  (tag-create-definer tag-iemap :states '(insert emacs))
  (tag-create-definer tag-nvmap :states '(normal visual))
  ;; these two don't have corresponding states
  (tag-create-definer tag-itomap :keymaps 'evil-inner-text-objects-map)
  (tag-create-definer tag-otomap :keymaps 'evil-outer-text-objects-map)
  (tag-create-definer tag-tomap
    :keymaps '(evil-outer-text-objects-map
               evil-inner-text-objects-map))
  (when short-names
    (defalias 'imap #'tag-imap)
    (defalias 'emap #'tag-emap)
    (defalias 'nmap #'tag-nmap)
    (defalias 'vmap #'tag-vmap)
    (defalias 'mmap #'tag-mmap)
    (defalias 'omap #'tag-omap)
    (defalias 'rmap #'tag-rmap)
    (defalias 'iemap #'tag-iemap)
    (defalias 'nvmap #'tag-nvmap)
    (defalias 'itomap #'tag-itomap)
    (defalias 'otomap #'tag-otomap)
    (defalias 'tomap #'tag-tomap)))

;; * Use-package Integration
;; maybe useful for something else in future
(defun tag--extract-autoloadable-symbol (def)
  "Extract an autoloadable symbol from DEF, a normal or extended definition.
This will also correctly extract the definition from a cons of the form (STRING
. DEFN). If the extracted definition is nil, a string, a lambda, a keymap symbol
from an extended definition, or some other definition that cannot be autoloaded,
return nil."
  ;; explicit null checks not required because nil return value means no def
  (when (tag--extended-def-p def)
    ;; extract definition
    (let ((first (car def)))
      (setq def (if (keywordp first)
                    (plist-get def :def)
                  first))))
  (cond ((symbolp def)
         def)
        ((and (consp def)
              (symbolp (cdr def)))
         (cdr def))))

(tag-with-eval-after-load 'use-package-core
  (declare-function use-package-concat "use-package-core")
  (declare-function use-package-process-keywords "use-package-core")
  (defvar use-package-keywords)
  (defvar use-package-deferring-keywords)

  (defun tag--sanitize-arglist (arglist)
    "Remove positional/separator arguments from ARGLIST."
    (let ((arglists (if (eq (car arglist) 'tag-defs)
                        (tag--parse-defs-arglists (cdr arglist))
                      (list arglist))))
      (cl-loop for arglist in arglists
               do (while (tag--positional-arg-p (car arglist))
                    (setq arglist (cdr arglist)))
               and append arglist)))

  ;; altered args will be passed to the autoloads and handler functions
  (defun use-package-normalize/:tag (_name _keyword tag-arglists)
    "Return a plist containing the original ARGLISTS and autoloadable symbols."
    (let* ((sanitized-arglist
            ;; combine arglists into one without function names or
            ;; positional arguments
            (cl-loop for arglist in tag-arglists
                     append (tag--sanitize-arglist arglist)))
           (commands
            (cl-loop for (key def) on sanitized-arglist by 'cddr
                     when (and (not (keywordp key))
                               (not (null def))
                               (ignore-errors
                                 ;; remove extra quote
                                 ;; `eval' works in some cases that `cadr' does
                                 ;; not (e.g. quoted string, '(list ...), etc.)
                                 ;; `ignore-errors' handles cases where it fails
                                 ;; (e.g. variable not defined at
                                 ;; macro-expansion time)
                                 (setq def (eval def))
                                 (setq def (tag--extract-autoloadable-symbol
                                            def))))
                     collect def)))
      (list :arglists tag-arglists :commands commands)))

  (defun use-package-autoloads/:tag (_name _keyword args)
    "Return an alist of commands extracted from ARGS.
Return something like '((some-command-to-autoload . command) ...)."
    (mapcar (lambda (command) (cons command 'command))
            (plist-get args :commands)))

  (defun use-package-handler/:tag (name _keyword args rest state)
    "Use-package handler for :tag."
    (use-package-concat
     (use-package-process-keywords name rest state)
     `(,@(mapcar (lambda (arglist)
                   ;; Note: prefix commands are not valid functions
                   (if (or (functionp (car arglist))
                           (macrop (car arglist)))
                       `(,@arglist :package ',name)
                     `(tag-def
                        ,@arglist
                        :package ',name)))
                 (plist-get args :arglists)))))

  (defun tag-normalize-hook-arglist (arglist mode-enable mode-hook
                                                 &optional symbol-is-function-p)
    "Rewrite a :g(f)hook ARGLIST to a `tag-add-hook' arglist.
MODE-ENABLE is the inferred command to enable the package's mode, and MODE-HOOK
is the mode inferred hook to enable the package's mode. When ARGLIST is a symbol
instead of a list, it will be considered to be a hook name unless
SYMBOL-IS-FUNCTION-P is non-nil, in which case it will considered to be a
function."
    (cond ((ignore-errors (memq (car arglist) (list 'quote 'function)))
           ;; user passed in a hook or function; leave as is
           (if symbol-is-function-p
               ;; '<package>-mode-hook <user specified function>
               `(',mode-hook ,arglist)
             ;; <user specified hook> #'<package>-mode
             `(,arglist ',mode-enable)))
          ((symbolp arglist)
           ;; user passed in an unquoted symbol (variable); don't quote it
           (if symbol-is-function-p
               `(',mode-hook ,arglist)
             `(,arglist ',mode-enable)))
          (t
           ;; actual list for `tag-add-hook'
           (if (= (length arglist) 1)
               ;; <user specified hook(s)> #'<package>-mode
               `(,(car arglist) ',mode-enable)
             (let ((hooks (car arglist))
                   (functions (cadr arglist)))
               (when (or (null hooks)
                         (not (or (symbolp hooks)
                                  (listp hooks))))
                 (setq hooks `',mode-hook))
               (when (or (null functions)
                         (not (or (symbolp functions)
                                  (listp functions))))
                 (setq functions `',mode-enable))
               ;; (cons hooks (cons functions (cddr arglist)))
               `(,hooks ,functions ,@(cddr arglist)))))))

  ;; altered args will be passed to the autoloads and handler functions
  (defun tag-normalize-hook (name _keyword args &optional gfhookp)
    "Return a plist containing arglists and autoloadable commands.
Transform ARGS into arglists suitable for `tag-add-hook'."
    (let* ((mode (if (string-match-p "mode\\'" (symbol-name name))
                     name
                   (intern (format "%s-mode" name))))
           (mode-hook (intern (format "%s-hook" mode))))
      (cl-loop for arg in args
               collect (tag-normalize-hook-arglist
                        arg mode mode-hook gfhookp))))

  (defalias 'use-package-normalize/:ghook #'tag-normalize-hook)

  (defun use-package-autoloads/:ghook (_name _keyword arglists)
    "Return an alist of commands extracted from ARGLISTS.
Return somethin"
    (let (functions)
      (dolist (arglist arglists)
        (let ((function-position (cadr arglist)))
          (cond ((not (listp function-position))
                 ;; (not (ignore-errors (car function-position)))
                 ;; ignore variables
                 )
                ((and (memq (car function-position) (list 'quote 'function))
                      (not (listp (cadr function-position))))
                 (push (cons (cadr function-position) 'command) functions))
                ((eq (car function-position) 'list)
                 (dolist (func (cdr function-position))
                   ;; ignore variables and function/macro calls
                   (when (and (listp func)
                              (memq (car func) (list 'quote 'function)))
                     (push (cons (cadr func) 'command) functions))))
                ((eq (car function-position) 'quote)
                 (dolist (func (cadr function-position))
                   (push (cons func 'command) functions))))))
      functions))

  (defun use-package-handler/:ghook (name _keyword arglists rest state)
    "Use-package handler for :ghook and :gfhook."
    (use-package-concat
     (use-package-process-keywords name rest state)
     `(,@(mapcar (lambda (arglist)
                   arglist
                   `(tag-add-hook ,@arglist))
                 arglists))))

  ;; ** :tag Keyword
  (setq use-package-keywords
        ;; should go in the same location as :bind
        ;; adding to end may not cause problems, but see issue #22
        (cl-loop for item in use-package-keywords
                 if (eq item :bind-keymap*)
                 collect :bind-keymap* and
                 collect :tag and
                 collect :gone and
                 collect :modalka
                 else
                 ;; don't add duplicates
                 unless (memq item '(:tag :gone :modalka))
                 collect item))

  (defun use-package-handler/:gone (name _keyword arglists rest state)
    "Use-package handler for :gone."
    (use-package-concat
     (use-package-process-keywords name rest state)
     `(,@(mapcar (lambda (arglist)
                   arglist
                   `(tag-def :keymaps '(override aiern-insert-state-map aiern-normal-state-map) ,@arglist))
                 arglists))))

  (defalias 'use-package-autoloads/:gone #'use-package-autoloads/:ghook)
  (defalias 'use-package-normalize/:gone #'use-package-normalize/:ghook)

  (defun use-package-handler/:modalka (name _keyword arglists rest state)
    "Use-package handler for :modalka."
    (use-package-concat
     (use-package-process-keywords name rest state)
     `(,@(mapcar (lambda (arglist)
                   arglist
                   `(tag-def :keymaps 'modalka-mode-map ,@arglist))
                 arglists))))

  (defalias 'use-package-autoloads/:modalka #'use-package-autoloads/:gone)
  (defalias 'use-package-normalize/:modalka #'use-package-normalize/:gone)

  ;; ** :ghook and :gfhook Keyword
  (setq use-package-keywords
        ;; should go in the same location as :bind
        ;; adding to end may not cause problems, but see issue #22
        (cl-loop for item in use-package-keywords
                 if (eq item :hook)
                 collect :hook and
                 collect :ghook and
                 collect :gfhook
                 else
                 ;; don't add duplicates
                 unless (memq item '(:ghook :gfhook))
                 collect item))

  (defun use-package-normalize/:gfhook (name keyword args)
    "Use-package normalizer for :gfhook."
    (tag-normalize-hook name keyword args t))

  (defalias 'use-package-handler/:gfhook #'use-package-handler/:ghook))

;; * Key-chord "Integration"
(defun tag-chord (keys)
  "Rewrite the string KEYS into a valid key-chord vector."
  ;; taken straight from key-chord.el
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (vector 'key-chord key1 key2)))

(provide 'tag)
;;; tag.el ends here
