;;; aiern-common.el --- Common functions and utilities -*- lexical-binding: t -*-
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

(require 'aiern-vars)
(require 'aiern-digraphs)
(require 'rect)
(require 'thingatpt)
(require 'cl-lib)
(require 'calc)

;;; Code:

(declare-function aiern-visual-state-p "aiern-states")
(declare-function aiern-visual-restore "aiern-states")
(declare-function aiern-motion-state "aiern-states")
(declare-function aiern-ex-p "aiern-ex")
(declare-function aiern-set-jump "aiern-jumps")

(condition-case nil
    (require 'windmove)
  (error
   (message "aiern: Could not load `windmove', \
window commands not available.")
   nil))

;;; Compatibility with different Emacs versions

;; x-set-selection and x-get-selection have been deprecated since 25.1
;; by gui-set-selection and gui-get-selection
(defalias 'aiern-get-selection
  (if (fboundp 'gui-get-selection) 'gui-get-selection 'x-get-selection))
(defalias 'aiern-set-selection
  (if (fboundp 'gui-set-selection) 'gui-set-selection 'x-set-selection))

(defmacro aiern-called-interactively-p ()
  "Wrapper for `called-interactively-p'.
In older versions of Emacs, `called-interactively-p' takes
no arguments.  In Emacs 23.2 and newer, it takes one argument."
  (called-interactively-p 'any))
(make-obsolete 'aiern-called-interactively-p
               "please use (called-interactively-p 'any) instead."
               "Git commit 222b791")

;; macro helper
(eval-and-compile
  (defun aiern-unquote (exp)
    "Return EXP unquoted."
    (while (eq (car-safe exp) 'quote)
      (setq exp (cadr exp)))
    exp))

(defun aiern-delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK. If APPEND is
non-nil, the entry is appended to the hook. If LOCAL is non-nil,
the buffer-local value of HOOK is modified."
  (if (and (not (booleanp condition)) (eval condition))
      (eval form)
    (let* ((name (or name (format "aiern-delay-form-in-%s" hook)))
           (fun (make-symbol name))
           (condition (or condition t)))
      (fset fun `(lambda (&rest args)
                   (when ,condition
                     (remove-hook ',hook #',fun ',local)
                     ,form)))
      (put fun 'permanent-local-hook t)
      (add-hook hook fun append local))))
(put 'aiern-delay 'lisp-indent-function 2)

;;; List functions

(defmacro aiern--add-to-alist (list-var &rest elements)
  "Add the assocation of KEY and VAL to the value of LIST-VAR.
If the list already contains an entry for KEY, update that entry;
otherwise add at the end of the list.

\(fn LIST-VAR KEY VAL &rest ELEMENTS)"
  (when (eq (car-safe list-var) 'quote)
    (setq list-var (cadr list-var)))
  `(progn
     ,@(if (version< emacs-version "26")
           ;; TODO: Remove this path when support for Emacs 25 is dropped
           (cl-loop for (key val) on elements by #'cddr
                    collect `(let* ((key ,key)
                                    (val ,val)
                                    (cell (assoc key ,list-var)))
                               (if cell
                                   (setcdr cell val)
                                 (push (cons key val) ,list-var))))
         (cl-loop for (key val) on elements by #'cddr
                  collect `(setf (alist-get ,key ,list-var nil nil #'equal) ,val)))
     ,list-var))

(defun aiern-add-to-alist (list-var key val &rest elements)
  "Add the assocation of KEY and VAL to the value of LIST-VAR.
If the list already contains an entry for KEY, update that entry;
otherwise add at the end of the list."
  (let ((tail (symbol-value list-var)))
    (while (and tail (not (equal (car-safe (car-safe tail)) key)))
      (setq tail (cdr tail)))
    (if tail
        (setcar tail (cons key val))
      (set list-var (append (symbol-value list-var)
                            (list (cons key val)))))
    (if elements
        (with-no-warnings
          (apply #'aiern-add-to-alist list-var elements))
      (symbol-value list-var))))

(make-obsolete 'aiern-add-to-alist
               "use `aiern--add-to-alist' instead. You may need to recompile code with aiern macros."
               "1.13.1")

;; custom version of `delete-if'
(defun aiern-filter-list (predicate list &optional pointer)
  "Delete by side-effect all items satisfying PREDICATE in LIST.
Stop when reaching POINTER.  If the first item satisfies PREDICATE,
there is no way to remove it by side-effect; therefore, write
\(setq foo (aiern-filter-list 'predicate foo)) to be sure of
changing the value of `foo'."
  (let ((tail list) elt head)
    (while (and tail (not (eq tail pointer)))
      (setq elt (car tail))
      (cond
       ((funcall predicate elt)
        (setq tail (cdr tail))
        (if head
            (setcdr head tail)
          (setq list tail)))
       (t
        (setq head tail
              tail (cdr tail)))))
    list))

(defun aiern-member-if (predicate list &optional pointer)
  "Find the first item satisfying PREDICATE in LIST.
Stop when reaching POINTER, which should point at a link
in the list."
  (let (elt)
    (catch 'done
      (while (and (consp list) (not (eq list pointer)))
        (setq elt (car list))
        (if (funcall predicate elt)
            (throw 'done elt)
          (setq list (cdr list)))))))

(defun aiern-member-recursive-if (predicate tree)
  "Find the first item satisfying PREDICATE in TREE."
  (cond
   ((funcall predicate tree)
    tree)
   ((listp tree)
    (catch 'done
      (dolist (elt tree)
        (when (setq elt (aiern-member-recursive-if predicate elt))
          (throw 'done elt)))))))

(defun aiern-concat-lists (&rest sequences)
  "Concatenate lists, removing duplicates.
Elements are compared with `eq'."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (push elt result)))
    (nreverse (cl-remove-duplicates result :test #'eq))))

(defun aiern-concat-alists (&rest sequences)
  "Concatenate association lists, removing duplicates.
An alist is a list of cons cells (KEY . VALUE) where each key
may occur only once. Later values overwrite earlier values."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (setq result (assq-delete-all (car-safe elt) result))
        (push elt result)))
    (nreverse result)))

(defun aiern-concat-plists (&rest sequences)
  "Concatenate property lists, removing duplicates.
A property list is a list (:KEYWORD1 VALUE1 :KEYWORD2 VALUE2...)
where each keyword may occur only once. Later values overwrite
earlier values."
  (let (result)
    (dolist (sequence sequences result)
      (while sequence
        (setq result
              (plist-put result (pop sequence) (pop sequence)))))))

(defun aiern-concat-keymap-alists (&rest sequences)
  "Concatenate keymap association lists, removing duplicates.
A keymap alist is a list of cons cells (VAR . MAP) where each keymap
may occur only once, but where the variables may be repeated
\(e.g., (VAR . MAP1) (VAR . MAP2) is allowed). The order matters,
with the highest priority keymaps being listed first."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (unless (rassq (cdr-safe elt) result)
          (push elt result))))
    (nreverse result)))

(defun aiern-plist-delete (prop plist)
  "Delete by side effect the property PROP from PLIST.
If PROP is the first property in PLIST, there is no way
to remove it by side-effect; therefore, write
\(setq foo (aiern-plist-delete :prop foo)) to be sure of
changing the value of `foo'."
  (let ((tail plist) elt head)
    (while tail
      (setq elt (car tail))
      (cond
       ((eq elt prop)
        (setq tail (cdr (cdr tail)))
        (if head
            (setcdr (cdr head) tail)
          (setq plist tail)))
       (t
        (setq head tail
              tail (cdr (cdr tail))))))
    plist))

(defun aiern-get-property (alist key &optional prop)
  "Return property PROP for KEY in ALIST.
ALIST is an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list.
If PROP is nil, return all properties for KEY.
If KEY is t, return an association list of keys
and their PROP values."
  (cond
   ((null prop)
    (cdr (assq key alist)))
   ((eq key t)
    (let (result val)
      (dolist (entry alist result)
        (setq key (car entry)
              val (cdr entry))
        (when (plist-member val prop)
          (setq val (plist-get val prop))
          (push (cons key val) result)))))
   (t
    (plist-get (cdr (assq key alist)) prop))))

(defun aiern-put-property (alist-var key prop val &rest properties)
  "Set PROP to VAL for KEY in ALIST-VAR.
ALIST-VAR points to an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list storing PROP and VAL."
  (set alist-var
       (let* ((alist (symbol-value alist-var))
              (plist (cdr (assq key alist))))
         (setq plist (plist-put plist prop val))
         (when properties
           (setq plist (aiern-concat-plists plist properties)
                 val (car (last properties))))
         (setq alist (assq-delete-all key alist))
         (push (cons key plist) alist)))
  val)

(defun aiern-state-property (state prop &optional value)
  "Return the value of property PROP for STATE.
PROP is a keyword as used by `aiern-define-state'.
STATE is the state's symbolic name.
If VALUE is non-nil and the value is a variable,
return the value of that variable."
  (let ((val (aiern-get-property aiern-state-properties state prop)))
    (if (and value (symbolp val) (boundp val))
        (symbol-value val)
      val)))

(defmacro aiern-swap (this that &rest vars)
  "Swap the values of variables THIS and THAT.
If three or more arguments are given, the values are rotated.
E.g., (aiern-swap A B C) sets A to B, B to C, and C to A."
  `(progn
     (setq ,this (prog1 ,that
                   (setq ,that ,this)))
     ,@(when vars
         `((aiern-swap ,that ,@vars)))))

(defmacro aiern-sort (min max &rest vars)
  "Place the smallest value in MIN and the largest in MAX.
If three or more arguments are given, place the smallest
value in the first argument and the largest in the last,
sorting in between."
  (let ((sorted (make-symbol "sortvar")))
    `(let ((,sorted (sort (list ,min ,max ,@vars) '<)))
       (setq ,min (pop ,sorted)
             ,max (pop ,sorted)
             ,@(apply #'append
                      (mapcar #'(lambda (var)
                                  (list var `(pop ,sorted)))
                              vars))))))

(defun aiern-vector-to-string (vector)
  "Turns vector into a string, changing <escape> to '\\e'"
  (mapconcat (lambda (c)
               (if (equal c 'escape)
                   "\e"
                 (make-string 1 c)))
             vector
             ""))

;;; Command properties

(defmacro aiern-define-command (command &rest body)
  "Define a command COMMAND.

\(fn COMMAND (ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name
                           [&optional lambda-list]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let ((interactive '(interactive))
        arg args doc doc-form key keys)
    ;; collect arguments
    (when (listp (car-safe body))
      (setq args (pop body)))
    ;; collect docstring
    (when (> (length body) 1)
      (if (eq (car-safe (car-safe body)) 'format)
          (setq doc-form (pop body))
        (when (stringp (car-safe body))
          (setq doc (pop body)))))
    ;; collect keywords
    (setq keys (plist-put keys :repeat t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (unless nil ; TODO: add keyword check
        (setq keys (plist-put keys key arg))))
    ;; collect `interactive' form
    (when (and body (consp (car body))
               (eq (car (car body)) 'interactive))
      (let* ((iform (pop body))
             (result (apply #'aiern-interactive-form (cdr iform)))
             (form (car result))
             (attrs (cdr result)))
        (setq interactive `(interactive ,form)
              keys (aiern-concat-plists keys attrs))))
    `(progn
       ;; the compiler does not recognize `defun' inside `let'
       ,(when (and command body)
          `(defun ,command ,args
             ,@(when doc `(,doc))
             ,interactive
             (ignore ,@(cl-set-difference args '(&optional &rest)))
             ,@body))
       ,(when (and command doc-form)
          `(put ',command 'function-documentation ,doc-form))
       ;; set command properties for symbol or lambda function
       (let ((func ',(if (and (null command) body)
                         `(lambda ,args
                            ,interactive
                            ,@body)
                       command)))
         (apply #'aiern-set-command-properties func ',keys)
         func))))

;; If no aiern properties are defined for the command, several parts of
;; aiern apply certain default rules; e.g., the repeat system decides
;; whether the command is repeatable by monitoring buffer changes.
(defun aiern-has-command-property-p (command property)
  "Whether COMMAND has aiern PROPERTY.
See also `aiern-has-command-properties-p'."
  (plist-member (aiern-get-command-properties command) property))

(defun aiern-has-command-properties-p (command)
  "Whether aiern properties are defined for COMMAND.
See also `aiern-has-command-property-p'."
  (and (aiern-get-command-properties command) t))

(defun aiern-get-command-property (command property &optional default)
  "Return the value of aiern PROPERTY of COMMAND.
If the command does not have the property, return DEFAULT.
See also `aiern-get-command-properties'."
  (if (aiern-has-command-property-p command property)
      (aiern-get-property aiern-command-properties command property)
    default))

(defun aiern-get-command-properties (command)
  "Return all aiern properties of COMMAND.
See also `aiern-get-command-property'."
  (aiern-get-property aiern-command-properties command))

(defun aiern-set-command-property (command property value)
  "Set PROPERTY to VALUE for COMMAND.
To set multiple properties at once, see
`aiern-set-command-properties' and `aiern-add-command-properties'."
  (aiern-put-property 'aiern-command-properties command property value))
(defalias 'aiern-put-command-property 'aiern-set-command-property)

(defun aiern-add-command-properties (command &rest properties)
  "Add PROPERTIES to COMMAND.
PROPERTIES should be a property list.
To replace all properties at once, use `aiern-set-command-properties'."
  (apply #'aiern-put-property
         'aiern-command-properties command properties))

(defun aiern-set-command-properties (command &rest properties)
  "Replace all of COMMAND's properties with PROPERTIES.
PROPERTIES should be a property list.
This erases all previous properties; to only add properties,
use `aiern-set-command-property'."
  (setq aiern-command-properties
        (assq-delete-all command aiern-command-properties))
  (when properties
    (apply #'aiern-add-command-properties command properties)))

(defun aiern-remove-command-properties (command &rest properties)
  "Remove PROPERTIES from COMMAND.
PROPERTIES should be a list of properties (:PROP1 :PROP2 ...).
If PROPERTIES is the empty list, all properties are removed."
  (let (plist)
    (when properties
      (setq plist (aiern-get-command-properties command))
      (dolist (property properties)
        (setq plist (aiern-plist-delete property plist))))
    (apply #'aiern-set-command-properties command plist)))

(defun aiern-yank-handler (&optional motion)
  "Return the yank handler for MOTION.
MOTION defaults to the current motion."
  (setq motion (or motion aiern-this-motion))
  (aiern-get-command-property motion :yank-handler))

(defun aiern-declare-motion (command)
  "Declare COMMAND to be a movement function.
This ensures that it behaves correctly in visual state."
  (aiern-add-command-properties command :keep-visual t :repeat 'motion))

(defun aiern-declare-repeat (command)
  "Declare COMMAND to be repeatable."
  (aiern-add-command-properties command :repeat t))

(defun aiern-declare-not-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (aiern-add-command-properties command :repeat nil))

(defun aiern-declare-ignore-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (aiern-add-command-properties command :repeat 'ignore))

(defun aiern-declare-change-repeat (command)
  "Declare COMMAND to be repeatable by buffer changes rather than
keystrokes."
  (aiern-add-command-properties command :repeat 'change))

(defun aiern-declare-insert-at-point-repeat (command)
  "Declare COMMAND to be repeatable by buffer changes."
  (aiern-add-command-properties command :repeat 'insert-at-point))

(defun aiern-declare-abort-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (aiern-add-command-properties command :repeat 'abort))

(defun aiern-delimited-arguments (string &optional num)
  "Parse STRING as a sequence of delimited arguments.
Returns a list of NUM strings, or as many arguments as
the string contains. The first non-blank character is
taken to be the delimiter. If some arguments are missing
from STRING, the resulting list is padded with nil values.
Two delimiters following directly after each other gives
an empty string."
  (save-match-data
    (let ((string (or string ""))
          (count (or num -1)) (idx 0)
          argument delim match result)
      (when (string-match "^[[:space:]]*\\([^[:space:]]\\)" string)
        (setq delim (match-string 1 string)
              argument (format "%s\\(\\(?:[\\].\\|[^%s]\\)*\\)"
                               (regexp-quote delim)
                               delim))
        (while (and (/= count 0) (string-match argument string idx))
          (setq match (match-string 1 string)
                idx (match-end 1)
                count (1- count))
          (when (= count 0)
            (unless (save-match-data
                      (string-match
                       (format "%s[[:space:]]*$" delim) string idx))
              (setq match (substring string (match-beginning 1)))))
          (unless (and (zerop (length match))
                       (zerop (length (substring string idx))))
            (push match result))))
      (when (and num (< (length result) num))
        (dotimes (_ (- num (length result)))
          (push nil result)))
      (nreverse result))))

(defun aiern-concat-charsets (&rest sets)
  "Concatenate character sets.
A character set is the part between [ and ] in a regular expression.
If any character set is complemented, the result is also complemented."
  (let ((bracket "") (complement "") (hyphen "") result)
    (save-match-data
      (dolist (set sets)
        (when (string-match-p "^\\^" set)
          (setq set (substring set 1)
                complement "^"))
        (when (string-match-p "^]" set)
          (setq set (substring set 1)
                bracket "]"))
        (when (string-match-p "^-" set)
          (setq set (substring set 1)
                hyphen "-"))
        (setq result (concat result set)))
      (format "%s%s%s%s" complement bracket hyphen result))))

;;; Key sequences

(defun aiern-keypress-parser (&optional input)
  "Read from keyboard or INPUT and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument.
Both COUNT and CMD may be nil."
  (let (count negative)
    (when input (setq unread-command-events (append input unread-command-events)))
    (catch 'done
      (while t
        (let ((seq (read-key-sequence "")))
          (when seq
            (let ((cmd (key-binding seq)))
              (cond
               ((null cmd) (throw 'done (list nil nil)))
               ((arrayp cmd) ; keyboard macro, recursive call
                (let ((cmd (aiern-keypress-parser cmd)))
                  (throw 'done
                         (list (car cmd)
                               (if (or count (cadr cmd))
                                   (list (car cmd) (* (or count 1)
                                                      (or (cadr cmd) 1))))))))
               ((or (eq cmd #'digit-argument)
                    (and (eq cmd 'aiern-digit-argument-or-aiern-beginning-of-line)
                         count))
                (let* ((event (aref seq (- (length seq) 1)))
                       (char (or (when (characterp event) event)
                                 (when (symbolp event)
                                   (get event 'ascii-character))))
                       (digit (if (or (characterp char) (integerp char))
                                  (- (logand char ?\177) ?0))))
                  (setq count (+ (* 10 (or count 0)) digit))))
               ((eq cmd #'negative-argument)
                (setq negative (not negative)))
               (t
                (throw 'done (list cmd
                                   (and count
                                        (* count
                                           (if negative -1 1))))))))))))))

(defun aiern-read-key (&optional prompt)
  "Read a key from the keyboard.
Translates it according to the input method."
  (let ((old-global-map (current-global-map))
        (new-global-map (make-sparse-keymap))
        (overriding-terminal-local-map nil)
        (overriding-local-map aiern-read-key-map)
        seq char cmd)
    (unwind-protect
        (condition-case nil
            (progn
              (define-key new-global-map [menu-bar]
                (lookup-key global-map [menu-bar]))
              (define-key new-global-map [tab-bar]
                (lookup-key global-map [tab-bar]))
              (define-key new-global-map [tool-bar]
                (lookup-key global-map [tool-bar]))
              (setq new-global-map
                    (append new-global-map
                            (list (make-char-table 'display-table
                                                   'self-insert-command))))
              (use-global-map new-global-map)
              (setq seq (read-key-sequence prompt nil t)
                    char (aref seq 0)
                    cmd (key-binding seq))
              (while (arrayp cmd)
                (setq char (aref cmd 0)
                      cmd (key-binding cmd)))
              (cond
               ((eq cmd 'self-insert-command)
                char)
               (cmd
                (call-interactively cmd))
               (t
                (user-error "No replacement character typed"))))
          (quit
           (when (fboundp 'aiern-repeat-abort)
             (aiern-repeat-abort))
           (signal 'quit nil)))
      (use-global-map old-global-map))))

(defun aiern-read-quoted-char ()
  "Command that calls `read-quoted-char'.
This command can be used wherever `read-quoted-char' is required
as a command. Its main use is in the `aiern-read-key-map'."
  (interactive)
  (read-quoted-char))

(defun aiern-read-digraph-char (&optional hide-chars)
  "Read two keys from keyboard forming a digraph.
This function creates an overlay at (point), hiding the next
HIDE-CHARS characters. HIDE-CHARS defaults to 1."
  (interactive)
  (let (char1 char2 string overlay)
    (unwind-protect
        (progn
          (setq overlay (make-overlay (point)
                                      (min (point-max)
                                           (+ (or hide-chars 1)
                                              (point)))))
          (overlay-put overlay 'invisible t)
          ;; create overlay prompt
          (setq string "?")
          (put-text-property 0 1 'face 'minibuffer-prompt string)
          ;; put cursor at (i.e., right before) the prompt
          (put-text-property 0 1 'cursor t string)
          (overlay-put overlay 'after-string string)
          (setq char1 (read-key))
          (setq string (string char1))
          (put-text-property 0 1 'face 'minibuffer-prompt string)
          (put-text-property 0 1 'cursor t string)
          (overlay-put overlay 'after-string string)
          (setq char2 (read-key)))
      (delete-overlay overlay))
    (or (aiern-digraph (list char1 char2))
        ;; use the last character if undefined
        char2)))

(defun aiern-read-motion (&optional motion count type modifier)
  "Read a MOTION, motion COUNT and motion TYPE from the keyboard.
The type may be overridden with MODIFIER, which may be a type
or a Visual selection as defined by `aiern-define-visual-selection'.
Return a list (MOTION COUNT [TYPE])."
  (let (command prefix)
    (setq aiern-this-type-modified nil)
    (unless motion
      (while (progn
               (setq command (aiern-keypress-parser)
                     motion (pop command)
                     prefix (pop command))
               (when prefix
                 (if count
                     (setq count (string-to-number
                                  (concat (number-to-string count)
                                          (number-to-string prefix))))
                   (setq count prefix)))
               ;; if the command is a type modifier, read more
               (when (rassq motion aiern-visual-alist)
                 (setq modifier
                       (or modifier
                           (car (rassq motion aiern-visual-alist))))))))
    (when modifier
      (setq type (or type (aiern-type motion 'exclusive)))
      (cond
       ((eq modifier 'char)
        ;; TODO: this behavior could be less hard-coded
        (if (eq type 'exclusive)
            (setq type 'inclusive)
          (setq type 'exclusive)))
       (t
        (setq type modifier)))
      (setq aiern-this-type-modified type))
    (list motion count type)))

(defun aiern-mouse-events-p (keys)
  "Returns non-nil iff KEYS contains a mouse event."
  (catch 'done
    (dotimes (i (length keys))
      (when (or (and (fboundp 'mouse-event-p)
                     (mouse-event-p (aref keys i)))
                (mouse-movement-p (aref keys i)))
        (throw 'done t)))
    nil))

(defun aiern-extract-count (keys)
  "Splits the key-sequence KEYS into prefix-argument and the rest.
Returns the list (PREFIX CMD SEQ REST), where PREFIX is the
prefix count, CMD the command to be executed, SEQ the subsequence
calling CMD, and REST is all remaining events in the
key-sequence. PREFIX and REST may be nil if they do not exist.
If a command is bound to some keyboard macro, it is expanded
recursively."
  (catch 'done
    (let* ((len (length keys))
           (beg 0)
           (end 1)
           (found-prefix nil))
      (while (and (<= end len))
        (let ((cmd (key-binding (substring keys beg end))))
          (cond
           ((memq cmd '(undefined nil))
            (user-error "No command bound to %s" (substring keys beg end)))
           ((arrayp cmd) ; keyboard macro, replace command with macro
            (setq keys (vconcat (substring keys 0 beg)
                                cmd
                                (substring keys end))
                  end (1+ beg)
                  len (length keys)))
           ((functionp cmd)
            (if (or (memq cmd '(digit-argument negative-argument))
                    (and found-prefix
                         (aiern-get-command-property
                          cmd :digit-argument-redirection)))
                ;; skip those commands
                (setq found-prefix t ; found at least one prefix argument
                      beg end
                      end (1+ end))
              ;; a real command, finish
              (throw 'done
                     (list (unless (zerop beg)
                             (string-to-number
                              (concat (substring keys 0 beg))))
                           cmd
                           (substring keys beg end)
                           (when (< end len)
                             (substring keys end))))))
           (t ; append a further event
            (setq end (1+ end))))))
      (user-error "Key sequence contains no complete binding"))))

(defmacro aiern-redirect-digit-argument (map keys target)
  "Bind a wrapper function calling TARGET or `digit-argument'.
MAP is a keymap for binding KEYS to the wrapper for TARGET.
The wrapper only calls `digit-argument' if a prefix-argument
has already been started; otherwise TARGET is called."
  (let* ((target (eval target))
         (wrapper (intern (format "aiern-digit-argument-or-%s"
                                  target))))
    `(progn
       (define-key ,map ,keys ',wrapper)
       (aiern-define-command ,wrapper ()
         :digit-argument-redirection ,target
         :keep-visual t
         :repeat nil
         (interactive)
         (cond
          (current-prefix-arg
           (setq this-command #'digit-argument)
           (call-interactively #'digit-argument))
          (t
           (let ((target (or (command-remapping #',target)
                             #',target)))
             (setq this-command target)
             (call-interactively target))))))))

(defun aiern-extract-append (file-or-append)
  "Return an (APPEND . FILENAME) pair based on FILE-OR-APPEND.
FILE-OR-APPEND should either be a filename or a \">> FILE\"
directive.  APPEND will be t if FILE-OR-APPEND is an append
directive and nil otherwise.  FILENAME will be the extracted
filename."
  (if (and (stringp file-or-append)
           (string-match "\\(>> *\\)" file-or-append))
      (cons t (substring file-or-append(match-end 1)))
    (cons nil file-or-append)))

(defun aiern-set-keymap-prompt (map prompt)
  "Set the prompt-string of MAP to PROMPT."
  (delq (keymap-prompt map) map)
  (when prompt
    (setcdr map (cons prompt (cdr map)))))

(defun aiern-lookup-key (map key)
  "Returns non-nil value if KEY is bound in MAP."
  (let ((definition (lookup-key map key)))
    (if (numberp definition) ; in-band error
        nil
      definition)))

;;; Display

(defun aiern-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above."
  (unless (and (not (functionp specs))
               (listp specs)
               (null (cdr-safe (last specs))))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond
     ((functionp spec)
      (condition-case nil
          (funcall spec)
        (error nil)))
     ((stringp spec)
      (aiern-set-cursor-color spec))
     (t
      (setq cursor-type spec)))))

(defun aiern-set-cursor-color (color)
  "Set the cursor color to COLOR."
  (unless (equal (frame-parameter nil 'cursor-color) color)
    ;; `set-cursor-color' forces a redisplay, so only
    ;; call it when the color actually changes
    (set-cursor-color color)))

(defun aiern-refresh-cursor (&optional state buffer)
  "Refresh the cursor for STATE in BUFFER.
BUFFER defaults to the current buffer.  If STATE is nil the
cursor type is either `aiern-force-cursor' or the current state."
  (when (and (boundp 'aiern-local-mode) aiern-local-mode)
    (let* ((state (or state aiern-force-cursor aiern-state 'normal))
           (default (or aiern-default-cursor t))
           (cursor (aiern-state-property state :cursor t))
           (color (or (and (stringp cursor) cursor)
                      (and (listp cursor)
                           (aiern-member-if #'stringp cursor))
                      (frame-parameter nil 'cursor-color))))
      (with-current-buffer (or buffer (current-buffer))
        ;; if both STATE and `aiern-default-cursor'
        ;; specify a color, don't set it twice
        (when (and color (listp default))
          (setq default (aiern-filter-list #'stringp default)))
        (aiern-set-cursor default)
        (aiern-set-cursor cursor)))))

(defmacro aiern-save-cursor (&rest body)
  "Save the current cursor; execute BODY; restore the cursor."
  (declare (indent defun)
           (debug t))
  `(let ((cursor cursor-type)
         (color (frame-parameter (selected-frame) 'cursor-color))
         (inhibit-quit t))
     (unwind-protect
         (progn ,@body)
       (aiern-set-cursor cursor)
       (aiern-set-cursor color))))

(defun aiern-echo (string &rest args)
  "Display an unlogged message in the echo area.
That is, the message is not logged in the *Messages* buffer.
\(To log the message, just use `message'.)"
  (unless aiern-no-display
    (let (message-log-max)
      (apply #'message string args))))

(defun aiern-echo-area-save ()
  "Save the current echo area in `aiern-echo-area-message'."
  (setq aiern-echo-area-message (current-message)))

(defun aiern-echo-area-restore ()
  "Restore the echo area from `aiern-echo-area-message'.
Does not restore if `aiern-write-echo-area' is non-nil."
  (unless aiern-write-echo-area
    (if aiern-echo-area-message
        (message "%s" aiern-echo-area-message)
      (message nil)))
  (setq aiern-echo-area-message nil
        aiern-write-echo-area nil))

;; toggleable version of `with-temp-message'
(defmacro aiern-save-echo-area (&rest body)
  "Save the echo area; execute BODY; restore the echo area.
Intermittent messages are not logged in the *Messages* buffer."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         aiern-echo-area-message
         aiern-write-echo-area)
     (unwind-protect
         (progn
           (aiern-echo-area-save)
           ,@body)
       (aiern-echo-area-restore))))

(defmacro aiern-without-display (&rest body)
  "Execute BODY without aiern displays.
Inhibits echo area messages, mode line updates and cursor changes."
  (declare (indent defun)
           (debug t))
  `(let ((aiern-no-display t))
     ,@body))

(defvar aiern-cached-header-line-height nil
  "Cached height of the header line.
Used for fallback implementation on older Emacsen.")

(defun aiern-header-line-height ()
  "Return the height of the header line.
If there is no header line, return 0.
Used as a fallback implementation of `window-header-line-height' on
older Emacsen."
  (let ((posn (posn-at-x-y 0 0)))
    (or (when (eq (posn-area posn) 'header-line)
          (cdr (posn-object-width-height posn)))
        0)))

(defun aiern-posn-x-y (position)
  "Return the x and y coordinates in POSITION.
This function returns y offset from the top of the buffer area including
the header line.

On Emacs 24 and later versions, the y-offset returned by
`posn-at-point' is relative to the text area excluding the header
line, while y offset taken by `posn-at-x-y' is relative to the buffer
area including the header line.  This asymmetry is by design according
to GNU Emacs team.  This function fixes the asymmetry between them.

Learned from mozc.el."
  (let ((xy (posn-x-y position)))
    (when header-line-format
      (setcdr xy (+ (cdr xy)
                    (or (and (fboundp 'window-header-line-height)
                             (window-header-line-height))
                        aiern-cached-header-line-height
                        (setq aiern-cached-header-line-height (aiern-header-line-height))))))
    xy))

(defun aiern-count-lines (beg end)
  "Return absolute line-number-difference betweeen `beg` and `end`.
This should give the same results no matter where on the line `beg`
and `end` are."
  (if (= beg end)
      0
    (let* ((last (max beg end))
           (end-at-bol (save-excursion (goto-char last)
                                       (bolp))))
      (if end-at-bol
          (count-lines beg end)
        (1- (count-lines beg end))))))

;;; Movement

(defun aiern-normalize-position (pos)
  "Return POS if it does not exceed the buffer boundaries.
If POS is less than `point-min', return `point-min'.
Is POS is more than `point-max', return `point-max'.
If POS is a marker, return its position."
  (cond
   ((not (number-or-marker-p pos))
    pos)
   ((< pos (point-min))
    (point-min))
   ((> pos (point-max))
    (point-max))
   ((markerp pos)
    (marker-position pos))
   (t
    pos)))

(defmacro aiern-save-goal-column (&rest body)
  "Restores the goal column after execution of BODY.
See also `aiern-save-column'."
  (declare (indent defun)
           (debug t))
  `(let ((goal-column goal-column)
         (temporary-goal-column temporary-goal-column))
     ,@body))

(defmacro aiern-save-column (&rest body)
  "Restores the column after execution of BODY.
See also `aiern-save-goal-column'."
  (declare (indent defun)
           (debug t))
  `(let ((col (current-column)))
     (aiern-save-goal-column
       ,@body
       (move-to-column col))))

(defun aiern-narrow (beg end)
  "Restrict the buffer to BEG and END.
BEG or END may be nil, specifying a one-sided restriction including
`point-min' or `point-max'. See also `aiern-with-restriction.'"
  (setq beg (or (aiern-normalize-position beg) (point-min)))
  (setq end (or (aiern-normalize-position end) (point-max)))
  (narrow-to-region beg end))

(defmacro aiern-with-restriction (beg end &rest body)
  "Execute BODY with the buffer narrowed to BEG and END.
BEG or END may be nil as passed to `aiern-narrow'; this creates
a one-sided restriction."
  (declare (indent 2)
           (debug t))
  `(save-restriction
     (let ((aiern-restriction-stack
            (cons (cons (point-min) (point-max)) aiern-restriction-stack)))
       (aiern-narrow ,beg ,end)
       ,@body)))

(defmacro aiern-without-restriction (&rest body)
  "Execute BODY with the top-most narrowing removed.
This works only if the previous narrowing has been generated by
`aiern-with-restriction'."
  (declare (indent defun)
           (debug t))
  `(save-restriction
     (widen)
     (narrow-to-region (car (car aiern-restriction-stack))
                       (cdr (car aiern-restriction-stack)))
     (let ((aiern-restriction-stack (cdr aiern-restriction-stack)))
       ,@body)))

(defmacro aiern-narrow-to-field (&rest body)
  "Narrow to the current field."
  (declare (indent defun)
           (debug t))
  `(aiern-with-restriction (field-beginning) (field-end)
     ,@body))

(defun aiern-move-beginning-of-line (&optional arg)
  "Move to the beginning of the line as displayed.
Like `move-beginning-of-line', but retains the goal column."
  (aiern-save-goal-column
    (move-beginning-of-line arg)
    (beginning-of-line)))

(defun aiern-move-end-of-line (&optional arg)
  "Move to the end of the line as displayed.
Like `move-end-of-line', but retains the goal column."
  (aiern-save-goal-column
    (move-end-of-line arg)
    (end-of-line)))

(defun aiern-adjust-cursor (&optional _)
  "Move point one character back if at the end of a non-empty line.
This behavior is controled by `aiern-move-beyond-eol'."
  (when (and (eolp)
             (not aiern-move-beyond-eol)
             (not (bolp))
             (= (point)
                (save-excursion
                  (aiern-move-end-of-line)
                  (point))))
    (aiern-move-cursor-back t)))

(defun aiern-move-cursor-back (&optional force)
  "Move point one character back within the current line.
Contingent on the variable `aiern-move-cursor-back' or the FORCE
argument. Honors field boundaries, i.e., constrains the movement
to the current field as recognized by `line-beginning-position'."
  (when (or aiern-move-cursor-back force)
    (unless (or (= (point) (line-beginning-position))
                (and (boundp 'visual-line-mode)
                     visual-line-mode
                     (= (point) (save-excursion
                                  (beginning-of-visual-line)
                                  (point)))))
      (backward-char))))

(defun aiern-line-position (line &optional column)
  "Return the position of LINE.
If COLUMN is specified, return its position on the line.
A negative number means the end of the line."
  (save-excursion
    (when (fboundp 'aiern-goto-line)
      (aiern-goto-line line))
    (if (numberp column)
        (if (< column 0)
            (beginning-of-line 2)
          (move-to-column column))
      (beginning-of-line))
    (point)))

(defun aiern-column (&optional pos)
  "Return the horizontal position of POS.
POS defaults to point."
  (save-excursion
    (when pos
      (goto-char pos))
    (current-column)))

(defun aiern-move-to-column (column &optional dir force)
  "Move point to column COLUMN in the current line.
Places point at left of the tab character (at the right if DIR
is non-nil) and returns point."
  (interactive "p")
  (move-to-column column force)
  (unless force
    (when (or (not dir) (and (numberp dir) (< dir 1)))
      (when (> (current-column) column)
        (aiern-move-cursor-back))))
  (point))

(defmacro aiern-loop (spec &rest body)
  "Loop with countdown variable.
Evaluate BODY with VAR counting down from COUNT to 0.
COUNT can be negative, in which case VAR counts up instead.
The return value is the value of VAR when the loop
terminates, which is 0 if the loop completes successfully.
RESULT specifies a variable for storing this value.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug dolist))
  (let* ((i (make-symbol "loopvar"))
         (var (pop spec))
         (count (pop spec))
         (result (pop spec)))
    (setq var (or (unless (eq var result) var) i)
          result (or result var))
    `(let ((,var ,count))
       (setq ,result ,var)
       (while (/= ,var 0)
         ,@body
         (if (> ,var 0)
             (setq ,var (1- ,var))
           (setq ,var (1+ ,var)))
         (setq ,result ,var))
       ,var)))

;;; Motions

(defmacro aiern-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with VAR bound to 1 or -1,
depending on the sign of COUNT. RESULT, if specified, holds
the number of unsuccessful iterations, which is 0 if the loop
completes successfully. This is also the return value.

Each iteration must move point; if point does not change,
the loop immediately quits. See also `aiern-loop'.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug ((symbolp form &optional symbolp) body)))
  (let* ((var (or (pop spec) (make-symbol "unitvar")))
         (countval (or (pop spec) 0))
         (result (pop spec))
         (i (make-symbol "loopvar"))
         (count (make-symbol "countvar"))
         (done (make-symbol "donevar"))
         (orig (make-symbol "origvar")))
    `(let* ((,count ,countval)
            (,var (if (< ,count 0) -1 1)))
       (catch ',done
         (aiern-loop (,i ,count ,result)
           (let ((,orig (point)))
             ,@body
             (when (= (point) ,orig)
               (throw ',done ,i))))))))

(defmacro aiern-signal-without-movement (&rest body)
  "Catches errors provided point moves within this scope."
  (declare (indent defun)
           (debug t))
  `(let ((p (point)))
     (condition-case err
         (progn ,@body)
       (error
        (when (= p (point))
          (signal (car err) (cdr err)))))))

(defun aiern-signal-at-bob-or-eob (&optional count)
  "Signals error if `point' is at boundaries.
If `point' is at bob and COUNT is negative this function signal
'beginning-of-buffer. If `point' is at eob and COUNT is positive
this function singal 'end-of-buffer. This function should be used
in motions. COUNT defaults to 1."
  (setq count (or count 1))
  (cond
   ((< count 0) (aiern-signal-at-bob))
   ((> count 0) (aiern-signal-at-eob))))

(defun aiern-signal-at-bob ()
  "Signals 'beginning-of-buffer if `point' is at bob.
This function should be used in backward motions. If `point' is at
bob so that no further backward motion is possible the error
'beginning-of-buffer is raised."
  (when (bobp) (signal 'beginning-of-buffer nil)))

(defun aiern-signal-at-eob ()
  "Signals 'end-of-buffer if `point' is at eob.
This function should be used in forward motions. If `point' is close
to eob so that no further forward motion is possible the error
'end-of-buffer is raised. This is the case if `point' is at
`point-max' or if is one position before `point-max',
`aiern-move-beyond-eol' is nil and `point' is not at the end
of a line. The latter is necessary because `point' cannot be
moved to `point-max' if `aiern-move-beyond-eol' is nil and
the last line in the buffer is not empty."
  (when (or (eobp)
            (and (not (eolp))
                 (not aiern-move-beyond-eol)
                 (save-excursion (forward-char) (eobp))))
    (signal 'end-of-buffer nil)))

(defmacro aiern-with-hproject-point-on-window (&rest body)
  "Project point after BODY to current window.
If point is on a position left or right of the current window
then it is moved to the left and right boundary of the window,
respectively. If `auto-hscroll-mode' is non-nil then the left and
right positions are increased or decreased, respectively, by
`horizontal-margin' so that no automatic scrolling occurs."
  (declare (indent defun)
           (debug t))
  (let ((diff (make-symbol "diff"))
        (left (make-symbol "left"))
        (right (make-symbol "right")))
    `(let ((,diff (if auto-hscroll-mode (1+ hscroll-margin) 0))
           auto-hscroll-mode)
       ,@body
       (let* ((,left (+ (window-hscroll) ,diff))
              (,right (+ (window-hscroll) (window-width) (- ,diff) -1)))
         (move-to-column (min (max (current-column) ,left) ,right))))))

(defun aiern-goto-min (&rest positions)
  "Go to the smallest position in POSITIONS.
Non-numerical elements are ignored.
See also `aiern-goto-max'."
  (when (setq positions (aiern-filter-list
                         #'(lambda (elt)
                             (not (number-or-marker-p elt)))
                         positions))
    (goto-char (apply #'min positions))))

(defun aiern-goto-max (&rest positions)
  "Go to the largest position in POSITIONS.
Non-numerical elements are ignored.
See also `aiern-goto-min'."
  (when (setq positions (aiern-filter-list
                         #'(lambda (elt)
                             (not (number-or-marker-p elt)))
                         positions))
    (goto-char (apply #'max positions))))

(defun aiern-forward-not-thing (thing &optional count)
  "Move point to the end or beginning of the complement of THING."
  (aiern-motion-loop (dir (or count 1))
    (let (bnd)
      (cond
       ((> dir 0)
        (while (and (setq bnd (bounds-of-thing-at-point thing))
                    (< (point) (cdr bnd)))
          (goto-char (cdr bnd)))
        ;; no thing at (point)
        (if (zerop (forward-thing thing))
            ;; now at the end of the next thing
            (let ((bnd (bounds-of-thing-at-point thing)))
              (if (or (< (car bnd) (point))    ; end of a thing
                      (= (car bnd) (cdr bnd))) ; zero width thing
                  (goto-char (car bnd))
                ;; beginning of yet another thing, go back
                (forward-thing thing -1)))
          (goto-char (point-max))))
       (t
        (while (and (not (bobp))
                    (or (backward-char) t)
                    (setq bnd (bounds-of-thing-at-point thing))
                    (< (point) (cdr bnd)))
          (goto-char (car bnd)))
        ;; either bob or no thing at point
        (goto-char
         (if (and (not (bobp))
                  (zerop (forward-thing thing -1))
                  (setq bnd (bounds-of-thing-at-point thing)))
             (cdr bnd)
           (point-min))))))))

(defun aiern-bounds-of-not-thing-at-point (thing &optional which)
  "Returns the bounds of a complement of THING at point.
If there is a THING at point nil is returned.  Otherwise if WHICH
is nil or 0 a cons cell (BEG . END) is returned. If WHICH is
negative the beginning is returned. If WHICH is positive the END
is returned."
  (let ((pnt (point)))
    (let ((beg (save-excursion
                 (and (zerop (forward-thing thing -1))
                      (forward-thing thing))
                 (if (> (point) pnt) (point-min) (point))))
          (end (save-excursion
                 (and (zerop (forward-thing thing))
                      (forward-thing thing -1))
                 (if (< (point) pnt) (point-max) (point)))))
      (when (and (<= beg (point)) (<= (point) end) (< beg end))
        (cond
         ((or (not which) (zerop which)) (cons beg end))
         ((< which 0) beg)
         ((> which 0) end))))))

(defun aiern-forward-nearest (count &rest forwards)
  "Moves point forward to the first of several motions.
FORWARDS is a list of forward motion functions (i.e. each moves
point forward to the next end of a text object (if passed a +1)
or backward to the preceeding beginning of a text object (if
passed a -1)). This function calls each of these functions once
and moves point to the nearest of the resulting positions. If
COUNT is positive point is moved forward COUNT times, if negative
point is moved backward -COUNT times."
  (aiern-motion-loop (dir (or count 1))
    (let ((pnt (point))
          (nxt (if (> dir 0) (point-max) (point-min))))
      (dolist (fwd forwards)
        (goto-char pnt)
        (condition-case nil
            (aiern-with-restriction
                (and (< dir 0)
                     (save-excursion
                       (goto-char nxt)
                       (line-beginning-position 0)))
                (and (> dir 0)
                     (save-excursion
                       (goto-char nxt)
                       (line-end-position 2)))
              (if (and (zerop (funcall fwd dir))
                       (/= (point) pnt)
                       (or (and (> dir 0) (< (point) nxt))
                           (and (< dir 0) (> (point) nxt))))
                  (setq nxt (point))))
          (error)))
      (goto-char nxt))))

(defun bounds-of-aiern-string-at-point (&optional state)
  "Return the bounds of a string at point.
If STATE is given it used a parsing state at point."
  (save-excursion
    (let ((state (or state (syntax-ppss))))
      (and (nth 3 state)
           (cons (nth 8 state)
                 (and (parse-partial-sexp (point)
                                          (point-max)
                                          nil
                                          nil
                                          state
                                          'syntax-table)
                      (point)))))))
(put 'aiern-string 'bounds-of-thing-at-point #'bounds-of-aiern-string-at-point)

(defun bounds-of-aiern-comment-at-point ()
  "Return the bounds of a string at point."
  (save-excursion
    (let ((state (syntax-ppss)))
      (and (nth 4 state)
           (cons (nth 8 state)
                 (and (parse-partial-sexp (point)
                                          (point-max)
                                          nil
                                          nil
                                          state
                                          'syntax-table)
                      (point)))))))
(put 'aiern-comment 'bounds-of-thing-at-point #'bounds-of-aiern-comment-at-point)

;; The purpose of this function is the provide line motions which
;; preserve the column. This is how `previous-line' and `next-line'
;; work, but unfortunately the behaviour is hard-coded: if and only if
;; the last command was `previous-line' or `next-line', the column is
;; preserved. Furthermore, in contrast to Vim, when we cannot go
;; further, those motions move point to the beginning resp. the end of
;; the line (we never want point to leave its column). The code here
;; comes from simple.el, and I hope it will work in future.
(defun aiern-line-move (count &optional noerror)
  "A wrapper for line motions which conserves the column.
Signals an error at buffer boundaries unless NOERROR is non-nil."
  (cond
   (noerror
    (condition-case nil
        (aiern-line-move count)
      (error nil)))
   (t
    (aiern-signal-without-movement
      (setq this-command (if (>= count 0)
                             #'next-line
                           #'previous-line))
      (let ((opoint (point)))
        (condition-case err
            (with-no-warnings
              (funcall this-command (abs count)))
          ((beginning-of-buffer end-of-buffer)
           (let ((col (or goal-column
                          (if (consp temporary-goal-column)
                              (car temporary-goal-column)
                            temporary-goal-column))))
             (if line-move-visual
                 (vertical-motion (cons col 0))
               (line-move-finish col opoint (< count 0)))
             ;; Maybe we should just `ding'?
             (signal (car err) (cdr err))))))))))

(defun aiern-forward-syntax (syntax &optional count)
  "Move point to the end or beginning of a sequence of characters in
SYNTAX.
Stop on reaching a character not in SYNTAX."
  (let ((notsyntax (if (= (aref syntax 0) ?^)
                       (substring syntax 1)
                     (concat "^" syntax))))
    (aiern-motion-loop (dir (or count 1))
      (cond
       ((< dir 0)
        (skip-syntax-backward notsyntax)
        (skip-syntax-backward syntax))
       (t
        (skip-syntax-forward notsyntax)
        (skip-syntax-forward syntax))))))

(defun aiern-forward-chars (chars &optional count)
  "Move point to the end or beginning of a sequence of CHARS.
CHARS is a character set as inside [...] in a regular expression."
  (let ((notchars (if (= (aref chars 0) ?^)
                      (substring chars 1)
                    (concat "^" chars))))
    (aiern-motion-loop (dir (or count 1))
      (cond
       ((< dir 0)
        (skip-chars-backward notchars)
        (skip-chars-backward chars))
       (t
        (skip-chars-forward notchars)
        (skip-chars-forward chars))))))

(defun aiern-up-block (beg end &optional count)
  "Move point to the end or beginning of text enclosed by BEG and END.
BEG and END should be regular expressions matching the opening
and closing delimiters, respectively. If COUNT is greater than
zero point is moved forward otherwise it is moved
backwards. Whenever an opening delimiter is found the COUNT is
increased by one, if a closing delimiter is found the COUNT is
decreased by one. The motion stops when COUNT reaches zero. The
match-data reflects the last successful match (that caused COUNT
to reach zero). The behaviour of this functions is similar to
`up-list'."
  (let* ((count (or count 1))
         (forwardp (> count 0))
         (dir (if forwardp +1 -1)))
    (catch 'done
      (while (not (zerop count))
        (let* ((pnt (point))
               (cl (save-excursion
                     (and (re-search-forward (if forwardp end beg) nil t dir)
                          (or (/= pnt (point))
                              (progn
                                ;; zero size match, repeat search from
                                ;; the next position
                                (forward-char dir)
                                (re-search-forward (if forwardp end beg) nil t dir)))
                          (point))))
               (match (match-data t))
               (op (save-excursion
                     (and (not (equal beg end))
                          (re-search-forward (if forwardp beg end) cl t dir)
                          (or (/= pnt (point))
                              (progn
                                ;; zero size match, repeat search from
                                ;; the next position
                                (forward-char dir)
                                (re-search-forward (if forwardp beg end) cl t dir)))
                          (point)))))
          (cond
           ((not cl)
            (goto-char (if forwardp (point-max) (point-min)))
            (set-match-data nil)
            (throw 'done count))
           (t
            (if op
                (progn
                  (setq count (if forwardp (1+ count) (1- count)))
                  (goto-char op))
              (setq count (if forwardp (1- count) (1+ count)))
              (if (zerop count) (set-match-data match))
              (goto-char cl))))))
      0)))

(defun aiern-up-paren (open close &optional count)
  "Move point to the end or beginning of balanced parentheses.
OPEN and CLOSE should be characters identifying the opening and
closing parenthesis, respectively. If COUNT is greater than zero
point is moved forward otherwise it is moved backwards. Whenever
an opening delimiter is found the COUNT is increased by one, if a
closing delimiter is found the COUNT is decreased by one. The
motion stops when COUNT reaches zero. The match-data reflects the
last successful match (that caused COUNT to reach zero)."
  ;; Always use the default `forward-sexp-function'. This is important
  ;; for modes that use a custom one like `python-mode'.
  ;; (addresses #364)
  (let (forward-sexp-function)
    (with-syntax-table (copy-syntax-table (syntax-table))
      (modify-syntax-entry open (format "(%c" close))
      (modify-syntax-entry close (format ")%c" open))
      (let ((rest (aiern-motion-loop (dir count)
                    (let ((pnt (point)))
                      (condition-case nil
                          (cond
                           ((> dir 0)
                            (while (progn
                                     (up-list dir)
                                     (/= (char-before) close))))
                           (t
                            (while (progn
                                     (up-list dir)
                                     (/= (char-after) open)))))
                        (error (goto-char pnt)))))))
        (cond
         ((= rest count) (set-match-data nil))
         ((> count 0) (set-match-data (list (1- (point)) (point))))
         (t (set-match-data (list (point) (1+ (point))))))
        rest))))

(defun aiern-up-xml-tag (&optional count)
  "Move point to the end or beginning of balanced xml tags.
OPEN and CLOSE should be characters identifying the opening and
closing parenthesis, respectively. If COUNT is greater than zero
point is moved forward otherwise it is moved backwards. Whenever
an opening delimiter is found the COUNT is increased by one, if a
closing delimiter is found the COUNT is decreased by one. The
motion stops when COUNT reaches zero. The match-data reflects the
last successful match (that caused COUNT to reach zero)."
  (let* ((dir (if (> (or count 1) 0) +1 -1))
         (count (abs (or count 1)))
         (op (if (> dir 0) 1 2))
         (cl (if (> dir 0) 2 1))
         (orig (point))
         pnt tags match)
    (catch 'done
      (while (> count 0)
        ;; find the previous opening tag
        (while
            (and (setq match
                       (re-search-forward
                        "<\\([^/ >\n]+\\)\\(?:[^\"/>]\\|\"[^\"]*\"\\)*?>\\|</\\([^>]+?\\)>"
                        nil t dir))
                 (cond
                  ((match-beginning op)
                   (push (match-string op) tags))
                  ((null tags) nil) ; free closing tag
                  ((and (< dir 0)
                        (string= (car tags) (match-string cl)))
                   ;; in backward direction we only accept matching
                   ;; tags. If the current tag is a free opener
                   ;; without matching closing tag, the subsequents
                   ;; test will make us ignore this tag
                   (pop tags))
                  ((and (> dir 0))
                   ;; non matching openers are considered free openers
                   (while (and tags
                               (not (string= (car tags)
                                             (match-string cl))))
                     (pop tags))
                   (pop tags)))))
        (unless (setq match (and match (match-data t)))
          (setq match nil)
          (throw 'done count))
        ;; found closing tag, look for corresponding opening tag
        (cond
         ((> dir 0)
          (setq pnt (match-end 0))
          (goto-char (match-beginning 0)))
         (t
          (setq pnt (match-beginning 0))
          (goto-char (match-end 0))))
        (let* ((tag (match-string cl))
               (refwd (concat "<\\(/\\)?"
                              (regexp-quote tag)
                              "\\(?:>\\|[ \n]\\(?:[^\"/>]\\|\"[^\"]*\"\\)*?>\\)"))
               (cnt 1))
          (while (and (> cnt 0) (re-search-backward refwd nil t dir))
            (setq cnt (+ cnt (if (match-beginning 1) dir (- dir)))))
          (if (zerop cnt) (setq count (1- count) tags nil))
          (goto-char pnt)))
      (if (> count 0)
          (set-match-data nil)
        (set-match-data match)
        (goto-char (if (> dir 0) (match-end 0) (match-beginning 0)))))
    ;; if not found, set to point-max/point-min
    (unless (zerop count)
      (set-match-data nil)
      (goto-char (if (> dir 0) (point-max) (point-min)))
      (if (/= (point) orig) (setq count (1- count))))
    (* dir count)))

(defun aiern-forward-quote (quote &optional count)
  "Move point to the end or beginning of a string.
QUOTE is the character delimiting the string. If COUNT is greater
than zero point is moved forward otherwise it is moved
backwards."
  (let (reset-parser)
    (with-syntax-table (copy-syntax-table (syntax-table))
      (unless (= (char-syntax quote) ?\")
        (modify-syntax-entry quote "\"")
        (setq reset-parser t))
      ;; global parser state is out of state, use local one
      (let* ((pnt (point))
             (state (save-excursion
                      (beginning-of-defun)
                      (parse-partial-sexp (point) pnt nil nil (syntax-ppss))))
             (bnd (bounds-of-aiern-string-at-point state)))
        (when (and bnd (< (point) (cdr bnd)))
          ;; currently within a string
          (if (> count 0)
              (progn
                (goto-char (cdr bnd))
                (setq count (1- count)))
            (goto-char (car bnd))
            (setq count (1+ count))))
        ;; forward motions work with local parser state
        (cond
         ((> count 0)
          ;; no need to reset global parser state because we only use
          ;; the local one
          (setq reset-parser nil)
          (catch 'done
            (while (and (> count 0) (not (eobp)))
              (setq state (parse-partial-sexp (point) (point-max)
                                              nil
                                              nil
                                              state
                                              'syntax-table))
              (cond
               ((nth 3 state)
                (setq bnd (bounds-of-thing-at-point 'aiern-string))
                (goto-char (cdr bnd))
                (setq count (1- count)))
               ((eobp) (goto-char pnt) (throw 'done nil))))))
         ((< count 0)
          ;; need to update global cache because of backward motion
          (setq reset-parser (and reset-parser (point)))
          (save-excursion
            (beginning-of-defun)
            (syntax-ppss-flush-cache (point)))
          (catch 'done
            (while (and (< count 0) (not (bobp)))
              (setq pnt (point))
              (while (and (not (bobp))
                          (or (eobp) (/= (char-after) quote)))
                (backward-char))
              (cond
               ((setq bnd (bounds-of-thing-at-point 'aiern-string))
                (goto-char (car bnd))
                (setq count (1+ count)))
               ((bobp) (goto-char pnt) (throw 'done nil))
               (t (backward-char))))))
         (t (setq reset-parser nil)))))
    (when reset-parser
      ;; reset global cache
      (save-excursion
        (goto-char reset-parser)
        (beginning-of-defun)
        (syntax-ppss-flush-cache (point))))
    count))

;;; Thing-at-point motion functions for aiern text objects and motions
(defun forward-aiern-empty-line (&optional count)
  "Move forward COUNT empty lines."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (while (and (> count 0) (not (eobp)))
      (when (and (bolp) (eolp))
        (setq count (1- count)))
      (forward-line 1)))
   (t
    (while (and (< count 0) (not (bobp))
                (zerop (forward-line -1)))
      (when (and (bolp) (eolp))
        (setq count (1+ count))))))
  count)

(defun forward-aiern-space (&optional count)
  "Move forward COUNT whitespace sequences [[:space:]]+."
  (aiern-forward-chars "[:space:]" count))

(defun forward-aiern-word (&optional count)
  "Move forward COUNT words.
Moves point COUNT words forward or (- COUNT) words backward if
COUNT is negative. Point is placed after the end of the word (if
forward) or at the first character of the word (if backward). A
word is a sequence of word characters matching
\[[:word:]] (recognized by `forward-word'), a sequence of
non-whitespace non-word characters '[^[:word:]\\n\\r\\t\\f ]', or
an empty line matching ^$."
  (aiern-forward-nearest
   count
   #'(lambda (&optional cnt)
       (let ((word-separating-categories aiern-cjk-word-separating-categories)
             (word-combining-categories aiern-cjk-word-combining-categories)
             (pnt (point)))
         (forward-word cnt)
         (if (= pnt (point)) cnt 0)))
   #'(lambda (&optional cnt)
       (aiern-forward-chars "^[:word:]\n\r\t\f " cnt))
   #'forward-aiern-empty-line))

(defun forward-aiern-WORD (&optional count)
  "Move forward COUNT \"WORDS\".
Moves point COUNT WORDS forward or (- COUNT) WORDS backward if
COUNT is negative. Point is placed after the end of the WORD (if
forward) or at the first character of the WORD (if backward). A
WORD is a sequence of non-whitespace characters
'[^\\n\\r\\t\\f ]', or an empty line matching ^$."
  (aiern-forward-nearest count
                        #'(lambda (&optional cnt)
                            (aiern-forward-chars "^\n\r\t\f " cnt))
                        #'forward-aiern-empty-line))

(defun forward-aiern-symbol (&optional count)
  "Move forward COUNT symbols.
Moves point COUNT symbols forward or (- COUNT) symbols backward
if COUNT is negative. Point is placed after the end of the
symbol (if forward) or at the first character of the symbol (if
backward). A symbol is either determined by `forward-symbol', or
is a sequence of characters not in the word, symbol or whitespace
syntax classes."
  (aiern-forward-nearest
   count
   #'(lambda (&optional cnt)
       (aiern-forward-syntax "^w_->" cnt))
   #'(lambda (&optional cnt)
       (let ((pnt (point)))
         (forward-symbol cnt)
         (if (= pnt (point)) cnt 0)))
   #'forward-aiern-empty-line))

(defun forward-aiern-defun (&optional count)
  "Move forward COUNT defuns.
Moves point COUNT defuns forward or (- COUNT) defuns backward
if COUNT is negative.  A defun is defined by
`beginning-of-defun' and `end-of-defun' functions."
  (aiern-motion-loop (dir (or count 1))
    (if (> dir 0) (end-of-defun) (beginning-of-defun))))

(defun forward-aiern-sentence (&optional count)
  "Move forward COUNT sentences.
Moves point COUNT sentences forward or (- COUNT) sentences
backward if COUNT is negative.  This function is the same as
`forward-sentence' but returns the number of sentences that could
NOT be moved over."
  (aiern-motion-loop (dir (or count 1))
    (condition-case nil
        (forward-sentence dir)
      (error))))

(defun forward-aiern-paragraph (&optional count)
  "Move forward COUNT paragraphs.
Moves point COUNT paragraphs forward or (- COUNT) paragraphs backward
if COUNT is negative.  A paragraph is defined by
`start-of-paragraph-text' and `forward-paragraph' functions."
  (aiern-motion-loop (dir (or count 1))
    (cond
     ((> dir 0) (forward-paragraph))
     ((not (bobp)) (start-of-paragraph-text) (beginning-of-line)))))

(defvar aiern-forward-quote-char ?\"
  "The character to be used by `forward-aiern-quote'.")

(defun forward-aiern-quote (&optional count)
  "Move forward COUNT strings.
The quotation character is specified by the global variable
`aiern-forward-quote-char'. This character is passed to
`aiern-forward-quote'."
  (aiern-forward-quote aiern-forward-quote-char count))

(defun forward-aiern-quote-simple (&optional count)
  "Move forward COUNT strings.
The quotation character is specified by the global variable
`aiern-forward-quote-char'. This functions uses Vim's rules
parsing from the beginning of the current line for quotation
characters. It should only be used when looking for strings
within comments and buffer *must* be narrowed to the comment."
  (let ((dir (if (> (or count 1) 0) 1 -1))
        (ch aiern-forward-quote-char)
        (pnt (point))
        (cnt 0))
    (beginning-of-line)
    ;; count number of quotes before pnt
    (while (< (point) pnt)
      (when (= (char-after) ch)
        (setq cnt (1+ cnt)))
      (forward-char))
    (setq cnt (- (* 2 (abs count)) (mod cnt 2)))
    (cond
     ((> dir 0)
      (while (and (not (eolp)) (not (zerop cnt)))
        (when (= (char-after) ch) (setq cnt (1- cnt)))
        (forward-char))
      (when (not (zerop cnt)) (goto-char (point-max))))
     (t
      (while (and (not (bolp)) (not (zerop cnt)))
        (when (= (char-before) ch) (setq cnt (1- cnt)))
        (forward-char -1))
      (when (not (zerop cnt)) (goto-char (point-min)))))
    (/ cnt 2)))

;;; Motion functions
(defun aiern-forward-beginning (thing &optional count)
  "Move forward to beginning of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (if (< count 0)
      (forward-thing thing count)
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when (and bnd (< (point) (cdr bnd)))
        (goto-char (cdr bnd)))
      (condition-case nil
          (when (zerop (setq rest (forward-thing thing count)))
            (when (and (bounds-of-thing-at-point thing)
                       (not (bobp))
                       ;; handle final empty line
                       (not (and (bolp) (eobp))))
              (forward-char -1))
            (beginning-of-thing thing))
        (error))
      rest)))

(defun aiern-backward-beginning (thing &optional count)
  "Move backward to beginning of THING.
The motion is repeated COUNT times. This is the same as calling
`aiern-backward-beginning' with -COUNT."
  (aiern-forward-beginning thing (- (or count 1))))

(defun aiern-forward-end (thing &optional count)
  "Move forward to end of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (unless (eobp) (forward-char))
    (prog1 (forward-thing thing count)
      (unless (bobp) (forward-char -1))))
   (t
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when (and bnd (< (point) (cdr bnd) ))
        (goto-char (car bnd)))
      (condition-case nil
          (when (zerop (setq rest (forward-thing thing count)))
            (end-of-thing thing)
            (forward-char -1))
        (error))
      rest))))

(defun aiern-backward-end (thing &optional count)
  "Move backward to end of THING.
The motion is repeated COUNT times. This is the same as calling
`aiern-backward-end' with -COUNT."
  (aiern-forward-end thing (- (or count 1))))

(defun aiern-forward-word (&optional count)
  "Move by words.
Moves point COUNT words forward or (- COUNT) words backward if
COUNT is negative. This function is the same as `forward-word'
but returns the number of words by which point could *not* be
moved."
  (setq count (or count 1))
  (let* ((dir (if (>= count 0) +1 -1))
         (count (abs count)))
    (while (and (> count 0)
                (forward-word dir))
      (setq count (1- count)))
    count))

(defun aiern-in-comment-p (&optional pos)
  "Checks if POS is within a comment according to current syntax.
If POS is nil, (point) is used. The return value is the beginning
position of the comment."
  (setq pos (or pos (point)))
  (let ((chkpos
         (cond
          ((eobp) pos)
          ((= (char-syntax (char-after)) ?<) (1+ pos))
          ((and (not (zerop (logand (car (syntax-after (point)))
                                    (lsh 1 16))))
                (not (zerop (logand (or (car (syntax-after (1+ (point)))) 0)
                                    (lsh 1 17)))))
           (+ pos 2))
          ((and (not (zerop (logand (car (syntax-after (point)))
                                    (lsh 1 17))))
                (not (zerop (logand (or (car (syntax-after (1- (point)))) 0)
                                    (lsh 1 16)))))
           (1+ pos))
          (t pos))))
    (let ((syn (save-excursion (syntax-ppss chkpos))))
      (and (nth 4 syn) (nth 8 syn)))))

(defun aiern-looking-at-start-comment (&optional move)
  "Returns t if point is at the start of a comment.
point must be on one of the opening characters of a block comment
according to the current syntax table. Futhermore these
characters must been parsed as opening characters, i.e. they
won't be considered as comment starters inside a string or
possibly another comment. Point is moved to the first character
of the comment opener if MOVE is non-nil."
  (cond
   ;; one character opener
   ((= (char-syntax (char-after)) ?<)
    (equal (point) (aiern-in-comment-p (1+ (point)))))
   ;; two character opener on first char
   ((and (not (zerop (logand (car (syntax-after (point)))
                             (lsh 1 16))))
         (not (zerop (logand (or (car (syntax-after (1+ (point)))) 0)
                             (lsh 1 17)))))
    (equal (point) (aiern-in-comment-p (+ 2 (point)))))
   ;; two character opener on second char
   ((and (not (zerop (logand (car (syntax-after (point)))
                             (lsh 1 17))))
         (not (zerop (logand (or (car (syntax-after (1- (point)))) 0)
                             (lsh 1 16)))))
    (and (equal (1- (point)) (aiern-in-comment-p (1+ (point))))
         (prog1 t (when move (backward-char)))))))

(defun aiern-looking-at-end-comment (&optional move)
  "Returns t if point is at the end of a comment.
point must be on one of the opening characters of a block comment
according to the current syntax table. Futhermore these
characters must been parsed as opening characters, i.e. they
won't be considered as comment starters inside a string or
possibly another comment. Point is moved right after the comment
closer if MOVE is non-nil."
  (cond
   ;; one char closer
   ((= (char-syntax (char-after)) ?>)
    (and (aiern-in-comment-p) ; in comment
         (not (aiern-in-comment-p (1+ (point))))
         (prog1 t (when move (forward-char)))))
   ;; two char closer on first char
   ((and (not (zerop (logand (car (syntax-after (point)))
                             (lsh 1 18))))
         (not (zerop (logand (or (car (syntax-after (1+ (point)))) 0)
                             (lsh 1 19)))))
    (and (aiern-in-comment-p)
         (not (aiern-in-comment-p (+ (point) 2)))
         (prog1 t (when move (forward-char 2)))))
   ;; two char closer on second char
   ((and (not (zerop (logand (car (syntax-after (point)))
                             (lsh 1 19))))
         (not (zerop (logand (or (car (syntax-after (1- (point)))) 0)
                             (lsh 1 18)))))
    (and (aiern-in-comment-p)
         (not (aiern-in-comment-p (1+ (point))))
         (prog1 t (when move (forward-char)))))))

(defun aiern-insert-newline-above ()
  "Inserts a new line above point and places point in that line
with regard to indentation."
  (aiern-narrow-to-field
    (aiern-move-beginning-of-line)
    (insert (if use-hard-newlines hard-newline "\n"))
    (forward-line -1)
    (back-to-indentation)))

(defun aiern-insert-newline-below ()
  "Inserts a new line below point and places point in that line
with regard to indentation."
  (aiern-narrow-to-field
    (aiern-move-end-of-line)
    (insert (if use-hard-newlines hard-newline "\n"))
    (back-to-indentation)))

;;; Markers

(defun aiern-global-marker-p (char)
  "Whether CHAR denotes a global marker."
  (or (and (>= char ?A) (<= char ?Z))
      (assq char (default-value 'aiern-markers-alist))))

(defun aiern-set-marker (char &optional pos advance)
  "Set the marker denoted by CHAR to position POS.
POS defaults to the current position of point.
If ADVANCE is t, the marker advances when inserting text at it;
otherwise, it stays behind."
  (interactive (list (read-char)))
  (catch 'done
    (let ((marker (aiern-get-marker char t)) alist)
      (unless (markerp marker)
        (cond
         ((and marker (symbolp marker) (boundp marker))
          (set marker (or (symbol-value marker) (make-marker)))
          (setq marker (symbol-value marker)))
         ((eq marker 'aiern-jump-backward-swap)
          (aiern-set-jump)
          (throw 'done nil))
         ((functionp marker)
          (user-error "Cannot set special marker `%c'" char))
         ((aiern-global-marker-p char)
          (setq alist (default-value 'aiern-markers-alist)
                marker (make-marker))
          (aiern--add-to-alist 'alist char marker)
          (setq-default aiern-markers-alist alist))
         (t
          (setq marker (make-marker))
          (aiern--add-to-alist 'aiern-markers-alist char marker))))
      (add-hook 'kill-buffer-hook #'aiern-swap-out-markers nil t)
      (set-marker-insertion-type marker advance)
      (set-marker marker (or pos (point))))))

(defun aiern-get-marker (char &optional raw)
  "Return the marker denoted by CHAR.
This is either a marker object as returned by `make-marker',
a number, a cons cell (FILE . POS) with FILE being a string
and POS a number, or nil. If RAW is non-nil, then the
return value may also be a variable, a movement function,
or a marker object pointing nowhere."
  (let ((marker (if (aiern-global-marker-p char)
                    (cdr-safe (assq char (default-value
                                           'aiern-markers-alist)))
                  (cdr-safe (assq char aiern-markers-alist)))))
    (save-excursion
      (if raw
          marker
        (when (and (symbolp marker) (boundp marker))
          (setq marker (symbol-value marker)))
        (when (functionp marker)
          (save-window-excursion
            (funcall marker)
            (setq marker (move-marker (make-marker) (point)))))
        (when (markerp marker)
          (if (eq (marker-buffer marker) (current-buffer))
              (setq marker (marker-position marker))
            (setq marker (and (marker-buffer marker) marker))))
        (when (or (numberp marker)
                  (markerp marker)
                  (and (consp marker)
                       (stringp (car marker))
                       (numberp (cdr marker))))
          marker)))))

(defun aiern-swap-out-markers ()
  "Turn markers into file references when the buffer is killed."
  (and buffer-file-name
       (dolist (entry aiern-markers-alist)
         (and (markerp (cdr entry))
              (eq (marker-buffer (cdr entry)) (current-buffer))
              (setcdr entry (cons buffer-file-name
                                  (marker-position (cdr entry))))))))
(put 'aiern-swap-out-markers 'permanent-local-hook t)

(defun aiern--eval-expr (input)
  "Eval INPUT and return stringified result, if of a suitable type.
If INPUT starts with a number, +, -, or . use `calc-eval' instead."
  (let* ((first-char (car (remove ?\s (string-to-list input))))
         (calcable-p (and first-char (or (<= ?0 first-char ?9)
                                         (memq first-char '(?- ?+ ?.)))))
         (result (if calcable-p
                     (let ((calc-multiplication-has-precedence nil))
                       (calc-eval input))
                   (eval (car (read-from-string input))))))
    (cond
     (calcable-p result)
     ((or (stringp result)
          (numberp result)
          (symbolp result))
      (format "%s" result))
     ((sequencep result)
      (mapconcat (lambda (x) (format "%s" x)) result "\n"))
     (t (user-error "Using %s as a string" (type-of result))))))

(defun aiern-get-register (register &optional noerror)
  "Return contents of REGISTER.
Signal an error if empty, unless NOERROR is non-nil.

The following special registers are supported.
  \"  the unnamed register
  *  the clipboard contents
  +  the clipboard contents
  <C-w> the word at point (ex mode only)
  <C-a> the WORD at point (ex mode only)
  <C-o> the symbol at point (ex mode only)
  <C-f> the current file at point (ex mode only)
  %  the current file name (read only)
  #  the alternate file name (read only)
  /  the last search pattern (read only)
  :  the last command line (read only)
  .  the last inserted text (read only)
  -  the last small (less than a line) delete
  _  the black hole register
  =  the expression register (read only)"
  (condition-case err
      (when (characterp register)
        (or (cond
             ((eq register ?\")
              (current-kill 0))
             ((<= ?1 register ?9)
              (let ((reg (- register ?1)))
                (and (< reg (length kill-ring))
                     (current-kill reg t))))
             ((memq register '(?* ?+))
              ;; the following code is modified from
              ;; `x-selection-value-internal'
              (let ((what (if (eq register ?*) 'PRIMARY 'CLIPBOARD))
                    (request-type (or (and (boundp 'x-select-request-type)
                                           x-select-request-type)
                                      '(UTF8_STRING COMPOUND_TEXT STRING)))
                    text)
                (unless (consp request-type)
                  (setq request-type (list request-type)))
                (while (and request-type (not text))
                  (condition-case nil
                      (setq text (aiern-get-selection what (pop request-type)))
                    (error nil)))
                (when text
                  (remove-text-properties 0 (length text) '(foreign-selection nil) text))
                text))
             ((eq register ?\C-W)
              (unless (aiern-ex-p)
                (user-error "Register <C-w> only available in ex state"))
              (with-current-buffer aiern-ex-current-buffer
                (thing-at-point 'aiern-word)))
             ((eq register ?\C-A)
              (unless (aiern-ex-p)
                (user-error "Register <C-a> only available in ex state"))
              (with-current-buffer aiern-ex-current-buffer
                (thing-at-point 'aiern-WORD)))
             ((eq register ?\C-O)
              (unless (aiern-ex-p)
                (user-error "Register <C-o> only available in ex state"))
              (with-current-buffer aiern-ex-current-buffer
                (thing-at-point 'aiern-symbol)))
             ((eq register ?\C-F)
              (unless (aiern-ex-p)
                (user-error "Register <C-f> only available in ex state"))
              (with-current-buffer aiern-ex-current-buffer
                (thing-at-point 'filename)))
             ((eq register ?%)
              (or (buffer-file-name (and (aiern-ex-p)
                                         (minibufferp)
                                         aiern-ex-current-buffer))
                  (user-error "No file name")))
             ((= register ?#)
              (or (with-current-buffer (other-buffer) (buffer-file-name))
                  (user-error "No file name")))
             ((eq register ?/)
              (or (car-safe
                   (or (and (boundp 'aiern-search-module)
                            (eq aiern-search-module 'aiern-search)
                            aiern-ex-search-history)
                       (and isearch-regexp regexp-search-ring)
                       search-ring))
                  (user-error "No previous regular expression")))
             ((eq register ?:)
              (or (car-safe aiern-ex-history)
                  (user-error "No previous command line")))
             ((eq register ?.)
              aiern-last-insertion)
             ((eq register ?-)
              aiern-last-small-deletion)
             ((eq register ?=)
              (let ((enable-recursive-minibuffers t))
                (setq aiern-last-=-register-input
                      (minibuffer-with-setup-hook
                          (lambda () (when aiern-last-=-register-input
                                       (add-hook 'pre-command-hook #'aiern-ex-remove-default)))
                        (read-from-minibuffer
                         "="
                         (and aiern-last-=-register-input
                              (propertize aiern-last-=-register-input 'face 'shadow))
                         aiern-eval-map
                         nil
                         'aiern-eval-history
                         aiern-last-=-register-input
                         t)))
                (aiern--eval-expr aiern-last-=-register-input)))
             ((eq register ?_) ; the black hole register
              "")
             (t
              (setq register (downcase register))
              (get-register register)))
            (user-error "Register `%c' is empty" register)))
    (error (unless noerror (signal (car err) (cdr err))))))

(defun aiern-append-register (register text)
  "Append TEXT to the contents of register REGISTER."
  (let ((content (get-register register)))
    (cond
     ((not content)
      (set-register register text))
     ((or (text-property-not-all 0 (length content)
                                 'yank-handler nil
                                 content)
          (text-property-not-all 0 (length text)
                                 'yank-handler nil
                                 text))
      ;; some non-trivial yank-handler -> always switch to line handler
      ;; ensure complete lines
      (when (and (> (length content) 0)
                 (/= (aref content (1- (length content))) ?\n))
        (setq content (concat content "\n")))
      (when (and (> (length text) 0)
                 (/= (aref text (1- (length text))) ?\n))
        (setq text (concat text "\n")))
      (setq text (concat content text))
      (remove-list-of-text-properties 0 (length text) '(yank-handler) text)
      (setq text (propertize text 'yank-handler '(aiern-yank-line-handler)))
      (set-register register text))
     (t
      (set-register register (concat content text))))))

(defun aiern-set-register (register text)
  "Set the contents of register REGISTER to TEXT.
If REGISTER is an upcase character then text is appended to that
register instead of replacing its content."
  (cond
   ((not (characterp register))
    (user-error "Invalid register"))
   ;; don't allow modification of read-only registers
   ((member register '(?: ?. ?%))
    (user-error "Can't modify read-only register"))
   ((eq register ?\")
    (kill-new text))
   ((and (<= ?1 register) (<= register ?9))
    (if (null kill-ring)
        (kill-new text)
      (let ((kill-ring-yank-pointer kill-ring-yank-pointer)
            interprogram-paste-function
            interprogram-cut-function)
        (current-kill (- register ?1))
        (setcar kill-ring-yank-pointer text))))
   ((eq register ?*)
    (aiern-set-selection 'PRIMARY text))
   ((eq register ?+)
    (aiern-set-selection 'CLIPBOARD text))
   ((eq register ?-)
    (setq aiern-last-small-deletion text))
   ((eq register ?_) ; the black hole register
    nil)
   ((and (<= ?A register) (<= register ?Z))
    (aiern-append-register (downcase register) text))
   (t
    (set-register register text))))

(defun aiern-register-list ()
  "Returns an alist of all registers, but only those named
with number or character. Registers with symbol or string in names are ignored
to keep Vim compatibility with register jumps."
  (sort (append (mapcar #'(lambda (reg)
                            (cons reg (aiern-get-register reg t)))
                        '(?\" ?* ?+ ?% ?# ?/ ?: ?. ?-
                              ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
                (list (cons ?= aiern-last-=-register-input))
                (cl-remove-if-not (lambda (reg) (number-or-marker-p (car reg))) register-alist)
                nil)
        #'(lambda (reg1 reg2) (< (car reg1) (car reg2)))))

(defsubst aiern-kbd-macro-suppress-motion-error ()
  "Returns non-nil if a motion error should be suppressed.
Whether the motion error should be suppressed depends on the
variable `aiern-kbd-macro-suppress-motion-error'."
  (or (and defining-kbd-macro
           (memq aiern-kbd-macro-suppress-motion-error '(t record)))
      (and executing-kbd-macro
           (memq aiern-kbd-macro-suppress-motion-error '(t replay)))))

;;; Region

;; `set-mark' does too much at once
(defun aiern-move-mark (pos)
  "Set buffer's mark to POS.
If POS is nil, delete the mark."
  (when pos
    (setq pos (aiern-normalize-position pos)))
  (set-marker (mark-marker) pos))

(defun aiern-save-transient-mark-mode ()
  "Save Transient Mark mode and make it buffer-local.
Any changes to Transient Mark mode are now local to the current
buffer, until `aiern-restore-transient-mark-mode' is called.

Variables pertaining to Transient Mark mode are listed in
`aiern-transient-vars', and their values are stored in
`aiern-transient-vals'."
  (dolist (var aiern-transient-vars)
    (when (and (boundp var)
               (not (assq var aiern-transient-vals)))
      (push (list var (symbol-value var)
                  (local-variable-p var))
            aiern-transient-vals)
      (make-variable-buffer-local var)
      (put var 'permanent-local t))))

(defun aiern-restore-transient-mark-mode ()
  "Restore Transient Mark mode.
This presupposes that `aiern-save-transient-mark-mode' has been
called earlier. If Transient Mark mode was disabled before but
enabled in the meantime, this function disables it; if it was
enabled before but disabled in the meantime, this function
enables it.

The earlier settings of Transient Mark mode are stored in
`aiern-transient-vals'."
  (let (entry local var val)
    (while (setq entry (pop aiern-transient-vals))
      (setq var (pop entry)
            val (pop entry)
            local (pop entry))
      (unless local
        (kill-local-variable var))
      (unless (equal (symbol-value var) val)
        (if (fboundp var)
            (funcall var (if val 1 -1))
          (setq var val))))))

(defun aiern-save-mark ()
  "Save the current mark, including whether it is transient.
See also `aiern-restore-mark'."
  (unless aiern-visual-previous-mark
    (setq aiern-visual-previous-mark (mark t))
    (aiern-save-transient-mark-mode)))

(defun aiern-restore-mark ()
  "Restore the mark, including whether it was transient.
See also `aiern-save-mark'."
  (when aiern-visual-previous-mark
    (aiern-restore-transient-mark-mode)
    (aiern-move-mark aiern-visual-previous-mark)
    (setq aiern-visual-previous-mark nil)))

;; In theory, an active region implies Transient Mark mode, and
;; disabling Transient Mark mode implies deactivating the region.
;; In practice, Emacs never clears `mark-active' except in Transient
;; Mark mode, so we define our own toggle functions to make things
;; more predictable.
(defun aiern-transient-mark (&optional arg)
  "Toggle Transient Mark mode.
Ensure that the region is properly deactivated.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if transient-mark-mode -1 1)))
  (cond
   ((< arg 1)
    (aiern-active-region -1)
    ;; Transient Mark mode cannot be disabled
    ;; while CUA mode is enabled
    (when (fboundp 'cua-mode)
      (cua-mode -1))
    (when transient-mark-mode
      (transient-mark-mode -1)))
   (t
    (unless transient-mark-mode
      (aiern-active-region -1)
      (transient-mark-mode 1)))))

(defun aiern-active-region (&optional arg)
  "Toggle active region.
Ensure that Transient Mark mode is properly enabled.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if (region-active-p) -1 1)))
  (cond
   ((and (< arg 1))
    (when (or transient-mark-mode mark-active)
      (setq mark-active nil
            deactivate-mark nil)
      (when (boundp 'cua--explicit-region-start)
        (setq cua--explicit-region-start nil))
      (run-hooks 'deactivate-mark-hook)))
   (t
    (aiern-transient-mark 1)
    (when deactivate-mark
      (setq deactivate-mark nil))
    (unless (mark t)
      (aiern-move-mark (point)))
    (unless (region-active-p)
      (set-mark (mark t)))
    (when (boundp 'cua--explicit-region-start)
      (setq cua--explicit-region-start t)))))

(defmacro aiern-with-transient-mark-mode (&rest body)
  "Execute BODY with Transient Mark mode.
Then restore Transient Mark mode to its previous setting."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         aiern-transient-vals)
     (unwind-protect
         (progn
           (aiern-save-transient-mark-mode)
           (aiern-transient-mark 1)
           ,@body)
       (aiern-restore-transient-mark-mode))))

(defmacro aiern-with-active-region (beg end &rest body)
  "Execute BODY with an active region from BEG to END."
  (declare (indent 2)
           (debug t))
  `(let ((beg ,beg) (end ,end)
         aiern-transient-vals)
     (aiern-with-transient-mark-mode
       (save-excursion
         (aiern-active-region 1)
         (aiern-move-mark beg)
         (goto-char end)
         ,@body))))

(defun aiern-exchange-point-and-mark ()
  "Exchange point and mark without activating the region."
  (let* ((point (point))
         (mark  (or (mark t) point)))
    (set-marker (mark-marker) point)
    (goto-char mark)))

(defun aiern-apply-on-block (func beg end pass-columns &rest args)
  "Call FUNC for each line of a block selection.
The selection is specified by the region BEG and END.  FUNC must
take at least two arguments, the beginning and end of each
line. If PASS-COLUMNS is non-nil, these values are the columns,
otherwise tey are buffer positions. Extra arguments to FUNC may
be passed via ARGS."
  (let ((eol-col (and (memq last-command '(next-line previous-line))
                      (numberp temporary-goal-column)
                      temporary-goal-column))
        startcol startpt endcol endpt)
    (save-excursion
      (goto-char beg)
      (setq startcol (current-column))
      (beginning-of-line)
      (setq startpt (point))
      (goto-char end)
      (setq endcol (current-column))
      (forward-line 1)
      (setq endpt (point-marker))
      ;; ensure the start column is the left one.
      (aiern-sort startcol endcol)
      ;; maybe find maximal column
      (when eol-col
        (setq eol-col 0)
        (goto-char startpt)
        (while (< (point) endpt)
          (setq eol-col (max eol-col
                             (aiern-column (line-end-position))))
          (forward-line 1))
        (setq endcol (max endcol
                          (min eol-col
                               (1+ (min (1- most-positive-fixnum)
                                        (truncate temporary-goal-column)))))))
      ;; start looping over lines
      (goto-char startpt)
      (while (< (point) endpt)
        (if pass-columns
            (apply func startcol endcol args)
          (apply func
                 (save-excursion (aiern-move-to-column startcol))
                 (save-excursion (aiern-move-to-column endcol t))
                 args))
        (forward-line 1)))))

(defun aiern-apply-on-rectangle (function start end &rest args)
  "Like `apply-on-rectangle' but maybe extends to eol.
If `temporary-goal-column' is set to a big number, then the
region of each line is extended to the end of each line. The end
column is set to the maximal column in all covered lines."
  (apply #'aiern-apply-on-block function start end t args))

;;; Insertion

(defun aiern-concat-ranges (ranges)
  "Concatenate RANGES.
RANGES must be a list of ranges.  They must be ordered so that
successive ranges share their boundaries.  The return value is a
single range of disjoint union of the ranges or nil if the
disjoint union is not a single range."
  (let ((range (car-safe ranges)) (ranges (cdr ranges)) r)
    (while (and range (setq r (car-safe ranges)))
      (setq range
            (cond ((and (= (cdr r) (car range))) (cons (car r) (cdr range)))
                  ((and (= (cdr range) (car r))) (cons (car range) (cdr r)))))
      (setq ranges (cdr ranges)))
    range))

(defun aiern-track-last-insertion (beg end len)
  "Track the last insertion range and its text.
The insertion range is stored as a pair of buffer positions in
`aiern-current-insertion'. If a subsequent change is compatible,
then the current range is modified, otherwise it is replaced by a
new range. Compatible changes are changes that do not create a
disjoin range."
  ;; deletion
  (when (> len 0)
    (if (and aiern-current-insertion
             (>= beg (car aiern-current-insertion))
             (<= (+ beg len) (cdr aiern-current-insertion)))
        (setcdr aiern-current-insertion
                (- (cdr aiern-current-insertion) len))
      (setq aiern-current-insertion nil)))
  ;; insertion
  (if (and aiern-current-insertion
           (>= beg (car aiern-current-insertion))
           (<= beg (cdr aiern-current-insertion)))
      (setcdr aiern-current-insertion
              (+ (- end beg)
                 (cdr aiern-current-insertion)))
    (setq aiern-current-insertion (cons beg end))))
(put 'aiern-track-last-insertion 'permanent-local-hook t)

(defun aiern-start-track-last-insertion ()
  "Start tracking the last insertion."
  (setq aiern-current-insertion nil)
  (add-hook 'after-change-functions #'aiern-track-last-insertion nil t))

(defun aiern-stop-track-last-insertion ()
  "Stop tracking the last insertion.
The tracked insertion is set to `aiern-last-insertion'."
  (setq aiern-last-insertion
        (and aiern-current-insertion
             ;; Check whether the insertion range is a valid buffer
             ;; range.  If a buffer modification is done from within
             ;; another change hook or modification-hook (yasnippet
             ;; does this using overlay modification-hooks), then the
             ;; insertion information may be invalid. There is no way
             ;; to detect this situation, but at least we should
             ;; ensure that no error occurs (see bug #272).
             (>= (car aiern-current-insertion) (point-min))
             (<= (cdr aiern-current-insertion) (point-max))
             (buffer-substring-no-properties (car aiern-current-insertion)
                                             (cdr aiern-current-insertion))))
  (remove-hook 'after-change-functions #'aiern-track-last-insertion t))

;;; Paste

(defun aiern-yank-characters (beg end &optional register yank-handler)
  "Saves the characters defined by the region BEG and END in the kill-ring."
  (let ((text (filter-buffer-substring beg end)))
    (when yank-handler
      (setq text (propertize text 'yank-handler (list yank-handler))))
    (when register
      (aiern-set-register register text))
    (when aiern-was-yanked-without-register
      (aiern-set-register ?0 text)) ; "0 register contains last yanked text
    (unless (eq register ?_)
      (kill-new text))))

(defun aiern-yank-lines (beg end &optional register yank-handler)
  "Saves the lines in the region BEG and END into the kill-ring."
  (let* ((text (filter-buffer-substring beg end))
         (yank-handler (list (or yank-handler
                                 #'aiern-yank-line-handler)
                             nil
                             t)))
    ;; Ensure the text ends with a newline. This is required
    ;; if the deleted lines were the last lines in the buffer.
    (when (or (zerop (length text))
              (/= (aref text (1- (length text))) ?\n))
      (setq text (concat text "\n")))
    (setq text (propertize text 'yank-handler yank-handler))
    (when register
      (aiern-set-register register text))
    (when aiern-was-yanked-without-register
      (aiern-set-register ?0 text)) ; "0 register contains last yanked text
    (unless (eq register ?_)
      (kill-new text))))

(defun aiern-yank-rectangle (beg end &optional register yank-handler)
  "Saves the rectangle defined by region BEG and END into the kill-ring."
  (let ((lines (list nil)))
    (aiern-apply-on-rectangle #'extract-rectangle-line beg end lines)
    ;; We remove spaces from the beginning and the end of the next.
    ;; Spaces are inserted explicitly in the yank-handler in order to
    ;; NOT insert lines full of spaces.
    (setq lines (nreverse (cdr lines)))
    ;; `text' is used as default insert text when pasting this rectangle
    ;; in another program, e.g., using the X clipboard.
    (let* ((yank-handler (list (or yank-handler
                                   #'aiern-yank-block-handler)
                               lines
                               t
                               'aiern-delete-yanked-rectangle))
           (text (propertize (mapconcat #'identity lines "\n")
                             'yank-handler yank-handler)))
      (when register
        (aiern-set-register register text))
      (when aiern-was-yanked-without-register
        (aiern-set-register ?0 text)) ; "0 register contains last yanked text
      (unless (eq register ?_)
        (kill-new text)))))

(defun aiern-remove-yank-excluded-properties (text)
  "Removes `yank-excluded-properties' from TEXT."
  (if (eq yank-excluded-properties t)
      (set-text-properties 0 (length text) nil text)
    (remove-list-of-text-properties 0 (length text)
                                    yank-excluded-properties text)))

(defun aiern-yank-line-handler (text)
  "Inserts the current text linewise."
  (let ((text (apply #'concat (make-list (or aiern-paste-count 1) text)))
        (opoint (point)))
    (aiern-remove-yank-excluded-properties text)
    (cond
     ((eq this-command 'aiern-paste-before)
      (aiern-move-beginning-of-line)
      (aiern-move-mark (point))
      (insert text)
      (setq aiern-last-paste
            (list 'aiern-paste-before
                  aiern-paste-count
                  opoint
                  (mark t)
                  (point)))
      (aiern-set-marker ?\[ (mark))
      (aiern-set-marker ?\] (1- (point)))
      (aiern-exchange-point-and-mark)
      (back-to-indentation))
     ((eq this-command 'aiern-paste-after)
      (aiern-move-end-of-line)
      (aiern-move-mark (point))
      (insert "\n")
      (insert text)
      (aiern-set-marker ?\[ (1+ (mark)))
      (aiern-set-marker ?\] (1- (point)))
      (delete-char -1) ; delete the last newline
      (setq aiern-last-paste
            (list 'aiern-paste-after
                  aiern-paste-count
                  opoint
                  (mark t)
                  (point)))
      (aiern-move-mark (1+ (mark t)))
      (aiern-exchange-point-and-mark)
      (back-to-indentation))
     (t
      (insert text)))))

(defun aiern-yank-block-handler (lines)
  "Inserts the current text as block."
  (let ((count (or aiern-paste-count 1))
        (col (if (eq this-command 'aiern-paste-after)
                 (1+ (current-column))
               (current-column)))
        (current-line (line-number-at-pos (point)))
        (opoint (point))
        epoint)
    (dolist (line lines)
      ;; concat multiple copies according to count
      (setq line (apply #'concat (make-list count line)))
      ;; strip whitespaces at beginning and end
      (string-match "^ *\\(.*?\\) *$" line)
      (let ((text (match-string 1 line))
            (begextra (match-beginning 1))
            (endextra (- (match-end 0) (match-end 1))))
        ;; maybe we have to insert a new line at eob
        (while (< (line-number-at-pos (point))
                  current-line)
          (goto-char (point-max))
          (insert "\n"))
        (setq current-line (1+ current-line))
        ;; insert text unless we insert an empty line behind eol
        (unless (and (< (aiern-column (line-end-position)) col)
                     (zerop (length text)))
          ;; if we paste behind eol, it may be sufficient to insert tabs
          (if (< (aiern-column (line-end-position)) col)
              (move-to-column (+ col begextra) t)
            (move-to-column col t)
            (insert (make-string begextra ?\s)))
          (aiern-remove-yank-excluded-properties text)
          (insert text)
          (unless (eolp)
            ;; text follows, so we have to insert spaces
            (insert (make-string endextra ?\s)))
          (setq epoint (point)))
        (forward-line 1)))
    (setq aiern-last-paste
          (list this-command
                aiern-paste-count
                opoint
                (length lines)                   ; number of rows
                (* count (length (car lines))))) ; number of colums
    (aiern-set-marker ?\[ opoint)
    (aiern-set-marker ?\] (1- epoint))
    (goto-char opoint)
    (when (and (eq this-command 'aiern-paste-after)
               (not (eolp)))
      (forward-char))))

(defun aiern-delete-yanked-rectangle (nrows ncols)
  "Special function to delete the block yanked by a previous paste command.
Supplied as the `undo' element of a yank handler."
  (let ((opoint (point))
        (col (if (eq last-command 'aiern-paste-after)
                 (1+ (current-column))
               (current-column))))
    (dotimes (_ nrows)
      (delete-region (save-excursion
                       (move-to-column col)
                       (point))
                     (save-excursion
                       (move-to-column (+ col ncols))
                       (point)))
      (unless (eobp) (forward-line)))
    (goto-char opoint)))

;; TODO: if undoing is disabled in the current buffer, paste-pop won't
;; work. Although this is probably not a big problem, because usually
;; buffers where `aiern-paste-pop' may be useful have undoing enabled.
;; A solution would be to temporarily enable undo when pasting and
;; store the undo information in a special variable that does not
;; interfere with `buffer-undo-list'.
(defun aiern-paste-pop (count)
  "Replace the just-yanked stretch of killed text with a different stretch.
This command is allowed only immediatly after a `yank',
`aiern-paste-before', `aiern-paste-after' or `aiern-paste-pop'.
This command uses the same paste command as before, i.e., when
used after `aiern-paste-after' the new text is also yanked using
`aiern-paste-after', used with the same paste-count argument.

The COUNT argument inserts the COUNTth previous kill.  If COUNT
is negative this is a more recent kill."
  (interactive "p")
  (unless (memq last-command
                '(aiern-paste-after
                  aiern-paste-before
                  aiern-visual-paste))
    (user-error "Previous command was not an aiern-paste: %s" last-command))
  (unless aiern-last-paste
    (user-error "Previous paste command used a register"))
  (aiern-undo-pop)
  (goto-char (nth 2 aiern-last-paste))
  (setq this-command (nth 0 aiern-last-paste))
  ;; use temporary kill-ring, so the paste cannot modify it
  (let ((kill-ring (list (current-kill
                          (if (and (> count 0) (nth 5 aiern-last-paste))
                              ;; if was visual paste then skip the
                              ;; text that has been replaced
                              (1+ count)
                            count))))
        (kill-ring-yank-pointer kill-ring))
    (when (eq last-command 'aiern-visual-paste)
      (let ((aiern-no-display t))
        (aiern-visual-restore)))
    (funcall (nth 0 aiern-last-paste) (nth 1 aiern-last-paste))
    ;; if this was a visual paste, then mark the last paste as NOT
    ;; being the first visual paste
    (when (eq last-command 'aiern-visual-paste)
      (setcdr (nthcdr 4 aiern-last-paste) nil))))

(defun aiern-paste-pop-next (count)
  "Same as `aiern-paste-pop' but with negative argument."
  (interactive "p")
  (aiern-paste-pop (- count)))

;;; Interactive forms

(defun aiern-match-interactive-code (interactive &optional pos)
  "Match an interactive code at position POS in string INTERACTIVE.
Returns the first matching entry in `aiern-interactive-alist', or nil."
  (let ((length (length interactive))
        (pos (or pos 0)))
    (catch 'done
      (dolist (entry aiern-interactive-alist)
        (let* ((string (car entry))
               (end (+ (length string) pos)))
          (when (and (<= end length)
                     (string= string
                              (substring interactive pos end)))
            (throw 'done entry)))))))

(defun aiern-concatenate-interactive-forms (&rest forms)
  "Concatenate interactive list expressions FORMS.
Returns a single expression where successive expressions
are joined, if possible."
  (let (result)
    (when forms
      (while (cdr forms)
        (cond
         ((null (car forms))
          (pop forms))
         ((and (eq (car (car forms)) 'list)
               (eq (car (cadr forms)) 'list))
          (setq forms (cons (append (car forms)
                                    (cdr (cadr forms)))
                            (cdr (cdr forms)))))
         (t
          (push (pop forms) result))))
      (when (car forms)
        (push (pop forms) result))
      (setq result (nreverse result))
      (cond
       ((null result))
       ((null (cdr result))
        (car result))
       (t
        `(append ,@result))))))

(defun aiern-interactive-string (string)
  "Evaluate the interactive string STRING.
The string may contain extended interactive syntax.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `aiern-define-command'."
  (let ((length (length string))
        (pos 0)
        code expr forms match plist prompt properties)
    (while (< pos length)
      (if (eq (aref string pos) ?\n)
          (setq pos (1+ pos))
        (setq match (aiern-match-interactive-code string pos))
        (if (null match)
            (user-error "Unknown interactive code: `%s'"
                        (substring string pos))
          (setq code (car match)
                expr (car (cdr match))
                plist (cdr (cdr match))
                pos (+ pos (length code)))
          (when (functionp expr)
            (setq prompt
                  (substring string pos
                             (or (string-match "\n" string pos)
                                 length))
                  pos (+ pos (length prompt))
                  expr `(funcall ,expr ,prompt)))
          (setq forms (append forms (list expr))
                properties (append properties plist)))))
    (cons `(append ,@forms) properties)))

(defun aiern-interactive-form (&rest args)
  "Evaluate interactive forms ARGS.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `aiern-define-command'."
  (let (forms properties)
    (dolist (arg args)
      (if (not (stringp arg))
          (setq forms (append forms (list arg)))
        (setq arg (aiern-interactive-string arg)
              forms (append forms (cdr (car arg)))
              properties (append properties (cdr arg)))))
    (cons (apply #'aiern-concatenate-interactive-forms forms)
          properties)))

;;; Types

(defun aiern-type (object &optional default)
  "Return the type of OBJECT, or DEFAULT if none."
  (let (type)
    (cond
     ((overlayp object)
      (setq type (overlay-get object :type)))
     ((aiern-range-p object)
      (setq type (nth 2 object)))
     ((listp object)
      (setq type (plist-get object :type)))
     ((commandp object)
      (setq type (aiern-get-command-property object :type)))
     ((symbolp object)
      (setq type (get object 'type))))
    (setq type (or type default))
    (and (aiern-type-p type) type)))

(defun aiern-set-type (object type)
  "Set the type of OBJECT to TYPE.
For example, (aiern-set-type 'next-line 'line)
will make `line' the type of the `next-line' command."
  (cond
   ((overlayp object)
    (overlay-put object :type type))
   ((aiern-range-p object)
    (aiern-set-range-type object type))
   ((listp object)
    (plist-put object :type type))
   ((commandp object)
    (aiern-set-command-property object :type type))
   ((symbolp object)
    (put object 'type type)))
  object)

(defun aiern-type-property (type prop)
  "Return property PROP for TYPE."
  (aiern-get-property aiern-type-properties type prop))

(defun aiern-type-p (sym)
  "Whether SYM is the name of a type."
  (assq sym aiern-type-properties))

(defun aiern-expand (beg end type &rest properties)
  "Expand BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'aiern-transform
         ;; don't expand if already expanded
         (unless (plist-get properties :expanded) :expand)
         beg end type properties))

(defun aiern-contract (beg end type &rest properties)
  "Contract BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'aiern-transform :contract beg end type properties))

(defun aiern-normalize (beg end type &rest properties)
  "Normalize BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'aiern-transform :normalize beg end type properties))

(defun aiern-transform (transform beg end type &rest properties)
  "Apply TRANSFORM on BEG and END with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list. If TRANSFORM is undefined,
return positions unchanged."
  (let* ((type (or type (aiern-type properties)))
         (transform (when (and type transform)
                      (aiern-type-property type transform))))
    (if transform
        (apply transform beg end properties)
      (apply #'aiern-range beg end type properties))))

(defun aiern-describe (beg end type &rest properties)
  "Return description of BEG and END with PROPERTIES.
If no description is available, return the empty string."
  (let* ((type (or type (aiern-type properties)))
         (properties (plist-put properties :type type))
         (describe (aiern-type-property type :string)))
    (or (when describe
          (apply describe beg end properties))
        "")))

;;; Ranges

(defun aiern-range (beg end &optional type &rest properties)
  "Return a list (BEG END [TYPE] PROPERTIES...).
BEG and END are buffer positions (numbers or markers),
TYPE is a type as per `aiern-type-p', and PROPERTIES is
a property list."
  (let ((beg (aiern-normalize-position beg))
        (end (aiern-normalize-position end)))
    (when (and (numberp beg) (numberp end))
      (append (list (min beg end) (max beg end))
              (when (aiern-type-p type)
                (list type))
              properties))))

(defun aiern-range-p (object)
  "Whether OBJECT is a range."
  (and (listp object)
       (>= (length object) 2)
       (numberp (nth 0 object))
       (numberp (nth 1 object))))

(defun aiern-range-beginning (range)
  "Return beginning of RANGE."
  (when (aiern-range-p range)
    (let ((beg (aiern-normalize-position (nth 0 range)))
          (end (aiern-normalize-position (nth 1 range))))
      (min beg end))))

(defun aiern-range-end (range)
  "Return end of RANGE."
  (when (aiern-range-p range)
    (let ((beg (aiern-normalize-position (nth 0 range)))
          (end (aiern-normalize-position (nth 1 range))))
      (max beg end))))

(defun aiern-range-properties (range)
  "Return properties of RANGE."
  (when (aiern-range-p range)
    (if (aiern-type range)
        (nthcdr 3 range)
      (nthcdr 2 range))))

(defun aiern-copy-range (range)
  "Return a copy of RANGE."
  (copy-sequence range))

(defun aiern-set-range (range &optional beg end type &rest properties)
  "Set RANGE to have beginning BEG and end END.
The TYPE and additional PROPERTIES may also be specified.
If an argument is nil, it's not used; the previous value is retained.
See also `aiern-set-range-beginning', `aiern-set-range-end',
`aiern-set-range-type' and `aiern-set-range-properties'."
  (when (aiern-range-p range)
    (let ((beg (or (aiern-normalize-position beg)
                   (aiern-range-beginning range)))
          (end (or (aiern-normalize-position end)
                   (aiern-range-end range)))
          (type (or type (aiern-type range)))
          (plist (aiern-range-properties range)))
      (aiern-sort beg end)
      (setq plist (aiern-concat-plists plist properties))
      (aiern-set-range-beginning range beg)
      (aiern-set-range-end range end)
      (aiern-set-range-type range type)
      (aiern-set-range-properties range plist)
      range)))

(defun aiern-set-range-beginning (range beg &optional copy)
  "Set RANGE's beginning to BEG.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (aiern-copy-range range)))
  (setcar range beg)
  range)

(defun aiern-set-range-end (range end &optional copy)
  "Set RANGE's end to END.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (aiern-copy-range range)))
  (setcar (cdr range) end)
  range)

(defun aiern-set-range-type (range type &optional copy)
  "Set RANGE's type to TYPE.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (aiern-copy-range range)))
  (if type
      (setcdr (cdr range)
              (cons type (aiern-range-properties range)))
    (setcdr (cdr range) (aiern-range-properties range)))
  range)

(defun aiern-set-range-properties (range properties &optional copy)
  "Set RANGE's properties to PROPERTIES.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (aiern-copy-range range)))
  (if (aiern-type range)
      (setcdr (cdr (cdr range)) properties)
    (setcdr (cdr range) properties))
  range)

(defun aiern-range-union (range1 range2 &optional type)
  "Return the union of the ranges RANGE1 and RANGE2.
If the ranges have conflicting types, use RANGE1's type.
This can be overridden with TYPE."
  (when (and (aiern-range-p range1)
             (aiern-range-p range2))
    (aiern-range (min (aiern-range-beginning range1)
                     (aiern-range-beginning range2))
                (max (aiern-range-end range1)
                     (aiern-range-end range2))
                (or type
                    (aiern-type range1)
                    (aiern-type range2)))))

(defun aiern-subrange-p (range1 range2)
  "Whether RANGE1 is contained within RANGE2."
  (and (aiern-range-p range1)
       (aiern-range-p range2)
       (<= (aiern-range-beginning range2)
           (aiern-range-beginning range1))
       (>= (aiern-range-end range2)
           (aiern-range-end range1))))

(defun aiern-select-inner-object (thing beg end type &optional count line)
  "Return an inner text object range of COUNT objects.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by thing-at-point.  BEG, END and TYPE specify the
current selection.  If LINE is non-nil, the text object should be
linewise, otherwise it is character wise."
  (let* ((count (or count 1))
         (bnd (or (let ((b (bounds-of-thing-at-point thing)))
                    (and b (< (point) (cdr b)) b))
                  (aiern-bounds-of-not-thing-at-point thing))))
    ;; check if current object is selected
    (when (or (not beg) (not end)
              (> beg (car bnd))
              (< end (cdr bnd))
              (and (eq type 'inclusive)
                   (= (1+ beg) end))) ; empty region does not count
      (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
      (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
      (setq count (if (> count 0) (1- count) (1+ count))))
    (goto-char (if (< count 0) beg end))
    (aiern-forward-nearest count
                          #'(lambda (cnt) (forward-thing thing cnt))
                          #'(lambda (cnt) (aiern-forward-not-thing thing cnt)))
    (aiern-range (if (>= count 0) beg (point))
                (if (< count 0) end (point))
                (if line 'line type)
                :expanded t)))

(defun aiern-select-an-object (thing beg end type count &optional line)
  "Return an outer text object range of COUNT objects.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by thing-at-point.  BEG, END and TYPE specify the
current selection.  If LINE is non-nil, the text object should be
linewise, otherwise it is character wise."
  (let* ((dir (if (> (or count 1) 0) +1 -1))
         (count (abs (or count 1)))
         (objbnd (let ((b (bounds-of-thing-at-point thing)))
                   (and b (< (point) (cdr b)) b)))
         (bnd (or objbnd (aiern-bounds-of-not-thing-at-point thing)))
         addcurrent other)
    ;; check if current object is not selected
    (when (or (not beg) (not end)
              (> beg (car bnd))
              (< end (cdr bnd))
              (and (eq type 'inclusive)
                   (= (1+ beg) end))) ; empty region does not count
      ;; if not, enlarge selection
      (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
      (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
      (if objbnd (setq addcurrent t)))
    ;; make other and (point) reflect the selection
    (cond
     ((> dir 0) (goto-char end) (setq other beg))
     (t (goto-char beg) (setq other end)))
    (cond
     ;; do nothing more than only current is selected
     ((not (and (= beg (car bnd)) (= end (cdr bnd)))))
     ;; current match is thing, add whitespace
     (objbnd
      (let ((wsend (aiern-with-restriction
                       ;; restrict to current line if we do non-line selection
                       (and (not line) (line-beginning-position))
                       (and (not line) (line-end-position))
                     (aiern-bounds-of-not-thing-at-point thing dir))))
        (cond
         (wsend
          ;; add whitespace at end
          (goto-char wsend)
          (setq addcurrent t))
         (t
          ;; no whitespace at end, try beginning
          (save-excursion
            (goto-char other)
            (setq wsend
                  (aiern-with-restriction
                      ;; restrict to current line if we do non-line selection
                      (and (not line) (line-beginning-position))
                      (and (not line) (line-end-position))
                    (aiern-bounds-of-not-thing-at-point thing (- dir))))
            (when wsend (setq other wsend addcurrent t)))))))
     ;; current match is whitespace, add thing
     (t
      (forward-thing thing dir)
      (setq addcurrent t)))
    ;; possibly count current object as selection
    (if addcurrent (setq count (1- count)))
    ;; move
    (dotimes (_ count)
      (let ((wsend (aiern-bounds-of-not-thing-at-point thing dir)))
        (if (and wsend (/= wsend (point)))
            ;; start with whitespace
            (forward-thing thing dir)
          ;; start with thing
          (forward-thing thing dir)
          (setq wsend (aiern-bounds-of-not-thing-at-point thing dir))
          (when wsend (goto-char wsend)))))
    ;; return range
    (aiern-range (if (> dir 0) other (point))
                (if (< dir 0) other (point))
                (if line 'line type)
                :expanded t)))

(defun aiern--get-block-range (op cl selection-type)
  "Return the exclusive range of a visual selection.
OP and CL are pairs of buffer positions for the opening and
closing delimiter of a range. SELECTION-TYPE is the desired type
of selection.  It is a symbol that determines which parts of the
block are selected.  If it is 'inclusive or t the returned range
is \(cons (car OP) (cdr CL)). If it is 'exclusive or nil the
returned range is (cons (cdr OP) (car CL)).  If it is
'exclusive-line the returned range will skip whitespace at the
end of the line of OP and at the beginning of the line of CL."
  (cond
   ((memq selection-type '(inclusive t)) (cons (car op) (cdr cl)))
   ((memq selection-type '(exclusive nil)) (cons (cdr op) (car cl)))
   ((eq selection-type 'exclusive-line)
    (let ((beg (cdr op))
          (end (car cl)))
      (save-excursion
        (goto-char beg)
        (when (and (eolp) (not (eobp)))
          (setq beg (line-beginning-position 2)))
        (goto-char end)
        (skip-chars-backward " \t")
        (when (bolp)
          (setq end (point))
          (goto-char beg)
          (when (and (not (bolp)) (< beg end))
            (setq end (1- end)))))
      (cons beg end)))
   (t
    (user-error "Unknown selection-type %s" selection-type))))

(defun aiern-select-block (thing beg end type count
                                &optional
                                selection-type
                                countcurrent
                                fixedscan)
  "Return a range (BEG END) of COUNT delimited text objects.
BEG END TYPE are the currently selected (visual) range.  The
delimited object must be given by THING-up function (see
`aiern-up-block').

SELECTION-TYPE is symbol that determines which parts of the block
are selected.  If it is 'inclusive or t OPEN and CLOSE are
included in the range. If it is 'exclusive or nil the delimiters
are not contained. If it is 'exclusive-line the delimiters are
not included as well as adjacent whitespace until the beginning
of the next line or the end of the previous line. If the
resulting selection consists of complete lines only and visual
state is not active, the returned selection is linewise.

If COUNTCURRENT is non-nil an objected is counted if the current
selection matches that object exactly.

Usually scanning for the surrounding block starts at (1+ beg)
and (1- end). If this might fail due to the behavior of THING
then FIXEDSCAN can be set to t. In this case the scan starts at
BEG and END. One example where this might fail is if BEG and END
are the delimiters of a string or comment."
  (save-excursion
    (save-match-data
      (let* ((orig-beg beg)
             (orig-end end)
             (beg (or beg (point)))
             (end (or end (point)))
             (count (abs (or count 1)))
             op cl op-end cl-end)
        ;; We always assume at least one selected character.
        (if (= beg end) (setq end (1+ end)))
        ;; We scan twice: starting at (1+ beg) forward and at (1- end)
        ;; backward. The resulting selection is the smaller one.
        (goto-char (if fixedscan beg (1+ beg)))
        (when (and (zerop (funcall thing +1)) (match-beginning 0))
          (setq cl (cons (match-beginning 0) (match-end 0)))
          (goto-char (car cl))
          (when (and (zerop (funcall thing -1)) (match-beginning 0))
            (setq op (cons (match-beginning 0) (match-end 0)))))
        ;; start scanning from end
        (goto-char (if fixedscan end (1- end)))
        (when (and (zerop (funcall thing -1)) (match-beginning 0))
          (setq op-end (cons (match-beginning 0) (match-end 0)))
          (goto-char (cdr op-end))
          (when (and (zerop (funcall thing +1)) (match-beginning 0))
            (setq cl-end (cons (match-beginning 0) (match-end 0)))))
        ;; Bug #607: use the tightest selection that contains the
        ;; original selection. If non selection contains the original,
        ;; use the larger one.
        (cond
         ((and (not op) (not cl-end))
          (error "No surrounding delimiters found"))
         ((or (not op) ; first not found
              (and cl-end ; second found
                   (>= (car op-end) (car op)) ; second smaller
                   (<= (cdr cl-end) (cdr cl))
                   (<= (car op-end) beg)      ; second contains orig
                   (>= (cdr cl-end) end)))
          (setq op op-end cl cl-end)))
        (setq op-end op cl-end cl) ; store copy
        ;; if the current selection contains the surrounding
        ;; delimiters, they do not count as new selection
        (let ((cnt (if (and orig-beg orig-end (not countcurrent))
                       (let ((sel (aiern--get-block-range op cl selection-type)))
                         (if (and (<= orig-beg (car sel))
                                  (>= orig-end (cdr sel)))
                             count
                           (1- count)))
                     (1- count))))
          ;; starting from the innermost surrounding delimiters
          ;; increase selection
          (when (> cnt 0)
            (setq op (progn
                       (goto-char (car op-end))
                       (funcall thing (- cnt))
                       (if (match-beginning 0)
                           (cons (match-beginning 0) (match-end 0))
                         op))
                  cl (progn
                       (goto-char (cdr cl-end))
                       (funcall thing cnt)
                       (if (match-beginning 0)
                           (cons (match-beginning 0) (match-end 0))
                         cl)))))
        (let ((sel (aiern--get-block-range op cl selection-type)))
          (setq op (car sel)
                cl (cdr sel)))
        (cond
         ((and (equal op orig-beg) (equal cl orig-end)
               (or (not countcurrent)
                   (and countcurrent (/= count 1))))
          (error "No surrounding delimiters found"))
         ((save-excursion
            (and (not (aiern-visual-state-p))
                 (eq type 'inclusive)
                 (progn (goto-char op) (bolp))
                 (progn (goto-char cl) (bolp))))
          (aiern-range op cl 'line :expanded t))
         (t
          (aiern-range op cl type :expanded t)))))))

(defun aiern-select-paren (open close beg end type count &optional inclusive)
  "Return a range (BEG END) of COUNT delimited text objects.
OPEN and CLOSE specify the opening and closing delimiter,
respectively. BEG END TYPE are the currently selected (visual)
range.  If INCLUSIVE is non-nil, OPEN and CLOSE are included in
the range; otherwise they are excluded.

The types of OPEN and CLOSE specify which kind of THING is used
for parsing with `aiern-select-block'. If OPEN and CLOSE are
characters `aiern-up-paren' is used. Otherwise OPEN and CLOSE
must be regular expressions and `aiern-up-block' is used.

If the selection is exclusive, whitespace at the end or at the
beginning of the selection until the end-of-line or beginning-of-line
is ignored."
  ;; we need special linewise exclusive selection
  (unless inclusive (setq inclusive 'exclusive-line))
  (cond
   ((and (characterp open) (characterp close))
    (let ((thing #'(lambda (&optional cnt)
                     (aiern-up-paren open close cnt)))
          (bnd (or (bounds-of-thing-at-point 'aiern-string)
                   (bounds-of-thing-at-point 'aiern-comment)
                   ;; If point is at the opening quote of a string,
                   ;; this must be handled as if point is within the
                   ;; string, i.e. the selection must be extended
                   ;; around the string. Otherwise
                   ;; `aiern-select-block' might do the wrong thing
                   ;; because it accidentally moves point inside the
                   ;; string (for inclusive selection) when looking
                   ;; for the current surrounding block. (re #364)
                   (and (= (point) (or beg (point)))
                        (save-excursion
                          (goto-char (1+ (or beg (point))))
                          (or (bounds-of-thing-at-point 'aiern-string)
                              (bounds-of-thing-at-point 'aiern-comment)))))))
      (if (not bnd)
          (aiern-select-block thing beg end type count inclusive)
        (or (aiern-with-restriction (car bnd) (cdr bnd)
              (condition-case nil
                  (aiern-select-block thing beg end type count inclusive)
                (error nil)))
            (save-excursion
              (setq beg (or beg (point))
                    end (or end (point)))
              (goto-char (car bnd))
              (let ((extbeg (min beg (car bnd)))
                    (extend (max end (cdr bnd))))
                (aiern-select-block thing
                                   extbeg extend
                                   type
                                   count
                                   inclusive
                                   (or (< extbeg beg) (> extend end))
                                   t)))))))
   (t
    (aiern-select-block #'(lambda (&optional cnt)
                           (aiern-up-block open close cnt))
                       beg end type count inclusive))))

(defun aiern-select-quote-thing (thing beg end _type count &optional inclusive)
  "Selection THING as if it described a quoted object.
THING is typically either 'aiern-quote or 'aiern-chars. This
function is called from `aiern-select-quote'."
  (save-excursion
    (let* ((count (or count 1))
           (dir (if (> count 0) 1 -1))
           (bnd (let ((b (bounds-of-thing-at-point thing)))
                  (and b (< (point) (cdr b)) b)))
           addcurrent
           wsboth)
      (if inclusive (setq inclusive t)
        (when (= (abs count) 2)
          (setq count dir)
          (setq inclusive 'quote-only))
        ;; never extend with exclusive selection
        (setq beg nil end nil))
      ;; check if the previously selected range does not contain a
      ;; string
      (unless (and beg end
                   (save-excursion
                     (goto-char (if (> dir 0) beg end))
                     (forward-thing thing dir)
                     (and (<= beg (point)) (< (point) end))))
        ;; if so forget the range
        (setq beg nil end nil))
      ;; check if there is a current object, if not fetch one
      (when (not bnd)
        (unless (and (zerop (forward-thing thing dir))
                     (setq bnd (bounds-of-thing-at-point thing)))
          (error "No quoted string found"))
        (if (> dir 0)
            (setq end (point))
          (setq beg (point)))
        (setq addcurrent t))
      ;; check if current object is not selected
      (when (or (not beg) (not end) (> beg (car bnd)) (< end (cdr bnd)))
        ;; if not, enlarge selection
        (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
        (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
        (setq addcurrent t wsboth t))
      ;; maybe count current element
      (when addcurrent
        (setq count (if (> dir 0) (1- count) (1+ count))))
      ;; enlarge selection
      (goto-char (if (> dir 0) end beg))
      (when (and (not addcurrent)
                 (= count (forward-thing thing count)))
        (error "No quoted string found"))
      (if (> dir 0) (setq end (point)) (setq beg (point)))
      ;; add whitespace
      (cond
       ((not inclusive) (setq beg (1+ beg) end (1- end)))
       ((not (eq inclusive 'quote-only))
        ;; try to add whitespace in forward direction
        (goto-char (if (> dir 0) end beg))
        (if (setq bnd (bounds-of-thing-at-point 'aiern-space))
            (if (> dir 0) (setq end (cdr bnd)) (setq beg (car bnd)))
          ;; if not found try backward direction
          (goto-char (if (> dir 0) beg end))
          (if (and wsboth (setq bnd (bounds-of-thing-at-point 'aiern-space)))
              (if (> dir 0) (setq beg (car bnd)) (setq end (cdr bnd)))))))
      (aiern-range beg end
                  ;; HACK: fixes #583
                  ;; When not in visual state, an empty range is
                  ;; possible. However, this cannot be achieved with
                  ;; inclusive ranges, hence we use exclusive ranges
                  ;; in this case. In visual state the range must be
                  ;; inclusive because otherwise the selection would
                  ;; be wrong.
                  (if (aiern-visual-state-p) 'inclusive 'exclusive)
                  :expanded t))))

(defun aiern-select-quote (quote beg end type count &optional inclusive)
  "Return a range (BEG END) of COUNT quoted text objects.
QUOTE specifies the quotation delimiter. BEG END TYPE are the
currently selected (visual) range.

If INCLUSIVE is nil the previous selection is ignore. If there is
quoted string at point this object will be selected, otherwise
the following (if (> COUNT 0)) or preceeding object (if (< COUNT
0)) is selected. If (/= (abs COUNT) 2) the delimiting quotes are not
contained in the range, otherwise they are contained in the range.

If INCLUSIVE is non-nil the selection depends on the previous
selection. If the currently selection contains at least one
character that is contained in a quoted string then the selection
is extended, otherwise it is thrown away. If there is a
non-selected object at point then this object is added to the
selection. Otherwise the selection is extended to the
following (if (> COUNT 0)) or preceeding object (if (< COUNT
0)). Any whitespace following (or preceeding if (< COUNT 0)) the
new selection is added to the selection. If no such whitespace
exists and the selection contains only one quoted string then the
preceeding (or following) whitespace is added to the range. "
  (let ((aiern-forward-quote-char quote))
    (or (let ((bnd (or (bounds-of-thing-at-point 'aiern-comment)
                       (bounds-of-thing-at-point 'aiern-string))))
          (when (and bnd (< (point) (cdr bnd))
                     (/= (char-after (car bnd)) quote)
                     (/= (char-before (cdr bnd)) quote))
            (aiern-with-restriction (car bnd) (cdr bnd)
              (condition-case nil
                  (aiern-select-quote-thing 'aiern-quote-simple
                                           beg end type
                                           count
                                           inclusive)
                (error nil)))))
        (let ((aiern-forward-quote-char quote))
          (aiern-select-quote-thing 'aiern-quote
                                   beg end type
                                   count
                                   inclusive)))))

(defun aiern-select-xml-tag (beg end type &optional count inclusive)
  "Return a range (BEG END) of COUNT matching XML tags.
If INCLUSIVE is non-nil, the tags themselves are included
from the range."
  (cond
   ((and (not inclusive) (= (abs (or count 1)) 1))
    (let ((rng (aiern-select-block #'aiern-up-xml-tag beg end type count nil t)))
      (if (or (and beg (= beg (aiern-range-beginning rng))
                   end (= end (aiern-range-end rng)))
              (= (aiern-range-beginning rng) (aiern-range-end rng)))
          (aiern-select-block #'aiern-up-xml-tag beg end type count t)
        rng)))
   (t
    (aiern-select-block #'aiern-up-xml-tag beg end type count inclusive))))

(defun aiern-expand-range (range &optional copy)
  "Expand RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (aiern-copy-range range)))
  (unless (plist-get (aiern-range-properties range) :expanded)
    (setq range (aiern-transform-range :expand range)))
  range)

(defun aiern-contract-range (range &optional copy)
  "Contract RANGE according to its type.
Return a new range if COPY is non-nil."
  (aiern-transform-range :contract range copy))

(defun aiern-normalize-range (range &optional copy)
  "Normalize RANGE according to its type.
Return a new range if COPY is non-nil."
  (aiern-transform-range :normalize range copy))

(defun aiern-transform-range (transform range &optional copy)
  "Apply TRANSFORM to RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (aiern-copy-range range)))
  (when (aiern-type range)
    (apply #'aiern-set-range range
           (apply #'aiern-transform transform range)))
  range)

(defun aiern-describe-range (range)
  "Return description of RANGE.
If no description is available, return the empty string."
  (apply #'aiern-describe range))

;;; Undo

(defun aiern-start-undo-step (&optional continue)
  "Start a undo step.
All following buffer modifications are grouped together as a
single action. If CONTINUE is non-nil, preceding modifications
are included. The step is terminated with `aiern-end-undo-step'."
  (when (and (listp buffer-undo-list)
             (not aiern-in-single-undo))
    (if aiern-undo-list-pointer
        (aiern-refresh-undo-step)
      (unless (or continue (null (car-safe buffer-undo-list)))
        (undo-boundary))
      (setq aiern-undo-list-pointer (or buffer-undo-list t)))))

(defun aiern-end-undo-step (&optional continue)
  "End a undo step started with `aiern-start-undo-step'.
Adds an undo boundary unless CONTINUE is specified."
  (when (and (listp buffer-undo-list)
             aiern-undo-list-pointer
             (not aiern-in-single-undo))
    (aiern-refresh-undo-step)
    (unless (or continue (null (car-safe buffer-undo-list)))
      (undo-boundary))
    (setq aiern-undo-list-pointer nil)))

(defun aiern-refresh-undo-step ()
  "Refresh `buffer-undo-list' entries for current undo step.
Undo boundaries until `aiern-undo-list-pointer' are removed to
make the entries undoable as a single action. See
`aiern-start-undo-step'."
  (when aiern-undo-list-pointer
    (setq buffer-undo-list
          (aiern-filter-list #'null buffer-undo-list aiern-undo-list-pointer))
    (setq aiern-undo-list-pointer (or buffer-undo-list t))))

(defmacro aiern-with-undo (&rest body)
  "Execute BODY with enabled undo.
If undo is disabled in the current buffer, the undo information
is stored in `aiern-temporary-undo' instead of `buffer-undo-list'."
  (declare (indent defun)
           (debug t))
  `(unwind-protect
       (let (buffer-undo-list)
         (unwind-protect
             (progn ,@body)
           (setq aiern-temporary-undo buffer-undo-list)
           ;; ensure aiern-temporary-undo starts with exactly one undo
           ;; boundary marker, i.e. nil
           (unless (null (car-safe aiern-temporary-undo))
             (push nil aiern-temporary-undo))))
     (unless (eq buffer-undo-list t)
       ;; undo is enabled, so update the global buffer undo list
       (setq buffer-undo-list
             ;; prepend new undos (if there are any)
             (if (cdr aiern-temporary-undo)
                 (nconc aiern-temporary-undo buffer-undo-list)
               buffer-undo-list)
             aiern-temporary-undo nil))))

(defmacro aiern-with-single-undo (&rest body)
  "Execute BODY as a single undo step."
  (declare (indent defun)
           (debug t))
  `(let (aiern-undo-list-pointer)
     (aiern-with-undo
       (unwind-protect
           (progn
             (aiern-start-undo-step)
             (let ((aiern-in-single-undo t))
               ,@body))
         (aiern-end-undo-step)))))

(defun aiern-undo-pop ()
  "Undo the last buffer change.
Removes the last undo information from `buffer-undo-list'.
If undo is disabled in the current buffer, use the information
in `aiern-temporary-undo' instead."
  (let ((paste-undo (list nil)))
    (let ((undo-list (if (eq buffer-undo-list t)
                         aiern-temporary-undo
                       buffer-undo-list)))
      (when (or (not undo-list) (car undo-list))
        (user-error "Can't undo previous change"))
      (while (and undo-list (null (car undo-list)))
        (pop undo-list)) ; remove nil
      (while (and undo-list (car undo-list))
        (push (pop undo-list) paste-undo))
      (let ((buffer-undo-list (nreverse paste-undo)))
        (aiern-save-echo-area
          (undo)))
      (if (eq buffer-undo-list t)
          (setq aiern-temporary-undo nil)
        (setq buffer-undo-list undo-list)))))

;;; Search
(defun aiern-transform-regexp (regexp replacements-alist)
  (replace-regexp-in-string
   "\\\\+[^\\\\]"
   #'(lambda (txt)
       (let* ((b (match-beginning 0))
              (e (match-end 0))
              (ch (aref txt (1- e)))
              (repl (assoc ch replacements-alist)))
         (if (and repl (zerop (mod (length txt) 2)))
             (concat (substring txt b (- e 2))
                     (cdr repl))
           txt)))
   regexp nil t))

(defun aiern-transform-magic (str magic quote transform &optional _start)
  "Transforms STR with magic characters.
MAGIC is a regexp that matches all potential magic
characters. Each occurence of CHAR as magic character within str
is replaced by the result of calling the associated TRANSFORM
function. TRANSFORM is a function taking two arguments, the
character to be transformed and the rest of string after the
character. The function should return a triple (REPLACEMENT REST
. STOP) where REPLACEMENT is the replacement and REST is the rest
of the string that has not been transformed. If STOP is non-nil
then the substitution stops immediately.  The replacement starts
at position START, everything before that position is returned
literally.  The result is a pair (RESULT . REST). RESULT is a
list containing the transformed parts in order. If two
subsequents parts are both strings, they are concatenated. REST
is the untransformed rest string (usually \"\" but may be more if
TRANSFORM stopped the substitution). Which characters are
considered as magic characters (i.e. the transformation happens
if the character is NOT preceeded by a backslash) is determined
by `aiern-magic'. The special tokens \\v, \\V, \\m and \\M have
always a special meaning (like in Vim) and should not be
contained in TRANSFORMS, otherwise their meaning is overwritten.

The parameter QUOTE is a quoting function applied to literal
transformations, usually `regexp-quote' or `replace-quote'."
  (save-match-data
    (let ((regexp (concat "\\(?:\\`\\|[^\\]\\)\\(\\\\\\(?:\\(" magic "\\)\\|\\(.\\)\\)\\|\\(" magic "\\)\\)"))
          (magic-chars (aiern-get-magic aiern-magic))
          (aiern-magic aiern-magic)
          (quote (or quote #'identity))
          result stop)
      (while (and (not stop) str (string-match regexp str))
        (unless (zerop (match-beginning 1))
          (push (substring str 0 (match-beginning 1)) result))
        (let ((char (or (match-string 2 str)
                        (match-string 3 str)
                        (match-string 4 str)))
              (rest (substring str (match-end 0))))
          (cond
           ((match-beginning 4)
            ;; magic character without backslash
            (if (string-match magic-chars char)
                ;; magic, do transform
                (let ((trans (funcall transform (aref char 0) rest)))
                  (push (car trans) result)
                  (setq str (cadr trans) stop (nthcdr 2 trans)))
              ;; non-magic, literal transformation
              (push (funcall quote char) result)
              (setq str rest)))
           ((match-beginning 2)
            ;; magic character with backslash
            (if (not (string-match magic-chars char))
                ;; non-magic, do transform
                (let ((trans (funcall transform (aref char 0) rest)))
                  (push (car trans) result)
                  (setq str (cadr trans) stop (nthcdr 2 trans)))
              ;; magic, literal transformation
              (push (funcall quote char) result)
              (setq str rest)))
           ((memq (aref char 0) '(?m ?M ?v ?V))
            (setq aiern-magic (cdr (assq (aref char 0)
                                        '((?m . t)
                                          (?M . nil)
                                          (?v . very-magic)
                                          (?V . very-nomagic)))))
            (setq magic-chars (aiern-get-magic aiern-magic))
            (setq str rest))
           (t
            ;; non-magic char with backslash, literal transformation
            (push (funcall quote char) result)
            (setq str rest)))))
      (cond
       ((and str (not stop))
        (push str result)
        (setq str ""))
       ((not str)
        (setq str "")))
      ;; concatenate subsequent strings
      ;; note that result is in reverse order
      (let (repl)
        (while result
          (cond
           ((and (stringp (car result))
                 (zerop (length (car result))))
            (pop result))
           ((and (stringp (car result))
                 (stringp (cadr result)))
            (setq result (cons (concat (cadr result)
                                       (car result))
                               (nthcdr 2 result))))
           (t
            (push (pop result) repl))))
        (cons repl str)))))

(defconst aiern-vim-regexp-replacements
  '((?n  . "\n")           (?r  . "\r")
    (?t  . "\t")           (?b  . "\b")
    (?s  . "[[:space:]]")  (?S  . "[^[:space:]]")
    (?d  . "[[:digit:]]")  (?D  . "[^[:digit:]]")
    (?x  . "[[:xdigit:]]") (?X  . "[^[:xdigit:]]")
    (?o  . "[0-7]")        (?O  . "[^0-7]")
    (?a  . "[[:alpha:]]")  (?A  . "[^[:alpha:]]")
    (?l  . "[a-z]")        (?L  . "[^a-z]")
    (?u  . "[A-Z]")        (?U  . "[^A-Z]")
    (?y  . "\\s")          (?Y  . "\\S")
    (?\( . "\\(")          (?\) . "\\)")
    (?{  . "\\{")          (?}  . "\\}")
    (?\[ . "[")            (?\] . "]")
    (?<  . "\\<")          (?>  . "\\>")
    (?_  . "\\_")
    (?*  . "*")            (?+  . "+")
    (??  . "?")            (?=  . "?")
    (?.  . ".")
    (?`  . "`")            (?^  . "^")
    (?$  . "$")            (?|  . "\\|")))

(defconst aiern-regexp-magic "[][(){}<>_dDsSxXoOaAlLuUwWyY.*+?=^$`|nrtb]")

(defun aiern-transform-vim-style-regexp (regexp)
  "Transforms vim-style backslash codes to Emacs regexp.
This includes the backslash codes \\d, \\D, \\s, \\S, \\x, \\X,
\\o, \\O, \\a, \\A, \\l, \\L, \\u, \\U and \\w, \\W. The new
codes \\y and \\Y can be used instead of the Emacs code \\s and
\\S which have a different meaning in Vim-style."
  (car
   (car
    (aiern-transform-magic
     regexp aiern-regexp-magic #'regexp-quote
     #'(lambda (char rest)
         (let ((repl (assoc char aiern-vim-regexp-replacements)))
           (if repl
               (list (cdr repl) rest)
             (list (concat "\\" (char-to-string char)) rest))))))))

;;; Substitute

(defun aiern-downcase-first (str)
  "Return STR with the first letter downcased."
  (if (zerop (length str))
      str
    (concat (downcase (substring str 0 1))
            (substring str 1))))

(defun aiern-upcase-first (str)
  "Return STR with the first letter upcased."
  (if (zerop (length str))
      str
    (concat (upcase (substring str 0 1))
            (substring str 1))))

(defun aiern-get-magic (magic)
  "Returns a regexp matching the magic characters according to MAGIC.
Depending on the value of MAGIC the following characters are
considered magic.
  t             [][{}*+?.&~$^
  nil           [][{}*+?$^
  'very-magic   not 0-9A-Za-z_
  'very-nomagic empty."
  (cond
   ((eq magic t) "[][}{*+?.&~$^]")
   ((eq magic 'very-magic) "[^0-9A-Za-z_]")
   ((eq magic 'very-nomagic) "\\\\")
   (t "[][}{*+?$^]")))

;; TODO: support magic characters in patterns
(defconst aiern-replacement-magic "[eElLuU0-9&#,rnbt=]"
  "All magic characters in a replacement string")

(defun aiern-compile-subreplacement (to &optional start)
  "Convert a regexp replacement TO to Lisp from START until \\e or \\E.
Returns a pair (RESULT . REST). RESULT is a list suitable for
`perform-replace' if necessary, the original string if not.
REST is the unparsed remainder of TO."
  (let ((result
         (aiern-transform-magic
          to aiern-replacement-magic #'replace-quote
          #'(lambda (char rest)
              (cond
               ((eq char ?#)
                (list '(number-to-string replace-count) rest))
               ((eq char ?r) (list "\r" rest))
               ((eq char ?n) (list "\n" rest))
               ((eq char ?b) (list "\b" rest))
               ((eq char ?t) (list "\t" rest))
               ((memq char '(?e ?E))
                `("" ,rest . t))
               ((memq char '(?l ?L ?u ?U))
                (let ((result (aiern-compile-subreplacement rest))
                      (func (cdr (assoc char
                                        '((?l . aiern-downcase-first)
                                          (?L . downcase)
                                          (?u . aiern-upcase-first)
                                          (?U . upcase))))))
                  (list `(,func
                          (replace-quote
                           (aiern-match-substitute-replacement
                            ,(car result)
                            (not case-replace))))
                        (cdr result))))
               ((eq char ?=)
                (when (or (zerop (length rest))
                          (not (eq (aref rest 0) ?@)))
                  (user-error "Expected @ after \\="))
                (when (< (length rest) 2)
                  (user-error "Expected register after \\=@"))
                (list (aiern-get-register (aref rest 1))
                      (substring rest 2)))
               ((eq char ?,)
                (let* ((obj (read-from-string rest))
                       (result `(replace-quote ,(car obj)))
                       (end
                        ;; swallow a space after a symbol
                        (if (and (or (symbolp (car obj))
                                     ;; swallow a space after 'foo,
                                     ;; but not after (quote foo)
                                     (and (eq (car-safe (car obj)) 'quote)
                                          (not (= ?\( (aref rest 0)))))
                                 (eq (string-match " " rest (cdr obj))
                                     (cdr obj)))
                            (1+ (cdr obj))
                          (cdr obj))))
                  (list result (substring rest end))))
               ((eq char ?0)
                (list "\\&" rest))
               (t
                (list (concat "\\" (char-to-string char)) rest))))
          start)))
    (let ((rest (cdr result))
          (result (car result)))
      (replace-match-string-symbols result)
      (cons (if (cdr result)
                (cons 'concat result)
              (or (car result) ""))
            rest))))

(defun aiern-compile-replacement (to)
  "Maybe convert a regexp replacement TO to Lisp.
Returns a list suitable for `perform-replace' if necessary, the
original string if not. Currently the following magic characters
in replacements are supported: 0-9&#lLuUrnbt,
The magic character , (comma) start an Emacs-lisp expression."
  (when (stringp to)
    (save-match-data
      (cons 'replace-eval-replacement
            (car (aiern-compile-subreplacement to))))))

(defun aiern-replace-match (replacement &optional fixedcase string)
  "Replace text match by last search with REPLACEMENT.
If REPLACEMENT is an expression it will be evaluated to compute
the replacement text, otherwise the function behaves as
`replace-match'."
  (if (stringp replacement)
      (replace-match replacement fixedcase nil string)
    (replace-match (funcall (car replacement)
                            (cdr replacement)
                            0)
                   fixedcase nil string)))

(defun aiern-match-substitute-replacement (replacement &optional fixedcase string)
  "Return REPLACEMENT as it will be inserted by `aiern-replace-match'."
  (if (stringp replacement)
      (match-substitute-replacement replacement fixedcase nil string)
    (match-substitute-replacement (funcall (car replacement)
                                           (cdr replacement)
                                           0)
                                  fixedcase nil string)))

;;; Alignment

(defun aiern-justify-lines (beg end justify position)
  "Justifes all lines in a range.
BEG and END specify the range of those lines to be
justified. JUSTIFY is either 'left, 'right or 'center according
to the justification type. POSITION is the maximal text width for
right and center justification or the column at which the lines
should be left-aligned for left justification."
  (let ((fill-column position)
        adaptive-fill-mode fill-prefix)
    (aiern-with-restriction
        (save-excursion
          (goto-char beg)
          (line-beginning-position))
        (save-excursion
          (goto-char end)
          (if (bolp)
              (line-end-position 0)
            (line-end-position)))
      (goto-char (point-min))
      (while (progn
               (if (eq justify 'left)
                   (indent-line-to position)
                 (when (re-search-forward "^[[:space:]]*" nil t)
                   (delete-region (match-beginning 0)
                                  (match-end 0)))
                 (justify-current-line justify nil t))
               (and (zerop (forward-line)) (bolp))))
      (goto-char (point-min))
      (back-to-indentation))))

;;; View helper

(defvar-local aiern-list-view-select-action nil)
(put 'aiern-list-view-select-action 'permanent-local t)

(define-derived-mode aiern-list-view-mode tabulated-list-mode
  "aiern List View"
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun aiern-list-view-goto-entry ()
  (interactive)
  (when (and aiern-list-view-select-action
             (not (eobp)))
    (let* ((line (line-number-at-pos (point)))
           (entry (elt tabulated-list-entries (1- line))))
      (funcall aiern-list-view-select-action (nth 1 entry)))))

(defun aiern-list-view-quit ()
  (interactive)
  (quit-window 'kill))

(define-key aiern-list-view-mode-map (kbd "q") #'aiern-list-view-quit)
(define-key aiern-list-view-mode-map [follow-link] nil) ;; allows mouse-1 to be activated
(define-key aiern-list-view-mode-map [mouse-1] #'aiern-list-view-goto-entry)
(define-key aiern-list-view-mode-map [return] #'aiern-list-view-goto-entry)

(defmacro aiern-with-view-list (&rest properties)
  "Opens new list view buffer.

PROPERTIES is a property-list which supports the following properties:

:name           (required)   The name of the buffer.
:mode-name      (required)   The name for the mode line.
:format         (required)   The value for `tabulated-list-format'.
:entries        (required)   The value for `tabulated-list-entries'.
:select-action  (optional)   A function for row selection.
                             It takes in a single parameter, which is the selected row's
                             vector value that is passed into `:entries'.
"
  (declare (indent defun) (debug t))
  `(let ((bufname (concat "*" ,(plist-get properties :name) "*"))
         (inhibit-read-only t))
     (and (get-buffer bufname)
          (kill-buffer bufname))
     (let ((buf (get-buffer-create bufname)))
       (with-current-buffer buf
         (setq tabulated-list-format ,(plist-get properties :format))
         (setq tabulated-list-entries ,(plist-get properties :entries))
         (setq aiern-list-view-select-action ,(plist-get properties :select-action))
         (aiern-list-view-mode)
         (setq mode-name ,(plist-get properties :mode-name))
         (aiern-motion-state))
       (switch-to-buffer-other-window buf))))

(provide 'aiern-common)

;;; aiern-common.el ends here
