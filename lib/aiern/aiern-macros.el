;;; aiern-macros.el --- Macros -*- lexical-binding: t -*-

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

(require 'aiern-common)
(require 'aiern-states)
(require 'aiern-repeat)

;;; Code:

(declare-function aiern-ex-p "aiern-ex")

;; set some error codes
(put 'beginning-of-line 'error-conditions '(beginning-of-line error))
(put 'beginning-of-line 'error-message "Beginning of line")
(put 'end-of-line 'error-conditions '(end-of-line error))
(put 'end-of-line 'error-message "End of line")

(defun aiern-motion-range (motion &optional count type)
  "Execute a motion and return the buffer positions.
The return value is a list (BEG END TYPE)."
  (let ((opoint   (point))
        (omark    (mark t))
        (obuffer  (current-buffer))
        (aiern-motion-marker (move-marker (make-marker) (point)))
        range)
    (aiern-with-transient-mark-mode
      (aiern-narrow-to-field
        (unwind-protect
            (let ((current-prefix-arg count)
                  ;; Store type in global variable `aiern-this-type'.
                  ;; If necessary, motions can change their type
                  ;; during execution by setting this variable.
                  (aiern-this-type
                   (or type (aiern-type motion 'exclusive))))
              (condition-case err
                  (let ((repeat-type (aiern-repeat-type motion t)))
                    (if (functionp repeat-type)
                        (funcall repeat-type 'pre))
                    (unless (with-local-quit
                              (setq range (call-interactively motion))
                              t)
                      (aiern-repeat-abort)
                      (setq quit-flag t))
                    (if (functionp repeat-type)
                        (funcall repeat-type 'post)))
                (error (prog1 nil
                         (aiern-repeat-abort)
                         ;; some operators depend on succeeding
                         ;; motions, in particular for
                         ;; `aiern-forward-char' (e.g., used by
                         ;; `aiern-substitute'), therefore we let
                         ;; end-of-line and end-of-buffer pass
                         (if (not (memq (car err) '(end-of-line end-of-buffer)))
                             (signal (car err) (cdr err))
                           (message (error-message-string err))))))
              (cond
               ;; the motion returned a range
               ((aiern-range-p range))
               ;; the motion made a Visual selection
               ((aiern-visual-state-p)
                (setq range (aiern-visual-range)))
               ;; the motion made an active region
               ((region-active-p)
                (setq range (aiern-range (region-beginning)
                                        (region-end)
                                        aiern-this-type)))
               ;; default: range from previous position to current
               (t
                (setq range (aiern-expand-range
                             (aiern-normalize aiern-motion-marker
                                             (point)
                                             aiern-this-type)))))
              (unless (or (null type) (eq (aiern-type range) type))
                (aiern-set-type range type)
                (aiern-expand-range range))
              (aiern-set-range-properties range nil)
              range)
          ;; restore point and mark like `save-excursion',
          ;; but only if the motion hasn't disabled the operator
          (unless aiern-inhibit-operator
            (set-buffer obuffer)
            (aiern-move-mark omark)
            (goto-char opoint))
          ;; delete marker so it doesn't slow down editing
          (move-marker aiern-motion-marker nil))))))

(defmacro aiern-define-motion (motion args &rest body)
  "Define a motion command MOTION.
ARGS is a list of arguments.  Motions can have any number of
arguments, but the first (if any) has the predefined meaning of
count.  BODY must execute the motion by moving point.

Optional keyword arguments are:
- `:type' - determines how the motion works after an operator (one of
  `inclusive', `line', `block' and `exclusive', or a self-defined
  motion type)
- `:jump' - if non-nil, the previous position is stored in the jump
  list, so that it can be restored with \
\\<aiern-motion-state-map>\\[aiern-jump-backward]

\(fn MOTION (COUNT ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let (arg doc interactive key keys)
    (when args
      (setq args `(&optional ,@(delq '&optional args))
            ;; the count is either numerical or nil
            interactive '("<c>")))
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :repeat 'motion))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body)
            keys (plist-put keys key arg)))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(progn
       ;; refresh echo area in Eldoc mode
       (when ',motion
         (eval-after-load 'eldoc
           '(and (fboundp 'eldoc-add-command)
                 (eldoc-add-command ',motion))))
       (aiern-define-command ,motion (,@args)
         ,@(when doc `(,doc))         ; avoid nil before `interactive'
         ,@keys
         :keep-visual t
         (interactive ,@interactive)
         ,@body))))

(defmacro aiern-narrow-to-line (&rest body)
  "Narrow BODY to the current line.
BODY will signal the errors 'beginning-of-line or 'end-of-line
upon reaching the beginning or end of the current line.

\(fn [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (debug t))
  `(let* ((range (aiern-expand (point) (point) 'line))
          (beg (aiern-range-beginning range))
          (end (aiern-range-end range))
          (min (point-min))
          (max (point-max)))
     (when (save-excursion (goto-char end) (bolp))
       (setq end (max beg (1- end))))
     ;; don't include the newline in Normal state
     (when (and (not aiern-move-beyond-eol)
                (not (aiern-visual-state-p))
                (not (aiern-operator-state-p)))
       (setq end (max beg (1- end))))
     (aiern-with-restriction beg end
       (aiern-signal-without-movement
         (condition-case err
             (progn ,@body)
           (beginning-of-buffer
            (if (= beg min)
                (signal (car err) (cdr err))
              (signal 'beginning-of-line nil)))
           (end-of-buffer
            (if (= end max)
                (signal (car err) (cdr err))
              (signal 'end-of-line nil))))))))

;; we don't want line boundaries to trigger the debugger
;; when `debug-on-error' is t
(add-to-list 'debug-ignored-errors "^Beginning of line$")
(add-to-list 'debug-ignored-errors "^End of line$")

(defun aiern-eobp (&optional pos)
  "Whether point is at end-of-buffer with regard to end-of-line."
  (save-excursion
    (when pos (goto-char pos))
    (cond
     ((eobp))
     ;; the rest only pertains to Normal state
     ((not (aiern-normal-state-p))
      nil)
     ;; at the end of the last line
     ((eolp)
      (forward-char)
      (eobp))
     ;; at the last character of the last line
     (t
      (forward-char)
      (cond
       ((eobp))
       ((eolp)
        (forward-char)
        (eobp)))))))

(defun aiern-move-beginning (count forward &optional backward)
  "Move to the beginning of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument."
  (let* ((count (or count 1))
         (backward (or backward
                       #'(lambda (count)
                           (funcall forward (- count)))))
         (forward (or forward
                      #'(lambda (count)
                          (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      (unwind-protect
          (aiern-motion-loop (nil count count)
            (funcall backward 1))
        (unless (zerop count)
          (goto-char (point-min)))))
     ((> count 0)
      (when (aiern-eobp)
        (signal 'end-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (<= (save-excursion
                  (funcall forward 1)
                  (funcall backward 1)
                  (point))
                opoint)
        (setq count (1+ count)))
      (unwind-protect
          (aiern-motion-loop (nil count count)
            (funcall forward 1))
        (if (zerop count)
            ;; go back to beginning of object
            (funcall backward 1)
          (goto-char (point-max)))))
     (t
      count))))

(defun aiern-move-end (count forward &optional backward inclusive)
  "Move to the end of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument.
If INCLUSIVE is non-nil, then point is placed at the last character
of the object; otherwise it is placed at the end of the object."
  (let* ((count (or count 1))
         (backward (or backward
                       #'(lambda (count)
                           (funcall forward (- count)))))
         (forward (or forward
                      #'(lambda (count)
                          (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (>= (save-excursion
                  (funcall backward 1)
                  (funcall forward 1)
                  (point))
                (if inclusive
                    (1+ opoint)
                  opoint))
        (setq count (1- count)))
      (unwind-protect
          (aiern-motion-loop (nil count count)
            (funcall backward 1))
        (if (not (zerop count))
            (goto-char (point-min))
          ;; go to end of object
          (funcall forward 1)
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (or (aiern-normal-state-p)
                    (aiern-motion-state-p))
            (aiern-adjust-cursor)))))
     ((> count 0)
      (when (aiern-eobp)
        (signal 'end-of-buffer nil))
      (when inclusive
        (forward-char))
      (unwind-protect
          (aiern-motion-loop (nil count count)
            (funcall forward 1))
        (if (not (zerop count))
            (goto-char (point-max))
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (or (aiern-normal-state-p)
                    (aiern-motion-state-p))
            (aiern-adjust-cursor)))))
     (t
      count))))

(defun aiern-text-object-make-linewise (range)
  "Turn the text object selection RANGE to linewise.
The selection is adjusted in a sensible way so that the selected
lines match the user intent. In particular, whitespace-only parts
at the first and last lines are omitted. This function returns
the new range."
  ;; Bug #607
  ;; If new type is linewise and the selection of the
  ;; first line consists of whitespace only, the
  ;; beginning is moved to the start of the next line. If
  ;; the selections of the last line consists of
  ;; whitespace only, the end is moved to the end of the
  ;; previous line.
  (if (eq (aiern-type range) 'line)
      range
    (let ((expanded (plist-get (aiern-range-properties range) :expanded))
          (newrange (aiern-expand-range range t)))
      (save-excursion
        ;; skip whitespace at the beginning
        (goto-char (aiern-range-beginning newrange))
        (skip-chars-forward " \t")
        (when (and (not (bolp)) (eolp))
          (aiern-set-range-beginning newrange (1+ (point))))
        ;; skip whitepsace at the end
        (goto-char (aiern-range-end newrange))
        (skip-chars-backward " \t")
        (when (and (not (eolp)) (bolp))
          (aiern-set-range-end newrange (1- (point))))
        ;; only modify range if result is not empty
        (if (> (aiern-range-beginning newrange)
               (aiern-range-end newrange))
            range
          (unless expanded
            (aiern-contract-range newrange))
          newrange)))))

(defmacro aiern-define-text-object (object args &rest body)
  "Define a text object command OBJECT.
BODY should return a range (BEG END) to the right of point
if COUNT is positive, and to the left of it if negative.

Optional keyword arguments:
- `:type' - determines how the range applies after an operator
  (`inclusive', `line', `block', and `exclusive', or a self-defined
  motion type).
- `:extend-selection' - if non-nil (default), the text object always
  enlarges the current selection.  Otherwise, it replaces the current
  selection.

\(fn OBJECT (COUNT) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((args (delq '&optional args))
         (count (or (pop args) 'count))
         (args (when args `(&optional ,@args)))
         (interactive '((interactive "<c><v>")))
         arg doc key keys)
    ;; collect docstring
    (when (stringp (car-safe body))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :extend-selection t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body)
            keys (plist-put keys key arg)))
    ;; interactive
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (list (pop body))))
    ;; macro expansion
    `(aiern-define-motion ,object (,count ,@args)
       ,@(when doc `(,doc))
       ,@keys
       ,@interactive
       (setq ,count (or ,count 1))
       (when (/= ,count 0)
         (let ((type (aiern-type ',object aiern-visual-char))
               (extend (and (aiern-visual-state-p)
                            (aiern-get-command-property
                             ',object :extend-selection
                             ',(plist-get keys :extend-selection))))
               (dir aiern-visual-direction)
               mark point range selection)
           (cond
            ;; Visual state: extend the current selection
            ((and (aiern-visual-state-p)
                  (called-interactively-p 'any))
             ;; if we are at the beginning of the Visual selection,
             ;; go to the left (negative COUNT); if at the end,
             ;; go to the right (positive COUNT)
             (setq dir aiern-visual-direction
                   ,count (* ,count dir))
             (setq range (progn ,@body))
             (when (aiern-range-p range)
               (setq range (aiern-expand-range range))
               (aiern-set-type range (aiern-type range type))
               (setq range (aiern-contract-range range))
               ;; the beginning is mark and the end is point
               ;; unless the selection goes the other way
               (setq mark  (aiern-range-beginning range)
                     point (aiern-range-end range)
                     type  (aiern-type
                            (if aiern-text-object-change-visual-type
                                range
                              (aiern-visual-range))))
               (when (and (eq type 'line)
                          (not (eq type (aiern-type range))))
                 (let ((newrange (aiern-text-object-make-linewise range)))
                   (setq mark (aiern-range-beginning newrange)
                         point (aiern-range-end newrange))))
               (when (< dir 0)
                 (aiern-swap mark point))
               ;; select the union
               (aiern-visual-make-selection mark point type)))
            ;; not Visual state: return a pair of buffer positions
            (t
             (setq range (progn ,@body))
             (unless (aiern-range-p range)
               (setq ,count (- ,count)
                     range (progn ,@body)))
             (when (aiern-range-p range)
               (setq selection (aiern-range (point) (point) type))
               (if extend
                   (setq range (aiern-range-union range selection))
                 (aiern-set-type range (aiern-type range type)))
               ;; possibly convert to linewise
               (when (eq aiern-this-type-modified 'line)
                 (setq range (aiern-text-object-make-linewise range)))
               (aiern-set-range-properties range nil)
               range))))))))

(defmacro aiern-define-operator (operator args &rest body)
  "Define an operator command OPERATOR.
The operator acts on the range of characters BEG through
END. BODY must execute the operator by potentially manipulating
the buffer contents, or otherwise causing side effects to happen.

Optional keyword arguments are:
- `:type' - force the input range to be of a given type (`inclusive',
  `line', `block', and `exclusive', or a self-defined motion type).
- `:motion' - use a predetermined motion instead of waiting for one
  from the keyboard.  This does not affect the behavior in visual
  state, where selection boundaries are always used.
- `:repeat' - if non-nil (default), then \
  \\<aiern-normal-state-map>\\[aiern-repeat] will repeat the
  operator.
- `:move-point' - if non-nil (default), the cursor will be moved to
  the beginning of the range before the body executes
- `:keep-visual' - if non-nil, the selection is not disabled when the
  operator is executed in visual state.  By default, visual state is
  exited automatically.

\(fn OPERATOR (BEG END ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let* ((args (delq '&optional args))
         (interactive (if (> (length args) 2) '("<R>") '("<r>")))
         (args (if (> (length args) 2)
                   `(,(nth 0 args) ,(nth 1 args)
                     &optional ,@(nthcdr 2 args))
                 args))
         arg doc key keys visual)
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :move-point t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :keep-visual)
        (setq visual arg))
       (t
        (setq keys (plist-put keys key arg)))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr-safe (pop body))))
    ;; transform extended interactive specs
    (setq interactive (apply #'aiern-interactive-form interactive))
    (setq keys (aiern-concat-plists keys (cdr-safe interactive))
          interactive (car-safe interactive))
    ;; macro expansion
    `(aiern-define-command ,operator ,args
       ,@(when doc `(,doc))
       ,@keys
       :keep-visual t
       :suppress-operator t
       (interactive
        (let* ((aiern-operator-range-motion
                (when (aiern-has-command-property-p ',operator :motion)
                  ;; :motion nil is equivalent to :motion undefined
                  (or (aiern-get-command-property ',operator :motion)
                      #'undefined)))
               (aiern-operator-range-type
                (aiern-get-command-property ',operator :type))
               (orig (point))
               aiern-operator-range-beginning
               aiern-operator-range-end
               aiern-inhibit-operator)
          (setq aiern-inhibit-operator-value nil
                aiern-this-operator this-command)
          (prog1 ,interactive
            (setq orig (point)
                  aiern-inhibit-operator-value aiern-inhibit-operator)
            (if ,visual
                (when (aiern-visual-state-p)
                  (aiern-visual-expand-region))
              (when (or (aiern-visual-state-p) (region-active-p))
                (setq deactivate-mark t)))
            (cond
             ((aiern-visual-state-p)
              (aiern-visual-rotate 'upper-left))
             ((aiern-get-command-property ',operator :move-point)
              (goto-char (or aiern-operator-range-beginning orig)))
             (t
              (goto-char orig))))))
       (unwind-protect
           (let ((aiern-inhibit-operator aiern-inhibit-operator-value))
             (unless (and aiern-inhibit-operator
                          (called-interactively-p 'any))
               ,@body))
         (setq aiern-inhibit-operator-value nil)))))

;; this is used in the `interactive' specification of an operator command
(defun aiern-operator-range (&optional return-type)
  "Read a motion from the keyboard and return its buffer positions.
The return value is a list (BEG END), or (BEG END TYPE) if
RETURN-TYPE is non-nil."
  (let* ((aiern-ex-p (and (not (minibufferp)) (aiern-ex-p)))
         (motion (or aiern-operator-range-motion
                     (when aiern-ex-p 'aiern-line)))
         (type aiern-operator-range-type)
         (range (aiern-range (point) (point)))
         command count)
    (setq aiern-this-type-modified nil)
    (aiern-save-echo-area
      (cond
       ;; Ex mode
       ((and aiern-ex-p aiern-ex-range)
        (setq range aiern-ex-range))
       ;; Visual selection
       ((and (not aiern-ex-p) (aiern-visual-state-p))
        (setq range (aiern-visual-range)))
       ;; active region
       ((and (not aiern-ex-p) (region-active-p))
        (setq range (aiern-range (region-beginning)
                                (region-end)
                                (or aiern-this-type 'exclusive))))
       (t
        ;; motion
        (aiern-save-state
          (unless motion
            (aiern-change-state 'operator)
            ;; Make linewise operator shortcuts. E.g., "d" yields the
            ;; shortcut "dd", and "g?" yields shortcuts "g??" and "g?g?".
            (let ((keys (nth 2 (aiern-extract-count (this-command-keys)))))
              (setq keys (listify-key-sequence keys))
              (dotimes (var (length keys))
                (define-key aiern-operator-shortcut-map
                  (vconcat (nthcdr var keys)) 'aiern-line-or-visual-line)))
            ;; read motion from keyboard
            (setq command (aiern-read-motion motion)
                  motion (nth 0 command)
                  count (nth 1 command)
                  type (or type (nth 2 command))))
          (cond
           ((eq motion #'undefined)
            (setq range (if return-type '(nil nil nil) '(nil nil))
                  motion nil))
           ((or (null motion) ; keyboard-quit
                (aiern-get-command-property motion :suppress-operator))
            (when (fboundp 'aiern-repeat-abort)
              (aiern-repeat-abort))
            (setq quit-flag t
                  motion nil))
           (aiern-repeat-count
            (setq count aiern-repeat-count
                  ;; only the first operator's count is overwritten
                  aiern-repeat-count nil))
           ((or count current-prefix-arg)
            ;; multiply operator count and motion count together
            (setq count
                  (* (prefix-numeric-value count)
                     (prefix-numeric-value current-prefix-arg)))))
          (when motion
            (let ((aiern-state 'operator)
                  mark-active)
              ;; calculate motion range
              (setq range (aiern-motion-range
                           motion
                           count
                           type))))
          ;; update global variables
          (setq aiern-this-motion motion
                aiern-this-motion-count count
                type (aiern-type range type)
                aiern-this-type type))))
      (when (aiern-range-p range)
        (unless (or (null type) (eq (aiern-type range) type))
          (aiern-contract-range range)
          (aiern-set-type range type)
          (aiern-expand-range range))
        (aiern-set-range-properties range nil)
        (unless return-type
          (aiern-set-type range nil))
        (setq aiern-operator-range-beginning (aiern-range-beginning range)
              aiern-operator-range-end (aiern-range-end range)
              aiern-operator-range-type (aiern-type range)))
      range)))

(defmacro aiern-define-type (type doc &rest body)
  "Define type TYPE.
DOC is a general description and shows up in all docstrings.

Optional keyword arguments:
- `:expand' - expansion function.  This function should accept two
  positions in the current buffer, BEG and END,and return a pair of
  expanded buffer positions.
- `:contract' - the opposite of `:expand'.  Optional.
- `:one-to-one' - non-nil if expansion is one-to-one.  This means that
  `:expand' followed by `:contract' always return the original range.
- `:normalize' - normalization function.  This function should accept
  two unexpanded positions and adjust them before expansion.  May be
  used to deal with buffer boundaries.
- `:string' - description function.  Takes two buffer positions and
  returns a human-readable string.  For example \"2 lines\"

If further keywords and functions are specified, they are assumed to
be transformations on buffer positions, like `:expand' and `:contract'.

\(fn TYPE DOC [[KEY FUNC]...])"
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (let (args defun-forms func key name plist string sym val)
    ;; standard values
    (setq plist (plist-put plist :one-to-one t))
    ;; keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            val (pop body))
      (if (plist-member plist key)      ; not a function
          (setq plist (plist-put plist key val))
        (setq func val
              sym (intern (replace-regexp-in-string
                           "^:" "" (symbol-name key)))
              name (intern (format "aiern-%s-%s" type sym))
              args (car (cdr-safe func))
              string (car (cdr (cdr-safe func)))
              string (if (stringp string)
                         (format "%s\n\n" string) "")
              plist (plist-put plist key `',name))
        (push
         (cond
          ((eq key :string)
           `(defun ,name (beg end &rest properties)
              ,(format "Return size of %s from BEG to END \
with PROPERTIES.\n\n%s%s" type string doc)
              (let ((beg (aiern-normalize-position beg))
                    (end (aiern-normalize-position end))
                    (type ',type)
                    plist range)
                (when (and beg end)
                  (save-excursion
                    (aiern-sort beg end)
                    (unless (plist-get properties :expanded)
                      (setq range (apply #'aiern-expand
                                         beg end type properties)
                            beg (aiern-range-beginning range)
                            end (aiern-range-end range)
                            type (aiern-type range type)
                            plist (aiern-range-properties range))
                      (setq properties
                            (aiern-concat-plists properties plist)))
                    (or (apply #',func beg end
                               (when ,(> (length args) 2)
                                 properties))
                        ""))))))
          (t
           `(defun ,name (beg end &rest properties)
              ,(format "Perform %s transformation on %s from BEG to END \
with PROPERTIES.\n\n%s%s" sym type string doc)
              (let ((beg (aiern-normalize-position beg))
                    (end (aiern-normalize-position end))
                    (type ',type)
                    plist range)
                (when (and beg end)
                  (save-excursion
                    (aiern-sort beg end)
                    (when (memq ,key '(:expand :contract))
                      (setq properties
                            (plist-put properties
                                       :expanded
                                       ,(eq key :expand))))
                    (setq range (or (apply #',func beg end
                                           (when ,(> (length args) 2)
                                             properties))
                                    (apply #'aiern-range
                                           beg end type properties))
                          beg (aiern-range-beginning range)
                          end (aiern-range-end range)
                          type (aiern-type range type)
                          plist (aiern-range-properties range))
                    (setq properties
                          (aiern-concat-plists properties plist))
                    (apply #'aiern-range beg end type properties)))))))
         defun-forms)))
    ;; :one-to-one requires both or neither of :expand and :contract
    (when (plist-get plist :expand)
      (setq plist (plist-put plist :one-to-one
                             (and (plist-get plist :contract)
                                  (plist-get plist :one-to-one)))))
    `(progn
       (aiern-put-property 'aiern-type-properties ',type ,@plist)
       ,@defun-forms
       ',type)))

(defmacro aiern-define-interactive-code (code &rest body)
  "Define an interactive code.
PROMPT, if given, is the remainder of the interactive string
up to the next newline. Command properties may be specified
via KEY-VALUE pairs. BODY should evaluate to a list of values.

\(fn CODE (PROMPT) [[KEY VALUE]...] BODY...)"
  (declare (indent defun))
  (let* ((args (when (and (> (length body) 1)
                          (listp (car-safe body)))
                 (pop body)))
         (doc (when (stringp (car-safe body)) (pop body)))
         func properties)
    (while (keywordp (car-safe body))
      (setq properties
            (append properties (list (pop body) (pop body)))))
    (cond
     (args
      (setq func `(lambda ,args
                    ,@(when doc `(,doc))
                    ,@body)))
     ((> (length body) 1)
      (setq func `(progn ,@body)))
     (t
      (setq func (car body))))
    `(eval-and-compile
       (let* ((code ,code)
              (entry (assoc code aiern-interactive-alist))
              (value (cons ',func ',properties)))
         (if entry
             (setcdr entry value)
           (push (cons code value) aiern-interactive-alist))
         code))))

;;; Highlighting

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   ;; Match all `aiern-define-' forms except `aiern-define-key'.
   ;; (In the interests of speed, this expression is incomplete
   ;; and does not match all three-letter words.)
   '(("(\\(aiern-\\(?:ex-\\)?define-\
\\(?:[^ k][^ e][^ y]\\|[-[:word:]]\\{4,\\}\\)\\)\
\\>[ \f\t\n\r\v]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     ("(\\(aiern-\\(?:delay\\|narrow\\|signal\\|save\\|with\\(?:out\\)?\\)\
\\(?:-[-[:word:]]+\\)?\\)\\>\[ \f\t\n\r\v]+"
      1 font-lock-keyword-face)
     ("(\\(aiern-\\(?:[-[:word:]]\\)*loop\\)\\>[ \f\t\n\r\v]+"
      1 font-lock-keyword-face))))

(provide 'aiern-macros)

;;; aiern-macros.el ends here
