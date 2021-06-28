;;; aiern-repeat.el --- Repeat system -*- lexical-binding: t -*-

;; Author: Frank Fischer <frank.fischer at mathematik.tu-chemnitz.de>
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

;; A repeat begins when leaving Normal state; it ends when re-entering
;; Normal state. The diagram below shows possible routes between
;; Normal state (N), Insert state (I), Visual state (V),
;; Operator-Pending state (O) and Replace state (R). (Emacs state
;; is an exception: nothing is repeated in that state.)
;;                              ___
;;                             /   \
;;                             | R |
;;                             \___/
;;                             ^   |
;;                             |   |
;;               ___           |___V           ___
;;              /   \ <------- /   \ -------> /   \
;;              | V |          | N |          | O |
;;              \___/ -------> \___/ <------- \___/
;;                  |          |   ^          |
;;                  |          |   |          |
;;                  |          V___|          |
;;                  |          /   \          |
;;                  +--------> | I | <--------+
;;                             \___/
;;
;; The recording of a repeat is started in one of two cases: Either a
;; command is about to be executed (in pre-command-hook) or normal
;; state is exited. The recording is stopped whenever a command has
;; been completed and aiern is in normal state afterwards. Therefore,
;; a non-inserting command in normal-state is recorded as a single
;; repeat unit. In contrast, if the command leaves normal state and
;; starts insert-state, all commands that are executed until
;; insert-state is left and normal state is reactivated are recorded
;; together in one repeat unit. In other words, a repeat unit consists
;; of all commands that are executed starting and ending in normal
;; state.
;;
;; Not all commands are recorded. There are several commands that are
;; completely ignored and other commands that even abort the currently
;; active recording, e.g., commands that switch buffer.
;;
;; During recording the repeat information is appended to the variable
;; `aiern-repeat-info', which is cleared when the recording
;; starts. This accumulated repeat information is put into the
;; `aiern-repeat-ring' when the recording is finished. The dot command,
;; `\[aiern-repeat]' (`aiern-repeat') replays the most recent entry in
;; the ring, preceeding repeats can be replayed using
;; `\[aiern-repeat-pop]' (`aiern-repeat-pop').
;;
;; Repeat information can be stored in almost arbitrary form. How the
;; repeat information for each single command is recored is determined
;; by the :repeat property of the command. This property has the
;; following interpretation:
;;
;; t         record commands by storing the key-sequence that invoked it
;; nil       ignore this command completely
;; ignore    synonym to nil
;; motion    command is recorded by storing the key-sequence but only in
;;           insert state, otherwise it is ignored.
;; abort     stop recording of repeat information immediately
;; change    record commands by storing buffer changes
;; SYMBOL    if SYMBOL is contained as key in `aiern-repeat-types'
;;           call the corresponding (function-)value, otherwise
;;           call the function associated with SYMBOL. In both
;;           cases the function should take exactly one argument
;;           which is either 'pre or 'post depending on whether
;;           the function is called before or after the execution
;;           of the command.
;;
;; Therefore, using a certain SYMBOL one can write specific repeation
;; functions for each command.
;;
;; Each value of ring `aiern-repeat-info', i.e., each single repeat
;; information must be one of the following two possibilities:
;; If element is a sequence, it is regarded as a key-sequence to
;; be repeated. Otherwise the element must be a list
;; (FUNCTION PARAMS ...) which will be called using
;; (apply FUNCTION PARAMS) whenever this repeat is being executed.
;;
;; A user supplied repeat function can use the functions
;; `aiern-record-repeat' to append further repeat-information of the
;; form described above to `aiern-repeat-info'. See the implementation
;; of `aiern-repeat-keystrokes' and `aiern-repeat-changes' for examples.
;; Those functions are called in different situations before and after
;; the execution of a command. Each function should take one argument
;; which can be either 'pre, 'post, 'pre-operator or 'post-operator
;; specifying when the repeat function has been called. If the command
;; is a usual command the function is called with 'pre before the
;; command is executed and with 'post after the command has been
;; executed.
;;
;; The repeat information is executed with `aiern-execute-repeat-info',
;; which passes key-sequence elements to `execute-kbd-macro' and
;; executes other elements as defined above.  A special version is
;; `aiern-execute-repeat-info-with-count'.  This function works as
;; `aiern-execute-repeat-info', but replaces the count of the first
;; command. This is done by parsing the key-sequence, ignoring all
;; calls to `digit-prefix-argument' and `negative-argument', and
;; prepending the count as a string to the vector of the remaining
;; key-sequence.

(require 'aiern-states)

;;; Code:

(declare-function aiern-visual-state-p "aiern-visual")
(declare-function aiern-visual-range "aiern-visual")
(declare-function aiern-visual-char "aiern-visual")
(declare-function aiern-visual-line "aiern-visual")
(declare-function aiern-visual-block "aiern-visual")

(defmacro aiern-without-repeat (&rest body)
  (declare (indent defun)
           (debug t))
  `(let ((pre-command-hook (remq 'aiern-repeat-pre-hook pre-command-hook))
         (post-command-hook (remq 'aiern-repeat-post-hook post-command-hook)))
     ,@body
     (aiern-repeat-abort)))

(defsubst aiern-repeat-recording-p ()
  "Returns non-nil iff a recording is in progress."
  (eq aiern-recording-repeat t))

(defun aiern-repeat-start ()
  "Start recording a new repeat into `aiern-repeat-info'."
  (aiern-repeat-reset t)
  (aiern-repeat-record-buffer)
  (when (aiern-visual-state-p)
    (let* ((range (aiern-visual-range))
           (beg (aiern-range-beginning range))
           (end (1- (aiern-range-end range)))
           (nfwdlines (aiern-count-lines beg end)))
      (aiern-repeat-record
       (cond
        ((eq aiern-visual-selection 'char)
         (list #'aiern-repeat-visual-char
               nfwdlines
               (- end
                  (if (zerop nfwdlines)
                      beg
                    (save-excursion
                      (goto-char end)
                      (line-beginning-position))))))
        ((eq aiern-visual-selection 'line)
         (list #'aiern-repeat-visual-line nfwdlines))
        ((eq aiern-visual-selection 'block)
         (list #'aiern-repeat-visual-block
               nfwdlines
               (abs (- (aiern-column beg) (aiern-column end))))))))))

(defun aiern-repeat-stop ()
  "Stop recording a repeat.
Update `aiern-repeat-ring' with the accumulated changes
in `aiern-repeat-info' and clear variables."
  (unwind-protect
      (when (aiern-repeat-recording-p)
        (setq aiern-repeat-info
              (aiern-normalize-repeat-info aiern-repeat-info))
        (when (and aiern-repeat-info aiern-repeat-ring)
          (ring-insert aiern-repeat-ring aiern-repeat-info)))
    (aiern-repeat-reset nil)))

(defun aiern-repeat-abort ()
  "Abort current repeation."
  (aiern-repeat-reset 'abort))

(defun aiern-repeat-reset (flag)
  "Clear all repeat recording variables.
Set `aiern-recording-repeat' to FLAG."
  (setq aiern-recording-repeat flag
        aiern-repeat-info nil
        aiern-repeat-buffer nil))

(defsubst aiern-repeat-record-position (&optional pos)
  "Set `aiern-repeat-pos' to POS or point."
  (setq aiern-repeat-pos (or pos (point))))

(defun aiern-repeat-record-buffer ()
  "Set `aiern-repeat-buffer' to the current buffer."
  (unless (minibufferp)
    (setq aiern-repeat-buffer (current-buffer))))

(defmacro aiern-save-repeat-info (&rest body)
  "Execute BODY, protecting the values of repeat variables."
  (declare (indent defun)
           (debug t))
  `(let (aiern-repeat-ring
         aiern-recording-repeat
         aiern-recording-current-command
         aiern-repeat-info
         aiern-repeat-changes
         aiern-repeat-pos
         aiern-repeat-keys
         aiern-repeat-buffer
         this-command
         last-command)
     ,@body))

(defun aiern-repeat-different-buffer-p (&optional strict)
  "Whether the buffer has changed in a repeat.
If STRICT is non-nil, returns t if the previous buffer
is unknown; otherwise returns t only if the previous
buffer is known and different from the current buffer."
  (and (or (buffer-live-p aiern-repeat-buffer) strict)
       (not (minibufferp))
       (not (eq (current-buffer) aiern-repeat-buffer))))

(defun aiern-repeat-type (command &optional default)
  "Return the :repeat property of COMMAND.
If COMMAND doesn't have this property, return DEFAULT."
  (when (functionp command) ; ignore keyboard macros
    (let* ((type (aiern-get-command-property command :repeat default))
           (repeat-type (assq type aiern-repeat-types)))
      (if repeat-type (cdr repeat-type) type))))

(defun aiern-repeat-force-abort-p (repeat-type)
  "Returns non-nil iff the current command should abort the recording of repeat information."
  (or (aiern-repeat-different-buffer-p)           ; ... buffer changed
      (eq repeat-type 'abort)                    ; ... explicitely forced
      (eq aiern-recording-repeat 'abort)          ; ... already aborted
      (aiern-emacs-state-p)                       ; ... in Emacs state
      (and (aiern-mouse-events-p (this-command-keys))  ; ... mouse events
           (eq repeat-type nil))
      (minibufferp)))                            ; ... minibuffer activated

(defun aiern-repeat-record (info)
  "Add INFO to the end of `aiern-repeat-info'."
  (when (aiern-repeat-recording-p)
    (setq aiern-repeat-info (nconc aiern-repeat-info (list info)))))

;; called from `aiern-normal-state-exit-hook'
(defun aiern-repeat-start-hook ()
  "Record a new repeat when exiting Normal state.
Does not record in Emacs state or if the current command
has :repeat nil."
  (when (and (eq (aiern-repeat-type this-command t) t)
             (not (aiern-emacs-state-p)))
    (aiern-repeat-start)))

;; called from `pre-command-hook'
(defun aiern-repeat-pre-hook ()
  "Prepare the current command for recording the repeation."
  (when aiern-local-mode
    (let ((repeat-type (aiern-repeat-type this-command t)))
      (cond
       ;; abort the repeat
       ((aiern-repeat-force-abort-p repeat-type)
        ;; We mark the current record as being aborted, because there
        ;; may be further pre-hooks following before the post-hook is
        ;; called.
        (aiern-repeat-abort))
       ;; ignore those commands completely
       ((or (null repeat-type)
            (aiern-mouse-events-p (this-command-keys))))
       ;; record command
       (t
        ;; In normal-state or visual state, each command is a single
        ;; repeation, therefore start a new repeation.
        (when (or (aiern-normal-state-p)
                  (aiern-visual-state-p))
          (aiern-repeat-start))
        (setq aiern-recording-current-command t)
        (funcall repeat-type 'pre))))))
(put 'aiern-repeat-pre-hook 'permanent-local-hook t)

;; called from `post-command-hook'
(defun aiern-repeat-post-hook ()
  "Finish recording of repeat-information for the current-command."
  (when (and aiern-local-mode aiern-recording-repeat)
    (let ((repeat-type (aiern-repeat-type this-command t)))
      (cond
       ;; abort the repeat
       ((aiern-repeat-force-abort-p repeat-type)
        ;; The command has been aborted but is complete, so just reset
        ;; the recording state.
        (aiern-repeat-reset nil))
       ;; ignore if command should not be recorded or the current
       ;; command is not being recorded
       ((or (null repeat-type)
            (not aiern-recording-current-command)))
       ;; record command
       (t
        (funcall repeat-type 'post)
        ;; In normal state, the repeat sequence is complete, so record it.
        (when (aiern-normal-state-p)
          (aiern-repeat-stop)))))
    ;; done with recording the current command
    (setq aiern-recording-current-command nil)))
(put 'aiern-repeat-post-hook 'permanent-local-hook t)

(defun aiern-clear-command-keys ()
  "Clear `this-command-keys' and all information about the current command keys.
Calling this function prevents further recording of the keys that
invoked the current command"
  (clear-this-command-keys t)
  (setq aiern-repeat-keys ""))

(defun aiern-this-command-keys (&optional post-cmd)
  "Version of `this-command-keys' with finer control over prefix args."
  (let ((arg (if post-cmd current-prefix-arg prefix-arg)))
    (vconcat
     (when (and (numberp arg)
                ;; Only add prefix if no repeat info recorded yet
                (null aiern-repeat-info))
       (string-to-vector (number-to-string arg)))
     (this-single-command-keys))))

(defun aiern-repeat-keystrokes (flag)
  "Repeation recording function for commands that are repeated by keystrokes."
  (cond
   ((eq flag 'pre)
    (when aiern-this-register
      (aiern-repeat-record
       `(set aiern-this-register ,aiern-this-register)))
    (setq aiern-repeat-keys (aiern-this-command-keys)))
   ((eq flag 'post)
    (aiern-repeat-record (if (zerop (length (aiern-this-command-keys t)))
                            aiern-repeat-keys
                          (aiern-this-command-keys t)))
    ;; erase commands keys to prevent double recording
    (aiern-clear-command-keys))))

(defun aiern-repeat-motion (flag)
  "Repeation for motions. Motions are recorded by keystroke but only in insert state."
  (when (memq aiern-state '(insert replace))
    (aiern-repeat-keystrokes flag)))

(defun aiern-repeat-changes (flag)
  "Repeation recording function for commands that are repeated by buffer changes."
  (cond
   ((eq flag 'pre)
    (add-hook 'after-change-functions #'aiern-repeat-change-hook nil t)
    (aiern-repeat-start-record-changes))
   ((eq flag 'post)
    (remove-hook 'after-change-functions #'aiern-repeat-change-hook t)
    (aiern-repeat-finish-record-changes))))

;; called from the `after-change-functions' hook
(defun aiern-repeat-change-hook (beg end length)
  "Record change information for current command."
  (let ((repeat-type (aiern-repeat-type this-command t)))
    (when (and (aiern-repeat-recording-p)
               (eq repeat-type 'aiern-repeat-changes)
               (not (aiern-emacs-state-p))
               (not (aiern-repeat-different-buffer-p t))
               aiern-state)
      (unless (aiern-repeat-recording-p)
        (aiern-repeat-start))
      (aiern-repeat-record-change (- beg aiern-repeat-pos)
                                 (buffer-substring beg end)
                                 length))))
(put 'aiern-repeat-change-hook 'permanent-local-hook t)

(defun aiern-repeat-record-change (relpos ins ndel)
  "Record the current buffer changes during a repeat.
If CHANGE is specified, it is added to `aiern-repeat-changes'."
  (when (aiern-repeat-recording-p)
    (setq aiern-repeat-changes
          (nconc aiern-repeat-changes (list (list relpos ins ndel))))))

(defun aiern-repeat-start-record-changes ()
  "Starts the recording of a new set of buffer changes."
  (setq aiern-repeat-changes nil)
  (aiern-repeat-record-position))

(defun aiern-repeat-finish-record-changes ()
  "Finishes the recording of buffer changes and records them as repeat."
  (when (aiern-repeat-recording-p)
    (aiern-repeat-record `(aiern-execute-change
                          ,aiern-repeat-changes
                          ,(- (point) aiern-repeat-pos)))
    (setq aiern-repeat-changes nil)))

(defun aiern-repeat-insert-at-point (flag)
  "Repeation recording function for commands that insert text in region.
For example `mouse-yank-primary'. This records text insertion when a command
inserts some text in a buffer between (point) and (mark)."
  (cond
   ((eq flag 'pre)
    (add-hook 'after-change-functions #'aiern-repeat-insert-at-point-hook nil t))
   ((eq flag 'post)
    (remove-hook 'after-change-functions #'aiern-repeat-insert-at-point-hook t))))

(defun aiern-repeat-insert-at-point-hook (beg end _length)
  (let ((repeat-type (aiern-repeat-type this-command t)))
    (when (and (aiern-repeat-recording-p)
               (eq repeat-type 'aiern-repeat-insert-at-point)
               (not (aiern-emacs-state-p))
               (not (aiern-repeat-different-buffer-p t))
               aiern-state)
      (setq aiern-repeat-pos beg)
      (aiern-repeat-record (list 'insert (buffer-substring beg end))))))
(put 'aiern-repeat-insert-at-point-hook 'permanent-local-hook t)

(defun aiern-normalize-repeat-info (repeat-info)
  "Concatenate consecutive arrays in REPEAT-INFO.
Returns a single array."
  (let* ((result (cons nil nil))
         (result-last result)
         cur cur-last)
    (dolist (rep repeat-info)
      (cond
       ((null rep))
       ((arrayp rep)
        (setq rep (listify-key-sequence rep))
        (cond
         (cur
          (setcdr cur-last (cons rep nil))
          (setq cur-last (cdr cur-last)))
         (t
          (setq cur (cons rep nil))
          (setq cur-last cur))))
       (t
        (when cur
          (setcdr result-last (cons (apply #'vconcat cur) nil))
          (setq result-last (cdr result-last))
          (setq cur nil))
        (setcdr result-last (cons rep nil))
        (setq result-last (cdr result-last)))))
    (when cur
      (setcdr result-last (cons (apply #'vconcat cur) nil)))
    (cdr result)))

(defun aiern-repeat-visual-char (nfwdlines nfwdchars)
  "Restores a character visual selection.
If the selection is in a single line, the restored visual
selection covers the same number of characters. If the selection
covers several lines, the restored selection covers the same
number of lines and the same number of characters in the last
line as the original selection."
  (aiern-visual-char)
  (when (> nfwdlines 0)
    (forward-line nfwdlines))
  (forward-char nfwdchars))

(defun aiern-repeat-visual-line (nfwdlines)
  "Restores a character visual selection.
If the selection is in a single line, the restored visual
selection covers the same number of characters. If the selection
covers several lines, the restored selection covers the same
number of lines and the same number of characters in the last
line as the original selection."
  (aiern-visual-line)
  (forward-line nfwdlines))

(defun aiern-repeat-visual-block (nfwdlines nfwdchars)
  "Restores a character visual selection.
If the selection is in a single line, the restored visual
selection covers the same number of characters. If the selection
covers several lines, the restored selection covers the same
number of lines and the same number of characters in the last
line as the original selection."
  (aiern-visual-block)
  (let ((col (current-column)))
    (forward-line nfwdlines)
    (move-to-column (+ col nfwdchars) t)))

(defun aiern-execute-change (changes rel-point)
  "Executes as list of changes.

CHANGES is a list of triples (REL-BEG INSERT-TEXT NDEL).
REL-BEG is the relative position (to point) where the change
takes place. INSERT-TEXT is the text to be inserted at that
position and NDEL the number of characters to be deleted at that
position before insertion.

REL-POINT is the relative position to point before the changed
where point should be placed after all changes."
  (aiern-save-repeat-info
    (let ((point (point)))
      (dolist (change changes)
        (goto-char (+ point (nth 0 change)))
        (delete-char (nth 2 change))
        (insert (nth 1 change)))
      (goto-char (+ point rel-point)))))

(defun aiern-execute-repeat-info (repeat-info)
  "Executes a repeat-information REPEAT-INFO."
  (aiern-save-repeat-info
    (dolist (rep repeat-info)
      (cond
       ((or (arrayp rep) (stringp rep))
        (let ((input-method current-input-method)
              (aiern-input-method nil))
          (deactivate-input-method)
          (unwind-protect
              (execute-kbd-macro rep)
            (activate-input-method input-method))))
       ((consp rep)
        (when (and (= 3 (length rep))
                   (eq (nth 0 rep) 'set)
                   (eq (nth 1 rep) 'aiern-this-register)
                   (>= (nth 2 rep) ?0)
                   (< (nth 2 rep) ?9))
          (setcar (nthcdr 2 rep) (1+ (nth 2 rep))))
        (apply (car rep) (cdr rep)))
       (t
        (error "Unexpected repeat-info: %S" rep))))))

;; TODO: currently we prepend the replacing count before the
;; key-sequence that calls the command. Can we use direct
;; modification of prefix-arg instead? Does it work in
;; conjunction with `execute-kbd-macro'?
(defun aiern-execute-repeat-info-with-count (count repeat-info)
  "Repeat the repeat-information REPEAT-INFO with the count of
the first command replaced by COUNT. The count is replaced if
and only if COUNT is non-nil."
  (aiern-save-repeat-info
    (cond
     ;; do nothing (zero repeating)
     ((and count (zerop count)))
     ;; replace count
     (count
      (let ((aiern-repeat-count count)
            done)
        (while (and repeat-info
                    (arrayp (car repeat-info))
                    (not done))
          (let* ((count-and-cmd (aiern-extract-count (pop repeat-info))))
            (push (vconcat (number-to-string count)
                           (nth 2 count-and-cmd)
                           (nth 3 count-and-cmd))
                  repeat-info)
            (setq done t)))
        (aiern-execute-repeat-info repeat-info)))
     ;; repeat with original count
     (t
      (aiern-execute-repeat-info repeat-info)))))

(aiern-define-command aiern-repeat (count &optional save-point)
  "Repeat the last editing command with count replaced by COUNT.
If SAVE-POINT is non-nil, do not move point."
  :repeat ignore
  :suppress-operator t
  (interactive (list current-prefix-arg
                     (not aiern-repeat-move-cursor)))
  (cond
   ((null aiern-repeat-ring)
    (error "Already executing repeat"))
   (save-point
    (save-excursion
      (aiern-repeat count)))
   (t
    (unwind-protect
        (let ((aiern-last-find-temp aiern-last-find)
              (confirm-kill-emacs t)
              (kill-buffer-hook
               (cons #'(lambda ()
                         (user-error "Cannot delete buffer in repeat command"))
                     kill-buffer-hook))
              (undo-pointer buffer-undo-list))
          (aiern-with-single-undo
            (setq aiern-last-repeat (list (point) count undo-pointer))
            (aiern-execute-repeat-info-with-count
             count (ring-ref aiern-repeat-ring 0))
            (setq aiern-last-find aiern-last-find-temp)))
      (if (eq 'aiern-execute-in-normal-state last-command)
          (aiern-change-state aiern--execute-normal-return-state)
        (aiern-normal-state))))))

;; TODO: the same issue concering disabled undos as for `aiern-paste-pop'
(aiern-define-command aiern-repeat-pop (count &optional save-point)
  "Replace the just repeated command with a previously executed command.
Only allowed after `aiern-repeat', `aiern-repeat-pop' or
`aiern-repeat-pop-next'. Uses the same repeat count that
was used for the first repeat.

The COUNT argument inserts the COUNT-th previous kill.
If COUNT is negative, this is a more recent kill."
  :repeat nil
  :suppress-operator t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not aiern-repeat-move-cursor)))
  (cond
   ((not (and (eq last-command #'aiern-repeat)
              aiern-last-repeat))
    (user-error "Previous command was not aiern-repeat: %s" last-command))
   (save-point
    (save-excursion
      (aiern-repeat-pop count)))
   (t
    (unless (eq buffer-undo-list (nth 2 aiern-last-repeat))
      (aiern-undo-pop))
    (goto-char (car aiern-last-repeat))
    ;; rotate the repeat-ring
    (while (> count 0)
      (when aiern-repeat-ring
        (ring-insert-at-beginning aiern-repeat-ring
                                  (ring-remove aiern-repeat-ring 0)))
      (setq count (1- count)))
    (while (< count 0)
      (when aiern-repeat-ring
        (ring-insert aiern-repeat-ring
                     (ring-remove aiern-repeat-ring)))
      (setq count (1+ count)))
    (setq this-command #'aiern-repeat)
    (aiern-repeat (cadr aiern-last-repeat)))))

(aiern-define-command aiern-repeat-pop-next (count &optional save-point)
  "Same as `aiern-repeat-pop', but with negative COUNT."
  :repeat nil
  :suppress-operator t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not aiern-repeat-move-cursor)))
  (aiern-repeat-pop (- count) save-point))

(defadvice read-key-sequence (before aiern activate)
  "Record `this-command-keys' before it is reset."
  (when (and (aiern-repeat-recording-p)
             aiern-recording-current-command)
    (let ((repeat-type (aiern-repeat-type this-command t)))
      (if (functionp repeat-type)
          (funcall repeat-type 'post)))))

(provide 'aiern-repeat)

;;; aiern-repeat.el ends here
