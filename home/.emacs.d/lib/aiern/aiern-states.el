;;; aiern-states.el --- States -*- lexical-binding: t -*-

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
(require 'aiern-god-state)

;;; Code:

;;; Normal state

(aiern-define-state normal
  "Normal state.
AKA \"Command\" state."
  :tag " <AN> "
  :enable (motion)
  :exit-hook (aiern-repeat-start-hook)
  (cond
   ((aiern-normal-state-p)
    (overwrite-mode -1)
    (add-hook 'post-command-hook #'aiern-normal-post-command nil t))
   (t
    (remove-hook 'post-command-hook #'aiern-normal-post-command t))))

(defun aiern-normal-post-command (&optional command)
  "Reset command loop variables in Normal state.
Also prevent point from reaching the end of the line.
If the region is activated, enter Visual state."
  (unless (or (aiern-initializing-p)
              (null this-command))
    (setq command (or command this-command))
    (when (aiern-normal-state-p)
      (setq aiern-this-type nil
            aiern-this-operator nil
            aiern-this-motion nil
            aiern-this-motion-count nil
            aiern-inhibit-operator nil
            aiern-inhibit-operator-value nil)
      (unless (memq command '(aiern-use-register
                              digit-argument
                              negative-argument
                              universal-argument
                              universal-argument-minus
                              universal-argument-more
                              universal-argument-other-key))
        (setq aiern-this-register nil))
      (aiern-adjust-cursor))))
(put 'aiern-normal-post-command 'permanent-local-hook t)

;;; Insert state

(defun aiern-maybe-remove-spaces (&optional do-remove)
  "Remove space from newly opened empty line.
This function removes (indentation) spaces that have been
inserted by opening a new empty line. The behavior depends on the
variable `aiern-maybe-remove-spaces'. If this variable is nil the
function does nothing. Otherwise the behavior depends on
DO-REMOVE.  If DO-REMOVE is non-nil the spaces are
removed. Otherwise `aiern-maybe-remove-spaces' is set to nil
unless the last command opened yet another new line.

This function should be added as a post-command-hook to track
commands opening a new line."
  (cond
   ((not aiern-maybe-remove-spaces)
    (remove-hook 'post-command-hook #'aiern-maybe-remove-spaces))
   (do-remove
    (when (save-excursion
            (beginning-of-line)
            (looking-at "^\\s-*$"))
      (delete-region (line-beginning-position)
                     (line-end-position)))
    (setq aiern-maybe-remove-spaces nil)
    (remove-hook 'post-command-hook #'aiern-maybe-remove-spaces))
   ((not (memq this-command
               '(aiern-open-above
                 aiern-open-below
                 aiern-append
                 aiern-append-line
                 newline
                 newline-and-indent
                 indent-and-newline)))
    (setq aiern-maybe-remove-spaces nil)
    (remove-hook 'post-command-hook #'aiern-maybe-remove-spaces))))

(aiern-define-state insert
  "Insert state."
  :tag " <AI> "
  :cursor (bar . 2)
  :message "-- INSERT --"
  :entry-hook (aiern-start-track-last-insertion)
  :exit-hook (aiern-cleanup-insert-state aiern-stop-track-last-insertion)
  :input-method t
  (cond
   ((aiern-insert-state-p)
    (add-hook 'post-command-hook #'aiern-maybe-remove-spaces)
    (add-hook 'pre-command-hook #'aiern-insert-repeat-hook)
    (setq aiern-maybe-remove-spaces t)
    (unless (eq aiern-want-fine-undo t)
      (aiern-start-undo-step)))
   (t
    (remove-hook 'post-command-hook #'aiern-maybe-remove-spaces)
    (remove-hook 'pre-command-hook #'aiern-insert-repeat-hook)
    (aiern-maybe-remove-spaces t)
    (setq aiern-insert-repeat-info aiern-repeat-info)
    (aiern-set-marker ?^ nil t)
    (unless (eq aiern-want-fine-undo t)
      (aiern-end-undo-step))
    (when (or (aiern-normal-state-p aiern-next-state)
              (aiern-motion-state-p aiern-next-state))
      (aiern-move-cursor-back
       (and (eolp) (not aiern-move-beyond-eol)))))))

(defun aiern-insert-repeat-hook ()
  "Record insertion keys in `aiern-insert-repeat-info'."
  (setq aiern-insert-repeat-info (last aiern-repeat-info))
  (remove-hook 'pre-command-hook #'aiern-insert-repeat-hook))
(put 'aiern-insert-repeat-hook 'permanent-local-hook t)

(defun aiern-cleanup-insert-state ()
  "Called when Insert state is about to be exited.
Handles the repeat-count of the insertion command."
  (when aiern-insert-count
    (dotimes (_ (1- aiern-insert-count))
      (when aiern-insert-lines
        (aiern-insert-newline-below)
        (when aiern-auto-indent
          (indent-according-to-mode)))
      (when (fboundp 'aiern-execute-repeat-info)
        (aiern-execute-repeat-info
         (cdr aiern-insert-repeat-info)))))
  (when aiern-insert-vcount
    (let ((buffer-invisibility-spec buffer-invisibility-spec))
      ;; make all lines hidden by hideshow temporarily visible
      (when (listp buffer-invisibility-spec)
        (setq buffer-invisibility-spec
              (aiern-filter-list
               #'(lambda (x)
                   (or (eq x 'hs)
                       (eq (car-safe x) 'hs)))
               buffer-invisibility-spec)))
      (let ((line (nth 0 aiern-insert-vcount))
            (col (nth 1 aiern-insert-vcount))
            (vcount (nth 2 aiern-insert-vcount)))
        (save-excursion
          (dotimes (v (1- vcount))
            (goto-char (point-min))
            (forward-line (+ line v))
            (when (or (not aiern-insert-skip-empty-lines)
                      (not (integerp col))
                      (save-excursion
                        (aiern-move-end-of-line)
                        (>= (current-column) col)))
              (if (integerp col)
                  (move-to-column col t)
                (funcall col))
              (dotimes (_ (or aiern-insert-count 1))
                (when (fboundp 'aiern-execute-repeat-info)
                  (aiern-execute-repeat-info
                   (cdr aiern-insert-repeat-info)))))))))))

;;; Visual state

;; Visual selections are implemented in terms of types, and are
;; compatible with the Emacs region. This is achieved by "translating"
;; the region to the selected text right before a command is executed.
;; If the command is a motion, the translation is postponed until a
;; non-motion command is invoked (distinguished by the :keep-visual
;; command property).
;;
;; Visual state activates the region, enabling Transient Mark mode if
;; not already enabled. This is only temporay: if Transient Mark mode
;; was disabled before entering Visual state, it is disabled when
;; exiting Visual state. This allows Visual state to harness the
;; "transient" behavior of many commands without overriding the user's
;; preferences in other states.

(defmacro aiern-define-visual-selection (selection doc &rest body)
  "Define a Visual selection SELECTION.
Creates a command aiern-visual-SELECTION for enabling the selection.
DOC is the function's documentation string. The following keywords
may be specified in BODY:

:message STRING         Status message when enabling the selection.
:type TYPE              Type to use (defaults to SELECTION).

Following the keywords is optional code which is executed each time
the selection is enabled.

\(fn SELECTION DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name stringp
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((name (intern (format "aiern-visual-%s" selection)))
         (message (intern (format "%s-message" name)))
         (tagvar (intern (format "%s-tag" name)))
         (type selection)
         (tag " <AV> ")
         arg key string)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :message)
        (setq string arg))
       ((eq key :type)
        (setq type arg))
       ((eq key :tag)
        (setq tag arg))))
    ;; macro expansion
    `(progn
       (add-to-list 'aiern-visual-alist (cons ',selection ',name))
       (defvar ,name ',type ,(format "*%s" doc))
       (defvar ,message ,string ,doc)
       (defvar ,tagvar ,tag ,doc)
       (aiern-define-command ,name (&optional mark point type message)
         ,@(when doc `(,doc))
         :keep-visual t
         :repeat nil
         (interactive
          (list nil nil
                (if (and (aiern-visual-state-p)
                         (eq aiern-visual-selection ',selection))
                    'exit ,name) t))
         (if (eq type 'exit)
             (aiern-exit-visual-state)
           (setq type (or type ,name)
                 aiern-visual-selection ',selection)
           (aiern-visual-make-region mark point type message)
           ,@body))
       ',selection)))

(aiern-define-visual-selection char
  "Characterwise selection."
  :type inclusive
  :message "-- VISUAL --"
  :tag " <AV> ")

(aiern-define-visual-selection line
  "Linewise selection."
  :message "-- VISUAL LINE --"
  :tag " <AVl> ")

(aiern-define-visual-selection screen-line
  "Linewise selection in `visual-line-mode'."
  :message "-- SCREEN LINE --"
  :tag " <AVs> ")

(aiern-define-visual-selection block
  "Blockwise selection."
  :message "-- VISUAL BLOCK --"
  :tag " <AVb> "
  (aiern-transient-mark -1)
  ;; refresh the :corner property
  (setq aiern-visual-properties
        (plist-put aiern-visual-properties :corner
                   (aiern-visual-block-corner 'upper-left))))

(aiern-define-state visual
  "Visual state."
  :tag 'aiern-visual-tag
  :enable (motion normal)
  :message 'aiern-visual-message
  (cond
   ((aiern-visual-state-p)
    (aiern-save-transient-mark-mode)
    (setq select-active-regions nil)
    (cond
     ((region-active-p)
      (if (< (aiern-visual-direction) 0)
          (aiern-visual-select (region-beginning) (region-end)
                              aiern-visual-char
                              (aiern-visual-direction))
        (aiern-visual-make-selection (mark t) (point)
                                    aiern-visual-char))
      (aiern-visual-highlight))
     (t
      (aiern-visual-make-region (point) (point) aiern-visual-char)))
    (add-hook 'pre-command-hook #'aiern-visual-pre-command nil t)
    (add-hook 'post-command-hook #'aiern-visual-post-command nil t)
    (add-hook 'deactivate-mark-hook #'aiern-visual-deactivate-hook nil t))
   (t
    ;; Postpone deactivation of region if next state is Insert.
    ;; This gives certain insertion commands (auto-pairing characters,
    ;; for example) an opportunity to access the region.
    (if (and (eq aiern-next-state 'insert)
             (eq aiern-visual-selection 'char))
        (add-hook 'aiern-normal-state-entry-hook
                  #'aiern-visual-deactivate-hook nil t)
      (aiern-visual-deactivate-hook))
    (setq aiern-visual-region-expanded nil)
    (remove-hook 'pre-command-hook #'aiern-visual-pre-command t)
    (remove-hook 'post-command-hook #'aiern-visual-post-command t)
    (remove-hook 'deactivate-mark-hook #'aiern-visual-deactivate-hook t)
    (aiern-visual-highlight -1))))

(defun aiern-visual-pre-command (&optional command)
  "Run before each COMMAND in Visual state.
Expand the region to the selection unless COMMAND is a motion."
  (when (aiern-visual-state-p)
    (setq command (or command this-command))
    (when aiern-visual-x-select-timer
      (cancel-timer aiern-visual-x-select-timer))
    (unless (aiern-get-command-property command :keep-visual)
      (aiern-visual-update-x-selection)
      (aiern-visual-expand-region
       ;; exclude final newline from linewise selection
       ;; unless the command has real need of it
       (and (eq (aiern-visual-type) 'line)
            (aiern-get-command-property command :exclude-newline))))))

(put 'aiern-visual-pre-command 'permanent-local-hook t)

(defun aiern-visual-post-command (&optional command)
  "Run after each COMMAND in Visual state.
If COMMAND is a motion, refresh the selection;
otherwise exit Visual state."
  (when (aiern-visual-state-p)
    (setq command (or command this-command))
    (if (or quit-flag
            (eq command #'keyboard-quit)
            ;; Is `mark-active' nil for an unexpanded region?
            deactivate-mark
            (and (not aiern-visual-region-expanded)
                 (not (region-active-p))
                 (not (eq aiern-visual-selection 'block))))
        (progn
          (aiern-exit-visual-state)
          (aiern-adjust-cursor))
      (if aiern-visual-region-expanded
          (aiern-visual-contract-region)
        (aiern-visual-refresh))
      (setq aiern-visual-x-select-timer
            (run-with-idle-timer aiern-visual-x-select-timeout nil
                                 #'aiern-visual-update-x-selection
                                 (current-buffer)))
      (aiern-visual-highlight))))
(put 'aiern-visual-post-command 'permanent-local-hook t)

(defun aiern-visual-update-x-selection (&optional buffer)
  "Update the X selection with the current visual region."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (aiern-visual-state-p)
                   (display-selections-p)
                   (not (eq aiern-visual-selection 'block)))
          (aiern-set-selection 'PRIMARY (buffer-substring-no-properties
                                        aiern-visual-beginning
                                        aiern-visual-end)))))))

(defun aiern-visual-activate-hook (&optional _command)
  "Enable Visual state if the region is activated."
  (unless (aiern-visual-state-p)
    (aiern-delay nil
        ;; the activation may only be momentary, so re-check
        ;; in `post-command-hook' before entering Visual state
        '(unless (or (aiern-visual-state-p)
                     (aiern-insert-state-p)
                     (aiern-emacs-state-p))
           (when (and (region-active-p)
                      (not deactivate-mark))
             (aiern-visual-state)))
      'post-command-hook nil t
      "aiern-activate-visual-state")))
(put 'aiern-visual-activate-hook 'permanent-local-hook t)

(defun aiern-visual-deactivate-hook (&optional command)
  "Deactivate the region and restore Transient Mark mode."
  (setq command (or command this-command))
  (remove-hook 'deactivate-mark-hook
               #'aiern-visual-deactivate-hook t)
  (remove-hook 'aiern-normal-state-entry-hook
               #'aiern-visual-deactivate-hook t)
  (cond
   ((and (aiern-visual-state-p) command
         (not (aiern-get-command-property command :keep-visual)))
    (setq aiern-visual-region-expanded nil)
    (aiern-exit-visual-state))
   ((not (aiern-visual-state-p))
    (aiern-active-region -1)
    (aiern-restore-transient-mark-mode))))
(put 'aiern-visual-deactivate-hook 'permanent-local-hook t)

(aiern-define-command aiern-exit-visual-state (&optional later buffer)
  "Exit from Visual state to the previous state.
If LATER is non-nil, exit after the current command."
  :keep-visual t
  :repeat abort
  (with-current-buffer (or buffer (current-buffer))
    (when (aiern-visual-state-p)
      (if later
          (setq deactivate-mark t)
        (when aiern-visual-region-expanded
          (aiern-visual-contract-region))
        (aiern-change-to-previous-state)))))

(defun aiern-visual-tag (&optional selection)
  "Return a mode-line tag for SELECTION.
SELECTION is a kind of selection as defined by
`aiern-define-visual-selection', such as `char', `line'
or `block'."
  (setq selection (or selection aiern-visual-selection))
  (when selection
    (symbol-value (intern (format "aiern-visual-%s-tag" selection)))))

(defun aiern-visual-message (&optional selection)
  "Create an echo area message for SELECTION.
SELECTION is a kind of selection as defined by
`aiern-define-visual-selection', such as `char', `line'
or `block'."
  (let (message)
    (setq selection (or selection aiern-visual-selection))
    (when selection
      (setq message
            (symbol-value (intern (format "aiern-visual-%s-message"
                                          selection))))
      (cond
       ((functionp message)
        (funcall message))
       ((stringp message)
        (aiern-echo "%s" message))))))

(defun aiern-visual-select (beg end &optional type dir message)
  "Create a Visual selection of type TYPE from BEG to END.
Point and mark are positioned so that the resulting selection
has the specified boundaries. If DIR is negative, point precedes mark,
otherwise it succedes it. To specify point and mark directly,
use `aiern-visual-make-selection'."
  (let* ((range (aiern-contract beg end type))
         (mark (aiern-range-beginning range))
         (point (aiern-range-end range))
         (dir (or dir 1)))
    (when (< dir 0)
      (aiern-swap mark point))
    (aiern-visual-make-selection mark point type message)))

(defun aiern-visual-make-selection (mark point &optional type message)
  "Create a Visual selection with point at POINT and mark at MARK.
The boundaries of the selection are inferred from these
and the current TYPE. To specify the boundaries and infer
mark and point, use `aiern-visual-select' instead."
  (let* ((selection (aiern-visual-selection-for-type type))
         (func (aiern-visual-selection-function selection))
         (prev (and (aiern-visual-state-p) aiern-visual-selection))
         (mark (aiern-normalize-position mark))
         (point (aiern-normalize-position point))
         (state aiern-state))
    (unless (aiern-visual-state-p)
      (aiern-visual-state))
    (setq aiern-visual-selection selection)
    (funcall func mark point type
             ;; signal a message when changing the selection
             (when (or (not (aiern-visual-state-p state))
                       (not (eq selection prev)))
               message))))

(defun aiern-visual-make-region (mark point &optional type message)
  "Create an active region from MARK to POINT.
If TYPE is given, also set the Visual type.
If MESSAGE is given, display it in the echo area."
  (interactive)
  (let* ((point (aiern-normalize-position
                 (or point (point))))
         (mark (aiern-normalize-position
                (or mark
                    (when (or (aiern-visual-state-p)
                              (region-active-p))
                      (mark t))
                    point))))
    (unless (aiern-visual-state-p)
      (aiern-visual-state))
    (aiern-active-region 1)
    (setq aiern-visual-region-expanded nil)
    (aiern-visual-refresh mark point type)
    (cond
     ((null aiern-echo-state))
     ((stringp message)
      (aiern-echo "%s" message))
     (message
      (cond
       ((stringp aiern-visual-state-message)
        (aiern-echo "%s" aiern-visual-state-message))
       ((functionp aiern-visual-state-message)
        (funcall aiern-visual-state-message)))))))

(defun aiern-visual-expand-region (&optional exclude-newline)
  "Expand the region to the Visual selection.
If EXCLUDE-NEWLINE is non-nil and the selection ends with a newline,
exclude that newline from the region."
  (when (and (aiern-visual-state-p)
             (not aiern-visual-region-expanded))
    (let ((mark aiern-visual-beginning)
          (point aiern-visual-end))
      (when (< aiern-visual-direction 0)
        (aiern-swap mark point))
      (setq aiern-visual-region-expanded t)
      (aiern-visual-refresh mark point)
      (when (and exclude-newline
                 (save-excursion
                   (goto-char aiern-visual-end)
                   (and (bolp) (not (bobp)))))
        (if (< aiern-visual-direction 0)
            (aiern-move-mark (max point (1- (mark))))
          (goto-char (max mark (1- (point)))))))))

(defun aiern-visual-contract-region ()
  "The inverse of `aiern-visual-expand-region'.
Create a Visual selection that expands to the current region."
  (aiern-visual-refresh)
  (setq aiern-visual-region-expanded nil)
  (aiern-visual-refresh aiern-visual-mark aiern-visual-point))

(defun aiern-visual-refresh (&optional mark point type &rest properties)
  "Refresh point, mark and Visual variables.
Refreshes `aiern-visual-beginning', `aiern-visual-end',
`aiern-visual-mark', `aiern-visual-point', `aiern-visual-selection',
`aiern-visual-direction', `aiern-visual-properties' and `aiern-this-type'."
  (let* ((point (or point (point)))
         (mark (or mark (mark t) point))
         (dir (aiern-visual-direction))
         (type (or type (aiern-visual-type aiern-visual-selection)
                   (aiern-visual-type)))
         range)
    (aiern-move-mark mark)
    (goto-char point)
    (setq aiern-visual-beginning
          (or aiern-visual-beginning
              (let ((marker (make-marker)))
                (move-marker marker (min point mark))))
          aiern-visual-end
          (or aiern-visual-end
              (let ((marker (make-marker)))
                (set-marker-insertion-type marker t)
                (move-marker marker (max point mark))))
          aiern-visual-mark
          (or aiern-visual-mark
              (let ((marker (make-marker)))
                (move-marker marker mark)))
          aiern-visual-point
          (or aiern-visual-point
              (let ((marker (make-marker)))
                (move-marker marker point))))
    (setq aiern-visual-properties
          (aiern-concat-plists aiern-visual-properties properties))
    (cond
     (aiern-visual-region-expanded
      (setq type (or (aiern-visual-type) type))
      (move-marker aiern-visual-beginning (min point mark))
      (move-marker aiern-visual-end (max point mark))
      ;; if the type is one-to-one, we can safely refresh
      ;; the unexpanded positions as well
      (when (aiern-type-property type :one-to-one)
        (setq range (apply #'aiern-contract point mark type
                           aiern-visual-properties)
              mark (aiern-range-beginning range)
              point (aiern-range-end range))
        (when (< dir 0)
          (aiern-swap mark point))
        (move-marker aiern-visual-mark mark)
        (move-marker aiern-visual-point point)))
     (t
      (setq range (apply #'aiern-expand point mark type
                         aiern-visual-properties)
            type (aiern-type range type))
      (move-marker aiern-visual-beginning (aiern-range-beginning range))
      (move-marker aiern-visual-end (aiern-range-end range))
      (move-marker aiern-visual-mark mark)
      (move-marker aiern-visual-point point)))
    (setq aiern-visual-direction dir
          aiern-this-type type)))

(defun aiern-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on the Visual type.
With negative ARG, disable highlighting."
  (cond
   ((and (numberp arg) (< arg 1))
    (when aiern-visual-overlay
      (delete-overlay aiern-visual-overlay)
      (setq aiern-visual-overlay nil))
    (when aiern-visual-block-overlays
      (mapc #'delete-overlay aiern-visual-block-overlays)
      (setq aiern-visual-block-overlays nil)))
   ((eq aiern-visual-selection 'block)
    (when aiern-visual-overlay
      (aiern-visual-highlight -1))
    (aiern-visual-highlight-block
     aiern-visual-beginning
     aiern-visual-end))
   (t
    (when aiern-visual-block-overlays
      (aiern-visual-highlight -1))
    (if aiern-visual-overlay
        (move-overlay aiern-visual-overlay
                      aiern-visual-beginning aiern-visual-end)
      (setq aiern-visual-overlay
            (make-overlay aiern-visual-beginning aiern-visual-end)))
    (overlay-put aiern-visual-overlay 'face 'region)
    (overlay-put aiern-visual-overlay 'priority 99))))

(defun aiern-visual-highlight-block (beg end &optional overlays)
  "Highlight rectangular region from BEG to END.
Do this by putting an overlay on each line within the rectangle.
Each overlay extends across all the columns of the rectangle.
Reuse overlays where possible to prevent flicker."
  (let* ((point (point))
         (overlays (or overlays 'aiern-visual-block-overlays))
         (old (symbol-value overlays))
         (eol-col (and (memq this-command '(next-line previous-line))
                       (numberp temporary-goal-column)
                       (1+ (min (round temporary-goal-column)
                                (1- most-positive-fixnum)))))
         beg-col end-col new nlines overlay window-beg window-end)
    (save-excursion
      ;; calculate the rectangular region represented by BEG and END,
      ;; but put BEG in the upper-left corner and END in the
      ;; lower-right if not already there
      (setq beg-col (aiern-column beg)
            end-col (aiern-column end))
      (when (>= beg-col end-col)
        (if (= beg-col end-col)
            (setq end-col (1+ end-col))
          (aiern-sort beg-col end-col))
        (setq beg (save-excursion
                    (goto-char beg)
                    (aiern-move-to-column beg-col))
              end (save-excursion
                    (goto-char end)
                    (aiern-move-to-column end-col 1))))
      ;; update end column with eol-col (extension to eol).
      (when (and eol-col (> eol-col end-col))
        (setq end-col eol-col))
      ;; force a redisplay so we can do reliable window
      ;; BEG/END calculations
      (sit-for 0)
      (setq window-beg (max (window-start) beg)
            window-end (min (window-end) (1+ end))
            nlines (count-lines window-beg
                                (min window-end (point-max))))
      ;; iterate over those lines of the rectangle which are
      ;; visible in the currently selected window
      (goto-char window-beg)
      (dotimes (_ nlines)
        (let (before after row-beg row-end)
          ;; beginning of row
          (aiern-move-to-column beg-col)
          (when (< (current-column) beg-col)
            ;; prepend overlay with virtual spaces if unable to
            ;; move directly to the first column
            (setq before
                  (propertize
                   (make-string
                    (- beg-col (current-column)) ?\s)
                   'face
                   (or (get-text-property (1- (point)) 'face)
                       'default))))
          (setq row-beg (point))
          ;; end of row
          (aiern-move-to-column end-col)
          (when (and (not (eolp))
                     (< (current-column) end-col))
            ;; append overlay with virtual spaces if unable to
            ;; move directly to the last column
            (setq after
                  (propertize
                   (make-string
                    (if (= (point) row-beg)
                        (- end-col beg-col)
                      (- end-col (current-column)))
                    ?\s) 'face 'region))
            ;; place cursor on one of the virtual spaces
            (if (= point row-beg)
                (put-text-property
                 0 (min (length after) 1)
                 'cursor t after)
              (put-text-property
               (max 0 (1- (length after))) (length after)
               'cursor t after)))
          (setq row-end (min (point) (line-end-position)))
          ;; trim old leading overlays
          (while (and old
                      (setq overlay (car old))
                      (< (overlay-start overlay) row-beg)
                      (/= (overlay-end overlay) row-end))
            (delete-overlay overlay)
            (setq old (cdr old)))
          ;; reuse an overlay if possible, otherwise create one
          (cond
           ((and old (setq overlay (car old))
                 (or (= (overlay-start overlay) row-beg)
                     (= (overlay-end overlay) row-end)))
            (move-overlay overlay row-beg row-end)
            (overlay-put overlay 'before-string before)
            (overlay-put overlay 'after-string after)
            (setq new (cons overlay new)
                  old (cdr old)))
           (t
            (setq overlay (make-overlay row-beg row-end))
            (overlay-put overlay 'before-string before)
            (overlay-put overlay 'after-string after)
            (setq new (cons overlay new)))))
        (forward-line 1))
      ;; display overlays
      (dolist (overlay new)
        (overlay-put overlay 'face 'region)
        (overlay-put overlay 'priority 99))
      ;; trim old overlays
      (dolist (overlay old)
        (delete-overlay overlay))
      (set overlays (nreverse new)))))

(defun aiern-visual-range ()
  "Return the Visual selection as a range.
This is a list (BEG END TYPE PROPERTIES...), where BEG is the
beginning of the selection, END is the end of the selection,
TYPE is the selection's type, and PROPERTIES is a property list
of miscellaneous selection attributes."
  (apply #'aiern-range
         aiern-visual-beginning aiern-visual-end
         (aiern-visual-type)
         :expanded t
         aiern-visual-properties))

(defun aiern-visual-direction ()
  "Return direction of Visual selection.
The direction is -1 if point precedes mark and 1 otherwise.
See also the variable `aiern-visual-direction', which holds
the direction of the last selection."
  (let* ((point (point))
         (mark (or (mark t) point)))
    (if (< point mark) -1 1)))

(defun aiern-visual-type (&optional selection)
  "Return the type of the Visual selection.
If SELECTION is specified, return the type of that instead."
  (if (and (null selection) (aiern-visual-state-p))
      (or aiern-this-type (aiern-visual-type aiern-visual-selection))
    (setq selection (or selection aiern-visual-selection))
    (symbol-value (cdr-safe (assq selection aiern-visual-alist)))))

(defun aiern-visual-goto-end ()
  "Go to the last line of the Visual selection.
This position may differ from `aiern-visual-end' depending on
the selection type, and is contained in the selection."
  (let ((range (aiern-contract-range (aiern-visual-range))))
    (goto-char (aiern-range-end range))))

(defun aiern-visual-alist ()
  "Return an association list from types to selection symbols."
  (mapcar #'(lambda (e)
              (cons (symbol-value (cdr-safe e)) (cdr-safe e)))
          aiern-visual-alist))

(defun aiern-visual-selection-function (selection)
  "Return a selection function for TYPE.
Default to `aiern-visual-make-region'."
  (or (cdr-safe (assq selection aiern-visual-alist))
      ;; generic selection function
      'aiern-visual-make-region))

(defun aiern-visual-selection-for-type (type)
  "Return a Visual selection for TYPE."
  (catch 'done
    (dolist (selection aiern-visual-alist)
      (when (eq (symbol-value (cdr selection)) type)
        (throw 'done (car selection))))))

(defun aiern-visual-block-corner (&optional corner point mark)
  "Block corner corresponding to POINT, with MARK in opposite corner.
Depending on POINT and MARK, the return value is `upper-left',
`upper-right', `lower-left' or `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

One-column or one-row blocks are ambiguous. In such cases,
the horizontal or vertical component of CORNER is used.
CORNER defaults to `upper-left'."
  (let* ((point (or point (point)))
         (mark (or mark (mark t)))
         (corner (symbol-name
                  (or corner
                      (and (overlayp aiern-visual-overlay)
                           (overlay-get aiern-visual-overlay
                                        :corner))
                      'upper-left)))
         (point-col (aiern-column point))
         (mark-col (aiern-column mark))
         horizontal vertical)
    (cond
     ((= point-col mark-col)
      (setq horizontal
            (or (and (string-match "left\\|right" corner)
                     (match-string 0 corner))
                "left")))
     ((< point-col mark-col)
      (setq horizontal "left"))
     ((> point-col mark-col)
      (setq horizontal "right")))
    (cond
     ((= (line-number-at-pos point)
         (line-number-at-pos mark))
      (setq vertical
            (or (and (string-match "upper\\|lower" corner)
                     (match-string 0 corner))
                "upper")))
     ((< point mark)
      (setq vertical "upper"))
     ((> point mark)
      (setq vertical "lower")))
    (intern (format "%s-%s" vertical horizontal))))

;;; Operator-Pending state

(aiern-define-state operator
  "Operator-Pending state."
  :tag " <AO> "
  :cursor aiern-half-cursor
  :enable (aiern-operator-shortcut-map operator motion normal))

(aiern-define-keymap aiern-operator-shortcut-map
  "Keymap for Operator-Pending shortcuts like \"dd\" and \"gqq\"."
  :local t
  (setq aiern-operator-shortcut-map (make-sparse-keymap))
  (aiern-initialize-local-keymaps))

;; the half-height "Operator-Pending cursor" cannot be specified
;; as a static `cursor-type' value, since its height depends on
;; the current font size
(defun aiern-half-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (let ((height (/ (window-pixel-height) (* (window-height) 2))))
    (setq cursor-type (cons 'hbar height))))

;;; Replace state

(aiern-define-state replace
  "Replace state."
  :tag " <AR> "
  :cursor hbar
  :message "-- REPLACE --"
  :input-method t
  (cond
   ((aiern-replace-state-p)
    (overwrite-mode 1)
    (add-hook 'pre-command-hook #'aiern-replace-pre-command nil t)
    (unless (eq aiern-want-fine-undo t)
      (aiern-start-undo-step)))
   (t
    (overwrite-mode -1)
    (remove-hook 'pre-command-hook #'aiern-replace-pre-command t)
    (unless (eq aiern-want-fine-undo t)
      (aiern-end-undo-step))
    (aiern-move-cursor-back)))
  (setq aiern-replace-alist nil))

(defun aiern-replace-pre-command ()
  "Remember the character under point."
  (when (aiern-replace-state-p)
    (unless (assq (point) aiern-replace-alist)
      (add-to-list 'aiern-replace-alist
                   (cons (point)
                         (unless (eolp)
                           (char-after)))))))
(put 'aiern-replace-pre-command 'permanent-local-hook t)

(defun aiern-replace-backspace ()
  "Restore character under cursor."
  (interactive)
  (let (char)
    (backward-char)
    (when (assq (point) aiern-replace-alist)
      (setq char (cdr (assq (point) aiern-replace-alist)))
      (save-excursion
        (delete-char 1)
        (when char
          (insert char))))))

;;; Motion state

(aiern-define-state motion
  "Motion state."
  :tag " <AM> "
  :suppress-keymap t)

;;; Emacs state

(aiern-define-state emacs
  "Emacs state."
  :tag " <AE> "
  :message "-- EMACS --"
  :input-method t
  :intercept-esc nil)

(provide 'aiern-states)

;;; aiern-states.el ends here
