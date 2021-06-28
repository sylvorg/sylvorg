;;; aiern-commands.el --- aiern commands and operators -*- lexical-binding: t -*-
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
(require 'aiern-digraphs)
(require 'aiern-search)
(require 'aiern-ex)
(require 'aiern-types)
(require 'aiern-command-window)
(require 'aiern-jumps)
(require 'aiern-vars)
(require 'flyspell)
(require 'cl-lib)
(require 'reveal)

(declare-function imenu--in-alist "imenu")

;;; Motions

;; Movement commands, or motions, are defined with the macro
;; `aiern-define-motion'. A motion is a command with an optional
;; argument COUNT (interactively accessed by the code "<c>").
;; It may specify the :type command property (e.g., :type line),
;; which determines how it is handled by an operator command.
;; Furthermore, the command must have the command properties
;; :keep-visual t and :repeat motion; these are automatically
;; set by the `aiern-define-motion' macro.

;;; Code:

(aiern-define-motion aiern-forward-char (count &optional crosslines noerror)
  "Move cursor to the right by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the end
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list aiern-cross-lines
                           (aiern-kbd-macro-suppress-motion-error)))
  (cond
   (noerror
    (condition-case nil
        (aiern-forward-char count crosslines nil)
      (error nil)))
   ((not crosslines)
    ;; for efficiency, narrow the buffer to the projected
    ;; movement before determining the current line
    (aiern-with-restriction
        (point)
        (save-excursion
          (aiern-forward-char (1+ (or count 1)) t t)
          (point))
      (condition-case err
          (aiern-narrow-to-line
            (aiern-forward-char count t noerror))
        (error
         ;; Restore the previous command (this one never happend).
         ;; Actually, this preserves the current column if the
         ;; previous command was `aiern-next-line' or
         ;; `aiern-previous-line'.
         (setq this-command last-command)
         (signal (car err) (cdr err))))))
   (t
    (aiern-motion-loop (nil (or count 1))
      (forward-char)
      ;; don't put the cursor on a newline
      (when (and (not aiern-move-beyond-eol)
                 (not (aiern-visual-state-p))
                 (not (aiern-operator-state-p))
                 (eolp) (not (eobp)) (not (bolp)))
        (forward-char))))))

(aiern-define-motion aiern-backward-char (count &optional crosslines noerror)
  "Move cursor to the left by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the beginning
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list aiern-cross-lines
                           (aiern-kbd-macro-suppress-motion-error)))
  (cond
   (noerror
    (condition-case nil
        (aiern-backward-char count crosslines nil)
      (error nil)))
   ((not crosslines)
    ;; restrict movement to the current line
    (aiern-with-restriction
        (save-excursion
          (aiern-backward-char (1+ (or count 1)) t t)
          (point))
        (1+ (point))
      (condition-case err
          (aiern-narrow-to-line
            (aiern-backward-char count t noerror))
        (error
         ;; Restore the previous command (this one never happened).
         ;; Actually, this preserves the current column if the
         ;; previous command was `aiern-next-line' or
         ;; `aiern-previous-line'.
         (setq this-command last-command)
         (signal (car err) (cdr err))))))
   (t
    (aiern-motion-loop (nil (or count 1))
      (backward-char)
      ;; don't put the cursor on a newline
      (unless (or (aiern-visual-state-p) (aiern-operator-state-p))
        (aiern-adjust-cursor))))))

(aiern-define-motion aiern-next-line (count)
  "Move the cursor COUNT lines down."
  :type line
  (let (line-move-visual)
    (aiern-line-move (or count 1))))

(aiern-define-motion aiern-previous-line (count)
  "Move the cursor COUNT lines up."
  :type line
  (let (line-move-visual)
    (aiern-line-move (- (or count 1)))))

(aiern-define-motion aiern-next-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (aiern-line-move (or count 1))))

(aiern-define-motion aiern-previous-visual-line (count)
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (aiern-line-move (- (or count 1)))))

;; used for repeated commands like "dd"
(aiern-define-motion aiern-line (count)
  "Move COUNT - 1 lines down."
  :type line
  (let (line-move-visual)
    ;; Catch bob and eob errors. These are caused when not moving
    ;; point starting in the first or last line, respectively. In this
    ;; case the current line should be selected.
    (condition-case _err
        (aiern-line-move (1- (or count 1)))
      ((beginning-of-buffer end-of-buffer)))))

(aiern-define-motion aiern-line-or-visual-line (count)
  "Move COUNT - 1 lines down."
  :type screen-line
  (let ((line-move-visual (and aiern-respect-visual-line-mode
                               visual-line-mode)))
    ;; Catch bob and eob errors. These are caused when not moving
    ;; point starting in the first or last line, respectively. In this
    ;; case the current line should be selected.
    (condition-case _err
        (aiern-line-move (1- (or count 1)))
      ((beginning-of-buffer end-of-buffer)))))

(aiern-define-motion aiern-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (move-beginning-of-line nil))

(aiern-define-motion aiern-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (move-end-of-line count)
  (when aiern-track-eol
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-line))
  (unless (aiern-visual-state-p)
    (aiern-adjust-cursor)
    (when (eolp)
      ;; prevent "c$" and "d$" from deleting blank lines
      (setq aiern-this-type 'exclusive))))

(aiern-define-motion aiern-beginning-of-visual-line ()
  "Move the cursor to the first character of the current screen line."
  :type exclusive
  (if (fboundp 'beginning-of-visual-line)
      (beginning-of-visual-line)
    (beginning-of-line)))

(aiern-define-motion aiern-end-of-visual-line (count)
  "Move the cursor to the last character of the current screen line.
If COUNT is given, move COUNT - 1 screen lines downward first."
  :type inclusive
  (if (fboundp 'end-of-visual-line)
      (end-of-visual-line count)
    (end-of-line count)))

(aiern-define-motion aiern-end-of-line-or-visual-line (count)
  "Move the cursor to the last character of the current screen
line if `visual-line-mode' is active and
`aiern-respect-visual-line-mode' is non-nil.  If COUNT is given,
move COUNT - 1 screen lines downward first."
  :type inclusive
  (if (and (fboundp 'end-of-visual-line)
           aiern-respect-visual-line-mode
           visual-line-mode)
      (end-of-visual-line count)
    (aiern-end-of-line count)))

(aiern-define-motion aiern-middle-of-visual-line ()
  "Move the cursor to the middle of the current visual line."
  :type exclusive
  (beginning-of-visual-line)
  (aiern-with-restriction
      nil
      (save-excursion (end-of-visual-line) (point))
    (move-to-column (+ (current-column)
                       -1
                       (/ (with-no-warnings (window-body-width)) 2)))))

(aiern-define-motion aiern-beginning-of-line-or-digit-argument ()
  "Move the cursor to the beginning of the current line.
This function passes its command to `digit-argument' (usually a 0)
if it is not the first event."
  :type exclusive
  (cond
   (current-prefix-arg
    (setq this-command #'digit-argument)
    (call-interactively #'digit-argument))
   (t
    (setq this-command #'aiern-beginning-of-line)
    (call-interactively #'aiern-beginning-of-line))))

(aiern-define-motion aiern-first-non-blank ()
  "Move the cursor to the first non-blank character of the current line."
  :type exclusive
  (aiern-narrow-to-line (back-to-indentation)))

(aiern-define-motion aiern-last-non-blank (count)
  "Move the cursor to the last non-blank character of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (goto-char
   (save-excursion
     (aiern-move-beginning-of-line count)
     (if (re-search-forward "[ \t]*$")
         (max (line-beginning-position)
              (1- (match-beginning 0)))
       (line-beginning-position)))))

(aiern-define-motion aiern-first-non-blank-of-visual-line ()
  "Move the cursor to the first non blank character
of the current screen line."
  :type exclusive
  (aiern-beginning-of-visual-line)
  (skip-chars-forward " \t\r"))

(aiern-define-motion aiern-next-line-first-non-blank (count)
  "Move the cursor COUNT lines down on the first non-blank character."
  :type line
  (let ((this-command this-command))
    (aiern-next-line (or count 1)))
  (aiern-first-non-blank))

(aiern-define-motion aiern-next-line-1-first-non-blank (count)
  "Move the cursor COUNT-1 lines down on the first non-blank character."
  :type line
  (let ((this-command this-command))
    (aiern-next-line (1- (or count 1))))
  (aiern-first-non-blank))

(aiern-define-motion aiern-previous-line-first-non-blank (count)
  "Move the cursor COUNT lines up on the first non-blank character."
  :type line
  (let ((this-command this-command))
    (aiern-previous-line (or count 1)))
  (aiern-first-non-blank))

(aiern-define-motion aiern-goto-line (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (if (null count)
      (with-no-warnings (end-of-buffer))
    (goto-char (point-min))
    (forward-line (1- count)))
  (aiern-first-non-blank))

(aiern-define-motion aiern-goto-first-line (count)
  "Go to the first non-blank character of line COUNT.
By default the first line."
  :jump t
  :type line
  (aiern-goto-line (or count 1)))

(aiern-define-motion aiern-forward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS.

If this command is called in operator-pending state it behaves
differently. If point reaches the beginning of a word on a new
line point is moved back to the end of the previous line.

If called after a change operator, i.e. cw or cW,
`aiern-want-change-word-to-end' is non-nil and point is on a word,
then both behave like ce or cE.

If point is at the end of the buffer and cannot be moved signal
'end-of-buffer is raised.
"
  :type exclusive
  (let ((thing (if bigword 'aiern-WORD 'aiern-word))
        (orig (point))
        (count (or count 1)))
    (aiern-signal-at-bob-or-eob count)
    (cond
     ;; default motion, beginning of next word
     ((not (aiern-operator-state-p))
      (aiern-forward-beginning thing count))
     ;; the aiern-change operator, maybe behave like ce or cE
     ((and aiern-want-change-word-to-end
           (memq aiern-this-operator aiern-change-commands)
           (< orig (or (cdr-safe (bounds-of-thing-at-point thing)) orig)))
      ;; forward-thing moves point to the correct position because
      ;; this is an exclusive motion
      (forward-thing thing count))
     ;; operator state
     (t
      (prog1 (aiern-forward-beginning thing count)
        ;; if we reached the beginning of a word on a new line in
        ;; Operator-Pending state, go back to the end of the previous
        ;; line
        (when (and (> (line-beginning-position) orig)
                   (looking-back "^[[:space:]]*" (line-beginning-position)))
          ;; move cursor back as long as the line contains only
          ;; whitespaces and is non-empty
          (aiern-move-end-of-line 0)
          ;; skip non-empty lines containing only spaces
          (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                      (not (<= (line-beginning-position) orig)))
            (aiern-move-end-of-line 0))
          ;; but if the previous line is empty, delete this line
          (when (bolp) (forward-char))))))))

(aiern-define-motion aiern-forward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'aiern-WORD 'aiern-word))
        (count (or count 1)))
    (aiern-signal-at-bob-or-eob count)
    ;; aiern special behaviour: e or E on a one-character word in
    ;; operator state does not move point
    (unless (and (aiern-operator-state-p)
                 (= 1 count)
                 (let ((bnd (bounds-of-thing-at-point thing)))
                   (and bnd
                        (= (car bnd) (point))
                        (= (cdr bnd) (1+ (point)))))
                 (looking-at "[[:word:]]"))
      (aiern-forward-end thing count))))

(aiern-define-motion aiern-backward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (let ((thing (if bigword 'aiern-WORD 'aiern-word)))
    (aiern-signal-at-bob-or-eob (- (or count 1)))
    (aiern-backward-beginning thing count)))

(aiern-define-motion aiern-backward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'aiern-WORD 'aiern-word)))
    (aiern-signal-at-bob-or-eob (- (or count 1)))
    (aiern-backward-end thing count)))

(aiern-define-motion aiern-forward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th next WORD."
  :type exclusive
  (aiern-forward-word-begin count t))

(aiern-define-motion aiern-forward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (aiern-forward-word-end count t))

(aiern-define-motion aiern-backward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous WORD."
  :type exclusive
  (aiern-backward-word-begin count t))

(aiern-define-motion aiern-backward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th previous WORD."
  :type inclusive
  (aiern-backward-word-end count t))

;; section movement
(aiern-define-motion aiern-forward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th next section."
  :jump t
  :type exclusive
  (aiern-signal-at-bob-or-eob count)
  (aiern-forward-beginning 'aiern-defun count))

(aiern-define-motion aiern-forward-section-end (count)
  "Move the cursor to the end of the COUNT-th next section."
  :jump t
  :type inclusive
  (aiern-signal-at-bob-or-eob count)
  (aiern-forward-end 'aiern-defun count)
  (unless (eobp) (forward-line)))

(aiern-define-motion aiern-backward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous section."
  :jump t
  :type exclusive
  (aiern-signal-at-bob-or-eob (- (or count 1)))
  (aiern-backward-beginning 'aiern-defun count))

(aiern-define-motion aiern-backward-section-end (count)
  "Move the cursor to the end of the COUNT-th previous section."
  :jump t
  :type inclusive
  (aiern-signal-at-bob-or-eob (- (or count 1)))
  (end-of-line -1)
  (aiern-backward-end 'aiern-defun count)
  (unless (eobp) (forward-line)))

(aiern-define-motion aiern-forward-sentence-begin (count)
  "Move to the next COUNT-th beginning of a sentence or end of a paragraph."
  :jump t
  :type exclusive
  (aiern-signal-at-bob-or-eob count)
  (aiern-forward-nearest count
                        #'(lambda (_cnt)
                            (aiern-forward-beginning 'aiern-sentence))
                        #'aiern-forward-paragraph))

(aiern-define-motion aiern-backward-sentence-begin (count)
  "Move to the previous COUNT-th beginning of a sentence or paragraph."
  :jump t
  :type exclusive
  (aiern-signal-at-bob-or-eob (- (or count 1)))
  (aiern-forward-nearest (- (or count 1))
                        #'(lambda (_cnt)
                            (aiern-backward-beginning 'aiern-sentence))
                        #'(lambda (_cnt)
                            (aiern-backward-paragraph))))

(aiern-define-motion aiern-forward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (aiern-signal-at-bob-or-eob count)
  (aiern-forward-end 'aiern-paragraph count)
  (unless (eobp) (forward-line)))

(aiern-define-motion aiern-backward-paragraph (count)
  "Move to the beginning of the COUNT-th previous paragraph."
  :jump t
  :type exclusive
  (aiern-signal-at-bob-or-eob (- (or count 1)))
  (unless (eobp) (forward-line))
  (aiern-backward-beginning 'aiern-paragraph count)
  (unless (bobp) (forward-line -1)))

(defvar hif-ifx-else-endif-regexp)
(aiern-define-motion aiern-jump-item (count)
  "Find the next item in this line after or under the cursor
and jump to the corresponding one."
  :jump t
  :type inclusive
  (cond
   ;; COUNT% jumps to a line COUNT percentage down the file
   (count
    (goto-char
     (aiern-normalize-position
      (let ((size (- (point-max) (point-min))))
        (+ (point-min)
           (if (> size 80000)
               (* count (/ size 100))
             (/ (* count size) 100))))))
    (back-to-indentation)
    (setq aiern-this-type 'line))
   ((and (aiern-looking-at-start-comment t)
         (let ((pnt (point)))
           (forward-comment 1)
           (or (not (bolp))
               (prog1 nil (goto-char pnt)))))
    (backward-char))
   ((and (not (eolp)) (aiern-looking-at-end-comment t))
    (forward-comment -1))
   ((and
     (memq major-mode '(c-mode c++-mode))
     (require 'hideif nil t)
     (with-no-warnings
       (let* ((hif-else-regexp (concat hif-cpp-prefix "\\(?:else\\|elif[ \t]+\\)"))
              (hif-ifx-else-endif-regexp
               (concat hif-ifx-regexp "\\|" hif-else-regexp "\\|" hif-endif-regexp)))
         (cond
          ((save-excursion (beginning-of-line) (or (hif-looking-at-ifX) (hif-looking-at-else)))
           (hif-find-next-relevant)
           (while (hif-looking-at-ifX)
             (hif-ifdef-to-endif)
             (hif-find-next-relevant))
           t)
          ((save-excursion (beginning-of-line) (hif-looking-at-endif))
           (hif-endif-to-ifdef)
           t))))))
   (t
    (let* ((open (point-max))
           (close (point-max))
           (open-pair (condition-case nil
                          (save-excursion
                            ;; consider the character right before eol given that
                            ;; point may be placed there, e.g. in visual state
                            (when (and (eolp) (not (bolp)))
                              (backward-char))
                            (setq open (1- (scan-lists (point) 1 -1)))
                            (when (< open (line-end-position))
                              (goto-char open)
                              (forward-list)
                              (1- (point))))
                        (error nil)))
           (close-pair (condition-case nil
                           (save-excursion
                             ;; consider the character right before eol given that
                             ;; point may be placed there, e.g. in visual state
                             (when (and (eolp) (not (bolp)))
                               (backward-char))
                             (setq close (1- (scan-lists (point) 1 1)))
                             (when (< close (line-end-position))
                               (goto-char (1+ close))
                               (backward-list)
                               (point)))
                         (error nil))))
      (cond
       ((not (or open-pair close-pair))
        ;; nothing found, check if we are inside a string
        (let ((pnt (point))
              (bnd (bounds-of-thing-at-point 'aiern-string)))
          (if (not (and bnd (< (point) (cdr bnd))))
              ;; no, then we really failed
              (user-error "No matching item found on the current line")
            ;; yes, go to the end of the string and try again
            (let ((endstr (cdr bnd)))
              (when (or (save-excursion
                          (goto-char endstr)
                          (let ((b (bounds-of-thing-at-point 'aiern-string)))
                            (and b (< (point) (cdr b))))) ; not at end of string
                        (condition-case nil
                            (progn
                              (goto-char endstr)
                              (aiern-jump-item)
                              nil)
                          (error t)))
                ;; failed again, go back to original point
                (goto-char pnt)
                (user-error "No matching item found on the current line"))))))
       ((< open close) (goto-char open-pair))
       (t (goto-char close-pair)))))))

(defun aiern--flyspell-overlays-in-p (beg end)
  (let ((ovs (overlays-in beg end))
        done)
    (while (and ovs (not done))
      (when (flyspell-overlay-p (car ovs))
        (setq done t))
      (setq ovs (cdr ovs)))
    done))

(defun aiern--flyspell-overlay-at (pos forwardp)
  (when (not forwardp)
    (setq pos (max (1- pos) (point-min))))
  (let ((ovs (overlays-at pos))
        done)
    (while (and ovs (not done))
      (if (flyspell-overlay-p (car ovs))
          (setq done t)
        (setq ovs (cdr ovs))))
    (when done
      (car ovs))))

(defun aiern--flyspell-overlay-after (pos limit forwardp)
  (let (done)
    (while (and (if forwardp
                    (< pos limit)
                  (> pos limit))
                (not done))
      (let ((ov (aiern--flyspell-overlay-at pos forwardp)))
        (when ov
          (setq done ov)))
      (setq pos (if forwardp
                    (next-overlay-change pos)
                  (previous-overlay-change pos))))
    done))

(defun aiern--next-flyspell-error (forwardp)
  (when (aiern--flyspell-overlays-in-p (point-min) (point-max))
    (let ((pos (point))
          limit
          ov)
      (when (aiern--flyspell-overlay-at pos forwardp)
        (if (/= pos (point-min))
            (setq pos (save-excursion (goto-char pos)
                                      (forward-word (if forwardp 1 -1))
                                      (point)))
          (setq pos (point-max))))
      (setq limit (if forwardp (point-max) (point-min))
            ov (aiern--flyspell-overlay-after pos limit forwardp))
      (if ov
          (goto-char (overlay-start ov))
        (when aiern-search-wrap
          (setq limit pos
                pos (if forwardp (point-min) (point-max))
                ov (aiern--flyspell-overlay-after pos limit forwardp))
          (when ov
            (goto-char (overlay-start ov))))))))

(aiern-define-motion aiern-next-flyspell-error (count)
  "Go to the COUNT'th spelling mistake after point."
  (interactive "p")
  (dotimes (_ count)
    (aiern--next-flyspell-error t)))

(aiern-define-motion aiern-prev-flyspell-error (count)
  "Go to the COUNT'th spelling mistake preceding point."
  (interactive "p")
  (dotimes (_ count)
    (aiern--next-flyspell-error nil)))

(aiern-define-motion aiern-previous-open-paren (count)
  "Go to [count] previous unmatched '('."
  :type exclusive
  (aiern-up-paren ?\( ?\) (- (or count 1))))

(aiern-define-motion aiern-next-close-paren (count)
  "Go to [count] next unmatched ')'."
  :type exclusive
  (forward-char)
  (aiern-up-paren ?\( ?\) (or count 1))
  (backward-char))

(aiern-define-motion aiern-previous-open-brace (count)
  "Go to [count] previous unmatched '{'."
  :type exclusive
  (aiern-up-paren ?{ ?} (- (or count 1))))

(aiern-define-motion aiern-next-close-brace (count)
  "Go to [count] next unmatched '}'."
  :type exclusive
  (forward-char)
  (aiern-up-paren ?{ ?} (or count 1))
  (backward-char))

(defun aiern--lowercase-markers ()
  "Get all lowercase markers."
  (cl-remove-if-not (lambda (x) (and (markerp (cdr x))
                                     (<= ?a (car x) ?z)))
                    aiern-markers-alist))

(defun aiern--next-mark (forwardp)
  "Move to next lowercase mark.
Move forward if FORWARDP is truthy or backward if falsey.
Loop back to the top of buffer if the end is reached."
  (let ((pos (point))
        (sorted-markers (sort (aiern--lowercase-markers)
                              (lambda (a b) (< (cdr a) (cdr b))))))
    (cond
     ((null sorted-markers)
      (user-error "No marks in this buffer"))
     (forwardp
      (let ((next-marker (cl-some (lambda (x) (and (< pos (cdr x)) (cdr x)))
                                  sorted-markers)))
        (if next-marker
            (goto-char (marker-position next-marker))
          (goto-char (marker-position (cdar sorted-markers))))))
     (t
      (let* ((descending-markers (reverse sorted-markers))
             (prev-marker (cl-some (lambda (x) (and (> pos (cdr x)) (cdr x)))
                                   descending-markers)))
        (if prev-marker
            (goto-char (marker-position prev-marker))
          (goto-char (marker-position (cdar descending-markers)))))))))

(aiern-define-motion aiern-next-mark (count)
  "Go to [count] next lowercase mark."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (dotimes (_ (or count 1))
    (aiern--next-mark t)))

(aiern-define-motion aiern-next-mark-line (count)
  "Go to [count] line of next lowercase mark after current line."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (if (aiern--lowercase-markers)
      (dotimes (_ (or count 1))
        (aiern-end-of-line)
        (aiern--next-mark t)
        (aiern-first-non-blank))
    (user-error "No marks in this buffer")))

(aiern-define-motion aiern-previous-mark (count)
  "Go to [count] previous lowercase mark."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (dotimes (_ (or count 1))
    (aiern--next-mark nil)))

(aiern-define-motion aiern-previous-mark-line (count)
  "Go to [count] line of previous lowercase mark before current line."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (if (aiern--lowercase-markers)
      (dotimes (_ (or count 1))
        (aiern-beginning-of-line)
        (aiern--next-mark nil)
        (aiern-first-non-blank))
    (user-error "No marks in this buffer")))

(aiern-define-command aiern-set-col-0-mark (_beg end mark)
  "Set MARK at column 0 of line of END. Default is cursor line."
  (interactive "<r><a>")
  (if (< 1 (length mark))
      (user-error "Trailing characters")
    (save-excursion
      (goto-char (if (eobp) end (1- end)))
      (aiern-beginning-of-line)
      (aiern-set-marker (string-to-char mark)))))

(aiern-define-motion aiern-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `aiern-cross-lines' is non-nil."
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (visual (and aiern-respect-visual-line-mode
                     visual-line-mode)))
    (setq aiern-last-find (list #'aiern-find-char char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless (prog1
                  (search-forward (char-to-string char)
                                  (cond (aiern-cross-lines
                                         nil)
                                        ((and fwd visual)
                                         (save-excursion
                                           (end-of-visual-line)
                                           (point)))
                                        (fwd
                                         (line-end-position))
                                        (visual
                                         (save-excursion
                                           (beginning-of-visual-line)
                                           (point)))
                                        (t
                                         (line-beginning-position)))
                                  t count)
                (when fwd (backward-char)))
        (user-error "Can't find %c" char)))))

(aiern-define-motion aiern-find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (aiern-find-char (- (or count 1)) char))

(aiern-define-motion aiern-find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (unwind-protect
      (progn
        (aiern-find-char count char)
        (if (> (or count 1) 0)
            (backward-char)
          (forward-char)))
    (setcar aiern-last-find #'aiern-find-char-to)))

(aiern-define-motion aiern-find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (aiern-find-char-to (- (or count 1)) char))

(aiern-define-motion aiern-repeat-find-char (count)
  "Repeat the last find COUNT times."
  :type inclusive
  (setq count (or count 1))
  (if aiern-last-find
      (let ((cmd (car aiern-last-find))
            (char (nth 1 aiern-last-find))
            (fwd (nth 2 aiern-last-find))
            aiern-last-find)
        ;; ensure count is non-negative
        (when (< count 0)
          (setq count (- count)
                fwd (not fwd)))
        ;; skip next character when repeating t or T
        (and (eq cmd #'aiern-find-char-to)
             aiern-repeat-find-to-skip-next
             (= count 1)
             (or (and fwd (= (char-after (1+ (point))) char))
                 (and (not fwd) (= (char-before) char)))
             (setq count (1+ count)))
        (funcall cmd (if fwd count (- count)) char)
        (unless (nth 2 aiern-last-find)
          (setq aiern-this-type 'exclusive)))
    (user-error "No previous search")))

(aiern-define-motion aiern-repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :type inclusive
  (aiern-repeat-find-char (- (or count 1))))

;; ceci n'est pas une pipe
(aiern-define-motion aiern-goto-column (count)
  "Go to column COUNT on the current line.
Columns are counted from zero."
  :type exclusive
  (move-to-column (or count 0)))

(aiern-define-command aiern-goto-mark (char &optional noerror)
  "Go to the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (interactive (list (read-char)))
  (let ((marker (aiern-get-marker char)))
    (cond
     ((markerp marker)
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker)))
     ((numberp marker)
      (goto-char marker))
     ((consp marker)
      (when (or (find-buffer-visiting (car marker))
                (and (y-or-n-p (format "Visit file %s again? "
                                       (car marker)))
                     (find-file (car marker))))
        (goto-char (cdr marker))))
     ((not noerror)
      (user-error "Marker `%c' is not set%s" char
                  (if (aiern-global-marker-p char) ""
                    " in this buffer"))))))

(aiern-define-command aiern-goto-mark-line (char &optional noerror)
  "Go to the line of the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type line
  :jump t
  (interactive (list (read-char)))
  (aiern-goto-mark char noerror)
  (aiern-first-non-blank))

(aiern-define-motion aiern-jump-backward (count)
  "Go to older position in jump list.
To go the other way, press \
\\<aiern-motion-state-map>\\[aiern-jump-forward]."
  (aiern--jump-backward count))

(aiern-define-motion aiern-jump-forward (count)
  "Go to newer position in jump list.
To go the other way, press \
\\<aiern-motion-state-map>\\[aiern-jump-backward]."
  (aiern--jump-forward count))

(aiern-define-motion aiern-jump-backward-swap (count)
  "Go to the previous position in jump list.
The current position is placed in the jump list."
  (let ((pnt (point)))
    (aiern--jump-backward 1)
    (aiern-set-jump pnt)))

(defvar xref-prompt-for-identifier)
(aiern-define-motion aiern-jump-to-tag (arg)
  "Jump to tag under point.
If called with a prefix argument, provide a prompt
for specifying the tag."
  :jump t
  (interactive "P")
  (cond
   ((fboundp 'xref-find-definitions)
    (let ((xref-prompt-for-identifier arg))
      (call-interactively #'xref-find-definitions)))
   ((fboundp 'find-tag)
    (if arg (call-interactively #'find-tag)
      (let ((tag (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              #'find-tag-default))))
        (unless tag (user-error "No tag candidate found around point"))
        (find-tag tag))))))

(aiern-define-motion aiern-lookup ()
  "Look up the keyword at point.
Calls `aiern-lookup-func'."
  (funcall aiern-lookup-func))

(defun aiern-ret-gen (count indent?)
  (let* ((field  (get-char-property (point) 'field))
         (button (get-char-property (point) 'button))
         (doc    (get-char-property (point) 'widget-doc))
         (widget (or field button doc)))
    (cond
     ((and widget
           (fboundp 'widget-type)
           (fboundp 'widget-button-press)
           (or (and (symbolp widget)
                    (get widget 'widget-type))
               (and (consp widget)
                    (get (widget-type widget) 'widget-type))))
      (when (aiern-operator-state-p)
        (setq aiern-inhibit-operator t))
      (when (fboundp 'widget-button-press)
        (widget-button-press (point))))
     ((and (fboundp 'button-at)
           (fboundp 'push-button)
           (button-at (point)))
      (when (aiern-operator-state-p)
        (setq aiern-inhibit-operator t))
      (push-button))
     ((or (aiern-emacs-state-p)
          (and (aiern-insert-state-p)
               (not buffer-read-only)))
      (if (not indent?)
          (newline count)
        (delete-horizontal-space t)
        (newline count)
        (indent-according-to-mode)))
     (t
      (aiern-next-line-first-non-blank count)))))

(aiern-define-motion aiern-ret (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline."
  :type line
  (aiern-ret-gen count nil))

(aiern-define-motion aiern-ret-and-indent (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline and indent."
  :type line
  (aiern-ret-gen count t))

(aiern-define-motion aiern-window-top (count)
  "Move the cursor to line COUNT from the top of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (max (or count 0)
                            (if (= (point-min) (window-start))
                                0
                              scroll-margin)))
  (back-to-indentation))

(aiern-define-motion aiern-window-middle ()
  "Move the cursor to the middle line in the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line
   (/ (1+ (save-excursion (move-to-window-line -1))) 2))
  (back-to-indentation))

(aiern-define-motion aiern-window-bottom (count)
  "Move the cursor to line COUNT from the bottom of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (- (max (or count 1) (1+ scroll-margin))))
  (back-to-indentation))

;; scrolling
(aiern-define-command aiern-scroll-line-up (count)
  "Scrolls the window COUNT lines upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-down count)))

(aiern-define-command aiern-scroll-line-down (count)
  "Scrolls the window COUNT lines downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-up count)))

(aiern-define-command aiern-scroll-count-reset ()
  "Sets `aiern-scroll-count' to 0.
`aiern-scroll-up' and `aiern-scroll-down' will scroll
for a half of the screen(default)."
  :repeat nil
  :keep-visual t
  (interactive)
  (setq aiern-scroll-count 0))

(aiern-define-command aiern-scroll-up (count)
  "Scrolls the window and the cursor COUNT lines upwards.
If COUNT is not specified the function scrolls down
`aiern-scroll-count', which is the last used count.
If the scroll count is zero the command scrolls half the screen."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (aiern-save-column
    (setq count (or count (max 0 aiern-scroll-count)))
    (setq aiern-scroll-count count)
    (when (= (point-min) (line-beginning-position))
      (signal 'beginning-of-buffer nil))
    (when (zerop count)
      (setq count (/ (window-body-height) 2)))
    (let ((xy (aiern-posn-x-y (posn-at-point))))
      (condition-case nil
          (progn
            (scroll-down count)
            (goto-char (posn-point (posn-at-x-y (car xy) (cdr xy)))))
        (beginning-of-buffer
         (condition-case nil
             (with-no-warnings (previous-line count))
           (beginning-of-buffer)))))))

(aiern-define-command aiern-scroll-down (count)
  "Scrolls the window and the cursor COUNT lines downwards.
If COUNT is not specified the function scrolls down
`aiern-scroll-count', which is the last used count.
If the scroll count is zero the command scrolls half the screen."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (aiern-save-column
    (setq count (or count (max 0 aiern-scroll-count)))
    (setq aiern-scroll-count count)
    (when (eobp) (signal 'end-of-buffer nil))
    (when (zerop count)
      (setq count (/ (window-body-height) 2)))
    ;; BUG #660: First check whether the eob is visible.
    ;; In that case we do not scroll but merely move point.
    (if (<= (point-max) (window-end))
        (with-no-warnings (next-line count nil))
      (let ((xy (aiern-posn-x-y (posn-at-point))))
        (condition-case nil
            (progn
              (scroll-up count)
              (let* ((wend (window-end nil t))
                     (p (posn-at-x-y (car xy) (cdr xy)))
                     (margin (max 0 (- scroll-margin
                                       (cdr (posn-col-row p))))))
                (goto-char (posn-point p))
                ;; ensure point is not within the scroll-margin
                (when (> margin 0)
                  (with-no-warnings (next-line margin))
                  (recenter scroll-margin))
                (when (<= (point-max) wend)
                  (save-excursion
                    (goto-char (point-max))
                    (recenter (- (max 1 scroll-margin)))))))
          (end-of-buffer
           (goto-char (point-max))
           (recenter (- (max 1 scroll-margin)))))))))

(aiern-define-command aiern-scroll-page-up (count)
  "Scrolls the window COUNT pages upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (aiern-save-column
    (dotimes (i count)
      (condition-case err
          (scroll-down nil)
        (beginning-of-buffer
         (if (and (bobp) (zerop i))
             (signal (car err) (cdr err))
           (goto-char (point-min))))))))

(aiern-define-command aiern-scroll-page-down (count)
  "Scrolls the window COUNT pages downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (aiern-save-column
    (dotimes (i count)
      (condition-case err
          (scroll-up nil)
        (end-of-buffer
         (if (and (eobp) (zerop i))
             (signal (car err) (cdr err))
           (goto-char (point-max))))))))

(aiern-define-command aiern-scroll-line-to-top (count)
  "Scrolls line number COUNT (or the cursor line) to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (aiern-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter (1- (max 1 scroll-margin)))))

(aiern-define-command aiern-scroll-line-to-center (count)
  "Scrolls line number COUNT (or the cursor line) to the center of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (aiern-save-column
    (when count
      (goto-char (point-min))
      (forward-line (1- count)))
    (recenter nil)))

(aiern-define-command aiern-scroll-line-to-bottom (count)
  "Scrolls line number COUNT (or the cursor line) to the bottom of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (aiern-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter (- (max 1 scroll-margin)))))

(aiern-define-command aiern-scroll-bottom-line-to-top (count)
  "Scrolls the line right below the window,
or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-end))
    (aiern-move-cursor-back))
  (recenter (1- (max 0 scroll-margin)))
  (aiern-first-non-blank))

(aiern-define-command aiern-scroll-top-line-to-bottom (count)
  "Scrolls the line right below the window,
or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-start)))
  (recenter (- (max 1 scroll-margin)))
  (aiern-first-non-blank))

(aiern-define-command aiern-scroll-left (count)
  "Scrolls the window COUNT half-screenwidths to the left."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (aiern-with-hproject-point-on-window
    (scroll-right (* count (/ (window-width) 2)))))

(aiern-define-command aiern-scroll-right (count)
  "Scrolls the window COUNT half-screenwidths to the right."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (aiern-with-hproject-point-on-window
    (scroll-left (* count (/ (window-width) 2)))))

(aiern-define-command aiern-scroll-column-left (count)
  "Scrolls the window COUNT columns to the left."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (aiern-with-hproject-point-on-window
    (scroll-right count)))

(aiern-define-command aiern-scroll-column-right (count)
  "Scrolls the window COUNT columns to the right."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (aiern-with-hproject-point-on-window
    (scroll-left count)))

;;; Text objects

;; Text objects are defined with `aiern-define-text-object'. In Visual
;; state, they modify the current selection; in Operator-Pending
;; state, they return a pair of buffer positions. Outer text objects
;; are bound in the keymap `aiern-outer-text-objects-map', and inner
;; text objects are bound in `aiern-inner-text-objects-map'.
;;
;; Common text objects like words, WORDS, paragraphs and sentences are
;; defined via a corresponding move-function. This function must have
;; the following properties:
;;
;;   1. Take exactly one argument, the count.
;;   2. When the count is positive, move point forward to the first
;;      character after the end of the next count-th object.
;;   3. When the count is negative, move point backward to the first
;;      character of the count-th previous object.
;;   4. If point is placed on the first character of an object, the
;;      backward motion does NOT count that object.
;;   5. If point is placed on the last character of an object, the
;;      forward motion DOES count that object.
;;   6. The return value is "count left", i.e., in forward direction
;;      count is decreased by one for each successful move and in
;;      backward direction count is increased by one for each
;;      successful move, returning the final value of count.
;;      Therefore, if the complete move is successful, the return
;;      value is 0.
;;
;; A useful macro in this regard is `aiern-motion-loop', which quits
;; when point does not move further and returns the count difference.
;; It also provides a "unit value" of 1 or -1 for use in each
;; iteration. For example, a hypothetical "foo-bar" move could be
;; written as such:
;;
;;     (defun foo-bar (count)
;;       (aiern-motion-loop (var count)
;;         (forward-foo var) ; `var' is 1 or -1 depending on COUNT
;;         (forward-bar var)))
;;
;; If "forward-foo" and "-bar" didn't accept negative arguments,
;; we could choose their backward equivalents by inspecting `var':
;;
;;     (defun foo-bar (count)
;;       (aiern-motion-loop (var count)
;;         (cond
;;          ((< var 0)
;;           (backward-foo 1)
;;           (backward-bar 1))
;;          (t
;;           (forward-foo 1)
;;           (forward-bar 1)))))
;;
;; After a forward motion, point has to be placed on the first
;; character after some object, unless no motion was possible at all.
;; Similarly, after a backward motion, point has to be placed on the
;; first character of some object. This implies that point should
;; NEVER be moved to eob or bob, unless an object ends or begins at
;; eob or bob. (Usually, Emacs motions always move as far as possible.
;; But we want to use the motion-function to identify certain objects
;; in the buffer, and thus exact movement to object boundaries is
;; required.)

(aiern-define-text-object aiern-a-word (count &optional beg end type)
  "Select a word."
  (aiern-select-an-object 'aiern-word beg end type count))

(aiern-define-text-object aiern-inner-word (count &optional beg end type)
  "Select inner word."
  (aiern-select-inner-object 'aiern-word beg end type count))

(aiern-define-text-object aiern-a-WORD (count &optional beg end type)
  "Select a WORD."
  (aiern-select-an-object 'aiern-WORD beg end type count))

(aiern-define-text-object aiern-inner-WORD (count &optional beg end type)
  "Select inner WORD."
  (aiern-select-inner-object 'aiern-WORD beg end type count))

(aiern-define-text-object aiern-a-symbol (count &optional beg end type)
  "Select a symbol."
  (aiern-select-an-object 'aiern-symbol beg end type count))

(aiern-define-text-object aiern-inner-symbol (count &optional beg end type)
  "Select inner symbol."
  (aiern-select-inner-object 'aiern-symbol beg end type count))

(aiern-define-text-object aiern-a-sentence (count &optional beg end type)
  "Select a sentence."
  (aiern-select-an-object 'aiern-sentence beg end type count))

(aiern-define-text-object aiern-inner-sentence (count &optional beg end type)
  "Select inner sentence."
  (aiern-select-inner-object 'aiern-sentence beg end type count))

(aiern-define-text-object aiern-a-paragraph (count &optional beg end type)
  "Select a paragraph."
  :type line
  (aiern-select-an-object 'aiern-paragraph beg end type count t))

(aiern-define-text-object aiern-inner-paragraph (count &optional beg end type)
  "Select inner paragraph."
  :type line
  (aiern-select-inner-object 'aiern-paragraph beg end type count t))

(aiern-define-text-object aiern-a-paren (count &optional beg end type)
  "Select a parenthesis."
  :extend-selection nil
  (aiern-select-paren ?\( ?\) beg end type count t))

(aiern-define-text-object aiern-inner-paren (count &optional beg end type)
  "Select inner parenthesis."
  :extend-selection nil
  (aiern-select-paren ?\( ?\) beg end type count))

(aiern-define-text-object aiern-a-bracket (count &optional beg end type)
  "Select a square bracket."
  :extend-selection nil
  (aiern-select-paren ?\[ ?\] beg end type count t))

(aiern-define-text-object aiern-inner-bracket (count &optional beg end type)
  "Select inner square bracket."
  :extend-selection nil
  (aiern-select-paren ?\[ ?\] beg end type count))

(aiern-define-text-object aiern-a-curly (count &optional beg end type)
  "Select a curly bracket (\"brace\")."
  :extend-selection nil
  (aiern-select-paren ?{ ?} beg end type count t))

(aiern-define-text-object aiern-inner-curly (count &optional beg end type)
  "Select inner curly bracket (\"brace\")."
  :extend-selection nil
  (aiern-select-paren ?{ ?} beg end type count))

(aiern-define-text-object aiern-an-angle (count &optional beg end type)
  "Select an angle bracket."
  :extend-selection nil
  (aiern-select-paren ?< ?> beg end type count t))

(aiern-define-text-object aiern-inner-angle (count &optional beg end type)
  "Select inner angle bracket."
  :extend-selection nil
  (aiern-select-paren ?< ?> beg end type count))

(aiern-define-text-object aiern-a-single-quote (count &optional beg end type)
  "Select a single-quoted expression."
  :extend-selection t
  (aiern-select-quote ?' beg end type count t))

(aiern-define-text-object aiern-inner-single-quote (count &optional beg end type)
  "Select inner single-quoted expression."
  :extend-selection nil
  (aiern-select-quote ?' beg end type count))

(aiern-define-text-object aiern-a-double-quote (count &optional beg end type)
  "Select a double-quoted expression."
  :extend-selection t
  (aiern-select-quote ?\" beg end type count t))

(aiern-define-text-object aiern-inner-double-quote (count &optional beg end type)
  "Select inner double-quoted expression."
  :extend-selection nil
  (aiern-select-quote ?\" beg end type count))

(aiern-define-text-object aiern-a-back-quote (count &optional beg end type)
  "Select a back-quoted expression."
  :extend-selection t
  (aiern-select-quote ?\` beg end type count t))

(aiern-define-text-object aiern-inner-back-quote (count &optional beg end type)
  "Select inner back-quoted expression."
  :extend-selection nil
  (aiern-select-quote ?\` beg end type count))

(aiern-define-text-object aiern-a-tag (count &optional beg end type)
  "Select a tag block."
  :extend-selection nil
  (aiern-select-xml-tag beg end type count t))

(aiern-define-text-object aiern-inner-tag (count &optional beg end type)
  "Select inner tag block."
  :extend-selection nil
  (aiern-select-xml-tag beg end type count))

(aiern-define-text-object aiern-next-match (count &optional beg end type)
  "Select next match."
  (unless (and (boundp 'aiern-search-module)
               (eq aiern-search-module 'aiern-search))
    (user-error "next-match text objects only work with aiern search module."))
  (let ((pnt (point)))
    (cond
     ((eq aiern-ex-search-direction 'forward)
      (unless (eobp) (forward-char))
      (aiern-ex-search-previous 1)
      (when (and (<= aiern-ex-search-match-beg pnt)
                 (> aiern-ex-search-match-end pnt)
                 (not (aiern-visual-state-p)))
        (setq count (1- count)))
      (if (> count 0) (aiern-ex-search-next count)))
     (t
      (unless (eobp) (forward-char))
      (aiern-ex-search-next count))))
  ;; active visual state if command is executed in normal state
  (when (aiern-normal-state-p)
    (aiern-visual-select aiern-ex-search-match-beg aiern-ex-search-match-end 'inclusive +1 t))
  (list aiern-ex-search-match-beg aiern-ex-search-match-end))

(aiern-define-text-object aiern-previous-match (count &optional beg end type)
  "Select next match."
  (unless (and (boundp 'aiern-search-module)
               (eq aiern-search-module 'aiern-search))
    (user-error "previous-match text objects only work with aiern search module."))
  (let ((aiern-ex-search-direction
         (if (eq aiern-ex-search-direction 'backward)
             'forward
           'backward)))
    (aiern-next-match count beg end type)))

;;; Operator commands

(aiern-define-operator aiern-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((aiern-was-yanked-without-register
         (and aiern-was-yanked-without-register (not register))))
    (cond
     ((and (fboundp 'cua--global-mark-active)
           (fboundp 'cua-copy-region-to-global-mark)
           (cua--global-mark-active))
      (cua-copy-region-to-global-mark beg end))
     ((eq type 'block)
      (aiern-yank-rectangle beg end register yank-handler))
     ((memq type '(line screen-line))
      (aiern-yank-lines beg end register yank-handler))
     (t
      (aiern-yank-characters beg end register yank-handler)))))

(aiern-define-operator aiern-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion aiern-line-or-visual-line
  :move-point nil
  (interactive "<R><x>")
  (when (aiern-visual-state-p)
    (unless (memq type '(line block screen-line))
      (let ((range (aiern-expand beg end
                                (if (and aiern-respect-visual-line-mode
                                         visual-line-mode)
                                    'screen-line
                                  'line))))
        (setq beg (aiern-range-beginning range)
              end (aiern-range-end range)
              type (aiern-type range))))
    (aiern-exit-visual-state))
  (aiern-yank beg end type register))

(aiern-define-operator aiern-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (aiern-set-register ?- text))))
  (let ((aiern-was-yanked-without-register nil))
    (aiern-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (aiern-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end)))
  ;; place cursor on beginning of line
  (when (and (called-interactively-p 'any)
             (eq type 'line))
    (aiern-first-non-blank)))

(aiern-define-operator aiern-delete-line (beg end type register yank-handler)
  "Delete to end of line."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg))
         (visual-line-mode (and aiern-respect-visual-line-mode
                                visual-line-mode))
         (line-end (if visual-line-mode
                       (save-excursion
                         (end-of-visual-line)
                         (point))
                     (line-end-position))))
    (when (aiern-visual-state-p)
      (unless (memq type '(line screen-line block))
        (let ((range (aiern-expand beg end
                                  (if visual-line-mode
                                      'screen-line
                                    'line))))
          (setq beg (aiern-range-beginning range)
                end (aiern-range-end range)
                type (aiern-type range))))
      (aiern-exit-visual-state))
    (cond
     ((eq type 'block)
      ;; equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `aiern-delete'. In this case we fake the call to
      ;; `aiern-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `aiern-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (aiern-delete beg end 'block register yank-handler)))
     ((memq type '(line screen-line))
      (aiern-delete beg end type register yank-handler))
     (t
      (aiern-delete beg line-end type register yank-handler)))))

(aiern-define-operator aiern-delete-whole-line
  (beg end type register yank-handler)
  "Delete whole line."
  :motion aiern-line-or-visual-line
  (interactive "<R><x>")
  (aiern-delete beg end type register yank-handler))

(aiern-define-operator aiern-delete-char (beg end type register)
  "Delete next character."
  :motion aiern-forward-char
  (interactive "<R><x>")
  (aiern-delete beg end type register))

(aiern-define-operator aiern-delete-backward-char (beg end type register)
  "Delete previous character."
  :motion aiern-backward-char
  (interactive "<R><x>")
  (aiern-delete beg end type register))

(aiern-define-command aiern-delete-backward-char-and-join (count)
  "Delete previous character and join lines.
If point is at the beginning of a line then the current line will
be joined with the previous line if and only if
`aiern-backspace-join-lines'."
  (interactive "p")
  (if (or aiern-backspace-join-lines (not (bolp)))
      (call-interactively 'delete-backward-char)
    (user-error "Beginning of line")))

(aiern-define-command aiern-delete-backward-word ()
  "Delete previous word."
  (if (and (bolp) (not (bobp)))
      (progn
        (unless aiern-backspace-join-lines (user-error "Beginning of line"))
        (delete-char -1))
    (delete-region (max
                    (save-excursion
                      (aiern-backward-word-begin)
                      (point))
                    (line-beginning-position))
                   (point))))

(aiern-define-command aiern-delete-back-to-indentation ()
  "Delete back to the first non-whitespace character.
If point is before the first non-whitespace character of a
current line then delete from the point to the beginning of the
current line.  If point is on the beginning of the line, behave
according to `aiern-backspace-join-lines'."
  (if (bolp)
      (aiern-delete-backward-char-and-join 1)
    (delete-region (if (<= (current-column) (current-indentation))
                       (line-beginning-position)
                     (save-excursion
                       (aiern-first-non-blank)
                       (point)))
                   (point))))

(defun aiern-ex-delete-or-yank (should-delete beg end type register count yank-handler)
  "Execute aiern-delete or aiern-yank on the given region.
If SHOULD-DELETE is t, aiern-delete will be executed, otherwise
aiern-yank.
The region specified by BEG and END will be adjusted if COUNT is
given."
  (when count
    ;; with COUNT, the command should go the end of the region and delete/yank
    ;; COUNT lines from there
    (setq beg (save-excursion
                (goto-char end)
                (forward-line -1)
                (point))
          end (save-excursion
                (goto-char end)
                (point-at-bol count))
          type 'line))
  (funcall (if should-delete 'aiern-delete 'aiern-yank) beg end type register yank-handler))

(aiern-define-operator aiern-ex-delete (beg end type register count yank-handler)
  "The Ex delete command.
\[BEG,END]delete [REGISTER] [COUNT]"
  (interactive "<R><xc/><y>")
  (aiern-ex-delete-or-yank t beg end type register count yank-handler))

(aiern-define-operator aiern-ex-yank (beg end type register count yank-handler)
  "The Ex yank command.
\[BEG,END]yank [REGISTER] [COUNT]"
  (interactive "<R><xc/><y>")
  (aiern-ex-delete-or-yank nil beg end type register count yank-handler))

(aiern-define-command aiern-ex-put (_beg end ex-arg &optional force)
  (interactive "<r><a><!>")
  (let* ((arg-chars (remove ?\s (string-to-list ex-arg)))
         (reg (or (car arg-chars) ?\"))
         (text (cond
                ((and (< 1 (length arg-chars))
                      (/= ?= reg))
                 (user-error "Trailing characters"))
                ((eq ?= reg)
                 (aiern--eval-expr (if (= 1 (length arg-chars))
                                      aiern-last-=-register-input
                                    (setq aiern-last-=-register-input (substring ex-arg 1)))))
                (t (aiern-get-register reg)))))
    (unless text (user-error "Nothing in register %c" reg))
    (aiern-remove-yank-excluded-properties text)
    (goto-char (if (= (point-max) end) end (1- end)))
    (if force (aiern-insert-newline-above) (aiern-insert-newline-below))
    (aiern-set-marker ?\[ (point))
    ;; `insert' rather than `insert-for-yank' as we want to ignore yank-handlers...
    (insert (if (and (< 0 (length text))
                     (eq ?\n (aref text (1- (length text)))))
                (substring text 0 (1- (length text)))
              text))
    (aiern-set-marker ?\] (1- (point)))
    (back-to-indentation)
    (aiern-normal-state)))

(aiern-define-operator aiern-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `aiern-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'aiern-delete))
        (nlines (1+ (aiern-count-lines beg end)))
        (opoint (save-excursion
                  (goto-char beg)
                  (line-beginning-position))))
    (unless (eq aiern-want-fine-undo t)
      (aiern-start-undo-step))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (if (= opoint (point))
          (aiern-open-above 1)
        (aiern-open-below 1)))
     ((eq type 'block)
      (aiern-insert 1 nlines))
     (t
      (aiern-insert 1)))))

(aiern-define-operator aiern-change-line (beg end type register yank-handler)
  "Change to end of line, or change whole line if characterwise visual mode."
  :motion aiern-end-of-line-or-visual-line
  (interactive "<R><x><y>")
  (if (and (aiern-visual-state-p) (eq 'inclusive type))
      (cl-destructuring-bind (beg* end* &rest) (aiern-line-expand beg end)
          (aiern-change-whole-line beg* end* register yank-handler))
    (aiern-change beg end type register yank-handler #'aiern-delete-line)))

(aiern-define-operator aiern-change-whole-line
  (beg end register yank-handler)
  "Change whole line."
  :motion aiern-line-or-visual-line
  :type line
  (interactive "<r><x>")
  (aiern-change beg end 'line register yank-handler #'aiern-delete-whole-line))

(aiern-define-command aiern-copy (beg end address)
  "Copy lines in BEG END below line given by ADDRESS."
  :motion aiern-line-or-visual-line
  (interactive "<r><addr>")
  (goto-char (point-min))
  (forward-line address)
  (let* ((txt (buffer-substring-no-properties beg end))
         (len (length txt)))
    ;; ensure text consists of complete lines
    (when (or (zerop len) (/= (aref txt (1- len)) ?\n))
      (setq txt (concat txt "\n")))
    (when (and (eobp) (not (bolp))) (newline)) ; incomplete last line
    (insert txt)
    (forward-line -1)))

(aiern-define-command aiern-move (beg end address)
  "Move lines in BEG END below line given by ADDRESS."
  :motion aiern-line-or-visual-line
  (interactive "<r><addr>")
  (goto-char (point-min))
  (forward-line address)
  (let* ((m (set-marker (make-marker) (point)))
         (txt (buffer-substring-no-properties beg end))
         (len (length txt)))
    (delete-region beg end)
    (goto-char m)
    (set-marker m nil)
    ;; ensure text consists of complete lines
    (when (or (zerop len) (/= (aref txt (1- len)) ?\n))
      (setq txt (concat txt "\n")))
    (when (and (eobp) (not (bolp))) (newline)) ; incomplete last line
    (when (aiern-visual-state-p)
      (move-marker aiern-visual-mark (point)))
    (insert txt)
    (forward-line -1)
    (when (aiern-visual-state-p)
      (move-marker aiern-visual-point (point)))))

(defun aiern--check-undo-system ()
  (when (and (eq aiern-undo-system 'undo-tree)
             (not (bound-and-true-p undo-tree-mode)))
    (user-error "Enable `global-undo-tree-mode' to use undo-tree commands.
Add (add-hook 'aiern-local-mode-hook 'turn-on-undo-tree-mode) to your init file for undo in non-file buffers.")))

(aiern-define-command aiern-undo (count)
  "Undo COUNT changes in buffer using `aiern-undo-function'."
  :repeat abort
  (interactive "*p")
  (aiern--check-undo-system)
  (funcall aiern-undo-function count))

(aiern-define-command aiern-redo (count)
  "Undo COUNT changes in buffer using `aiern-redo-function'."
  :repeat abort
  (interactive "*p")
  (aiern--check-undo-system)
  (funcall aiern-redo-function count))

(aiern-define-operator aiern-substitute (beg end type register)
  "Change a character."
  :motion aiern-forward-char
  (interactive "<R><x>")
  (aiern-change beg end type register))

(aiern-define-operator aiern-upcase (beg end type)
  "Convert text to upper case."
  (if (eq type 'block)
      (aiern-apply-on-block #'aiern-upcase beg end nil)
    (upcase-region beg end)))

(aiern-define-operator aiern-downcase (beg end type)
  "Convert text to lower case."
  (if (eq type 'block)
      (aiern-apply-on-block #'aiern-downcase beg end nil)
    (downcase-region beg end)))

(aiern-define-operator aiern-invert-case (beg end type)
  "Invert case of text."
  (let (char)
    (if (eq type 'block)
        (aiern-apply-on-block #'aiern-invert-case beg end nil)
      (save-excursion
        (goto-char beg)
        (while (< beg end)
          (setq char (following-char))
          (delete-char 1 nil)
          (if (eq (upcase char) char)
              (insert-char (downcase char) 1)
            (insert-char (upcase char) 1))
          (setq beg (1+ beg)))))))

(aiern-define-operator aiern-invert-char (beg end type)
  "Invert case of character."
  :motion aiern-forward-char
  (if (eq type 'block)
      (aiern-apply-on-block #'aiern-invert-case beg end nil)
    (aiern-invert-case beg end)
    (when aiern-this-motion
      (goto-char end)
      (when (and aiern-cross-lines
                 (not aiern-move-beyond-eol)
                 (not (aiern-visual-state-p))
                 (not (aiern-operator-state-p))
                 (eolp) (not (eobp)) (not (bolp)))
        (forward-char)))))

(aiern-define-operator aiern-rot13 (beg end type)
  "ROT13 encrypt text."
  (if (eq type 'block)
      (aiern-apply-on-block #'aiern-rot13 beg end nil)
    (rot13-region beg end)))

(aiern-define-operator aiern-join (beg end)
  "Join the selected lines."
  :motion aiern-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (goto-char beg)
    (dotimes (_ count)
      (join-line 1))))

(aiern-define-operator aiern-join-whitespace (beg end)
  "Join the selected lines without changing whitespace.
\\<aiern-normal-state-map>Like \\[aiern-join], \
but doesn't insert or remove any spaces."
  :motion aiern-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (goto-char beg)
    (dotimes (_ count)
      (aiern-move-end-of-line 1)
      (unless (eobp)
        (delete-char 1)))))

(aiern-define-operator aiern-ex-join (beg end &optional count bang)
  "Join the selected lines with optional COUNT and BANG."
  (interactive "<r><a><!>")
  (if (and count (not (string-match-p "^[1-9][0-9]*$" count)))
      (user-error "Invalid count")
    (let ((join-fn (if bang 'aiern-join-whitespace 'aiern-join)))
      (cond
       ((not count)
        ;; without count - just join the given region
        (funcall join-fn beg end))
       (t
        ;; emulate vim's :join when count is given - start from the
        ;; end of the region and join COUNT lines from there
        (let* ((count-num (string-to-number count))
               (beg-adjusted (save-excursion
                               (goto-char end)
                               (forward-line -1)
                               (point)))
               (end-adjusted (save-excursion
                               (goto-char end)
                               (point-at-bol count-num))))
          (funcall join-fn beg-adjusted end-adjusted)))))))

(aiern-define-operator aiern-fill (beg end)
  "Fill text."
  :move-point nil
  :type line
  (save-excursion
    (condition-case nil
        (fill-region beg end)
      (error nil))))

(aiern-define-operator aiern-fill-and-move (beg end)
  "Fill text and move point to the end of the filled region."
  :move-point nil
  :type line
  (let ((marker (make-marker)))
    (move-marker marker (1- end))
    (condition-case nil
        (progn
          (fill-region beg end)
          (goto-char marker)
          (aiern-first-non-blank))
      (error nil))))

(aiern-define-operator aiern-indent (beg end)
  "Indent text."
  :move-point nil
  :type line
  (if (and (= beg (line-beginning-position))
           (= end (line-beginning-position 2)))
      ;; since some Emacs modes can only indent one line at a time,
      ;; implement "==" as a call to `indent-according-to-mode'
      (indent-according-to-mode)
    (goto-char beg)
    (indent-region beg end))
  ;; We also need to tabify or untabify the leading white characters
  (when aiern-indent-convert-tabs
    (let* ((beg-line (line-number-at-pos beg))
           (end-line (line-number-at-pos end))
           (ln beg-line)
           (convert-white (if indent-tabs-mode 'tabify 'untabify)))
      (save-excursion
        (while (<= ln end-line)
          (goto-char (point-min))
          (forward-line (- ln 1))
          (back-to-indentation)
          ;; Whether tab or space should be used is determined by indent-tabs-mode
          (funcall convert-white (line-beginning-position) (point))
          (setq ln (1+ ln)))))
    (back-to-indentation)))

(aiern-define-operator aiern-indent-line (beg end)
  "Indent the line."
  :motion aiern-line
  (aiern-indent beg end))

(aiern-define-operator aiern-shift-left (beg end &optional count preserve-empty)
  "Shift text from BEG to END to the left.
The text is shifted to the nearest multiple of `aiern-shift-width'
\(the rounding can be disabled by setting `aiern-shift-round').
If PRESERVE-EMPTY is non-nil, lines that contain only spaces are
indented, too, otherwise they are ignored.  The relative column
of point is preserved if this function is not called
interactively. Otherwise, if the function is called as an
operator, point is moved to the first non-blank character.
See also `aiern-shift-right'."
  :type line
  (interactive "<r><vc>")
  (aiern-shift-right beg end (- (or count 1)) preserve-empty))

(aiern-define-operator aiern-shift-right (beg end &optional count preserve-empty)
  "Shift text from BEG to END to the right.
The text is shifted to the nearest multiple of `aiern-shift-width'
\(the rounding can be disabled by setting `aiern-shift-round').
If PRESERVE-EMPTY is non-nil, lines that contain only spaces are
indented, too, otherwise they are ignored.  The relative column
of point is preserved if this function is not called
interactively. Otherwise, if the function is called as an
operator, point is moved to the first non-blank character.
See also `aiern-shift-left'."
  :type line
  (interactive "<r><vc>")
  (setq count (or count 1))
  (let ((beg (set-marker (make-marker) beg))
        (end (set-marker (make-marker) end))
        (pnt-indent (current-column))
        first-shift) ; shift of first line
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((indent (current-indentation))
               (new-indent
                (max 0
                     (if (not aiern-shift-round)
                         (+ indent (* count aiern-shift-width))
                       (* (+ (/ indent aiern-shift-width)
                             count
                             (cond
                              ((> count 0) 0)
                              ((zerop (mod indent aiern-shift-width)) 0)
                              (t 1)))
                          aiern-shift-width)))))
          (unless first-shift
            (setq first-shift (- new-indent indent)))
          (when (or preserve-empty
                    (save-excursion
                      (skip-chars-forward " \t")
                      (not (eolp))))
            (indent-to new-indent 0))
          (delete-region (point) (progn (skip-chars-forward " \t") (point)))
          (forward-line 1))))
    ;; in case we're in an empty buffer first-shift is still unchanged
    (unless first-shift
      (if (< count 0)
          (setq first-shift 0)
        (setq first-shift (* count aiern-shift-width))
        (indent-to first-shift)))
    ;; When called from insert state (C-t or C-d) the cursor should shift with the line,
    ;; otherwise (normal state) it should end up on the first non-whitespace character
    (if (aiern-insert-state-p)
        (move-to-column (max 0 (+ pnt-indent first-shift)))
      (aiern-first-non-blank))))

(aiern-define-command aiern-shift-right-line (count)
  "Shift the current line COUNT times to the right.
The text is shifted to the nearest multiple of
`aiern-shift-width'. Like `aiern-shift-right' but always works on
the current line."
  (interactive "<c>")
  (aiern-shift-right (line-beginning-position) (line-beginning-position 2) count t))

(aiern-define-command aiern-shift-left-line (count)
  "Shift the current line COUNT times to the left.
The text is shifted to the nearest multiple of
`aiern-shift-width'. Like `aiern-shift-left' but always works on
the current line."
  (interactive "<c>")
  (aiern-shift-left (line-beginning-position) (line-beginning-position 2) count t))

(aiern-define-operator aiern-align-left (beg end type &optional width)
  "Right-align lines in the region at WIDTH columns.
The default for width is the value of `fill-column'."
  :motion aiern-line
  :type line
  (interactive "<R><a>")
  (aiern-justify-lines beg end 'left (if width
                                        (string-to-number width)
                                      0)))

(aiern-define-operator aiern-align-right (beg end type &optional width)
  "Right-align lines in the region at WIDTH columns.
The default for width is the value of `fill-column'."
  :motion aiern-line
  :type line
  (interactive "<R><a>")
  (aiern-justify-lines beg end 'right (if width
                                         (string-to-number width)
                                       fill-column)))

(aiern-define-operator aiern-align-center (beg end type &optional width)
  "Centers lines in the region between WIDTH columns.
The default for width is the value of `fill-column'."
  :motion aiern-line
  :type line
  (interactive "<R><a>")
  (aiern-justify-lines beg end 'center (if width
                                          (string-to-number width)
                                        fill-column)))

(aiern-define-operator aiern-replace (beg end type char)
  "Replace text from BEG to END with CHAR."
  :motion aiern-forward-char
  (interactive "<R>"
               (unwind-protect
                   (let ((aiern-force-cursor 'replace))
                     (aiern-refresh-cursor)
                     (list (aiern-read-key)))
                 (aiern-refresh-cursor)))
  (when char
    (if (eq type 'block)
        (save-excursion
          (aiern-apply-on-rectangle
           #'(lambda (begcol endcol char)
               (let ((maxcol (aiern-column (line-end-position))))
                 (when (< begcol maxcol)
                   (setq endcol (min endcol maxcol))
                   (let ((beg (aiern-move-to-column begcol nil t))
                         (end (aiern-move-to-column endcol nil t)))
                     (delete-region beg end)
                     (insert (make-string (- endcol begcol) char))))))
           beg end char))
      (goto-char beg)
      (cond
       ((eq char ?\n)
        (delete-region beg end)
        (newline)
        (when aiern-auto-indent
          (indent-according-to-mode)))
       (t
        (while (< (point) end)
          (if (eq (char-after) ?\n)
              (forward-char)
            (delete-char 1)
            (insert-char char 1)))
        (goto-char (max beg (1- end))))))))

(aiern-define-command aiern-paste-before
  (count &optional register yank-handler)
  "Pastes the latest yanked text before the cursor position.
The return value is the yanked text."
  :suppress-operator t
  (interactive "*P<x>")
  (setq count (prefix-numeric-value count))
  (if (aiern-visual-state-p)
      (aiern-visual-paste count register)
    (aiern-with-undo
      (let* ((text (if register
                       (aiern-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when aiern-paste-clear-minibuffer-first
          (delete-minibuffer-contents)
          (setq aiern-paste-clear-minibuffer-first nil))
        (when text
          (if (functionp yank-handler)
              (let ((aiern-paste-count count)
                    ;; for non-interactive use
                    (this-command #'aiern-paste-before))
                (push-mark opoint t)
                (insert-for-yank text))
            ;; no yank-handler, default
            (when (vectorp text)
              (setq text (aiern-vector-to-string text)))
            (set-text-properties 0 (length text) nil text)
            (push-mark opoint t)
            (dotimes (_ (or count 1))
              (insert-for-yank text))
            (setq aiern-last-paste
                  (list #'aiern-paste-before
                        count
                        opoint
                        opoint    ; beg
                        (point))) ; end
            (aiern-set-marker ?\[ opoint)
            (aiern-set-marker ?\] (1- (point)))
            (when (and aiern-move-cursor-back
                       (> (length text) 0))
              (backward-char))))
        ;; no paste-pop after pasting from a register
        (when register
          (setq aiern-last-paste nil))
        (and (> (length text) 0) text)))))

(aiern-define-command aiern-paste-after
  (count &optional register yank-handler)
  "Pastes the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (interactive "*P<x>")
  (setq count (prefix-numeric-value count))
  (if (aiern-visual-state-p)
      (aiern-visual-paste count register)
    (aiern-with-undo
      (let* ((text (if register
                       (aiern-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when text
          (if (functionp yank-handler)
              (let ((aiern-paste-count count)
                    ;; for non-interactive use
                    (this-command #'aiern-paste-after))
                (insert-for-yank text))
            ;; no yank-handler, default
            (when (vectorp text)
              (setq text (aiern-vector-to-string text)))
            (set-text-properties 0 (length text) nil text)
            (unless (eolp) (forward-char))
            (push-mark (point) t)
            ;; TODO: Perhaps it is better to collect a list of all
            ;; (point . mark) pairs to undo the yanking for COUNT > 1.
            ;; The reason is that this yanking could very well use
            ;; `yank-handler'.
            (let ((beg (point)))
              (dotimes (_ (or count 1))
                (insert-for-yank text))
              (setq aiern-last-paste
                    (list #'aiern-paste-after
                          count
                          opoint
                          beg       ; beg
                          (point))) ; end
              (aiern-set-marker ?\[ beg)
              (aiern-set-marker ?\] (1- (point)))
              (when (aiern-normal-state-p)
                (aiern-move-cursor-back)))))
        (when register
          (setq aiern-last-paste nil))
        (and (> (length text) 0) text)))))

(aiern-define-command aiern-visual-paste (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "*P<x>")
  (setq count (prefix-numeric-value count))
  ;; aiern-visual-paste is typically called from aiern-paste-before or
  ;; aiern-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'aiern-visual-paste)
  (let* ((text (if register
                   (aiern-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text)))
         paste-eob)
    (aiern-with-undo
      (let ((kill-ring-yank-pointer (when kill-ring (list (current-kill 0)))))
        (when (aiern-visual-state-p)
          (aiern-visual-rotate 'upper-left)
          ;; if we replace the last buffer line that does not end in a
          ;; newline, we use `aiern-paste-after' because `aiern-delete'
          ;; will move point to the line above
          (when (and (= aiern-visual-end (point-max))
                     (/= (char-before (point-max)) ?\n))
            (setq paste-eob t))
          (aiern-delete aiern-visual-beginning
                       aiern-visual-end
                       (aiern-visual-type)
                       (unless aiern-kill-on-visual-paste ?_))
          (when (and (eq yank-handler #'aiern-yank-line-handler)
                     (not (eq (aiern-visual-type) 'line))
                     (not (= aiern-visual-end (point-max))))
            (insert "\n"))
          (aiern-normal-state)
          (when kill-ring (current-kill 1)))
        ;; Effectively memoize `aiern-get-register' because it can be
        ;; side-effecting (e.g. for the `=' register)...
        (cl-letf (((symbol-function 'aiern-get-register)
                   (lambda (&rest _) text)))
          (if paste-eob
              (aiern-paste-after count register)
            (aiern-paste-before count register))))
      (when aiern-kill-on-visual-paste
        (current-kill -1))
      ;; mark the last paste as visual-paste
      (setq aiern-last-paste
            (list (nth 0 aiern-last-paste)
                  (nth 1 aiern-last-paste)
                  (nth 2 aiern-last-paste)
                  (nth 3 aiern-last-paste)
                  (nth 4 aiern-last-paste)
                  t)))))

(defun aiern-paste-from-register (register)
  "Paste from REGISTER."
  (interactive
   (let ((overlay (make-overlay (point) (point)))
         (string "\""))
     (unwind-protect
         (progn
           ;; display " in the buffer while reading register
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (list (or aiern-this-register (read-char))))
       (delete-overlay overlay))))
  (when (aiern-paste-before nil register t)
    ;; go to end of pasted text
    (unless (eobp)
      (forward-char))))

(defun aiern-paste-last-insertion ()
  "Paste last insertion."
  (interactive)
  (aiern-paste-from-register ?.))

(aiern-define-command aiern-use-register (register)
  "Use REGISTER for the next command."
  :keep-visual t
  :repeat ignore
  (interactive "<C>")
  (setq aiern-this-register register))

(defvar aiern-macro-buffer nil
  "The buffer that has been active on macro recording.")

(aiern-define-command aiern-record-macro (register)
  "Record a keyboard macro into REGISTER.
If REGISTER is :, /, or ?, the corresponding command line window
will be opened instead."
  :keep-visual t
  :suppress-operator t
  (interactive
   (list (unless (and aiern-this-macro defining-kbd-macro)
           (or aiern-this-register (aiern-read-key)))))
  (cond
   ((eq register ?\C-g)
    (keyboard-quit))
   ((and aiern-this-macro defining-kbd-macro)
    (setq aiern-macro-buffer nil)
    (condition-case nil
        (end-kbd-macro)
      (error nil))
    (when last-kbd-macro
      (when (member last-kbd-macro '("" []))
        (setq last-kbd-macro nil))
      (aiern-set-register aiern-this-macro last-kbd-macro))
    (setq aiern-this-macro nil))
   ((eq register ?:)
    (aiern-command-window-ex))
   ((eq register ?/)
    (aiern-command-window-search-forward))
   ((eq register ??)
    (aiern-command-window-search-backward))
   ((or (and (>= register ?0) (<= register ?9))
        (and (>= register ?a) (<= register ?z))
        (and (>= register ?A) (<= register ?Z)))
    (when defining-kbd-macro (end-kbd-macro))
    (setq aiern-this-macro register)
    (aiern-set-register aiern-this-macro nil)
    (start-kbd-macro nil)
    (setq aiern-macro-buffer (current-buffer)))
   (t (error "Invalid register"))))

(aiern-define-command aiern-execute-macro (count macro)
  "Execute keyboard macro MACRO, COUNT times.
When called with a non-numerical prefix \
\(such as \\[universal-argument]),
COUNT is infinite. MACRO is read from a register
when called interactively."
  :keep-visual t
  :suppress-operator t
  (interactive
   (let (count macro register)
     (setq count (if current-prefix-arg
                     (if (numberp current-prefix-arg)
                         current-prefix-arg
                       0) 1)
           register (or aiern-this-register (read-char)))
     (cond
      ((or (and (eq register ?@) (eq aiern-last-register ?:))
           (eq register ?:))
       (setq macro (lambda () (aiern-ex-repeat nil))
             aiern-last-register ?:))
      ((eq register ?@)
       (unless aiern-last-register
         (user-error "No previously executed keyboard macro."))
       (setq macro (aiern-get-register aiern-last-register t)))
      (t
       (setq macro (aiern-get-register register t)
             aiern-last-register register)))
     (list count macro)))
  (cond
   ((functionp macro)
    (aiern-repeat-abort)
    (dotimes (_ (or count 1))
      (funcall macro)))
   ((or (and (not (stringp macro))
             (not (vectorp macro)))
        (member macro '("" [])))
    ;; allow references to currently empty registers
    ;; when defining macro
    (unless aiern-this-macro
      (user-error "No previous macro")))
   (t
    (condition-case err
        (aiern-with-single-undo
          (execute-kbd-macro macro count))
      ;; enter Normal state if the macro fails
      (error
       (aiern-normal-state)
       (aiern-normalize-keymaps)
       (signal (car err) (cdr err)))))))

;;; Visual commands

(aiern-define-motion aiern-visual-restore ()
  "Restore previous selection."
  (let* ((point (point))
         (mark (or (mark t) point))
         (dir aiern-visual-direction)
         (type (aiern-visual-type))
         range)
    (unless (aiern-visual-state-p)
      (cond
       ;; No previous selection.
       ((or (null aiern-visual-selection)
            (null aiern-visual-mark)
            (null aiern-visual-point)))
       ;; If the type was one-to-one, it is preferable to infer
       ;; point and mark from the selection's boundaries. The reason
       ;; is that a destructive operation may displace the markers
       ;; inside the selection.
       ((aiern-type-property type :one-to-one)
        (setq range (aiern-contract-range (aiern-visual-range))
              mark (aiern-range-beginning range)
              point (aiern-range-end range))
        (when (< dir 0)
          (aiern-swap mark point)))
       ;; If the type wasn't one-to-one, we have to restore the
       ;; selection on the basis of the previous point and mark.
       (t
        (setq mark aiern-visual-mark
              point aiern-visual-point)))
      (aiern-visual-make-selection mark point type t))))

(aiern-define-motion aiern-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+           +---M
        |   |    <=>    |   |
        +---P           P---+

For example, if mark is in the upper left corner and point
in the lower right, this function puts mark in the upper right
corner and point in the lower left."
  (cond
   ((eq aiern-visual-selection 'block)
    (let* ((point (point))
           (mark (or (mark t) point))
           (point-col (aiern-column point))
           (mark-col (aiern-column mark))
           (mark (save-excursion
                   (goto-char mark)
                   (aiern-move-to-column point-col)
                   (point)))
           (point (save-excursion
                    (goto-char point)
                    (aiern-move-to-column mark-col)
                    (point))))
      (aiern-visual-refresh mark point)))
   (t
    (aiern-exchange-point-and-mark)
    (aiern-visual-refresh))))

(aiern-define-command aiern-visual-rotate (corner &optional beg end type)
  "In Visual Block selection, put point in CORNER.
Corner may be one of `upper-left', `upper-right', `lower-left'
and `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

When called interactively, the selection is rotated blockwise."
  :keep-visual t
  (interactive
   (let ((corners '(upper-left upper-right lower-right lower-left)))
     (list (or (cadr (memq (aiern-visual-block-corner) corners))
               'upper-left))))
  (let* ((beg (or beg (point)))
         (end (or end (mark t) beg))
         (type (or type aiern-this-type))
         range)
    (cond
     ((memq type '(rectangle block))
      (setq range (aiern-block-rotate beg end :corner corner)
            beg (pop range)
            end (pop range))
      (unless (eq corner (aiern-visual-block-corner corner beg end))
        (aiern-swap beg end))
      (goto-char beg)
      (when (aiern-visual-state-p)
        (aiern-move-mark end)
        (aiern-visual-refresh nil nil nil :corner corner)))
     ((memq corner '(upper-right lower-right))
      (goto-char (max beg end))
      (when (aiern-visual-state-p)
        (aiern-move-mark (min beg end))))
     (t
      (goto-char (min beg end))
      (when (aiern-visual-state-p)
        (aiern-move-mark (max beg end)))))))

;;; Insertion commands

(defun aiern-insert (count &optional vcount skip-empty-lines)
  "Switch to Insert state just before point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT - 1 lines starting at the same column.
If SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of the
lines.  This is the default behaviour for Visual-state insertion."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (and (aiern-visual-state-p)
              (memq (aiern-visual-type) '(line block))
              (save-excursion
                (let ((m (mark)))
                  ;; go to upper-left corner temporarily so
                  ;; `count-lines' yields accurate results
                  (aiern-visual-rotate 'upper-left)
                  (prog1 (count-lines aiern-visual-beginning aiern-visual-end)
                    (set-mark m)))))
         (aiern-visual-state-p)))
  (if (and (called-interactively-p 'any)
           (aiern-visual-state-p))
      (cond
       ((eq (aiern-visual-type) 'line)
        (aiern-visual-rotate 'upper-left)
        (aiern-insert-line count vcount))
       ((eq (aiern-visual-type) 'block)
        (let ((column (min (aiern-column aiern-visual-beginning)
                           (aiern-column aiern-visual-end))))
          (aiern-visual-rotate 'upper-left)
          (move-to-column column t)
          (aiern-insert count vcount skip-empty-lines)))
       (t
        (aiern-visual-rotate 'upper-left)
        (aiern-insert count vcount skip-empty-lines)))
    (setq aiern-insert-count count
          aiern-insert-lines nil
          aiern-insert-vcount (and vcount
                                  (> vcount 1)
                                  (list (line-number-at-pos)
                                        (current-column)
                                        vcount))
          aiern-insert-skip-empty-lines skip-empty-lines)
    (aiern-insert-state 1)))

(defun aiern-append (count &optional vcount skip-empty-lines)
  "Switch to Insert state just after point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT - 1 lines starting at the same column.  If
SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of
the lines."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (and (aiern-visual-state-p)
              (memq (aiern-visual-type) '(line block))
              (save-excursion
                (let ((m (mark)))
                  ;; go to upper-left corner temporarily so
                  ;; `count-lines' yields accurate results
                  (aiern-visual-rotate 'upper-left)
                  (prog1 (count-lines aiern-visual-beginning aiern-visual-end)
                    (set-mark m)))))))
  (if (and (called-interactively-p 'any)
           (aiern-visual-state-p))
      (cond
       ((or (eq (aiern-visual-type) 'line)
            (and (eq (aiern-visual-type) 'block)
                 (memq last-command '(next-line previous-line))
                 (numberp temporary-goal-column)
                 (= temporary-goal-column most-positive-fixnum)))
        (aiern-visual-rotate 'upper-left)
        (aiern-append-line count vcount))
       ((eq (aiern-visual-type) 'block)
        (let ((column (max (aiern-column aiern-visual-beginning)
                           (aiern-column aiern-visual-end))))
          (aiern-visual-rotate 'upper-left)
          (move-to-column column t)
          (aiern-insert count vcount skip-empty-lines)))
       (t
        (aiern-visual-rotate 'lower-right)
        (backward-char)
        (aiern-append count)))
    (unless (eolp) (forward-char))
    (aiern-insert count vcount skip-empty-lines)
    (add-hook 'post-command-hook #'aiern-maybe-remove-spaces)))

(defun aiern-insert-resume (count)
  "Switch to Insert state at previous insertion point.
The insertion will be repeated COUNT times. If called from visual
state, only place point at the previous insertion position but do not
switch to insert state."
  (interactive "p")
  (aiern-goto-mark ?^ t)
  (unless (aiern-visual-state-p)
    (aiern-insert count)))

(defun aiern-open-above (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (unless (eq aiern-want-fine-undo t)
    (aiern-start-undo-step))
  (aiern-insert-newline-above)
  (setq aiern-insert-count count
        aiern-insert-lines t
        aiern-insert-vcount nil)
  (aiern-insert-state 1)
  (when aiern-auto-indent
    (indent-according-to-mode)))

(defun aiern-open-below (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (unless (eq aiern-want-fine-undo t)
    (aiern-start-undo-step))
  (push (point) buffer-undo-list)
  (aiern-insert-newline-below)
  (setq aiern-insert-count count
        aiern-insert-lines t
        aiern-insert-vcount nil)
  (aiern-insert-state 1)
  (when aiern-auto-indent
    (indent-according-to-mode)))

(defun aiern-insert-line (count &optional vcount)
  "Switch to insert state at beginning of current line.
Point is placed at the first non-blank character on the current
line.  The insertion will be repeated COUNT times.  If VCOUNT is
non nil it should be number > 0. The insertion will be repeated
in the next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (push (point) buffer-undo-list)
  (if (and visual-line-mode
           aiern-respect-visual-line-mode)
      (goto-char
       (max (save-excursion
              (back-to-indentation)
              (point))
            (save-excursion
              (beginning-of-visual-line)
              (point))))
    (back-to-indentation))
  (setq aiern-insert-count count
        aiern-insert-lines nil
        aiern-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'aiern-first-non-blank
                   vcount)))
  (aiern-insert-state 1))

(defun aiern-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (if (and visual-line-mode
           aiern-respect-visual-line-mode)
      (aiern-end-of-visual-line)
    (aiern-move-end-of-line))
  (setq aiern-insert-count count
        aiern-insert-lines nil
        aiern-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'end-of-line
                   vcount)))
  (aiern-insert-state 1))

(aiern-define-command aiern-insert-digraph (count)
  "Insert COUNT digraphs."
  :repeat change
  (interactive "p")
  (let ((digraph (aiern-read-digraph-char 0)))
    (insert-char digraph count)))

(aiern-define-command aiern-ex-show-digraphs ()
  "Shows a list of all available digraphs."
  :repeat nil
  (let ((columns 3))
    (aiern-with-view-list
      :name "aiern-digraphs"
      :mode-name "aiern Digraphs"
      :format
      (cl-loop repeat columns
               vconcat [("Digraph" 8 nil)
                        ("Sequence" 16 nil)])
      :entries
      (let* ((digraphs (mapcar #'(lambda (digraph)
                                   (cons (cdr digraph)
                                         (car digraph)))
                               (append aiern-digraphs-table
                                       aiern-digraphs-table-user)))
             (entries (cl-loop for digraph in digraphs
                               collect `(,(concat (char-to-string (nth 1 digraph))
                                                  (char-to-string (nth 2 digraph)))
                                         ,(char-to-string (nth 0 digraph)))))
             (row)
             (rows)
             (clength (* columns 2)))
        (cl-loop for e in entries
                 do
                 (push (nth 0 e) row)
                 (push (nth 1 e) row)
                 (when (eq (length row) clength)
                   (push `(nil ,(apply #'vector row)) rows)
                   (setq row nil)))
        rows))))

(defun aiern--self-insert-string (string)
  "Insert STRING as if typed interactively."
  (let ((chars (append string nil)))
    (dolist (char chars)
      (let ((last-command-event char))
        (self-insert-command 1)))))

(defun aiern-copy-from-above (arg)
  "Copy characters from preceding non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move backward.
See also \\<aiern-insert-state-map>\\[aiern-copy-from-below]."
  (interactive
   (cond
    ;; if a prefix argument was given, repeat it for subsequent calls
    ((and (null current-prefix-arg)
          (eq last-command #'aiern-copy-from-above))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (aiern--self-insert-string (aiern-copy-chars-from-line arg -1)))

(defun aiern-copy-from-below (arg)
  "Copy characters from following non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move forward.
See also \\<aiern-insert-state-map>\\[aiern-copy-from-above]."
  (interactive
   (cond
    ((and (null current-prefix-arg)
          (eq last-command #'aiern-copy-from-below))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (aiern--self-insert-string (aiern-copy-chars-from-line arg 1)))

;; adapted from `copy-from-above-command' in misc.el
(defun aiern-copy-chars-from-line (n num &optional col)
  "Return N characters from line NUM, starting at column COL.
NUM is relative to the current line and can be negative.
COL defaults to the current column."
  (interactive "p")
  (let ((col (or col (current-column))) prefix)
    (save-excursion
      (forward-line num)
      (when (looking-at "[[:space:]]*$")
        (if (< num 0)
            (skip-chars-backward " \t\n")
          (skip-chars-forward " \t\n")))
      (aiern-move-beginning-of-line)
      (move-to-column col)
      ;; if the column winds up in middle of a tab,
      ;; return the appropriate number of spaces
      (when (< col (current-column))
        (if (eq (preceding-char) ?\t)
            (let ((len (min n (- (current-column) col))))
              (setq prefix (make-string len ?\s)
                    n (- n len)))
          ;; if in middle of a control char, return the whole char
          (backward-char 1)))
      (concat prefix
              (buffer-substring (point)
                                (min (line-end-position)
                                     (+ n (point))))))))

;; completion
(aiern-define-command aiern-complete-next (&optional arg)
  "Complete to the nearest following word.
Search backward if a match isn't found.
Calls `aiern-complete-next-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall aiern-complete-next-minibuffer-func)
    (funcall aiern-complete-next-func arg)))

(aiern-define-command aiern-complete-previous (&optional arg)
  "Complete to the nearest preceding word.
Search forward if a match isn't found.
Calls `aiern-complete-previous-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall aiern-complete-previous-minibuffer-func)
    (funcall aiern-complete-previous-func arg)))

(aiern-define-command aiern-complete-next-line (&optional arg)
  "Complete a whole line.
Calls `aiern-complete-next-line-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall aiern-complete-next-minibuffer-func)
    (funcall aiern-complete-next-line-func arg)))

(aiern-define-command aiern-complete-previous-line (&optional arg)
  "Complete a whole line.
Calls `aiern-complete-previous-line-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall aiern-complete-previous-minibuffer-func)
    (funcall aiern-complete-previous-line-func arg)))

;;; Search

(defun aiern-repeat-search (flag)
  "Called to record a search command.
FLAG is either 'pre or 'post if the function is called before resp.
after executing the command."
  (cond
   ((and (aiern-operator-state-p) (eq flag 'pre))
    (aiern-repeat-record (this-command-keys))
    (aiern-clear-command-keys))
   ((and (aiern-operator-state-p) (eq flag 'post))
    ;; The value of (this-command-keys) at this point should be the
    ;; key-sequence that called the last command that finished the
    ;; search, usually RET. Therefore this key-sequence will be
    ;; recorded in the post-command of the operator. Alternatively we
    ;; could do it here.
    (aiern-repeat-record (if aiern-regexp-search
                            (car-safe regexp-search-ring)
                          (car-safe search-ring))))
   (t (aiern-repeat-motion flag))))

(aiern-define-motion aiern-search-forward ()
  (format "Search forward for user-entered text.
Searches for regular expression if `aiern-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat aiern-repeat-search
  (aiern-search-incrementally t aiern-regexp-search))

(aiern-define-motion aiern-search-backward ()
  (format "Search backward for user-entered text.
Searches for regular expression if `aiern-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat aiern-repeat-search
  (aiern-search-incrementally nil aiern-regexp-search))

(aiern-define-motion aiern-search-next (count)
  "Repeat the last search."
  :jump t
  :type exclusive
  (let ((orig (point))
        (search-string (if aiern-regexp-search
                           (car-safe regexp-search-ring)
                         (car-safe search-ring))))
    (goto-char
     ;; Wrap in `save-excursion' so that multiple searches have no visual effect.
     (save-excursion
       (aiern-search search-string isearch-forward aiern-regexp-search)
       (when (and (> (point) orig)
                  (save-excursion
                    (aiern-adjust-cursor)
                    (= (point) orig)))
         ;; Point won't move after first attempt and `aiern-adjust-cursor' takes
         ;; effect, so start again.
         (aiern-search search-string isearch-forward aiern-regexp-search))
       (point)))
    (when (and count (> count 1))
      (dotimes (_ (1- count))
        (aiern-search search-string isearch-forward aiern-regexp-search)))))

(aiern-define-motion aiern-search-previous (count)
  "Repeat the last search in the opposite direction."
  :jump t
  :type exclusive
  (dotimes (_ (or count 1))
    (aiern-search (if aiern-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 (not isearch-forward) aiern-regexp-search)))

(aiern-define-motion aiern-search-word-backward (count &optional symbol)
  "Search backward for symbol under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     aiern-symbol-word-search))
  (dotimes (_ (or count 1))
    (aiern-search-word nil nil symbol)))

(aiern-define-motion aiern-search-word-forward (count &optional symbol)
  "Search forward for symbol under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     aiern-symbol-word-search))
  (dotimes (_ (or count 1))
    (aiern-search-word t nil symbol)))

(aiern-define-motion aiern-search-unbounded-word-backward (count &optional symbol)
  "Search backward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     aiern-symbol-word-search))
  (dotimes (_ (or count 1))
    (aiern-search-word nil t symbol)))

(aiern-define-motion aiern-search-unbounded-word-forward (count &optional symbol)
  "Search forward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     aiern-symbol-word-search))
  (dotimes (_ (or count 1))
    (aiern-search-word t t symbol)))

(defun aiern-goto-definition-imenu (string _position)
  "Find definition for STRING with imenu."
  (require 'imenu nil t)
  (let (ientry ipos)
    (when (fboundp 'imenu--make-index-alist)
      (ignore-errors (setq ientry (imenu--make-index-alist)))
      (setq ientry (imenu--in-alist string ientry))
      (setq ipos (cdr ientry))
      (when (and (markerp ipos)
                 (eq (marker-buffer ipos) (current-buffer)))
        (setq ipos (marker-position ipos))
        (when (numberp ipos)
          (aiern-search (format "\\_<%s\\_>" (regexp-quote string)) t t ipos)
          t)))))

(defun aiern-goto-definition-semantic (_string position)
  "Find definition for POSITION with semantic."
  (and (fboundp 'semantic-ia-fast-jump)
       (ignore-errors (semantic-ia-fast-jump position))))

(declare-function xref-backend-identifier-at-point "xref")

(defun aiern-goto-definition-xref (_string position)
  "Find definition at POSITION with xref."
  (when (fboundp 'xref-find-definitions)
    (let ((identifier (save-excursion
                        (goto-char position)
                        (xref-backend-identifier-at-point (xref-find-backend)))))
      (condition-case ()
          (progn
            (xref-find-definitions identifier)
            t)
        (user-error nil)))))

(defun aiern-goto-definition-search (string _position)
  "Find definition for STRING with aiern-search."
  (aiern-search (format "\\_<%s\\_>" (regexp-quote string)) t t (point-min))
  t)

(aiern-define-motion aiern-goto-definition ()
  "Go to definition or first occurrence of symbol under point.
See also `aiern-goto-definition-functions'."
  :jump t
  :type exclusive
  (let* ((match (aiern--find-thing t 'symbol))
         (string (car match))
         (position (cdr match)))
    (if (null string)
        (user-error "No symbol under cursor")
      (setq isearch-forward t)
      (run-hook-with-args-until-success 'aiern-goto-definition-functions
                                        string position))))

;;; Folding
(defun aiern-fold-action (list action)
  "Perform fold ACTION for each matching major or minor mode in LIST.

ACTION will be performed for the first matching handler in LIST.  For more
information on its features and format, see the documentation for
`aiern-fold-list'.

If no matching ACTION is found in LIST, an error will signaled.

Handler errors will be demoted, so a problem in one handler will (hopefully)
not interfere with another."
  (if (null list)
      (user-error
       "Enable one of the following modes for folding to work: %s"
       (mapconcat 'symbol-name (mapcar 'caar aiern-fold-list) ", "))
    (let* ((modes (caar list)))
      (if (aiern--mode-p modes)
          (let* ((actions (cdar list))
                 (fn      (plist-get actions action)))
            (when fn
              (with-demoted-errors (funcall fn))))
        (aiern-fold-action (cdr list) action)))))

(defun aiern--mode-p (modes)
  "Determines whether any symbol in MODES represents the current
buffer's major mode or any of its minors."
  (unless (eq modes '())
    (let ((mode (car modes)))
      (or (eq major-mode mode)
          (and (boundp mode) (symbol-value mode))
          (aiern--mode-p (cdr modes))))))

(aiern-define-command aiern-toggle-fold ()
  "Open or close a fold under point.
See also `aiern-open-fold' and `aiern-close-fold'."
  (aiern-fold-action aiern-fold-list :toggle))

(aiern-define-command aiern-open-folds ()
  "Open all folds.
See also `aiern-close-folds'."
  (aiern-fold-action aiern-fold-list :open-all))

(aiern-define-command aiern-close-folds ()
  "Close all folds.
See also `aiern-open-folds'."
  (aiern-fold-action aiern-fold-list :close-all))

(aiern-define-command aiern-open-fold ()
  "Open fold at point.
See also `aiern-close-fold'."
  (aiern-fold-action aiern-fold-list :open))

(aiern-define-command aiern-open-fold-rec ()
  "Open fold at point recursively.
See also `aiern-open-fold' and `aiern-close-fold'."
  (aiern-fold-action aiern-fold-list :open-rec))

(aiern-define-command aiern-close-fold ()
  "Close fold at point.
See also `aiern-open-fold'."
  (aiern-fold-action aiern-fold-list :close))

;;; Ex

(aiern-define-operator aiern-write (beg end type file-or-append &optional bang)
  "Save the current buffer, from BEG to END, to FILE-OR-APPEND.
If FILE-OR-APPEND is of the form \">> FILE\", append to FILE
instead of overwriting.  The current buffer's filename is not
changed unless it has no associated file and no region is
specified.  If the file already exists and the BANG argument is
non-nil, it is overwritten without confirmation."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<R><fsh><!>")
  (let* ((append-and-filename (aiern-extract-append file-or-append))
         (append (car append-and-filename))
         (filename (cdr append-and-filename))
         (bufname (buffer-file-name (buffer-base-buffer))))
    (when (zerop (length filename))
      (setq filename bufname))
    (cond
     ((zerop (length filename))
      (user-error "Please specify a file name for the buffer"))
     ;; execute command on region
     ((eq (aref filename 0) ?!)
      (shell-command-on-region beg end (substring filename 1)))
     ;; with region or append, always save to file without resetting
     ;; modified flag
     ((or append (and beg end))
      (write-region beg end filename append nil nil (not (or append bang))))
     ;; no current file
     ((null bufname)
      (write-file filename (not bang)))
     ;; save current buffer to its file
     ((string= filename bufname)
      (if (not bang) (save-buffer) (write-file filename)))
     ;; save to another file
     (t
      (write-region nil nil filename
                    nil (not bufname) nil
                    (not bang))))))

(aiern-define-command aiern-write-all (bang)
  "Saves all buffers visiting a file.
If BANG is non nil then read-only buffers are saved, too,
otherwise they are skipped. "
  :repeat nil
  :move-point nil
  (interactive "<!>")
  (if bang
      (save-some-buffers t)
    ;; save only buffer that are not read-only and
    ;; that are visiting a file
    (save-some-buffers t
                       #'(lambda ()
                           (and (not buffer-read-only)
                                (buffer-file-name))))))

(aiern-define-command aiern-save (filename &optional bang)
  "Save the current buffer to FILENAME.
Changes the file name of the current buffer to FILENAME.  If no
FILENAME is given, the current file name is used."
  :repeat nil
  :move-point nil
  (interactive "<f><!>")
  (when (zerop (length filename))
    (setq filename (buffer-file-name (buffer-base-buffer))))
  (write-file filename (not bang)))

(aiern-define-command aiern-edit (file &optional bang)
  "Open FILE.
If no FILE is specified, reload the current buffer from disk."
  :repeat nil
  (interactive "<f><!>")
  (if file
      (find-file file)
    (revert-buffer bang (or bang (not (buffer-modified-p))) t)))

(aiern-define-command aiern-read (count file)
  "Inserts the contents of FILE below the current line or line COUNT."
  :repeat nil
  :move-point nil
  (interactive "P<fsh>")
  (when (and file (not (zerop (length file))))
    (when count (goto-char (point-min)))
    (when (or (not (zerop (forward-line (or count 1))))
              (not (bolp)))
      (insert "\n"))
    (if (/= (aref file 0) ?!)
        (let ((result (insert-file-contents file)))
          (save-excursion
            (forward-char (cadr result))
            (unless (bolp) (insert "\n"))))
      (shell-command (substring file 1) t)
      (save-excursion
        (goto-char (mark))
        (unless (bolp) (insert "\n"))))))

(aiern-define-command aiern-show-files ()
  "Shows the file-list.
The same as `buffer-menu', but shows only buffers visiting
files."
  :repeat nil
  (buffer-menu 1))

(aiern-define-command aiern-goto-error (count)
  "Go to error number COUNT.

If no COUNT supplied, move to the current error.

Acts like `first-error' other than when given no counts, goes
to the current error instead of the first, like in Vim's :cc
command."
  :repeat nil
  (interactive "<c>")
  (if count
      (first-error (if (eql 0 count) 1 count))
    (next-error 0)))

(aiern-define-command aiern-buffer (buffer)
  "Switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (cond
   ;; no buffer given, switch to "other" buffer
   ((null buffer) (switch-to-buffer (other-buffer)))
   ;; we are given the name of an existing buffer
   ((get-buffer buffer) (switch-to-buffer buffer))
   ;; try to complete the buffer
   ((let ((all-buffers (internal-complete-buffer buffer nil t)))
      (when (= (length all-buffers) 1)
        (switch-to-buffer (car all-buffers)))))
   (t
    (when (y-or-n-p
           (format "No buffer with name \"%s\" exists. Create new buffer? "
                   buffer))
      (switch-to-buffer buffer)))))

(aiern-define-command aiern-next-buffer (&optional count)
  "Goes to the `count'-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (_ (or count 1))
    (next-buffer)))

(aiern-define-command aiern-prev-buffer (&optional count)
  "Goes to the `count'-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (_ (or count 1))
    (previous-buffer)))

(aiern-define-command aiern-delete-buffer (buffer &optional bang)
  "Deletes a buffer.
All windows currently showing this buffer will be closed except
for the last window in each frame."
  (interactive "<b><!>")
  (with-current-buffer (or buffer (current-buffer))
    (when bang
      (set-buffer-modified-p nil)
      (dolist (process (process-list))
        (when (eq (process-buffer process) (current-buffer))
          (set-process-query-on-exit-flag process nil))))
    ;; get all windows that show this buffer
    (let ((wins (get-buffer-window-list (current-buffer) nil t)))
      ;; if the buffer which was initiated by emacsclient,
      ;; call `server-edit' from server.el to avoid
      ;; "Buffer still has clients" message
      (if (and (fboundp 'server-edit)
               (boundp 'server-buffer-clients)
               server-buffer-clients)
          (server-edit)
        (kill-buffer nil))
      ;; close all windows that showed this buffer
      (mapc #'(lambda (w)
                (condition-case nil
                    (delete-window w)
                  (error nil)))
            wins))))

(aiern-define-command aiern-quit (&optional force)
  "Closes the current window, current frame, Emacs.
If the current frame belongs to some client the client connection
is closed."
  :repeat nil
  (interactive "<!>")
  (condition-case nil
      (delete-window)
    (error
     (if (and (boundp 'server-buffer-clients)
              (fboundp 'server-edit)
              (fboundp 'server-buffer-done)
              server-buffer-clients)
         (if force
             (server-buffer-done (current-buffer))
           (server-edit))
       (condition-case nil
           (delete-frame)
         (error
          (if force
              (kill-emacs)
            (save-buffers-kill-emacs))))))))

(aiern-define-command aiern-quit-all (&optional bang)
  "Exits Emacs, asking for saving."
  :repeat nil
  (interactive "<!>")
  (if (null bang)
      (save-buffers-kill-terminal)
    (let ((proc (frame-parameter (selected-frame) 'client)))
      (if proc
          (with-no-warnings
            (server-delete-client proc))
        (dolist (process (process-list))
          (set-process-query-on-exit-flag process nil))
        (kill-emacs)))))

(aiern-define-command aiern-quit-all-with-error-code (&optional force)
  "Exits Emacs without saving, returning an non-zero error code.
The FORCE argument is only there for compatibility and is ignored.
This function fails with an error if Emacs is run in server mode."
  :repeat nil
  (interactive "<!>")
  (if (and (boundp 'server-buffer-clients)
           (fboundp 'server-edit)
           (fboundp 'server-buffer-done)
           server-buffer-clients)
      (user-error "Cannot exit client process with error code.")
    (kill-emacs 1)))

(aiern-define-command aiern-save-and-quit ()
  "Save all buffers and exit Emacs."
  (save-buffers-kill-terminal t))

(aiern-define-command aiern-save-and-close (file &optional bang)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (aiern-write nil nil nil file bang)
  (aiern-quit))

(aiern-define-command aiern-save-modified-and-close (file &optional bang)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (when (buffer-modified-p)
    (aiern-write nil nil nil file bang))
  (aiern-quit))

(aiern-define-operator aiern-shell-command
  (beg end type command &optional previous)
  "Execute a shell command.
If BEG, END and TYPE is specified, COMMAND is executed on the region,
which is replaced with the command's output. Otherwise, the
output is displayed in its own buffer. If PREVIOUS is non-nil,
the previous shell command is executed instead."
  (interactive "<R><sh><!>")
  (if (not (aiern-ex-p))
      (let ((aiern-ex-initial-input
             (if (and beg
                      (not (aiern-visual-state-p))
                      (not current-prefix-arg))
                 (let ((range (aiern-range beg end type)))
                   (aiern-contract-range range)
                   ;; TODO: this is not exactly the same as Vim, which
                   ;; uses .,+count as range. However, this is easier
                   ;; to achieve with the current implementation and
                   ;; the very inconvenient range interface.
                   ;;
                   ;; TODO: the range interface really needs some
                   ;; rework!
                   (format
                    "%d,%d!"
                    (line-number-at-pos (aiern-range-beginning range))
                    (line-number-at-pos (aiern-range-end range))))
               "!")))
        (call-interactively 'aiern-ex))
    (when command
      (setq command (aiern-ex-replace-special-filenames command)))
    (if (zerop (length command))
        (when previous (setq command aiern-previous-shell-command))
      (setq aiern-previous-shell-command command))
    (cond
     ((zerop (length command))
      (if previous (user-error "No previous shell command")
        (user-error "No shell command")))
     (aiern-ex-range
      (if (not aiern-display-shell-error-in-message)
          (shell-command-on-region beg end command nil t)
        (let ((output-buffer (generate-new-buffer " *temp*"))
              (error-buffer (generate-new-buffer " *temp*")))
          (unwind-protect
              (if (zerop (shell-command-on-region beg end
                                                  command
                                                  output-buffer nil
                                                  error-buffer))
                  (progn
                    (delete-region beg end)
                    (insert-buffer-substring output-buffer)
                    (goto-char beg)
                    (aiern-first-non-blank))
                (display-message-or-buffer error-buffer))
            (kill-buffer output-buffer)
            (kill-buffer error-buffer)))))
     (t
      (shell-command command)))))

(aiern-define-command aiern-make (arg)
  "Call a build command in the current directory.
If ARG is nil this function calls `recompile', otherwise it calls
`compile' passing ARG as build command."
  (interactive "<sh>")
  (if (and (fboundp 'recompile)
           (not arg))
      (recompile)
    (compile arg)))

;; TODO: escape special characters (currently only \n) ... perhaps
;; there is some Emacs function doing this?
(aiern-define-command aiern-show-registers (registers)
  "Shows the contents of REGISTERS, or all registers, if none supplied."
  :repeat nil
  (interactive "<a>")
  (let* ((all-registers (aiern-register-list))
         (reg-chars (string-to-list registers))
         (display-regs (if reg-chars
                           (cl-remove-if-not (lambda (r) (memq (car r) reg-chars))
                                             all-registers)
                         all-registers)))
    (aiern-with-view-list
      :name "aiern-registers"
      :mode-name "aiern Registers"
      :format
      [("Register" 10 nil)
       ("Value" 1000 nil)]
      :entries
      (cl-loop for (key . val) in display-regs
               collect `(nil [,(char-to-string key)
                              ,(cond ((stringp val)
                                      (replace-regexp-in-string "\n" "^J" val))
                                     ((vectorp val)
                                      (key-description val))
                                     (t ""))])))))

(aiern-define-command aiern-show-marks (mrks)
  "Shows all marks.
If MRKS is non-nil it should be a string and only registers
corresponding to the characters of this string are shown."
  :repeat nil
  (interactive "<a>")
  ;; To get markers and positions, we can't rely on 'global-mark-ring'
  ;; provided by Emacs (although it will be much simpler and faster),
  ;; because 'global-mark-ring' does not store mark characters, but
  ;; only buffer name and position. Instead, 'aiern-markers-alist' is
  ;; used; this is list maintained by aiern for each buffer.
  (let ((all-markers
         ;; get global and local marks
         (append (cl-remove-if (lambda (m)
                                 (or (aiern-global-marker-p (car m))
                                     (not (markerp (cdr m)))))
                               aiern-markers-alist)
                 (cl-remove-if (lambda (m)
                                 (or (not (aiern-global-marker-p (car m)))
                                     (not (markerp (cdr m)))))
                               (default-value 'aiern-markers-alist)))))
    (when mrks
      (setq mrks (string-to-list mrks))
      (setq all-markers (cl-delete-if (lambda (m)
                                        (not (member (car m) mrks)))
                                      all-markers)))
    ;; map marks to list of 4-tuples (char row col file)
    (setq all-markers
          (mapcar (lambda (m)
                    (with-current-buffer (marker-buffer (cdr m))
                      (save-excursion
                        (goto-char (cdr m))
                        (list (car m)
                              (line-number-at-pos (point))
                              (current-column)
                              (buffer-name)))))
                  all-markers))
    (aiern-with-view-list
      :name "aiern-marks"
      :mode-name "aiern Marks"
      :format [("Mark" 8 nil)
               ("Line" 8 nil)
               ("Column" 8 nil)
               ("Buffer" 1000 nil)]
      :entries (cl-loop for m in (sort all-markers (lambda (a b) (< (car a) (car b))))
                        collect `(nil [,(char-to-string (nth 0 m))
                                       ,(number-to-string (nth 1 m))
                                       ,(number-to-string (nth 2 m))
                                       (,(nth 3 m))]))
      :select-action #'aiern--show-marks-select-action)))

(defun aiern--show-marks-select-action (entry)
  (kill-buffer)
  (switch-to-buffer (car (elt entry 3)))
  (aiern-goto-mark (string-to-char (elt entry 0))))

(defun aiern--parse-delmarks (to-be-parsed &optional parsed)
  "Where TO-BE-PARSED can contain ranges in the form `x-y'.
PARSED is a list of characters whose marks should be deleted.
Like vim, on invalid input, preceeding valid input is still parsed."
  (cl-destructuring-bind (&optional a b c &rest) to-be-parsed
    (cond
     ((null to-be-parsed) parsed)
     ;; single mark...
     ((and (not (eq ?- b)) (or (<= ?a a ?z) (<= ?A a ?Z) (<= ?0 a ?9)
                               (memq a '(?\" ?^ ?. ?\[ ?\] ?< ?>))))
      (aiern--parse-delmarks (cdr to-be-parsed) (cons a parsed)))
     ;; range of marks...
     ((and (eq ?- b) c (or (<= ?a a c ?z) (<= ?A a c ?Z) (<= ?0 a c ?9)))
      (aiern--parse-delmarks (nthcdr 3 to-be-parsed)
                            (append parsed (number-sequence a c))))
     (t (progn (message "Invalid input: %s" (apply #'string (remove nil to-be-parsed)))
               parsed)))))

(aiern-define-command aiern-delete-marks (marks &optional force)
  "MARKS is a string denoting all marks to be deleted. Mark names are
either single characters or a range of characters in the form `x-y'.
If FORCE is non-nil and MARKS is blank, all local marks except 0-9 are removed."
  (interactive "<a><!>")
  (let ((mark-chars (remove ?\s (append marks nil))))
    (cond
     ((and force mark-chars) (message "Invalid input"))
     (mark-chars
      (let* ((delmarks (aiern--parse-delmarks mark-chars))
             (delmarkp (lambda (m) (member (car m) delmarks))))
        ;; delete all parsed marks...
        (setq aiern-markers-alist
              (cl-remove-if delmarkp aiern-markers-alist))
        ;; ensure all parsed marks are deleted globally...
        (set-default 'aiern-markers-alist
                     (cl-remove-if delmarkp (default-value 'aiern-markers-alist)))))
     ;; delete local marks except 0-9...
     (force (setq aiern-markers-alist
                  (cl-remove-if-not (lambda (m) (<= ?0 (car m) ?9))
                                    aiern-markers-alist))))))

(eval-when-compile (require 'ffap))
(aiern-define-command aiern-find-file-at-point-with-line ()
  "Opens the file at point and goes to line-number."
  (require 'ffap)
  (let ((fname (with-no-warnings (ffap-file-at-point))))
    (if fname
        (let ((line
               (save-excursion
                 (goto-char (cadr ffap-string-at-point-region))
                 (and (re-search-backward ":\\([0-9]+\\)\\="
                                          (line-beginning-position) t)
                      (string-to-number (match-string 1))))))
          (with-no-warnings (ffap-other-window fname))
          (when line
            (goto-char (point-min))
            (forward-line (1- line))))
      (user-error "File does not exist."))))

(aiern-define-command aiern-find-file-at-point-visual ()
  "Find the filename selected by the visual region.
    Returns an error message if the file does not exist."
  (require 'ffap)
  (let ((region (buffer-substring (region-beginning) (region-end))))
    (if (file-exists-p region)
        (find-file-at-point region)
      (user-error (format "Can't find file \"%s\" in path" region)))))

(aiern-ex-define-argument-type state
  "Defines an argument type which can take state names."
  :collection
  (lambda (arg predicate flag)
    (let ((completions
           (append '("nil")
                   (mapcar #'(lambda (state)
                               (format "%s" (car state)))
                           aiern-state-properties))))
      (when arg
        (cond
         ((eq flag nil)
          (try-completion arg completions predicate))
         ((eq flag t)
          (all-completions arg completions predicate))
         ((eq flag 'lambda)
          (test-completion arg completions predicate))
         ((eq (car-safe flag) 'boundaries)
          (cons 'boundaries
                (completion-boundaries arg
                                       completions
                                       predicate
                                       (cdr flag)))))))))

(aiern-define-interactive-code "<state>"
  "A valid aiern state."
  :ex-arg state
  (list (when (and (aiern-ex-p) aiern-ex-argument)
          (intern aiern-ex-argument))))

;; TODO: should we merge this command with `aiern-set-initial-state'?
(aiern-define-command aiern-ex-set-initial-state (state)
  "Set the initial state for the current major mode to STATE.
This is the state the buffer comes up in. See `aiern-set-initial-state'."
  :repeat nil
  (interactive "<state>")
  (if (not (or (assq state aiern-state-properties)
               (null state)))
      (user-error "State %s cannot be set as initial aiern state" state)
    (let ((current-initial-state (aiern-initial-state major-mode)))
      (unless (eq current-initial-state state)
        ;; only if we selected a new mode
        (when (y-or-n-p (format "Major-mode `%s' has initial mode `%s'. \
Change to `%s'? "
                                major-mode
                                (or current-initial-state "DEFAULT")
                                (or state "DEFAULT")))
          (aiern-set-initial-state major-mode state)
          (when (y-or-n-p "Save setting in customization file? ")
            (dolist (s (list current-initial-state state))
              (when s
                (let ((var (intern (format "aiern-%s-state-modes" s))))
                  (customize-save-variable var (symbol-value var)))))))))))

(aiern-define-command aiern-force-normal-state ()
  "Switch to normal state without recording current command."
  :repeat abort
  :suppress-operator t
  (aiern-normal-state))

(aiern-define-motion aiern-ex-search-next (count)
  "Goes to the next occurrence."
  :jump t
  :type exclusive
  (aiern-ex-search count))

(aiern-define-motion aiern-ex-search-previous (count)
  "Goes the the previous occurrence."
  :jump t
  :type exclusive
  (let ((aiern-ex-search-direction
         (if (eq aiern-ex-search-direction 'backward) 'forward 'backward)))
    (aiern-ex-search count)))

(defun aiern-repeat-ex-search (flag)
  "Called to record a search command.
FLAG is either 'pre or 'post if the function is called before
resp.  after executing the command."
  (cond
   ((and (aiern-operator-state-p) (eq flag 'pre))
    (aiern-repeat-record (this-command-keys))
    (aiern-clear-command-keys))
   ((and (aiern-operator-state-p) (eq flag 'post))
    ;; The value of (this-command-keys) at this point should be the
    ;; key-sequence that called the last command that finished the
    ;; search, usually RET. Therefore this key-sequence will be
    ;; recorded in the post-command of the operator. Alternatively we
    ;; could do it here.
    (aiern-repeat-record (aiern-ex-pattern-regex aiern-ex-search-pattern)))
   (t (aiern-repeat-motion flag))))

(aiern-define-motion aiern-ex-search-forward (count)
  "Starts a forward search."
  :jump t
  :type exclusive
  :repeat aiern-repeat-ex-search
  (aiern-ex-start-search 'forward count))

(aiern-define-motion aiern-ex-search-backward (count)
  "Starts a forward search."
  :jump t
  :repeat aiern-repeat-ex-search
  (aiern-ex-start-search 'backward count))

(aiern-define-motion aiern-ex-search-word-forward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     aiern-symbol-word-search))
  (aiern-ex-start-word-search nil 'forward count symbol))

(aiern-define-motion aiern-ex-search-word-backward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     aiern-symbol-word-search))
  (aiern-ex-start-word-search nil 'backward count symbol))

(aiern-define-motion aiern-ex-search-unbounded-word-forward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     aiern-symbol-word-search))
  (aiern-ex-start-word-search t 'forward count symbol))

(aiern-define-motion aiern-ex-search-unbounded-word-backward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     aiern-symbol-word-search))
  (aiern-ex-start-word-search t 'backward count symbol))

(defun aiern-revert-reveal (open-spots)
  "Unconditionally close overlays in OPEN-SPOTS in current window.
Modified version of `reveal-close-old-overlays' from
reveal.el. OPEN-SPOTS is a local version of `reveal-open-spots'."
  (dolist (spot open-spots)
    (let ((window (car spot))
          (ol (cdr spot)))
      (unless (eq window (selected-window))
        (error "aiern-revert-reveal: slot with wrong window"))
      (let* ((inv (overlay-get ol 'reveal-invisible))
             (open (or (overlay-get ol 'reveal-toggle-invisible)
                       (get inv 'reveal-toggle-invisible)
                       (overlay-get ol 'isearch-open-invisible-temporary))))
        (if (and (overlay-start ol) ;Check it's still live.
                 open)
            (condition-case err
                (funcall open ol t)
              (error (message "!!Reveal-hide (funcall %s %s t): %s !!"
                              open ol err)))
          (overlay-put ol 'invisible inv))
        ;; Remove the overlay from the list of open spots.
        (overlay-put ol 'reveal-invisible nil)))))

(aiern-define-operator aiern-ex-substitute
  (beg end pattern replacement flags)
  "The Ex substitute command.
\[BEG,END]substitute/PATTERN/REPLACEMENT/FLAGS"
  :repeat nil
  :jump t
  :move-point nil
  :motion aiern-line
  (interactive "<r><s/>")
  (aiern-ex-nohighlight)
  (unless pattern
    (user-error "No pattern given"))
  (setq replacement (or replacement ""))
  (setq aiern-ex-last-was-search nil)
  (let* ((flags (append flags nil))
         (count-only (memq ?n flags))
         (confirm (and (memq ?c flags) (not count-only)))
         (case-fold-search (aiern-ex-pattern-ignore-case pattern))
         (case-replace case-fold-search)
         (aiern-ex-substitute-regex (aiern-ex-pattern-regex pattern))
         (aiern-ex-substitute-nreplaced 0)
         (aiern-ex-substitute-last-point (point))
         (whole-line (aiern-ex-pattern-whole-line pattern))
         (aiern-ex-substitute-overlay (make-overlay (point) (point)))
         (orig-point-marker (move-marker (make-marker) (point)))
         (end-marker (move-marker (make-marker) end))
         (use-reveal confirm)
         reveal-open-spots
         zero-length-match
         match-contains-newline
         transient-mark-mode)
    (setq aiern-ex-substitute-pattern pattern
          aiern-ex-substitute-replacement replacement
          aiern-ex-substitute-flags flags
          isearch-string aiern-ex-substitute-regex)
    (isearch-update-ring aiern-ex-substitute-regex t)
    (unwind-protect
        (progn
          (aiern-ex-hl-change 'aiern-ex-substitute pattern)
          (overlay-put aiern-ex-substitute-overlay 'face 'isearch)
          (overlay-put aiern-ex-substitute-overlay 'priority 1001)
          (goto-char beg)
          (catch 'exit-search
            (while (re-search-forward aiern-ex-substitute-regex end-marker t)
              (when (not (and query-replace-skip-read-only
                              (text-property-any (match-beginning 0) (match-end 0) 'read-only t)))
                (let ((match-str (match-string 0))
                      (match-beg (move-marker (make-marker) (match-beginning 0)))
                      (match-end (move-marker (make-marker) (match-end 0)))
                      (match-data (match-data)))
                  (goto-char match-beg)
                  (setq match-contains-newline
                        (string-match-p "\n" (buffer-substring-no-properties
                                              match-beg match-end)))
                  (setq zero-length-match (= match-beg match-end))
                  (when (and (string= "^" aiern-ex-substitute-regex)
                             (= (point) end-marker))
                    ;; The range (beg end) includes the final newline which means
                    ;; end-marker is on one line down. With the regex "^" the
                    ;; beginning of this last line will be matched which we don't
                    ;; want, so we abort here.
                    (throw 'exit-search t))
                  (setq aiern-ex-substitute-last-point match-beg)
                  (if confirm
                      (let ((prompt
                             (format "Replace %s with %s (y/n/a/q/l/^E/^Y)? "
                                     match-str
                                     (aiern-match-substitute-replacement
                                      aiern-ex-substitute-replacement
                                      (not case-replace))))
                            (search-invisible t)
                            response)
                        (move-overlay aiern-ex-substitute-overlay match-beg match-end)
                        ;; Simulate `reveal-mode'. `reveal-mode' uses
                        ;; `post-command-hook' but that won't work here.
                        (when use-reveal
                          (reveal-post-command))
                        (catch 'exit-read-char
                          (while (setq response (read-char prompt))
                            (when (member response '(?y ?a ?l))
                              (unless count-only
                                (set-match-data match-data)
                                (aiern-replace-match aiern-ex-substitute-replacement
                                                    (not case-replace)))
                              (setq aiern-ex-substitute-nreplaced
                                    (1+ aiern-ex-substitute-nreplaced))
                              (aiern-ex-hl-set-region 'aiern-ex-substitute
                                                     (save-excursion
                                                       (forward-line)
                                                       (point))
                                                     (aiern-ex-hl-get-max
                                                      'aiern-ex-substitute)))
                            (cl-case response
                              ((?y ?n) (throw 'exit-read-char t))
                              (?a (setq confirm nil)
                                  (throw 'exit-read-char t))
                              ((?q ?l ?\C-\[) (throw 'exit-search t))
                              (?\C-e (aiern-scroll-line-down 1))
                              (?\C-y (aiern-scroll-line-up 1))))))
                    (setq aiern-ex-substitute-nreplaced
                          (1+ aiern-ex-substitute-nreplaced))
                    (unless count-only
                      (set-match-data match-data)
                      (aiern-replace-match aiern-ex-substitute-replacement
                                          (not case-replace))))
                  (goto-char match-end)
                  (cond ((>= (point) end-marker)
                         ;; Don't want to perform multiple replacements at the end
                         ;; of the search region.
                         (throw 'exit-search t))
                        ((and (not whole-line)
                              (not match-contains-newline))
                         (forward-line)
                         ;; forward-line just moves to the end of the line on the
                         ;; last line of the buffer.
                         (when (or (eobp)
                                   (> (point) end-marker))
                           (throw 'exit-search t)))
                        ;; For zero-length matches check to see if point won't
                        ;; move next time. This is a problem when matching the
                        ;; regexp "$" because we can enter an infinite loop,
                        ;; repeatedly matching the same character
                        ((and zero-length-match
                              (let ((pnt (point)))
                                (save-excursion
                                  (and
                                   (re-search-forward
                                    aiern-ex-substitute-regex end-marker t)
                                   (= pnt (point))))))
                         (if (or (eobp)
                                 (>= (point) end-marker))
                             (throw 'exit-search t)
                           (forward-char)))))))))
      (aiern-ex-delete-hl 'aiern-ex-substitute)
      (delete-overlay aiern-ex-substitute-overlay)

      (if count-only
          (goto-char orig-point-marker)
        (goto-char aiern-ex-substitute-last-point))

      (move-marker orig-point-marker nil)
      (move-marker end-marker nil)

      (when use-reveal
        (aiern-revert-reveal reveal-open-spots)))

    (message "%s %d occurrence%s"
             (if count-only "Found" "Replaced")
             aiern-ex-substitute-nreplaced
             (if (/= aiern-ex-substitute-nreplaced 1) "s" ""))
    (aiern-first-non-blank)))

(aiern-define-operator aiern-ex-repeat-substitute
  (beg end flags)
  "Repeat last substitute command.
This is the same as :s//~/"
  :repeat nil
  :jump t
  :move-point nil
  :motion aiern-line
  (interactive "<r><a>")
  (apply #'aiern-ex-substitute beg end
         (aiern-ex-get-substitute-info (concat "//~/" flags))))

(aiern-define-operator aiern-ex-repeat-substitute-with-flags
  (beg end flags)
  "Repeat last substitute command with last flags.
This is the same as :s//~/&"
  :repeat nil
  :jump t
  :move-point nil
  :motion aiern-line
  (interactive "<r><a>")
  (apply #'aiern-ex-substitute beg end
         (aiern-ex-get-substitute-info (concat "//~/&" flags))))

(aiern-define-operator aiern-ex-repeat-substitute-with-search
  (beg end flags)
  "Repeat last substitute command with last search pattern.
This is the same as :s//~/r"
  :repeat nil
  :jump t
  :move-point nil
  :motion aiern-line
  (interactive "<r><a>")
  (apply #'aiern-ex-substitute beg end
         (aiern-ex-get-substitute-info (concat "//~/r" flags))))

(aiern-define-operator aiern-ex-repeat-substitute-with-search-and-flags
  (beg end flags)
  "Repeat last substitute command with last search pattern and last flags.
This is the same as :s//~/&r"
  :repeat nil
  :jump t
  :move-point nil
  :motion aiern-line
  (interactive "<r><a>")
  (apply #'aiern-ex-substitute beg end
         (aiern-ex-get-substitute-info (concat "//~/&r" flags))))

(aiern-define-operator aiern-ex-repeat-global-substitute ()
  "Repeat last substitute command on the whole buffer.
This is the same as :%s//~/&"
  :repeat nil
  :jump t
  :move-point nil
  :motion aiern-line
  (interactive)
  (apply #'aiern-ex-substitute (point-min) (point-max)
         (aiern-ex-get-substitute-info (concat "//~/&"))))

(aiern-define-operator aiern-ex-global
  (beg end pattern command &optional invert)
  "The Ex global command.
\[BEG,END]global[!]/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (unless pattern
    (user-error "No pattern given"))
  (unless command
    (user-error "No command given"))
  ;; TODO: `aiern-ex-make-substitute-pattern' should be executed so
  ;; :substitute can re-use :global's pattern depending on its `r'
  ;; flag. This isn't supported currently but should be simple to add
  (aiern-with-single-undo
    (let ((case-fold-search
           (eq (aiern-ex-regex-case pattern aiern-ex-search-case) 'insensitive))
          (command-form (aiern-ex-parse command))
          (transient-mark-mode transient-mark-mode)
          (deactivate-mark deactivate-mark)
          match markers)
      (when (and pattern command)
        (setq isearch-string pattern)
        (isearch-update-ring pattern t)
        (goto-char beg)
        (aiern-move-beginning-of-line)
        (while (< (point) end)
          (setq match (re-search-forward pattern (line-end-position) t))
          (when (or (and match (not invert))
                    (and invert (not match)))
            (push (move-marker (make-marker)
                               (or (and match (match-beginning 0))
                                   (line-beginning-position)))
                  markers))
          (forward-line))
        (setq markers (nreverse markers))
        (unwind-protect
            (dolist (marker markers)
              (goto-char marker)
              (eval command-form))
          ;; ensure that all markers are deleted afterwards,
          ;; even in the event of failure
          (dolist (marker markers)
            (set-marker marker nil)))))))

(aiern-define-operator aiern-ex-global-inverted
  (beg end pattern command &optional invert)
  "The Ex vglobal command.
\[BEG,END]vglobal/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (aiern-ex-global beg end pattern command (not invert)))

(aiern-define-operator aiern-ex-normal (beg end commands)
  "The Ex normal command.
Execute the argument as normal command on each line in the
range. The given argument is passed straight to
`execute-kbd-macro'.  The default is the current line."
  :motion aiern-line
  (interactive "<r><a>")
  (aiern-with-single-undo
    (let (markers aiern-ex-current-buffer prefix-arg current-prefix-arg)
      (goto-char beg)
      (while
          (and (< (point) end)
               (progn
                 (push (move-marker (make-marker) (line-beginning-position))
                       markers)
                 (and (= (forward-line) 0) (bolp)))))
      (setq markers (nreverse markers))
      (deactivate-mark)
      (aiern-force-normal-state)
      ;; replace ^[ by escape
      (setq commands
            (vconcat
             (mapcar #'(lambda (ch) (if (equal ch ?) 'escape ch))
                     (append commands nil))))
      (dolist (marker markers)
        (goto-char marker)
        (condition-case nil
            (execute-kbd-macro commands)
          (error nil))
        (aiern-force-normal-state)
        (set-marker marker nil)))))

(aiern-define-command aiern-goto-char (position)
  "Go to POSITION in the buffer.
Default position is the beginning of the buffer."
  :jump t
  (interactive "<N>Goto position: ")
  (let ((position (aiern-normalize-position
                   (or position (point-min)))))
    (goto-char position)))

(aiern-define-operator aiern-ex-line-number (beg end)
  "Print the last line number."
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r>")
  (message "%d" (count-lines (point-min) end)))

(aiern-define-command aiern-show-file-info ()
  "Shows basic file information."
  (let* ((nlines   (count-lines (point-min) (point-max)))
         (curr     (line-number-at-pos (point)))
         (perc     (if (> nlines 0)
                       (format "%d%%" (* (/ (float curr) (float nlines)) 100.0))
                     "No lines in buffer"))
         (file     (buffer-file-name (buffer-base-buffer)))
         (writable (and file (file-writable-p file)))
         (readonly (if (and file (not writable)) "[readonly] " "")))
    (if file
        (message "\"%s\" %d %slines --%s--" file nlines readonly perc)
      (message "%d lines --%s--" nlines perc))))

(defvar sort-fold-case)
(aiern-define-operator aiern-ex-sort (beg end &optional options reverse)
  "The Ex sort command.
\[BEG,END]sort[!] [i][u]
The following additional options are supported:

  * i   ignore case
  * u   remove duplicate lines

The 'bang' argument means to sort in reverse order."
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><a><!>")
  (let ((beg (copy-marker beg))
        (end (copy-marker end))
        sort-fold-case uniq)
    (dolist (opt (append options nil))
      (cond
       ((eq opt ?i) (setq sort-fold-case t))
       ((eq opt ?u) (setq uniq t))
       (t (user-error "Unsupported sort option: %c" opt))))
    (sort-lines reverse beg end)
    (when uniq
      (let (line prev-line)
        (goto-char beg)
        (while (and (< (point) end) (not (eobp)))
          (setq line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
          (if (and (stringp prev-line)
                   (eq t (compare-strings line nil nil
                                          prev-line nil nil
                                          sort-fold-case)))
              (delete-region (progn (forward-line 0) (point))
                             (progn (forward-line 1) (point)))
            (setq prev-line line)
            (forward-line 1)))))
    (goto-char beg)
    (set-marker beg nil)
    (set-marker end nil)))

;;; Window navigation

(defmacro aiern-save-side-windows (&rest body)
  "Toggle side windows, evaluate BODY, restore side windows."
  (declare (indent defun) (debug (&rest form)))
  (let ((sides (make-symbol "sidesvar")))
    `(let ((,sides (and (functionp 'window-toggle-side-windows)
                        (window-with-parameter 'window-side))))
       (when ,sides
         (window-toggle-side-windows))
       (unwind-protect
           (progn ,@body)
         (when ,sides
           (window-toggle-side-windows))))))

(defun aiern-resize-window (new-size &optional horizontal)
  "Set the current window's width or height to NEW-SIZE.
If HORIZONTAL is non-nil the width of the window is changed,
otherwise its height is changed."
  (let ((count (- new-size (if horizontal (window-width) (window-height)))))
    (enlarge-window count horizontal)))

(defun aiern-move-window (side)
  "Move the `selected-window' to SIDE.
The state of the `selected-window' is saved along with the state
of the window tree consisting of all the other windows. Then, all
windows are deleted, the remaining window is split according to
SIDE, the state of the window at SIDE is replaced with the saved
state of the `selected-window', and, finally, the state of the
saved window tree is reconstructed on the opposite side.

SIDE has the same meaning as in `split-window'.

Note, this function only operates on the window tree rooted in
the frame's main window and effectively preserves any side
windows \(i.e. windows with a valid window-side window
parameter\)."
  (aiern-save-side-windows
    (unless (one-window-p)
      (save-excursion
        (let ((w (window-state-get (selected-window))))
          (delete-window)
          (let ((wtree (window-state-get)))
            (delete-other-windows)
            (let ((subwin (selected-window))
                  ;; NOTE: SIDE is new in Emacs 24
                  (newwin (split-window nil nil side)))
              (window-state-put wtree subwin)
              (window-state-put w newwin)
              (select-window newwin)))))
      (balance-windows))))

(defun aiern-alternate-buffer (&optional window)
  "Return the last buffer WINDOW has displayed other than the
current one (equivalent to Vim's alternate buffer).

Returns the first item in `window-prev-buffers' that isn't
`window-buffer' of WINDOW."
  ;; If the last buffer visited has been killed, then `window-prev-buffers'
  ;; returns a list with `current-buffer' at the head, we account for this
  ;; possibility.
  (let* ((prev-buffers (window-prev-buffers))
         (head (car prev-buffers)))
    (if (eq (car head) (window-buffer window))
        (cadr prev-buffers)
      head)))

(aiern-define-command aiern-switch-to-windows-last-buffer ()
  "Switch to current windows last open buffer."
  :repeat nil
  (let ((previous-place (aiern-alternate-buffer)))
    (when previous-place
      (switch-to-buffer (car previous-place))
      (goto-char (car (last previous-place))))))

(aiern-define-command aiern-window-delete ()
  "Deletes the current window.
If `aiern-auto-balance-windows' is non-nil then all children of
the deleted window's parent window are rebalanced."
  (let ((p (window-parent)))
    (delete-window)
    (when aiern-auto-balance-windows
      ;; balance-windows raises an error if the parent does not have
      ;; any further children (then rebalancing is not necessary anyway)
      (condition-case nil
          (balance-windows p)
        (error)))))

(aiern-define-command aiern-window-split (&optional count file)
  "Splits the current window horizontally, COUNT lines height,
editing a certain FILE. The new window will be created below
when `aiern-split-window-below' is non-nil. If COUNT and
`aiern-auto-balance-windows' are both non-nil then all children
of the parent of the splitted window are rebalanced."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if aiern-split-window-below 'above 'below))
  (when (and (not count) aiern-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (aiern-edit file)))

(aiern-define-command aiern-window-vsplit (&optional count file)
  "Splits the current window vertically, COUNT columns width,
editing a certain FILE. The new window will be created to the
right when `aiern-vsplit-window-right' is non-nil. If COUNT and
`aiern-auto-balance-windows'are both non-nil then all children
of the parent of the splitted window are rebalanced."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if aiern-vsplit-window-right 'left 'right))
  (when (and (not count) aiern-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (aiern-edit file)))

(aiern-define-command aiern-split-buffer (buffer)
  "Splits window and switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (aiern-window-split)
  (aiern-buffer buffer))

(aiern-define-command aiern-split-next-buffer (&optional count)
  "Splits the window and goes to the COUNT-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (aiern-window-split)
  (aiern-next-buffer count))

(aiern-define-command aiern-split-prev-buffer (&optional count)
  "Splits window and goes to the COUNT-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (aiern-window-split)
  (aiern-prev-buffer count))

(aiern-define-command aiern-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (_ count)
    (windmove-left)))

(aiern-define-command aiern-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (_ count)
    (windmove-right)))

(aiern-define-command aiern-window-up (count)
  "Move the cursor to new COUNT-th window above the current one."
  :repeat nil
  (interactive "p")
  (dotimes (_ (or count 1))
    (windmove-up)))

(aiern-define-command aiern-window-down (count)
  "Move the cursor to new COUNT-th window below the current one."
  :repeat nil
  (interactive "p")
  (dotimes (_ (or count 1))
    (windmove-down)))

(aiern-define-command aiern-window-bottom-right ()
  "Move the cursor to bottom-right window."
  :repeat nil
  (let ((last-sibling (frame-root-window)))
    (while (and last-sibling (not (window-live-p last-sibling)))
      (setq last-sibling (window-last-child last-sibling)))
    (when last-sibling
      (select-window last-sibling))))

(aiern-define-command aiern-window-top-left ()
  "Move the cursor to top-left window."
  :repeat nil
  (let ((first-child (window-child (frame-root-window))))
    (while (and first-child (not (window-live-p first-child)))
      (setq first-child (window-child first-child)))
    (when first-child
      (select-window
       first-child))))

(aiern-define-command aiern-window-mru ()
  "Move the cursor to the previous (last accessed) buffer in another window.
More precisely, it selects the most recently used buffer that is
shown in some other window, preferably of the current frame, and
is different from the current one."
  :repeat nil
  (catch 'done
    (dolist (buf (buffer-list (selected-frame)))
      (let ((win (get-buffer-window buf)))
        (when (and (not (eq buf (current-buffer)))
                   win
                   (not (eq win (selected-window))))
          (select-window win)
          (throw 'done nil))))))

(aiern-define-command aiern-window-next (count)
  "Move the cursor to the next window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "<c>")
  (if (not count)
      (select-window (next-window))
    (aiern-window-top-left)
    (other-window (1- count))))

(aiern-define-command aiern-window-prev (count)
  "Move the cursor to the previous window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "<c>")
  (if (not count)
      (select-window (previous-window))
    (aiern-window-top-left)
    (other-window (1- count))))

(aiern-define-command aiern-window-new (count file)
  "Splits the current window horizontally
and opens a new buffer or edits a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (let ((new-window (split-window (selected-window) count
                                  (if aiern-split-window-below 'below 'above))))
    (when (and (not count) aiern-auto-balance-windows)
      (balance-windows (window-parent)))
    (if file
        (aiern-edit file)
      (let ((buffer (generate-new-buffer "*new*")))
        (set-window-buffer new-window buffer)
        (select-window new-window)
        (with-current-buffer buffer
          (funcall (default-value 'major-mode)))))))

(aiern-define-command aiern-window-vnew (count file)
  "Splits the current window vertically
and opens a new buffer name or edits a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (let ((new-window (split-window (selected-window) count
                                  (if aiern-vsplit-window-right 'right 'left))))
    (when (and (not count) aiern-auto-balance-windows)
      (balance-windows (window-parent)))
    (if file
        (aiern-edit file)
      (let ((buffer (generate-new-buffer "*new*")))
        (set-window-buffer new-window buffer)
        (select-window new-window)
        (with-current-buffer buffer
          (funcall (default-value 'major-mode)))))))

(aiern-define-command aiern-buffer-new (count file)
  "Creates a new buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (aiern-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))

(aiern-define-command aiern-window-increase-height (count)
  "Increase current window height by COUNT."
  :repeat nil
  (interactive "p")
  (aiern-resize-window (+ (window-height) count)))

(aiern-define-command aiern-window-decrease-height (count)
  "Decrease current window height by COUNT."
  :repeat nil
  (interactive "p")
  (aiern-resize-window (- (window-height) count)))

(aiern-define-command aiern-window-increase-width (count)
  "Increase current window width by COUNT."
  :repeat nil
  (interactive "p")
  (aiern-resize-window (+ (window-width) count) t))

(aiern-define-command aiern-window-decrease-width (count)
  "Decrease current window width by COUNT."
  :repeat nil
  (interactive "p")
  (aiern-resize-window (- (window-width) count) t))

(aiern-define-command aiern-window-set-height (count)
  "Sets the height of the current window to COUNT."
  :repeat nil
  (interactive "<c>")
  (aiern-resize-window (or count (frame-height)) nil))

(aiern-define-command aiern-window-set-width (count)
  "Sets the width of the current window to COUNT."
  :repeat nil
  (interactive "<c>")
  (aiern-resize-window (or count (frame-width)) t))

(aiern-define-command aiern-ex-resize (arg)
  "The ex :resize command.

If ARG is a signed positive integer, increase the current window
height by ARG.

If ARG is a signed negative integer, decrease the current window
height by ARG.

If ARG is a positive integer without explicit sign, set the current
window height to ARG.

If ARG is empty, maximize the current window height."
  (interactive "<a>")
  (if (or (not arg) (= 0 (length arg)))
      (aiern-window-set-height nil)
    (let ((n (string-to-number arg)))
      (if (> n 0)
          (if (= ?+ (aref arg 0))
              (aiern-window-increase-height n)
            (aiern-window-set-height n))
        (aiern-window-decrease-height (- n))))))

(aiern-define-command aiern-window-rotate-upwards ()
  "Rotates the windows according to the current cyclic ordering."
  :repeat nil
  (aiern-save-side-windows
    (let ((wlist (window-list))
          (slist (mapcar #'window-state-get (window-list))))
      (setq slist (append (cdr slist) (list (car slist))))
      (while (and wlist slist)
        (window-state-put (car slist) (car wlist))
        (setq wlist (cdr wlist)
              slist (cdr slist)))
      (select-window (car (last (window-list)))))))

(aiern-define-command aiern-window-rotate-downwards ()
  "Rotates the windows according to the current cyclic ordering."
  :repeat nil
  (aiern-save-side-windows
    (let ((wlist (window-list))
          (slist (mapcar #'window-state-get (window-list))))
      (setq slist (append (last slist) slist))
      (while (and wlist slist)
        (window-state-put (car slist) (car wlist))
        (setq wlist (cdr wlist)
              slist (cdr slist)))
      (select-window (cadr (window-list))))))

(aiern-define-command aiern-window-move-very-top ()
  "Closes the current window, splits the upper-left one horizontally
and redisplays the current buffer there."
  :repeat nil
  (aiern-move-window 'above))

(aiern-define-command aiern-window-move-far-left ()
  "Closes the current window, splits the upper-left one vertically
and redisplays the current buffer there."
  :repeat nil
  (aiern-move-window 'left))

(aiern-define-command aiern-window-move-far-right ()
  "Closes the current window, splits the lower-right one vertically
and redisplays the current buffer there."
  :repeat nil
  (aiern-move-window 'right))

(aiern-define-command aiern-window-move-very-bottom ()
  "Closes the current window, splits the lower-right one horizontally
and redisplays the current buffer there."
  :repeat nil
  (aiern-move-window 'below))

;;; Mouse handling

;; Large parts of this code are taken from mouse.el which is
;; distributed with GNU Emacs
(defun aiern-mouse-drag-region (start-event)
  "Set the region to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.

If the click is in the echo area, display the `*Messages*' buffer.

START-EVENT should be the event that started the drag."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (aiern-mouse-drag-track start-event t))
(aiern-set-command-property 'aiern-mouse-drag-region :keep-visual t)

(defun aiern-mouse-drag-track (start-event &optional
                                          do-mouse-drag-region-post-process)
  "Track mouse drags by highlighting area between point and cursor.
The region will be defined with mark and point.
DO-MOUSE-DRAG-REGION-POST-PROCESS should only be used by
`mouse-drag-region'."
  (mouse-minibuffer-check start-event)
  (setq mouse-selection-click-count-buffer (current-buffer))
  (deactivate-mark)
  (let* ((scroll-margin 0) ; Avoid margin scrolling (Bug#9541).
         (original-window (selected-window))
         ;; We've recorded what we needed from the current buffer and
         ;; window, now let's jump to the place of the event, where things
         ;; are happening.
         (_ (mouse-set-point start-event))
         (echo-keystrokes 0)
         (start-posn (event-start start-event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (start-window-start (window-start start-window))
         (start-hscroll (window-hscroll start-window))
         (bounds (window-edges start-window))
         (make-cursor-line-fully-visible nil)
         (top (nth 1 bounds))
         (bottom (if (or (window-minibuffer-p start-window)
                         (not mode-line-format))
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds))))
         (on-link (and mouse-1-click-follows-link
                       (or mouse-1-click-in-non-selected-windows
                           (eq start-window original-window))
                       ;; Use start-point before the intangibility
                       ;; treatment, in case we click on a link inside an
                       ;; intangible text.
                       (mouse-on-link-p start-posn)))
         (click-count (1- (event-click-count start-event)))
         (remap-double-click (and on-link
                                  (eq mouse-1-click-follows-link 'double)
                                  (= click-count 1)))
         ;; Suppress automatic hscrolling, because that is a nuisance
         ;; when setting point near the right fringe (but see below).
         (auto-hscroll-mode-saved auto-hscroll-mode)
         (auto-hscroll-mode nil)
         event end end-point)

    (setq mouse-selection-click-count click-count)
    ;; In case the down click is in the middle of some intangible text,
    ;; use the end of that text, and put it in START-POINT.
    (if (< (point) start-point)
        (goto-char start-point))
    (setq start-point (point))
    (if remap-double-click
        (setq click-count 0))

    (setq click-count (mod click-count 4))

    ;; activate correct visual state
    (let ((range (aiern-mouse-start-end start-point start-point click-count)))
      (set-mark (nth 0 range))
      (goto-char (nth 1 range)))

    (cond
     ((= click-count 0)
      (when (aiern-visual-state-p) (aiern-exit-visual-state)))
     ((= click-count 1)
      (aiern-visual-char)
      (aiern-visual-post-command))
     ((= click-count 2)
      (aiern-visual-line)
      (aiern-visual-post-command))
     ((= click-count 3)
      (aiern-visual-block)
      (aiern-visual-post-command)))

    ;; Track the mouse until we get a non-movement event.
    (track-mouse
      (while (progn
               (setq event (read-key))
               (or (mouse-movement-p event)
                   (memq (car-safe event) '(switch-frame select-window))))
        (unless (aiern-visual-state-p)
          (cond
           ((= click-count 0) (aiern-visual-char))
           ((= click-count 1) (aiern-visual-char))
           ((= click-count 2) (aiern-visual-line))
           ((= click-count 3) (aiern-visual-block))))

        (aiern-visual-pre-command)
        (unless (memq (car-safe event) '(switch-frame select-window))
          ;; Automatic hscrolling did not occur during the call to
          ;; `read-event'; but if the user subsequently drags the
          ;; mouse, go ahead and hscroll.
          (let ((auto-hscroll-mode auto-hscroll-mode-saved))
            (redisplay))
          (setq end (event-end event)
                end-point (posn-point end))
          (if (and (eq (posn-window end) start-window)
                   (integer-or-marker-p end-point))
              (aiern-mouse--drag-set-mark-and-point start-point
                                                   end-point click-count)
            (let ((mouse-row (cdr (cdr (mouse-position)))))
              (cond
               ((null mouse-row))
               ((< mouse-row top)
                (mouse-scroll-subr start-window (- mouse-row top)
                                   nil start-point))
               ((>= mouse-row bottom)
                (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                   nil start-point))))))
        (aiern-visual-post-command)))

    ;; Handle the terminating event if possible.
    (when (consp event)
      ;; Ensure that point is on the end of the last event.
      (when (and (setq end-point (posn-point (event-end event)))
                 (eq (posn-window end) start-window)
                 (integer-or-marker-p end-point)
                 (/= start-point end-point))
        (aiern-mouse--drag-set-mark-and-point start-point
                                             end-point click-count))

      ;; Find its binding.
      (let* ((fun (key-binding (vector (car event))))
             (do-multi-click (and (> (event-click-count event) 0)
                                  (functionp fun)
                                  (not (memq fun '(mouse-set-point
                                                   mouse-set-region))))))
        (if (and (or (/= (mark) (point))
                     (= click-count 1) ; word selection
                     (and (memq (aiern-visual-type) '(line block))))
                 (not do-multi-click))

            ;; If point has moved, finish the drag.
            (let (last-command this-command)
              (and mouse-drag-copy-region
                   do-mouse-drag-region-post-process
                   (let (deactivate-mark)
                     (aiern-visual-expand-region)
                     (copy-region-as-kill (mark) (point))
                     (aiern-visual-contract-region))))

          ;; If point hasn't moved, run the binding of the
          ;; terminating up-event.
          (if do-multi-click
              (goto-char start-point)
            (deactivate-mark))
          (when (and (functionp fun)
                     (= start-hscroll (window-hscroll start-window))
                     ;; Don't run the up-event handler if the window
                     ;; start changed in a redisplay after the
                     ;; mouse-set-point for the down-mouse event at
                     ;; the beginning of this function.  When the
                     ;; window start has changed, the up-mouse event
                     ;; contains a different position due to the new
                     ;; window contents, and point is set again.
                     (or end-point
                         (= (window-start start-window)
                            start-window-start)))
            (when (and on-link
                       (= start-point (point))
                       (aiern-mouse--remap-link-click-p start-event event))
              ;; If we rebind to mouse-2, reselect previous selected
              ;; window, so that the mouse-2 event runs in the same
              ;; situation as if user had clicked it directly.  Fixes
              ;; the bug reported by juri@jurta.org on 2005-12-27.
              (if (or (vectorp on-link) (stringp on-link))
                  (setq event (aref on-link 0))
                (select-window original-window)
                (setcar event 'mouse-2)
                ;; If this mouse click has never been done by the
                ;; user, it doesn't have the necessary property to be
                ;; interpreted correctly.
                (put 'mouse-2 'event-kind 'mouse-click)))
            (push event unread-command-events)))))))

;; This function is a plain copy of `mouse--drag-set-mark-and-point',
;; which is only available in Emacs 24
(defun aiern-mouse--drag-set-mark-and-point (start click click-count)
  (let* ((range (aiern-mouse-start-end start click click-count))
         (beg (nth 0 range))
         (end (nth 1 range)))
    (cond ((eq (mark) beg)
           (goto-char end))
          ((eq (mark) end)
           (goto-char beg))
          ((< click (mark))
           (set-mark end)
           (goto-char beg))
          (t
           (set-mark beg)
           (goto-char end)))))

;; This function is a plain copy of `mouse--remap-link-click-p',
;; which is only available in Emacs 23
(defun aiern-mouse--remap-link-click-p (start-event end-event)
  (or (and (eq mouse-1-click-follows-link 'double)
           (= (event-click-count start-event) 2))
      (and
       (not (eq mouse-1-click-follows-link 'double))
       (= (event-click-count start-event) 1)
       (= (event-click-count end-event) 1)
       (or (not (integerp mouse-1-click-follows-link))
           (let ((t0 (posn-timestamp (event-start start-event)))
                 (t1 (posn-timestamp (event-end   end-event))))
             (and (integerp t0) (integerp t1)
                  (if (> mouse-1-click-follows-link 0)
                      (<= (- t1 t0) mouse-1-click-follows-link)
                    (< (- t0 t1) mouse-1-click-follows-link))))))))

(defun aiern-mouse-start-end (start end mode)
  "Return a list of region bounds based on START and END according to MODE.
If MODE is not 1 then set point to (min START END), mark to (max
START END).  If MODE is 1 then set point to start of word at (min
START END), mark to end of word at (max START END)."
  (aiern-sort start end)
  (setq mode (mod mode 4))
  (if (/= mode 1) (list start end)
    (list
     (save-excursion
       (goto-char (min (point-max) (1+ start)))
       (if (zerop (forward-thing aiern-mouse-word -1))
           (let ((bpnt (point)))
             (forward-thing aiern-mouse-word +1)
             (if (> (point) start) bpnt (point)))
         (point-min)))
     (save-excursion
       (goto-char end)
       (1-
        (if (zerop (forward-thing aiern-mouse-word +1))
            (let ((epnt (point)))
              (forward-thing aiern-mouse-word -1)
              (if (<= (point) end) epnt (point)))
          (point-max)))))))

;;; State switching

(aiern-define-command aiern-exit-emacs-state (&optional buffer message)
  "Exit Emacs state.
Changes the state to the previous state, or to Normal state
if the previous state was Emacs state."
  :keep-visual t
  :suppress-operator t
  (interactive '(nil t))
  (with-current-buffer (or buffer (current-buffer))
    (when (aiern-emacs-state-p)
      (aiern-change-to-previous-state buffer message)
      (when (aiern-emacs-state-p)
        (aiern-normal-state (and message 1))))))

(aiern-define-local-var aiern--execute-normal-eol-pos nil
  "Vim has special behaviour for executing in normal state at eol.
This var stores the eol position, so it can be restored when necessary.")

(defun aiern--restore-repeat-hooks ()
  "No insert-state repeat info is recorded after executing in normal state.
Restore the disabled repeat hooks on insert-state exit."
  (aiern-repeat-stop)
  (add-hook 'pre-command-hook 'aiern-repeat-pre-hook)
  (add-hook 'post-command-hook 'aiern-repeat-post-hook)
  (remove-hook 'aiern-insert-state-exit-hook 'aiern--restore-repeat-hooks))

(defvar aiern--execute-normal-return-state nil
  "The state to return to after executing in normal state.")

(defun aiern-execute-in-normal-state ()
  "Execute the next command in Normal state."
  (interactive)
  (aiern-delay '(not (memq this-command
                          '(nil
                            aiern-execute-in-normal-state
                            aiern-replace-state
                            aiern-use-register
                            digit-argument
                            negative-argument
                            universal-argument
                            universal-argument-minus
                            universal-argument-more
                            universal-argument-other-key)))
      `(progn
         (with-current-buffer ,(current-buffer)
           (when (and aiern--execute-normal-eol-pos
                      (= (point) (1- aiern--execute-normal-eol-pos))
                      (not (memq this-command '(aiern-insert
                                                aiern-goto-mark))))
             (forward-char))
           (unless (eq 'replace aiern-state)
             (aiern-change-state ',aiern-state))
           (when (eq 'insert aiern-state)
             (remove-hook 'pre-command-hook 'aiern-repeat-pre-hook)
             (remove-hook 'post-command-hook 'aiern-repeat-post-hook)
             (add-hook 'aiern-insert-state-exit-hook 'aiern--restore-repeat-hooks))
           (setq aiern-move-cursor-back ',aiern-move-cursor-back
                 aiern-move-beyond-eol ',aiern-move-beyond-eol)))
    'post-command-hook)
  (setq aiern-insert-count nil
        aiern--execute-normal-return-state aiern-state
        aiern--execute-normal-eol-pos (when (eolp) (point))
        aiern-move-cursor-back nil)
  (aiern-normal-state)
  (setq aiern-move-beyond-eol t)
  (aiern-echo "Switched to Normal state for the next command ..."))

(defun aiern-stop-execute-in-emacs-state ()
  (when (and (not (eq this-command #'aiern-execute-in-emacs-state))
             (not (minibufferp)))
    (remove-hook 'post-command-hook 'aiern-stop-execute-in-emacs-state)
    (when (buffer-live-p aiern-execute-in-emacs-state-buffer)
      (with-current-buffer aiern-execute-in-emacs-state-buffer
        (if (and (eq aiern-previous-state 'visual)
                 (not (use-region-p)))
            (progn
              (aiern-change-to-previous-state)
              (aiern-exit-visual-state))
          (aiern-change-to-previous-state))))
    (setq aiern-execute-in-emacs-state-buffer nil)))

(aiern-define-command aiern-execute-in-emacs-state ()
  "Execute the next command in Emacs state."
  (add-hook 'post-command-hook #'aiern-stop-execute-in-emacs-state t)
  (setq aiern-execute-in-emacs-state-buffer (current-buffer))
  (cond
   ((aiern-visual-state-p)
    (let ((mrk (mark))
          (pnt (point)))
      (aiern-emacs-state)
      (set-mark mrk)
      (goto-char pnt)))
   (t
    (aiern-emacs-state)))
  (aiern-echo "Switched to Emacs state for the next command ..."))

(defun aiern-exit-visual-and-repeat (event)
  "Exit insert state and repeat event.
This special command should be used if some command called from
visual state should actually be called in normal-state.  The main
reason for doing this is that the repeat system should *not*
record the visual state information for some command.  This
command should be bound to exactly the same event in visual state
as the original command is bound in normal state.  EVENT is the
event that triggered the execution of this command."
  (interactive "e")
  (when (aiern-visual-state-p)
    (aiern-exit-visual-state)
    (push event unread-command-events)))
(aiern-declare-ignore-repeat 'aiern-exit-visual-and-repeat)

(provide 'aiern-commands)

;;; aiern-commands.el ends here
