;;; aiern-search.el --- Search and substitute -*- lexical-binding: t -*-

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
(require 'aiern-common)
(require 'aiern-ex)

;;; Code:

(defun aiern-select-search-module (option module)
  "Change the search module according to MODULE.
If MODULE is `isearch', then Emacs' isearch module is used.
If MODULE is `aiern-search', then aiern's own interactive
search module is used."
  (let ((search-functions
         '(forward
           backward
           word-forward
           word-backward
           unbounded-word-forward
           unbounded-word-backward
           next
           previous)))
    (dolist (fun search-functions)
      (let ((isearch (intern (format "aiern-search-%s" fun)))
            (aiern-search (intern (format "aiern-ex-search-%s" fun))))
        (if (eq module 'isearch)
            (substitute-key-definition
             aiern-search isearch aiern-motion-state-map)
          (substitute-key-definition
           isearch aiern-search aiern-motion-state-map)))))
  (set-default option module))

;; this customization is here because it requires
;; the knowledge of `aiern-select-search-mode'
(defcustom aiern-search-module 'isearch
  "The search module to be used.  May be either `isearch', for
Emacs' isearch module, or `aiern-search', for aiern's own
interactive search module."
  :type '(radio (const :tag "Emacs built-in isearch." :value isearch)
                (const :tag "aiern interactive search." :value aiern-search))
  :group 'aiern
  :set 'aiern-select-search-module
  :initialize 'aiern-custom-initialize-pending-reset)

(defun aiern-push-search-history (string forward)
  "Push STRING into the appropriate search history (determined by FORWARD)."
  (let* ((history-var (if forward
                          'aiern-search-forward-history
                        'aiern-search-backward-history))
         (history (symbol-value history-var)))
    (unless (equal (car-safe history) string)
      (set history-var (cons string history)))))

(defun aiern-search-incrementally (forward regexp-p)
  "Search incrementally for user-entered text."
  (let ((aiern-search-prompt (aiern-search-prompt forward))
        (isearch-search-fun-function 'aiern-isearch-function)
        (point (point))
        search-nonincremental-instead)
    (setq isearch-forward forward)
    (aiern-save-echo-area
      (aiern-without-input-method-hooks
       ;; set the input method locally rather than globally to ensure that
       ;; isearch clears the input method when it's finished
       (setq current-input-method aiern-input-method)
       (if forward
           (isearch-forward regexp-p)
         (isearch-backward regexp-p))
       (aiern-push-search-history isearch-string forward)
       (setq current-input-method nil))
      (when (/= (point) point)
        ;; position the point at beginning of the match only if the call to
        ;; `isearch' has really moved the point. `isearch' doesn't move the
        ;; point only if "C-g" is hit twice to exit the search, in which case we
        ;; shouldn't move the point either.
        (when (and forward isearch-other-end)
          (goto-char isearch-other-end))
        (when (and (eq point (point))
                   (not (string= isearch-string "")))
          (if forward
              (isearch-repeat-forward)
            (isearch-repeat-backward))
          (isearch-exit)
          (when (and forward isearch-other-end)
            (goto-char isearch-other-end)))
        (aiern-flash-search-pattern
         (aiern-search-message isearch-string forward))))))

(defun aiern-flash-search-pattern (string &optional all)
  "Flash last search matches for duration of `aiern-flash-delay'.
If ALL is non-nil, flash all matches. STRING is a message
to display in the echo area."
  (let ((lazy-highlight-initial-delay 0)
        (isearch-search-fun-function 'aiern-isearch-function)
        (isearch-case-fold-search case-fold-search)
        (disable #'(lambda (&optional _arg) (aiern-flash-hook t))))
    (when aiern-flash-timer
      (cancel-timer aiern-flash-timer))
    (unless (or (null string)
                (string= string ""))
      (aiern-echo-area-save)
      (aiern-echo "%s" string)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (when all
        (setq isearch-lazy-highlight-wrapped nil
              isearch-lazy-highlight-start (point)
              isearch-lazy-highlight-end (point))
        (isearch-lazy-highlight-new-loop)
        (unless isearch-lazy-highlight-overlays
          (isearch-lazy-highlight-update)))
      (add-hook 'pre-command-hook #'aiern-flash-hook nil t)
      (add-hook 'aiern-operator-state-exit-hook #'aiern-flash-hook nil t)
      (add-hook 'pre-command-hook #'aiern-clean-isearch-overlays nil t)
      (setq aiern-flash-timer
            (run-at-time aiern-flash-delay nil disable)))))

(defun aiern-clean-isearch-overlays ()
  "Clean isearch overlays unless `this-command' is search."
  (remove-hook 'pre-command-hook #'aiern-clean-isearch-overlays t)
  (unless (memq this-command
                '(aiern-search-backward
                  aiern-search-forward
                  aiern-search-next
                  aiern-search-previous
                  aiern-search-word-backward
                  aiern-search-word-forward))
    (isearch-clean-overlays)))
(put 'aiern-clean-isearch-overlays 'permanent-local-hook t)

(defun aiern-flash-hook (&optional force)
  "Disable hightlighting if `this-command' is not search.
Disable anyway if FORCE is t."
  (when (or force
            ;; to avoid flicker, don't disable highlighting
            ;; if the next command is also a search command
            (not (memq this-command
                       '(aiern-search-backward
                         aiern-search-forward
                         aiern-search-next
                         aiern-search-previous
                         aiern-search-word-backward
                         aiern-search-word-forward))))
    (aiern-echo-area-restore)
    (isearch-dehighlight)
    (setq isearch-lazy-highlight-last-string nil)
    (lazy-highlight-cleanup t)
    (when aiern-flash-timer
      (cancel-timer aiern-flash-timer)))
  (remove-hook 'pre-command-hook #'aiern-flash-hook t)
  (remove-hook 'aiern-operator-state-exit-hook #'aiern-flash-hook t))
(put 'aiern-flash-hook 'permanent-local-hook t)

(defun aiern-search-with-predicate (search-fun pred string bound noerror count)
  "Execute a search with a predicate function.
SEARCH-FUN is a search function (e.g. `re-search-forward') and
PREDICATE is a two-argument function satisfying the interface of
`isearch-filter-predicate', or `nil'.  STRING, BOUND, NOERROR and
COUNT are passed unchanged to SEARCH-FUN.  The first match
satisfying the predicate (or `nil') is returned."
  (catch 'done
    (while t
      (let ((result (funcall search-fun string bound noerror count)))
        (cond
         ((not result) (throw 'done nil))
         ((not pred) (throw 'done result))
         ((funcall pred (match-beginning 0) (match-end 0)) (throw 'done result)))))))

(defun aiern-search-function (&optional forward regexp-p wrap predicate)
  "Return a search function.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, the input is a regular expression.
If WRAP is non-nil, the search wraps around the top or bottom
of the buffer.
If PREDICATE is non-nil, it must be a function accepting two
arguments: the bounds of a match, returning non-nil if that match is
acceptable."
  `(lambda (string &optional bound noerror count)
     (let ((start (point))
           (search-fun ',(if regexp-p
                             (if forward
                                 're-search-forward
                               're-search-backward)
                           (if forward
                               'search-forward
                             'search-backward)))
           result)
       (setq result (aiern-search-with-predicate
                     search-fun ,predicate string
                     bound ,(if wrap t 'noerror) count))
       (when (and ,wrap (null result))
         (goto-char ,(if forward '(point-min) '(point-max)))
         (unwind-protect
             (setq result (aiern-search-with-predicate
                           search-fun ,predicate string bound noerror count))
           (unless result
             (goto-char start))))
       result)))

(defun aiern-isearch-function ()
  "Return a search function for use with isearch.
Based on `isearch-regexp' and `isearch-forward'."
  (aiern-search-function isearch-forward aiern-regexp-search aiern-search-wrap 'isearch-filter-predicate))

(defun aiern-search (string forward &optional regexp-p start)
  "Search for STRING and highlight matches.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, STRING is taken to be a regular expression.
START is the position to search from; if unspecified, it is
one more than the current position."
  (when (and (stringp string)
             (not (string= string "")))
    (let* ((orig (point))
           (start (or start
                      (if forward
                          (min (point-max) (1+ orig))
                        orig)))
           (isearch-regexp regexp-p)
           (isearch-forward forward)
           (case-fold-search
            (unless (and search-upper-case
                         (not (isearch-no-upper-case-p string nil)))
              case-fold-search))
           (search-func (aiern-search-function
                         forward regexp-p aiern-search-wrap 'isearch-filter-predicate)))
      ;; no text properties, thank you very much
      (set-text-properties 0 (length string) nil string)
      ;; position to search from
      (goto-char start)
      (setq isearch-string string)
      (isearch-update-ring string regexp-p)
      (condition-case nil
          (funcall search-func string)
        (search-failed
         (goto-char orig)
         (user-error "\"%s\": %s not found"
                     string (if regexp-p "pattern" "string"))))
      ;; always position point at the beginning of the match
      (goto-char (match-beginning 0))
      ;; determine message for echo area
      (cond
       ((and forward (< (point) start))
        (setq string "Search wrapped around BOTTOM of buffer"))
       ((and (not forward) (> (point) start))
        (setq string "Search wrapped around TOP of buffer"))
       (t
        (setq string (aiern-search-message string forward))))
      (aiern-flash-search-pattern string t))))

(defun aiern-search-word (forward unbounded symbol)
  "Search for word near point.
If FORWARD is nil, search backward, otherwise forward. If SYMBOL
is non-nil then the functions searches for the symbol at point,
otherwise for the word at point."
  (let ((string (car-safe regexp-search-ring)))
    (setq isearch-forward forward)
    (cond
     ((and (memq last-command
                 '(aiern-search-word-forward
                   aiern-search-word-backward))
           (stringp string)
           (not (string= string "")))
      (aiern-search string forward t))
     (t
      (setq string (aiern-find-thing forward (if symbol 'symbol 'aiern-word)))
      (cond
       ((null string)
        (user-error "No word under point"))
       (unbounded
        (setq string (regexp-quote string)))
       (t
        (setq string
              (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                      (regexp-quote string)))))
      (aiern-push-search-history string forward)
      (aiern-search string forward t)))))

(defun aiern--find-thing (forward thing)
  "Return a cons of THING near point as a string and its position.
THING should be a symbol understood by `thing-at-point',
e.g. 'symbol or 'word.  If FORWARD is nil, search backward,
otherwise forward.  Returns nil if nothing is found."
  (let ((move (if forward #'forward-char #'backward-char))
        (end (if forward #'eobp #'bobp))
        string)
    (save-excursion
      (setq string (thing-at-point thing))
      ;; if there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (null string) (not (funcall end)))
        (funcall move)
        (setq string (thing-at-point thing)))
      (when (stringp string)
        (set-text-properties 0 (length string) nil string))
      (when (> (length string) 0)
        (cons string (point))))))

(defun aiern-find-thing (forward thing)
  "Return a THING near point as a string.
THING should be a symbol understood by `thing-at-point',
e.g. 'symbol or 'word.  If FORWARD is nil, search backward,
otherwise forward.  Returns nil if nothing is found."
  (car (aiern--find-thing forward thing)))

(defun aiern-find-word (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (aiern-find-thing forward 'word))

(defun aiern-find-symbol (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (aiern-find-thing forward 'symbol))

(defun aiern-search-prompt (forward)
  "Return the search prompt for the given direction."
  (if forward "/" "?"))

(defun aiern-search-message (string forward)
  "Prefix STRING with the search prompt."
  (format "%s%s" (aiern-search-prompt forward) string))

(defadvice isearch-message-prefix (around aiern activate)
  "Use `aiern-search-prompt'."
  (if aiern-search-prompt
      (setq ad-return-value aiern-search-prompt)
    ad-do-it))

(defadvice isearch-delete-char (around aiern activate)
  "Exit search if no search string."
  (cond
   ((and aiern-search-prompt (string= isearch-string ""))
    (let (search-nonincremental-instead)
      (setq isearch-success nil)
      (isearch-exit)))
   (t
    ad-do-it)))

(defadvice isearch-lazy-highlight-search (around aiern activate)
  "Never wrap the search in this context."
  (let (aiern-search-wrap)
    ad-do-it))

;;; Ex search

(defun aiern-ex-regex-without-case (re)
  "Return the regular expression without all occurrences of \\c and \\C."
  (aiern-transform-regexp re '((?c . "") (?C . ""))))

(defun aiern-ex-regex-case (re default-case)
  "Return the case as implied by \\c or \\C in regular expression RE.
If \\c appears anywhere in the pattern, the pattern is case
insensitive. If \\C appears, the pattern is case sensitive.
Only the first occurrence of \\c or \\C is used, all others are
ignored. If neither \\c nor \\C appears in the pattern, the case
specified by DEFAULT-CASE is used. DEFAULT-CASE should be either
`sensitive', `insensitive' or `smart'. In the latter case, the pattern
will be case-sensitive if and only if it contains an upper-case
letter, otherwise it will be case-insensitive."
  (cond
   ((string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\\\\\([cC]\\)" re)
    (if (eq (aref (match-string 1 re) 0) ?c) 'insensitive 'sensitive))
   ((eq default-case 'smart)
    (if (isearch-no-upper-case-p re t)
        'insensitive
      'sensitive))
   (t default-case)))

;; a pattern
(defun aiern-ex-make-substitute-pattern (regexp flags)
  "Creates a PATTERN for substitution with FLAGS.
This function respects the values of `aiern-ex-substitute-case'
and `aiern-ex-substitute-global'."
  (aiern-ex-make-pattern regexp
                        (cond
                         ((memq ?i flags) 'insensitive)
                         ((memq ?I flags) 'sensitive)
                         ((not aiern-ex-substitute-case)
                          aiern-ex-search-case)
                         (t aiern-ex-substitute-case))
                        (or (and aiern-ex-substitute-global
                                 (not (memq ?g flags)))
                            (and (not aiern-ex-substitute-global)
                                 (memq ?g flags)))))

(defun aiern-ex-make-search-pattern (regexp)
  "Creates a PATTERN for search.
This function respects the values of `aiern-ex-search-case'."
  (aiern-ex-make-pattern regexp aiern-ex-search-case t))

(defun aiern-ex-make-pattern (regexp case whole-line)
  "Create a new search pattern.
REGEXP is the regular expression to be searched for. CASE should
be either 'sensitive, 'insensitive for case-sensitive and
case-insensitive search, respectively, or anything else.  In the
latter case the pattern is smart-case, i.e. it is automatically
sensitive of the pattern contains one upper case letter,
otherwise it is insensitive.  The input REGEXP is considered a
Vim-style regular expression if `aiern-ex-search-vim-style-regexp'
is non-nil, in which case it is transformed to an Emacs style
regular expression (i.e. certain backslash-codes are
transformed. Otherwise REGEXP must be an Emacs style regular
expression and is not transformed."
  (let ((re (aiern-ex-regex-without-case regexp))
        (ignore-case (eq (aiern-ex-regex-case regexp case) 'insensitive)))
    ;; possibly transform regular expression from vim-style to
    ;; Emacs-style.
    (if aiern-ex-search-vim-style-regexp
        (setq re (aiern-transform-vim-style-regexp re))
      ;; Even for Emacs regular expressions we translate certain
      ;; whitespace sequences
      (setq re (aiern-transform-regexp re
                                      '((?t . "\t")
                                        (?n . "\n")
                                        (?r . "\r")))))
    (list re ignore-case whole-line)))

(defun aiern-ex-pattern-regex (pattern)
  "Return the regular expression of a search PATTERN."
  (nth 0 pattern))

(defun aiern-ex-pattern-ignore-case (pattern)
  "Return t if and only if PATTERN should ignore case."
  (nth 1 pattern))

(defun aiern-ex-pattern-whole-line (pattern)
  "Return t if and only if PATTERN should match all occurences of a line.
Otherwise PATTERN matches only the first occurence."
  (nth 2 pattern))

;; Highlight
(defun aiern-ex-make-hl (name &rest args)
  "Create a new highlight object with name NAME and properties ARGS.
The following properties are supported:
:face The face to be used for the highlighting overlays.
:win The window in which the highlighting should be shown.
     Note that the highlight will be visible in all windows showing
     the corresponding buffer, but only the matches visible in the
     specified window will actually be highlighted. If :win is nil,
     the matches in all windows will be highlighted.
:min The minimal buffer position for highlighted matches.
:max The maximal buffer position for highlighted matches.
:match-hook A hook to be called once for each highlight.
            The hook must take two arguments, the highlight and
            the overlay for that highlight.
:update-hook A hook called once after updating the highlighting
             with two arguments, the highlight and a message string
             describing the current match status."
  (unless (symbolp name)
    (user-error "Expected symbol as name of highlight"))
  (let ((face 'aiern-ex-lazy-highlight)
        (win (selected-window))
        min max match-hook update-hook)
    (while args
      (let ((key (pop args))
            (val (pop args)))
        (cond
         ((eq key :face) (setq face val))
         ((eq key :win)  (setq win val))
         ((eq key :min)  (setq min val))
         ((eq key :max)  (setq max val))
         ((eq key :match-hook) (setq match-hook val))
         ((eq key :update-hook) (setq update-hook val))
         (t (user-error "Unexpected keyword: %s" key)))))
    (when (assoc name aiern-ex-active-highlights-alist)
      (aiern-ex-delete-hl name))
    (when (null aiern-ex-active-highlights-alist)
      (add-hook 'window-scroll-functions
                #'aiern-ex-hl-update-highlights-scroll nil t)
      (add-hook 'window-size-change-functions
                #'aiern-ex-hl-update-highlights-resize nil))
    (push (cons name (vector name
                             nil
                             face
                             win
                             min
                             max
                             match-hook
                             update-hook
                             nil))
          aiern-ex-active-highlights-alist)))

(defun aiern-ex-hl-name (hl)
  "Return the name of the highlight HL."
  (aref hl 0))

(defun aiern-ex-hl-pattern (hl)
  "Return the pattern of the highlight HL."
  (aref hl 1))

(defun aiern-ex-hl-set-pattern (hl pattern)
  "Set the pattern of the highlight HL to PATTERN."
  (aset hl 1 pattern))

(defun aiern-ex-hl-face (hl)
  "Return the face of the highlight HL."
  (aref hl 2))

(defun aiern-ex-hl-window (hl)
  "Return the window of the highlight HL."
  (aref hl 3))

(defun aiern-ex-hl-min (hl)
  "Return the minimal buffer position of the highlight HL."
  (aref hl 4))

(defun aiern-ex-hl-set-min (hl min)
  "Set the minimal buffer position of the highlight HL to MIN."
  (aset hl 4 min))

(defun aiern-ex-hl-max (hl)
  "Return the maximal buffer position of the highlight HL."
  (aref hl 5))

(defun aiern-ex-hl-set-max (hl max)
  "Set the minimal buffer position of the highlight HL to MAX."
  (aset hl 5 max))

(defun aiern-ex-hl-match-hook (hl)
  "Return the match-hook of the highlight HL."
  (aref hl 6))

(defun aiern-ex-hl-update-hook (hl)
  "Return the update-hook of the highlight HL."
  (aref hl 7))

(defun aiern-ex-hl-overlays (hl)
  "Return the list of active overlays of the highlight HL."
  (aref hl 8))

(defun aiern-ex-hl-set-overlays (hl overlays)
  "Set the list of active overlays of the highlight HL to OVERLAYS."
  (aset hl 8 overlays))

(defun aiern-ex-delete-hl (name)
  "Remove the highlighting object with a certain NAME."
  (let ((hl (cdr-safe (assoc name aiern-ex-active-highlights-alist))))
    (when hl
      (mapc #'delete-overlay (aiern-ex-hl-overlays hl))
      (setq aiern-ex-active-highlights-alist
            (assq-delete-all name aiern-ex-active-highlights-alist))
      (aiern-ex-hl-update-highlights))
    (when (null aiern-ex-active-highlights-alist)
      (remove-hook 'window-scroll-functions
                   #'aiern-ex-hl-update-highlights-scroll t)
      (remove-hook 'window-size-change-functions
                   #'aiern-ex-hl-update-highlights-resize))))

(defun aiern-ex-hl-active-p (name)
  "Whether the highlight with a certain NAME is active."
  (and (assoc name aiern-ex-active-highlights-alist) t))

(defun aiern-ex-hl-change (name pattern)
  "Set the regular expression of highlight NAME to PATTERN."
  (let ((hl (cdr-safe (assoc name aiern-ex-active-highlights-alist))))
    (when hl
      (aiern-ex-hl-set-pattern hl
                              (if (zerop (length pattern))
                                  nil
                                pattern))
      (aiern-ex-hl-idle-update))))

(defun aiern-ex-hl-set-region (name beg end &optional _type)
  "Set minimal and maximal position of highlight NAME to BEG and END."
  (let ((hl (cdr-safe (assoc name aiern-ex-active-highlights-alist))))
    (when hl
      (aiern-ex-hl-set-min hl beg)
      (aiern-ex-hl-set-max hl end)
      (aiern-ex-hl-idle-update))))

(defun aiern-ex-hl-get-max (name)
  "Return the maximal position of the highlight with name NAME."
  (let ((hl (cdr-safe (assoc name aiern-ex-active-highlights-alist))))
    (and hl (aiern-ex-hl-max hl))))

(defun aiern-ex-hl-update-highlights ()
  "Update the overlays of all active highlights."
  (dolist (hl (mapcar #'cdr aiern-ex-active-highlights-alist))
    (let* ((old-ovs (aiern-ex-hl-overlays hl))
           new-ovs
           (pattern (aiern-ex-hl-pattern hl))
           (case-fold-search (aiern-ex-pattern-ignore-case pattern))
           (case-replace case-fold-search)
           (face (aiern-ex-hl-face hl))
           (match-hook (aiern-ex-hl-match-hook hl))
           result)
      (if pattern
          ;; collect all visible ranges
          (let (ranges sranges)
            (dolist (win (if (eq aiern-ex-interactive-search-highlight
                                 'all-windows)
                             (get-buffer-window-list (current-buffer) nil t)
                           (list (aiern-ex-hl-window hl))))
              (when (window-live-p win)
                (let ((beg (max (window-start win)
                                (or (aiern-ex-hl-min hl) (point-min))))
                      (end (min (window-end win)
                                (or (aiern-ex-hl-max hl) (point-max)))))
                  (when (< beg end)
                    (push (cons beg end) ranges)))))
            (setq ranges
                  (sort ranges #'(lambda (r1 r2) (< (car r1) (car r2)))))
            (while ranges
              (let ((r1 (pop ranges))
                    (r2 (pop ranges)))
                (cond
                 ;; last range
                 ((null r2)
                  (push r1 sranges))
                 ;; ranges overlap, union
                 ((>= (cdr r1) (car r2))
                  (push (cons (car r1)
                              (max (cdr r1) (cdr r2)))
                        ranges))
                 ;; ranges distinct
                 (t
                  (push r1 sranges)
                  (push r2 ranges)))))

            ;; run through all ranges
            (condition-case lossage
                (save-match-data
                  (dolist (r sranges)
                    (let ((beg (car r))
                          (end (cdr r)))
                      (save-excursion
                        (goto-char beg)
                        ;; set the overlays for the current highlight,
                        ;; reusing old overlays (if possible)
                        (while (and (not (eobp))
                                    (aiern-ex-search-find-next-pattern pattern)
                                    (<= (match-end 0) end)
                                    (not (and (= (match-end 0) end)
                                              (string= (aiern-ex-pattern-regex pattern)
                                                       "^"))))
                          (let ((ov (or (pop old-ovs) (make-overlay 0 0))))
                            (move-overlay ov (match-beginning 0) (match-end 0))
                            (overlay-put ov 'face face)
                            (overlay-put ov 'aiern-ex-hl (aiern-ex-hl-name hl))
                            (overlay-put ov 'priority 1000)
                            (push ov new-ovs)
                            (when match-hook (funcall match-hook hl ov)))
                          (cond
                           ((and (not (aiern-ex-pattern-whole-line pattern))
                                 (not (string-match-p "\n" (buffer-substring-no-properties
                                                            (match-beginning 0)
                                                            (match-end 0)))))
                            (forward-line))
                           ((= (match-beginning 0) (match-end 0))
                            (forward-char))
                           (t (goto-char (match-end 0))))))))
                  (mapc #'delete-overlay old-ovs)
                  (aiern-ex-hl-set-overlays hl new-ovs)
                  (if (or (null pattern) new-ovs)
                      (setq result t)
                    ;; Maybe the match could just not be found somewhere else?
                    (save-excursion
                      (goto-char (or (aiern-ex-hl-min hl) (point-min)))
                      (if (and (aiern-ex-search-find-next-pattern pattern)
                               (< (match-end 0) (or (aiern-ex-hl-max hl)
                                                    (point-max))))
                          (setq result (format "Match in line %d"
                                               (line-number-at-pos
                                                (match-beginning 0))))
                        (setq result "No match")))))

              (invalid-regexp
               (setq result (cadr lossage)))

              (search-failed
               (setq result (nth 2 lossage)))

              (error
               (setq result (format "%s" (cadr lossage))))

              (user-error
               (setq result (format "%s" (cadr lossage))))))
        ;; no pattern, remove all highlights
        (mapc #'delete-overlay old-ovs)
        (aiern-ex-hl-set-overlays hl new-ovs))
      (when (aiern-ex-hl-update-hook hl)
        (funcall (aiern-ex-hl-update-hook hl) hl result)))))

(defun aiern-ex-search-find-next-pattern (pattern &optional direction)
  "Look for the next occurrence of PATTERN in a certain DIRECTION.
Note that this function ignores the whole-line property of PATTERN."
  (setq direction (or direction 'forward))
  (let ((case-fold-search (aiern-ex-pattern-ignore-case pattern)))
    (cond
     ((eq direction 'forward)
      (re-search-forward (aiern-ex-pattern-regex pattern) nil t))
     ((eq direction 'backward)
      (let* ((pnt (point))
             (ret (re-search-backward (aiern-ex-pattern-regex pattern) nil t))
             (m (and ret (match-data))))
        (if ret
            (forward-char)
          (goto-char (point-min)))
        (let ((fwdret
               (re-search-forward (aiern-ex-pattern-regex pattern) nil t)))
          (cond
           ((and fwdret (< (match-beginning 0) pnt))
            (setq ret fwdret)
            (goto-char (match-beginning 0)))
           (ret
            (set-match-data m)
            (goto-char (match-beginning 0)))
           (t
            (goto-char pnt)
            ret)))))
     (t
      (user-error "Unknown search direction: %s" direction)))))

(defun aiern-ex-hl-idle-update ()
  "Triggers the timer to update the highlights in the current buffer."
  (when (and aiern-ex-interactive-search-highlight
             aiern-ex-active-highlights-alist)
    (when aiern-ex-hl-update-timer
      (cancel-timer aiern-ex-hl-update-timer))
    (setq aiern-ex-hl-update-timer
          (run-at-time aiern-ex-hl-update-delay nil
                       #'aiern-ex-hl-do-update-highlight
                       (current-buffer)))))

(defun aiern-ex-hl-do-update-highlight (&optional buffer)
  "Timer function for updating the highlights."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (aiern-ex-hl-update-highlights)))
  (setq aiern-ex-hl-update-timer nil))

(defun aiern-ex-hl-update-highlights-scroll (win _beg)
  "Update highlights after scrolling in some window."
  (with-current-buffer (window-buffer win)
    (aiern-ex-hl-idle-update)))
(put 'aiern-ex-hl-update-highlights-scroll 'permanent-local-hook t)

(defun aiern-ex-hl-update-highlights-resize (frame)
  "Update highlights after resizing a window."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (aiern-ex-hl-idle-update)))))
(put 'aiern-ex-hl-update-highlights-resize 'permanent-local-hook t)

;; interactive search
(defun aiern-ex-search-activate-highlight (pattern)
  "Activate highlighting of the search pattern set to PATTERN.
This function does nothing if `aiern-ex-search-interactive' or
`aiern-ex-search-highlight-all' is nil. "
  (when (and aiern-ex-search-interactive aiern-ex-search-highlight-all)
    (with-current-buffer (or aiern-ex-current-buffer (current-buffer))
      (unless (aiern-ex-hl-active-p 'aiern-ex-search)
        (aiern-ex-make-hl 'aiern-ex-search
                         :win (or (minibuffer-selected-window) (selected-window))))
      (if pattern
          (aiern-ex-hl-change 'aiern-ex-search pattern)))))

(defun aiern-ex-search (&optional count)
  "Search forward or backward COUNT times for the current ex search pattern.
The search pattern is determined by `aiern-ex-search-pattern' and
the direcion is determined by `aiern-ex-search-direction'."
  (setq aiern-ex-search-start-point (point)
        aiern-ex-last-was-search t
        count (or count 1))
  (let ((orig (point))
        wrapped)
    (dotimes (_ (or count 1))
      (when (eq aiern-ex-search-direction 'forward)
        (unless (eobp) (forward-char))
        ;; maybe skip end-of-line
        (when (and (not aiern-move-beyond-eol) (eolp) (not (eobp)))
          (forward-char)))
      (let ((res (aiern-ex-find-next nil nil (not aiern-search-wrap))))
        (cond
         ((not res)
          (goto-char orig)
          (signal 'search-failed
                  (list (aiern-ex-pattern-regex aiern-ex-search-pattern))))
         ((eq res 'wrapped) (setq wrapped t)))))
    (if wrapped
        (let (message-log-max)
          (message "Search wrapped")))
    (goto-char (match-beginning 0))
    (setq aiern-ex-search-match-beg (match-beginning 0)
          aiern-ex-search-match-end (match-end 0))
    (aiern-ex-search-goto-offset aiern-ex-search-offset)
    (aiern-ex-search-activate-highlight aiern-ex-search-pattern)))

(defun aiern-ex-find-next (&optional pattern direction nowrap)
  "Search for the next occurrence of the PATTERN in DIRECTION.
PATTERN must be created using `aiern-ex-make-pattern', DIRECTION
is either 'forward or 'backward. If NOWRAP is non nil, the search
does not wrap at buffer boundaries. Furthermore this function
only searches invisible text if `search-invisible' is t. If
PATTERN is not specified the current global pattern
`aiern-ex-search-pattern' and if DIRECTION is not specified the
current global direction `aiern-ex-search-direction' is used.
This function returns t if the search was successful, nil if it
was unsuccessful and 'wrapped if the search was successful but
has been wrapped at the buffer boundaries."
  (setq pattern (or pattern aiern-ex-search-pattern)
        direction (or direction aiern-ex-search-direction))
  (unless (and pattern (aiern-ex-pattern-regex pattern))
    (signal 'search-failed (list "No search pattern")))
  (catch 'done
    (let (wrapped)
      (while t
        (let ((search-result (aiern-ex-search-find-next-pattern pattern
                                                               direction)))
          (cond
           ((and search-result
                 (or (eq search-invisible t)
                     (not (isearch-range-invisible
                           (match-beginning 0) (match-end 0)))))
            ;; successful search and not invisible
            (throw 'done (if wrapped 'wrapped t)))
           ((not search-result)
            ;; unsuccessful search
            (if nowrap
                (throw 'done nil)
              (setq nowrap t
                    wrapped t)
              (goto-char (if (eq direction 'forward)
                             (point-min)
                           (point-max)))))))))))

(defun aiern-ex-search-update (pattern offset beg end message)
  "Update the highlighting and info-message for the search pattern.
PATTERN is the search pattern and OFFSET the associated offset.
BEG and END specifiy the current match, MESSAGE is the info
message to be shown. This function does nothing if
`aiern-ex-search-interactive' is nil."
  (when aiern-ex-search-interactive
    (cond
     ((and beg end)
      ;; update overlay
      (if aiern-ex-search-overlay
          (move-overlay aiern-ex-search-overlay beg end)
        (setq aiern-ex-search-overlay
              (make-overlay beg end))
        (overlay-put aiern-ex-search-overlay 'priority 1001)
        (overlay-put aiern-ex-search-overlay 'face 'aiern-ex-search))
      ;; move point
      (goto-char beg)
      (aiern-ex-search-goto-offset offset)
      ;; update highlights
      (when aiern-ex-search-highlight-all
        (aiern-ex-hl-change 'aiern-ex-search pattern)))
     (t
      ;; no match
      (when aiern-ex-search-overlay
        ;; remove overlay
        (delete-overlay aiern-ex-search-overlay)
        (setq aiern-ex-search-overlay nil))
      ;; no highlights
      (when aiern-ex-search-highlight-all
        (aiern-ex-hl-change 'aiern-ex-search nil))
      ;; and go to initial position
      (goto-char aiern-ex-search-start-point)))
    (when (stringp message)
      (aiern-ex-echo "%s" message))))

(defun aiern-ex-search-start-session ()
  "Initialize Ex for interactive search."
  (remove-hook 'minibuffer-setup-hook #'aiern-ex-search-start-session)
  (add-hook 'after-change-functions #'aiern-ex-search-update-pattern nil t)
  (add-hook 'minibuffer-exit-hook #'aiern-ex-search-stop-session)
  (aiern-ex-search-activate-highlight nil))
(put 'aiern-ex-search-start-session 'permanent-local-hook t)

(defun aiern-ex-search-stop-session ()
  "Stop interactive search."
  (with-current-buffer aiern-ex-current-buffer
    ;; TODO: This is a bad fix to remove duplicates. The duplicates
    ;;       exist because `isearch-range-invisible' may add a single
    ;;       overlay multiple times if we are in an unlucky situation
    ;;       of overlapping overlays. This happens in our case because
    ;;       of the overlays that are used for (lazy) highlighting.
    ;;       Perhaps it would be better to disable those overlays
    ;;       temporarily before calling `isearch-range-invisible'.
    (setq isearch-opened-overlays (delete-dups isearch-opened-overlays))
    (isearch-clean-overlays))
  (remove-hook 'minibuffer-exit-hook #'aiern-ex-search-stop-session)
  (remove-hook 'after-change-functions #'aiern-ex-search-update-pattern t)
  (when aiern-ex-search-overlay
    (delete-overlay aiern-ex-search-overlay)
    (setq aiern-ex-search-overlay nil)))
(put 'aiern-ex-search-stop-session 'permanent-local-hook t)

(defun aiern-ex-split-search-pattern (pattern direction)
  "Split PATTERN in regexp, offset and next-pattern parts.
Returns a triple (regexp  offset next-search)."
  (save-match-data
    (if (or (and (eq direction 'forward)
                 (string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\(/\\([^;]*\\)\\(?:;\\([/?].*\\)?\\)?\\)?$"
                               pattern))
            (and (eq direction 'backward)
                 (string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\(\\?\\([^;]*\\)\\(?:;\\([/?].*\\)?\\)?\\)?$"
                               pattern)))
        (list (substring pattern 0 (match-beginning 1))
              (match-string 2 pattern)
              (match-string 3 pattern))
      (list pattern nil nil))))

(defun aiern-ex-search-full-pattern (pattern-string count direction)
  "Search for a full search pattern PATTERN-STRING in DIRECTION.
This function split PATTERN-STRING in
pattern/offset/;next-pattern parts and performs the search in
DIRECTION which must be either 'forward or 'backward. The first
search is repeated COUNT times. If the pattern part of
PATTERN-STRING is empty, the last global pattern stored in
`aiern-ex-search-pattern' is used instead if in addition the
offset part is nil (i.e. no pattern/offset separator), the last
global offset stored in `aiern-ex-search-offset' is used as
offset. The current match data will correspond to the last
successful match.  This function returns a triple (RESULT PATTERN
OFFSET) where RESULT is

  t              the search has been successful without wrap
  'wrap          the search has been successful with wrap
  'empty-pattern the last pattern has been empty
  nil            the search has not been successful

and PATTERN and OFFSET are the last pattern and offset this
function searched for. Note that this function does not handle
any error conditions."
  (setq count (or count 1))
  (catch 'done
    (while t
      (let* ((res (aiern-ex-split-search-pattern pattern-string direction))
             (pat (pop res))
             (offset (pop res))
             (next-pat (pop res)))
        ;; use last pattern if no new pattern has been specified
        (if (not (zerop (length pat)))
            (setq pat (aiern-ex-make-search-pattern pat))
          (setq pat aiern-ex-search-pattern
                offset (or offset aiern-ex-search-offset)))
        (when (zerop (length pat))
          (throw 'done (list 'empty-pattern pat offset)))
        (let (search-result)
          (while (> count 0)
            (let ((result (aiern-ex-find-next pat direction
                                             (not aiern-search-wrap))))
              (if (not result) (setq search-result nil count 0)
                (setq search-result
                      (if (or (eq result 'wrap)
                              (eq search-result 'wrap))
                          'wrap t)
                      count (1- count)))))
          (cond
           ;; search failed
           ((not search-result) (throw 'done (list nil pat offset)))
           ;; no next pattern, search complete
           ((zerop (length next-pat))
            (aiern-ex-search-goto-offset offset)
            (throw 'done (list search-result pat offset)))
           ;; next pattern but empty
           ((= 1 (length next-pat))
            (aiern-ex-search-goto-offset offset)
            (throw 'done (list 'empty-pattern pat offset)))
           ;; next non-empty pattern, next search iteration
           (t
            (aiern-ex-search-goto-offset offset)
            (setq count 1
                  pattern-string (substring next-pat 1)
                  direction (if (= (aref next-pat 0) ?/)
                                'forward
                              'backward)))))))))

(defun aiern-ex-search-update-pattern (_beg _end _range)
  "Update the current search pattern."
  (save-match-data
    (let ((pattern-string (minibuffer-contents)))
      (with-current-buffer aiern-ex-current-buffer
        (with-selected-window (minibuffer-selected-window)
          (goto-char (1+ aiern-ex-search-start-point))
          (condition-case err
              (let* ((result (aiern-ex-search-full-pattern pattern-string
                                                          (or aiern-ex-search-count 1)
                                                          aiern-ex-search-direction))
                     (success (pop result))
                     (pattern (pop result))
                     (offset (pop result)))
                (cond
                 ((eq success 'wrap)
                  (aiern-ex-search-update pattern offset
                                         (match-beginning 0) (match-end 0)
                                         "Wrapped"))
                 ((eq success 'empty-pattern)
                  (aiern-ex-search-update nil nil nil nil nil))
                 (success
                  (aiern-ex-search-update pattern offset
                                         (match-beginning 0) (match-end 0)
                                         nil))
                 (t
                  (aiern-ex-search-update nil nil
                                         nil nil
                                         "search failed"))))
            (invalid-regexp
             (aiern-ex-search-update nil nil nil nil (cadr err)))
            (error
             (aiern-ex-search-update nil nil nil nil (format "%s" err)))))))))
(put 'aiern-ex-search-update-pattern 'permanent-local-hook t)

(defun aiern-ex-search-exit ()
  "Exit interactive search, keeping lazy highlighting active."
  (interactive)
  (aiern-ex-search-stop-session)
  (exit-minibuffer))

(defun aiern-ex-search-abort ()
  "Abort interactive search, disabling lazy highlighting."
  (interactive)
  (aiern-ex-search-stop-session)
  (aiern-ex-delete-hl 'aiern-ex-search)
  (abort-recursive-edit))

(defun aiern-ex-search-goto-offset (offset)
  "Move point according to search OFFSET and set `aiern-this-type' accordingly.
This function assumes that the current match data represents the
current search result."
  (unless (zerop (length offset))
    (let ((beg (match-beginning 0))
          (end (match-end 0)))
      (save-match-data
        (unless
            (string-match
             "^\\([esb]\\)?\\(\\([-+]\\)?\\([0-9]*\\)\\)$"
             offset)
          (user-error "Invalid search offset: %s" offset))
        (let ((count (if (= (match-beginning 4) (match-end 4))
                         (cond
                          ((not (match-beginning 3)) 0)
                          ((= (aref offset (match-beginning 3)) ?+) +1)
                          (t -1))
                       (string-to-number (match-string 2 offset)))))
          (cond
           ((not (match-beginning 1))
            (setq aiern-this-type 'line)
            (forward-line count))
           ((= (aref offset (match-beginning 1)) ?e)
            (goto-char (+ end count -1))
            (setq aiern-this-type 'inclusive))
           ((memq (aref offset (match-beginning 1)) '(?s ?b))
            (goto-char (+ beg count))
            (setq aiern-this-type 'inclusive))))))))

(defun aiern-ex-search-setup ()
  "Hook to initialize the minibuffer for ex search."
  (add-hook 'pre-command-hook #'aiern-ex-remove-default))

(defun aiern-ex-start-search (direction count)
  "Start a new search in a certain DIRECTION."
  ;; store buffer and window where the search started
  (let ((aiern-ex-current-buffer (current-buffer)))
    (setq aiern-ex-search-count count
          aiern-ex-search-direction direction
          aiern-ex-search-start-point (point)
          aiern-ex-last-was-search t)
    (progn
      ;; ensure minibuffer is initialized accordingly
      (add-hook 'minibuffer-setup-hook #'aiern-ex-search-start-session)
      ;; read the search string
      (let* ((minibuffer-local-map aiern-ex-search-keymap)
             (search-string
              (condition-case err
                  (minibuffer-with-setup-hook
                      #'aiern-ex-search-setup
                    (read-string (if (eq aiern-ex-search-direction 'forward)
                                     "/" "?")
                                 (and aiern-ex-search-history
                                      (propertize
                                       (car aiern-ex-search-history)
                                       'face 'shadow))
                                 'aiern-ex-search-history))
                (quit
                 (aiern-ex-search-stop-session)
                 (aiern-ex-delete-hl 'aiern-ex-search)
                 (goto-char aiern-ex-search-start-point)
                 (signal (car err) (cdr err))))))
        ;; pattern entered successful
        (goto-char (if (eq aiern-ex-search-direction 'forward)
                       (1+ aiern-ex-search-start-point)
                     (1- aiern-ex-search-start-point)))
        (let* ((result
                (aiern-ex-search-full-pattern search-string
                                             aiern-ex-search-count
                                             aiern-ex-search-direction))
               (success (pop result))
               (pattern (pop result))
               (offset (pop result)))
          (setq aiern-ex-search-pattern pattern
                aiern-ex-search-offset offset)
          (cond
           ((memq success '(t wrap))
            (goto-char (match-beginning 0))
            (setq aiern-ex-search-match-beg (match-beginning 0)
                  aiern-ex-search-match-end (match-end 0))
            (aiern-ex-search-goto-offset offset)
            (aiern-push-search-history search-string (eq direction 'forward))
            (unless aiern-ex-search-persistent-highlight
              (aiern-ex-delete-hl 'aiern-ex-search)))
           (t
            (goto-char aiern-ex-search-start-point)
            (aiern-ex-delete-hl 'aiern-ex-search)
            (signal 'search-failed (list search-string)))))))))

(defun aiern-ex-start-word-search (unbounded direction count &optional symbol)
  "Search for the symbol under point.
The search matches the COUNT-th occurrence of the word.  If the
UNBOUNDED argument is nil, the search matches only at symbol
boundaries, otherwise it matches anywhere.  The DIRECTION
argument should be either `forward' or `backward', determining
the search direction. If SYMBOL is non-nil then the functions
searches for the symbol at point, otherwise for the word at
point."
  (let ((string (aiern-find-thing (eq direction 'forward)
                                 (if symbol 'symbol 'word))))
    (if (null string)
        (user-error "No word under point")
      (let ((regex (if unbounded
                       (regexp-quote string)
                     (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                             (regexp-quote string)))))
        (setq aiern-ex-search-count count
              aiern-ex-search-direction direction
              aiern-ex-search-pattern
              (aiern-ex-make-search-pattern regex)
              aiern-ex-search-offset nil
              aiern-ex-last-was-search t)
        ;; update search history unless this pattern equals the
        ;; previous pattern
        (unless (equal (car-safe aiern-ex-search-history) regex)
          (push regex aiern-ex-search-history))
        (aiern-push-search-history regex (eq direction 'forward)))
      (aiern-ex-delete-hl 'aiern-ex-search)
      (when (fboundp 'aiern-ex-search-next)
        (aiern-ex-search-next count)))))

;; substitute
(aiern-ex-define-argument-type substitution
  "A substitution pattern argument /pattern/replacement/flags.
This handler highlights the pattern of the current substitution."
  :runner
  (lambda (flag &optional arg)
    (with-selected-window (minibuffer-selected-window)
      (with-current-buffer aiern-ex-current-buffer
        (cond
         ((eq flag 'start)
          (aiern-ex-make-hl
           'aiern-ex-substitute
           :face 'aiern-ex-substitute-matches
           :update-hook #'aiern-ex-pattern-update-ex-info
           :match-hook (and aiern-ex-substitute-interactive-replace
                            #'aiern-ex-pattern-update-replacement))
          (setq flag 'update))

         ((eq flag 'stop)
          (aiern-ex-delete-hl 'aiern-ex-substitute))))

      (when (and (eq flag 'update)
                 aiern-ex-substitute-highlight-all
                 (not (zerop (length arg))))
        (condition-case lossage
            (let* ((result (aiern-ex-get-substitute-info arg t))
                   (pattern (pop result))
                   (replacement (pop result))
                   (range (or (aiern-copy-range aiern-ex-range)
                              (aiern-range (line-beginning-position)
                                          (line-end-position)
                                          'line
                                          :expanded t))))
              (setq aiern-ex-substitute-current-replacement replacement)
              (aiern-expand-range range)
              (aiern-ex-hl-set-region 'aiern-ex-substitute
                                     (aiern-range-beginning range)
                                     (aiern-range-end range))
              (aiern-ex-hl-change 'aiern-ex-substitute pattern))
          (end-of-file
           (aiern-ex-pattern-update-ex-info nil
                                           "incomplete replacement"))
          (user-error
           (aiern-ex-pattern-update-ex-info nil
                                           (format "%s" lossage))))))))

(defun aiern-ex-pattern-update-ex-info (_hl result)
  "Update the Ex info string."
  (when (stringp result)
    (aiern-ex-echo "%s" result)))

(defun aiern-ex-pattern-update-replacement (_hl overlay)
  "Update the replacement display."
  (when (fboundp 'match-substitute-replacement)
    (let ((fixedcase (not case-replace))
          repl)
      (setq repl (if aiern-ex-substitute-current-replacement
                     (aiern-match-substitute-replacement
                      aiern-ex-substitute-current-replacement
                      fixedcase)
                   ""))
      (put-text-property 0 (length repl)
                         'face 'aiern-ex-substitute-replacement
                         repl)
      (overlay-put overlay 'after-string repl))))

(defun aiern-ex-parse-global (string)
  "Parse STRING as a global argument."
  (let* ((pattern (nth 0 (aiern-delimited-arguments string 2)))
         (command (and pattern
                       (>= (- (length string) (length pattern)) 2)
                       (substring string (+ (length pattern) 2)))))
    ;; use last pattern if none given
    (when (zerop (length pattern))
      (setq pattern
            (cond
             ((and (eq aiern-search-module 'aiern-search) aiern-ex-search-pattern)
              (aiern-ex-pattern-regex aiern-ex-search-pattern))
             ((and (eq aiern-search-module 'isearch) (not (zerop (length isearch-string))))
              isearch-string)
             (t (user-error "No previous pattern")))))
    (list pattern command)))

(defun aiern-ex-get-substitute-info (string &optional implicit-r)
  "Returns the substitution info of command line STRING.
This function returns a three-element list \(PATTERN REPLACEMENT
FLAGS) consisting of the substitution parts of STRING. PATTERN is
a ex-pattern (see `aiern-ex-make-pattern') and REPLACEMENT in a
compiled replacement expression (see `aiern-compile-replacement').
The information returned is the actual substitution information
w.r.t. to special situations like empty patterns or repetition of
previous substitution commands. If IMPLICIT-R is non-nil, then
the flag 'r' is assumed, i.e. in the case of an empty pattern the
last search pattern is used. This will be used when called from
a :substitute command with arguments."
  (let (pattern replacement flags)
    (cond
     ((or (null string) (string-match "^[a-zA-Z]" string))
      ;; starts with letter so there is no pattern because the
      ;; separator must not be a letter repeat last substitute
      (setq replacement aiern-ex-substitute-replacement)
      ;; flags are everything that is not a white space
      (when (and string (string-match "[^[:space:]]+" string))
        (setq flags (match-string 0 string))))
     (t
      (let ((args (aiern-delimited-arguments string 3)))
        (setq pattern (pop args)
              replacement (pop args)
              flags (pop args))
        ;; if replacment equals "~" use previous replacement
        (if (equal replacement "~")
            (setq replacement aiern-ex-substitute-replacement)
          (setq replacement (aiern-compile-replacement replacement)))
        ;; append implicit "r" flag if required
        (when (and implicit-r (not (memq ?r (append flags nil))))
          (setq flags (concat flags "r"))))))
    ;; if flags equals "&" add previous flags
    (if (and (not (zerop (length flags)))
             (= (aref flags 0) ?&))
        (setq flags (append (substring flags 1)
                            aiern-ex-substitute-flags))
      (setq flags (append flags nil)))
    ;; if no pattern, use previous pattern, either search or
    ;; substitute pattern depending on `aiern-ex-last-was-search' and
    ;; the r flag
    (when (zerop (length pattern))
      (setq pattern
            (if (eq aiern-search-module 'aiern-search)
                (if (and aiern-ex-last-was-search (memq ?r flags))
                    (and aiern-ex-search-pattern
                         (aiern-ex-pattern-regex aiern-ex-search-pattern))
                  (and aiern-ex-substitute-pattern
                       (aiern-ex-pattern-regex aiern-ex-substitute-pattern)))
              (if (eq case-fold-search t)
                  isearch-string
                (concat isearch-string "\\C")))
            flags (remq ?r flags)))
    ;; generate pattern
    (when pattern
      (setq pattern (aiern-ex-make-substitute-pattern pattern flags)))
    (list pattern replacement flags)))

(defun aiern-ex-nohighlight ()
  "Disable the active search highlightings."
  (interactive)
  (aiern-ex-delete-hl 'aiern-ex-substitute)
  (aiern-ex-delete-hl 'aiern-ex-search))

(provide 'aiern-search)

;;; aiern-search.el ends here
