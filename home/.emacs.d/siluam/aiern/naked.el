;;; naked.el --- Provide for naked key descriptions: no angle brackets.
;;
;; Filename: naked.el
;; Description: Provide for naked key descriptions: no angle brackets.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2011-2020, Drew Adams, all rights reserved.
;; Created: Fri Oct  7 13:12:52 2011 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Wed Oct 14 08:46:05 2020 (-0700)
;;           By: dradams
;;     Update #: 206
;; URL: https://www.emacswiki.org/emacs/download/naked.el
;; Doc URL: https://www.emacswiki.org/emacs/NaKeD
;; Keywords: lisp, key, print, format, help
;; Compatibility: GNU Emacs 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Lets you use key-sequence descriptions that do not bother with
;;  angle brackets: `<' and `>'.
;;
;;  Prior to Emacs 21, vanilla GNU Emacs did not bother with angle
;;  brackets either, but someone around the turn of the century got
;;  the idea that Emacs could not do without them.  So instead of the
;;  `delete' key we now have the `<delete>' key.  And instead of`C-x
;;  M-delete' we now have `C-x M-<delete>'.  "On n'arrete pas le
;;  progres !"
;;
;;  Angle brackets are not needed - why?  Because we already use
;;  spaces to separate keys in a key-sequence description (we use
;;  `SPC' to indicate the SPACE key).
;;
;;  To be fair, it is true that sometimes people have taken the
;;  shortcut when writing about user input of writing, e.g., `M-x
;;  forward-char RET' instead of writing `M-x f o r w a r d - c h a
;;  r'. And if you write `forward' that way to stand for an input
;;  sequence of seven chars, then you cannot also expect `forward' to
;;  stand for a function key named `forward', can you?
;;
;;  Well, yes you can, if the context makes things clear enough.  And
;;  I for one find `C-M-<insert>' butt-ugly (likewise `<C-M-insert>')
;;  - and, more importantly, insulting to Occam and his razor.
;;
;;  So go ahead and go NaKeD - shed your angles.
;;
;;  Here's what you need.  It won't completely purge Emacs from
;;  insulting you with the occasional pair of angle brackets, but it
;;  at least lets you DTRT in code that you write:
;;
;;  * Use function `naked-key-description' instead of
;;    `key-description'.
;;
;;    The former outputs naked key descriptions: no angle brackets
;;    around function keys.  E.g., if KEY is the sequence of events
;;    produced by holding the Shift key while hitting the Insert key,
;;    then `(naked-key-description KEY)' returns "S-insert" (and not
;;    "S-<insert>").  (Internally, this sequence of events is the
;;    vector [S-insert].)
;;
;;  * Use function `naked' instead of `kbd' (which is a function
;;    starting with Emacs 24.3 and a macro before then).
;;
;;    The former allows its argument key-sequence description to use
;;    naked keys, not clothed in angle brackets.  E.g., (naked
;;    "C-M-delete") has the same effect as (kbd "C-M-<delete>").
;;
;;  * Use command `naked-read-kbd-macro' instead of `read-kbd-macro'.
;;
;;  * Use function `naked-edmacro-parse-keys' instead of
;;    `edmacro-parse-keys' (if you happen to use that lower-level
;;    function).
;;
;;  But you can also have it both ways if or when you might need to:
;;  All of these accept an optional argument ANGLES which, if
;;  non-`nil', returns the behavior to the vanilla one, expecting
;;  function keys to be fully clothed in angle brackets.  E.g.: (naked
;;  "C-M-<delete>" t).
;;
;;  In addition, even without an non-nil ANGLES argument, function
;;  `naked' does the right thing for keys expressed using angle
;;  brackets.  IOW, (naked "M-<foobar>") returns the same thing that
;;  (naked "M-foobar") does: [M-foobar].
;;
;;  Enjoy!
;;
;;
;;  Command defined here:
;;
;;    `naked-read-kbd-macro'.
;;
;;  Non-interactive functions defined here:
;;
;;    `naked', `naked-edmacro-parse-keys', `naked-key-description',
;;    `naked-read-kbd-macro', `naked-string-match-p'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2020/10/13 dadams
;;     Added: naked-string-match-p.
;;     naked-edmacro-parse-keys: Use naked-string-match-p when don't need to set match data.
;;     naked-read-kbd-macro: Fixed typo: \P -> \nP.
;; 2017/04/18 dadams
;;     Added forgotten compile-time require of cl.el, for macro cl-loop.
;; 2013/02/26 dadams
;;     Added (put 'naked 'pure t)
;; 2012/12/01 dadams
;;     Change naked from a macro to a function (like Emacs 24.3).
;; 2012/09/28 dadams
;;     naked-edmacro-parse-keys:
;;       Better fix for M-TAB.  Do not assume that function keys are lowercase.
;;       Handle angle brackets even if ANGLES is nil.  But keep vanilla bug #12535 if non-nil.
;; 2012/09/27 dadams
;;     naked-edmacro-parse-keys: Fix handling of M-TAB.  Function keys are lowercase.
;;                               So M-TAB returns [134217737], but M-tab returns [M-tab].
;; 2011/10/07 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl-lib)) ;; cl-loop

;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'string-match-p)
    (defalias 'naked-string-match-p 'string-match-p) ; Emacs 23+
  (defun naked-string-match-p (regexp string &optional start)
    "Like `string-match', but this saves and restores the match data."
    (save-match-data (string-match regexp string start))))

;; Same as `icicle-key-description' in `icicles-fn.el'.
(defun naked-key-description (keys &optional prefix angles)
  "Like `key-description', but does not use angle brackets, by default.
Non-nil optional arg ANGLES means use angle brackets."
  (let ((result  (if (< emacs-major-version 22)
                     (key-description keys)
                   (key-description keys prefix))))
    (unless angles                      ; Assume that spaces separate function keys.
      (setq result  (replace-regexp-in-string "<\\([^>]+\\)>" "\\1" result 'fixed-case)))
    result))

;; Same as `icicle-edmacro-parse-keys' in `icicles-mac.el'.
;; Based on `edmacro-parse-keys' in standard library `edmacro.el'
;; Differences are:
;;
;; 1. Addition of optional arg ANGLES.
;; 2. Ensure same behavior as `edmacro-parse-keys', if ANGLES is non-nil.
;; 2. Handle angle brackets, whether ANGLES is nil or non-nil.
;; 3. Handle `TAB' correctly, if ANGLES is nil.
;; 4. Handle names without angle brackets, if ANGLES is nil.
;; 5. Works for all Emacs versions.
;;
(defun naked-edmacro-parse-keys (string &optional need-vector angles)
  "Like `edmacro-parse-keys', but does not use angle brackets, by default.
Non-nil optional arg ANGLES means to use angle brackets, exactly like
`edmacro-parse-keys'.  See `naked-read-kbd-macro' for more about
ANGLES."
  (let ((case-fold-search  nil)
    (len               (length string)) ; We won't alter string in the cl-loop below.
        (pos               0)
        (res               []))
    (while (and (< pos len)  (string-match "[^ \t\n\f]+" string pos))
      (let* ((word-beg  (match-beginning 0))
         (word-end  (match-end 0))
         (word      (substring string word-beg len))
         (times     1)
             (key       nil))
    ;; Try to catch events of the form "<as df>".
        (if (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
            (setq word  (match-string 0 word)
                  pos   (+ word-beg (match-end 0)))
          (setq word  (substring string word-beg word-end)
                pos   word-end))
        (when (string-match "\\([0-9]+\\)\\*." word)
          (setq times  (string-to-number (substring word 0 (match-end 1)))
                word   (substring word (1+ (match-end 1)))))
        (cond ((naked-string-match-p "^$" word)
               (setq key  (vconcat (if (eq (key-binding [?\M-x]) 'execute-extended-command)
                                       [?\M-x]
                                     (or (car (where-is-internal 'execute-extended-command))  [?\M-x]))
                                   (substring word 2 -2) "\r")))

              ;; Must test this before [ACHMsS]- etc., to prevent match.
              ((or (equal word "REM")  (naked-string-match-p "^;;" word))
               (setq pos  (naked-string-match-p "$" string pos)))

              ;; Straight `edmacro-parse-keys' case - ensure same behavior.
              ;; Includes same bugged handling of `TAB'.  That is Emacs bug #12535.
              ;; The bug fix is to add `TAB' to the list in this clause.
          ((and angles
                    (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
            (progn
              (setq word  (concat (substring word (match-beginning 1) (match-end 1))
                                          (substring word (match-beginning 3) (match-end 3))))
              (not (naked-string-match-p "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$" word))))
           (setq key  (list (intern word))))

              ;; NaKeD handling of <...>.  Recognize it anyway, even without non-nil ANGLES.
              ;; But unlike `edmacro-parse-keys', include <TAB>, to handle it correctly.
              ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$" word)
                    (progn
                      (setq word  (concat (substring word (match-beginning 1) (match-end 1))
                                          (substring word (match-beginning 3) (match-end 3))))
                      (not (naked-string-match-p "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\|TAB\\)$" word))))
               (setq key  (list (intern word))))

              ;; NaKeD handling of names without <...>.
              ((and (not angles)
                    (string-match "^\\(\\([ACHMsS]-\\)*\\)\\([^ \t\f\n][^ \t\f\n]+\\)$" word)
                    ;; Do not count `C-' etc. when at end of string.
                    (not (naked-string-match-p "\\([ACHMsS]-.\\)+$" word))
                    (progn
                      (setq word  (concat (substring word (match-beginning 1) (match-end 1))
                                          (substring word (match-beginning 3) (match-end 3))))
                      (not (naked-string-match-p "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\|TAB\\)$" word))))
               (setq key  (list (intern word))))

              (t
               (let ((orig-word  word)
                     (prefix     0)
                     (bits       0))
                 (while (naked-string-match-p "^[ACHMsS]-." word)
                   (cl-incf bits (cdr (assq (aref word 0) '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                                         (?H . ?\H-\^@) (?M . ?\M-\^@)
                                                         (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                   (cl-incf prefix 2)
                   (cl-callf substring word 2))
                 (when (naked-string-match-p "^\\^.$" word)
                   (cl-incf bits ?\C-\^@)
                   (cl-incf prefix)
                   (cl-callf substring word 1))
                 (let ((found  (assoc word '(("NUL" . "\0") ("RET" . "\r") ("LFD" . "\n")
                                             ("ESC" . "\e") ("SPC" . " ") ("DEL" . "\177")
                                             ("TAB" . "\t")))))
                   (when found (setq word  (cdr found))))
                 (when (naked-string-match-p "^\\\\[0-7]+$" word)
                   (cl-loop for ch across word
                         for n = 0 then (+ (* n 8) ch -48)
                         finally do (setq word  (vector n))))
                 (cond ((= bits 0) (setq key  word))
                       ((and (= bits ?\M-\^@)
                             (stringp word)
                             (naked-string-match-p "^-?[0-9]+$" word))
                        (setq key  (cl-loop for x across word collect (+ x bits))))
                       ((/= (length word) 1)
                        (error "%s must prefix a single character, not %s"
                               (substring orig-word 0 prefix) word))
                       ((and (/= (logand bits ?\C-\^@) 0)
                             (stringp word)
                             ;; Used to accept `.' and `?' here, but `.' is simply wrong,
                             ;; and `C-?' is not used (so use `DEL' instead).
                             (naked-string-match-p "[@-_a-z]" word))
                        (setq key  (list (+ bits (- ?\C-\^@) (logand (aref word 0) 31)))))
                       (t
                        (setq key  (list (+ bits (aref word 0)))))))))
        (when key (cl-loop repeat times do (cl-callf vconcat res key)))))
    (when (and (>= (length res) 4)  (eq (aref res 0) ?\C-x)  (eq (aref res 1) ?\()
               (eq (aref res (- (length res) 2)) ?\C-x)  (eq (aref res (- (length res) 1)) ?\)))
      (setq res  (edmacro-subseq res 2 -2)))
    (if (and (not need-vector)
         (cl-loop for ch across res
           always (and (if (fboundp 'characterp) (characterp ch) (char-valid-p ch))
                   (let ((ch2  (logand ch (lognot ?\M-\^@))))
                 (and (>= ch2 0)  (<= ch2 127))))))
    (concat (cl-loop for ch across res collect (if (= (logand ch ?\M-\^@) 0)  ch  (+ ch 128))))
      res)))

;; Same as `icicle-read-kbd-macro' in `icicles-mac.el'.
;;;###autoload
(defun naked-read-kbd-macro (start &optional end angles)
  "Read the region as a keyboard macro definition.
Like `read-kbd-macro', but does not use angle brackets, by default.

With a prefix arg use angle brackets, exactly like `read-kbd-macro'.
That is, with non-nil arg ANGLES, expect key descriptions to use angle
brackets (<...>).  Otherwise, expect key descriptions not to use angle
brackets.  For example:

 (naked-read-kbd-macro  \"mode-line\"  t)   returns [mode-line]
 (naked-read-kbd-macro \"<mode-line>\" t t) returns [mode-line]"
  (interactive "r\nP")
  (if (stringp start)
      (naked-edmacro-parse-keys start end angles)
    (setq last-kbd-macro  (naked-edmacro-parse-keys (buffer-substring start end) nil angles))))

(put 'naked 'pure t)
;; Same as `icicle-kbd' in `icicles-opt.el'.
(defun naked (keys &optional angles)
  "Like `kbd', but does not use angle brackets, by default.
With non-nil optional arg ANGLES, expect key descriptions to use angle
brackets (<...>), exactly like `kbd'.  Otherwise, expect key
descriptions not to use angle brackets.  For example:

 (naked \"mode-line\")     returns [mode-line]
 (naked \"<mode-line>\" t) returns [mode-line]

The default behavior lets you use, e.g., \"C-x delete\" and \"C-delete\"
instead of \"C-x <delete>\" and \"C-<delete>\"."
  (naked-read-kbd-macro keys nil angles))

;;;;;;;;;;;;;;;;;

(provide 'naked)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; naked.el ends here
