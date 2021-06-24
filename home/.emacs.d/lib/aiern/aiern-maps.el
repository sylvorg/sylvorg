;;; aiern-maps.el --- Default keymaps -*- lexical-binding: t -*-

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

(require 'aiern-states)
(require 'aiern-ex)
(require 'aiern-commands)
(require 'aiern-command-window)
(require 'aiern-common)
(require 'tag)
(require 'deino)
(require 'naked)

;;; Code:

;;; Normal state

;; (define-key aiern-normal-state-map "a" 'aiern-append)
;; (define-key aiern-normal-state-map "A" 'aiern-append-line)
;; (define-key aiern-normal-state-map "c" 'aiern-change)
;; (define-key aiern-normal-state-map "C" 'aiern-change-line)
(define-key aiern-normal-state-map "d" 'aiern-delete)
(define-key aiern-normal-state-map "D" 'aiern-delete-line)
;; (define-key aiern-normal-state-map "i" 'aiern-insert)
;; (define-key aiern-normal-state-map (kbd "<insert>") 'aiern-insert)
;; (define-key aiern-normal-state-map (kbd "<insertchar>") 'aiern-insert)
(define-key aiern-normal-state-map "i" 'aiern-insert)
(define-key aiern-normal-state-map (kbd "<insert>") 'aiern-insert)
(define-key aiern-normal-state-map (kbd "<insertchar>") 'aiern-insert)
;; (define-key aiern-normal-state-map "I" 'aiern-insert-line)
;; (define-key aiern-normal-state-map "J" 'aiern-join)
;; (define-key aiern-normal-state-map "m" 'aiern-set-marker)
(define-key aiern-normal-state-map "o" 'aiern-open-below)
(define-key aiern-normal-state-map "O" 'aiern-open-above)
(define-key aiern-normal-state-map "p" 'aiern-paste-after)
(define-key aiern-normal-state-map "P" 'aiern-paste-before)
;; (define-key aiern-normal-state-map "q" 'aiern-record-macro)
(define-key aiern-normal-state-map "r" 'aiern-replace)
;; (define-key aiern-normal-state-map "R" 'aiern-replace-state)
;; (define-key aiern-normal-state-map "s" 'aiern-substitute)
;; (define-key aiern-normal-state-map "S" 'aiern-change-whole-line)
(define-key aiern-normal-state-map "x" 'aiern-delete-char)
(define-key aiern-normal-state-map "X" 'aiern-delete-backward-char)
(define-key aiern-normal-state-map [deletechar] 'aiern-delete-char)
(define-key aiern-normal-state-map "y" 'aiern-yank)
(define-key aiern-normal-state-map "Y" 'aiern-yank-line)
;; (define-key aiern-normal-state-map "&" 'aiern-ex-repeat-substitute)
;; (define-key aiern-normal-state-map "g&" 'aiern-ex-repeat-global-substitute)
;; (define-key aiern-normal-state-map "g8" 'what-cursor-position)
;; (define-key aiern-normal-state-map "ga" 'what-cursor-position)
;; (define-key aiern-normal-state-map "gi" 'aiern-insert-resume)
;; (define-key aiern-normal-state-map "gJ" 'aiern-join-whitespace)
;; (define-key aiern-normal-state-map "gq" 'aiern-fill-and-move)
;; (define-key aiern-normal-state-map "gw" 'aiern-fill)
(define-key aiern-normal-state-map "gu" 'aiern-downcase)
(define-key aiern-normal-state-map "gU" 'aiern-upcase)
;; (define-key aiern-normal-state-map "gf" 'find-file-at-point)
;; (define-key aiern-normal-state-map "gF" 'aiern-find-file-at-point-with-line)
;; (define-key aiern-normal-state-map "gx" 'browse-url-at-point)
;; (define-key aiern-normal-state-map "g?" 'aiern-rot13)
;; (define-key aiern-normal-state-map "g~" 'aiern-invert-case)
(define-key aiern-normal-state-map "~" 'aiern-invert-case)
;; (define-key aiern-normal-state-map "zo" 'aiern-open-fold)
;; (define-key aiern-normal-state-map "zO" 'aiern-open-fold-rec)
;; (define-key aiern-normal-state-map "zc" 'aiern-close-fold)
;; (define-key aiern-normal-state-map "za" 'aiern-toggle-fold)
;; (define-key aiern-normal-state-map "zr" 'aiern-open-folds)
;; (define-key aiern-normal-state-map "zm" 'aiern-close-folds)
;; (define-key aiern-normal-state-map "z=" 'ispell-word)
;; (define-key aiern-normal-state-map "\C-n" 'aiern-paste-pop-next)
;; (define-key aiern-normal-state-map "\C-p" 'aiern-paste-pop)
;; (define-key aiern-normal-state-map "\C-t" 'pop-tag-mark)
;; (define-key aiern-normal-state-map (kbd "C-.") 'aiern-repeat-pop)
;; (define-key aiern-normal-state-map (kbd "M-.") 'aiern-repeat-pop-next)
(define-key aiern-normal-state-map "." 'aiern-repeat)
;; (define-key aiern-normal-state-map "@" 'aiern-execute-macro)
(define-key aiern-normal-state-map "\"" 'aiern-use-register)
;; (define-key aiern-normal-state-map "~" 'aiern-invert-char)
;; (define-key aiern-normal-state-map "=" 'aiern-indent)
;; (define-key aiern-normal-state-map "<" 'aiern-shift-left)
;; (define-key aiern-normal-state-map ">" 'aiern-shift-right)
(define-key aiern-normal-state-map "ZZ" 'aiern-save-modified-and-close)
(define-key aiern-normal-state-map "ZQ" 'aiern-quit)
(define-key aiern-normal-state-map (kbd "DEL") 'aiern-backward-char)
(define-key aiern-normal-state-map [escape] 'aiern-force-normal-state)
;; (define-key aiern-normal-state-map [remap cua-paste-pop] 'aiern-paste-pop)
;; (define-key aiern-normal-state-map [remap yank-pop] 'aiern-paste-pop)

;; (when (featurep 'tab-bar)
;;   (define-key aiern-normal-state-map "gt" 'tab-bar-switch-to-next-tab)
;;   (define-key aiern-normal-state-map "gT" 'tab-bar-switch-to-prev-tab))

;; go to last change
(define-key aiern-normal-state-map "g;" 'goto-last-change)
(define-key aiern-normal-state-map "g," 'goto-last-change-reverse)

;; undo
(define-key aiern-normal-state-map "u" 'aiern-undo)
(define-key aiern-normal-state-map "\C-r" 'aiern-redo)

(define-key aiern-normal-state-map ";" 'aiern-ex)
(define-key aiern-normal-state-map (tag-chord ";;") 'evil-ex)

;; window commands
(define-prefix-command 'aiern-window-map)
(define-key aiern-window-map "b" 'aiern-window-bottom-right)
(define-key aiern-window-map "c" 'aiern-window-delete)
(define-key aiern-window-map "h" 'aiern-window-left)
(define-key aiern-window-map "H" 'aiern-window-move-far-left)
(define-key aiern-window-map "j" 'aiern-window-down)
(define-key aiern-window-map "J" 'aiern-window-move-very-bottom)
(define-key aiern-window-map "k" 'aiern-window-up)
(define-key aiern-window-map "K" 'aiern-window-move-very-top)
(define-key aiern-window-map "l" 'aiern-window-right)
(define-key aiern-window-map "L" 'aiern-window-move-far-right)
(define-key aiern-window-map "n" 'aiern-window-new)
(define-key aiern-window-map "o" 'delete-other-windows)
(define-key aiern-window-map "p" 'aiern-window-mru)
(define-key aiern-window-map "q" 'aiern-quit)
(define-key aiern-window-map "r" 'aiern-window-rotate-downwards)
(define-key aiern-window-map "R" 'aiern-window-rotate-upwards)
(define-key aiern-window-map "s" 'aiern-window-split)
(define-key aiern-window-map "S" 'aiern-window-split)
(define-key aiern-window-map "t" 'aiern-window-top-left)
(define-key aiern-window-map "v" 'aiern-window-vsplit)
(define-key aiern-window-map "w" 'aiern-window-next)
(define-key aiern-window-map "W" 'aiern-window-prev)
(define-key aiern-window-map "+" 'aiern-window-increase-height)
(define-key aiern-window-map "-" 'aiern-window-decrease-height)
(define-key aiern-window-map "_" 'aiern-window-set-height)
(define-key aiern-window-map "<" 'aiern-window-decrease-width)
(define-key aiern-window-map ">" 'aiern-window-increase-width)
(define-key aiern-window-map "=" 'balance-windows)
(define-key aiern-window-map "|" 'aiern-window-set-width)
(define-key aiern-window-map "\C-b" 'aiern-window-bottom-right)
(define-key aiern-window-map "\C-c" 'aiern-window-delete)
(define-key aiern-window-map (kbd "C-S-h") 'aiern-window-move-far-left)
(define-key aiern-window-map (kbd "C-S-j") 'aiern-window-move-very-bottom)
(define-key aiern-window-map (kbd "C-S-k") 'aiern-window-move-very-top)
(define-key aiern-window-map (kbd "C-S-l") 'aiern-window-move-far-right)
(define-key aiern-window-map "\C-n" 'aiern-window-new)
(define-key aiern-window-map "\C-o" 'delete-other-windows)
(define-key aiern-window-map "\C-p" 'aiern-window-mru)
(define-key aiern-window-map "\C-r" 'aiern-window-rotate-downwards)
(define-key aiern-window-map (kbd "C-S-r") 'aiern-window-rotate-upwards)
(define-key aiern-window-map "\C-s" 'aiern-window-split)
(define-key aiern-window-map (kbd "C-S-s") 'aiern-window-split)
(define-key aiern-window-map "\C-t" 'aiern-window-top-left)
(define-key aiern-window-map "\C-v" 'aiern-window-vsplit)
(define-key aiern-window-map "\C-w" 'aiern-window-next)
(define-key aiern-window-map (kbd "C-S-W") 'aiern-window-prev)
(define-key aiern-window-map "\C-_" 'aiern-window-set-height)
(define-key aiern-window-map "\C-f" 'ffap-other-window)

;;; Motion state

;; "0" is a special command when called first
(aiern-redirect-digit-argument aiern-motion-state-map "0" 'aiern-beginning-of-line)
(define-key aiern-motion-state-map "1" 'digit-argument)
(define-key aiern-motion-state-map "2" 'digit-argument)
(define-key aiern-motion-state-map "3" 'digit-argument)
(define-key aiern-motion-state-map "4" 'digit-argument)
(define-key aiern-motion-state-map "5" 'digit-argument)
(define-key aiern-motion-state-map "6" 'digit-argument)
(define-key aiern-motion-state-map "7" 'digit-argument)
(define-key aiern-motion-state-map "8" 'digit-argument)
(define-key aiern-motion-state-map "9" 'digit-argument)
(define-key aiern-motion-state-map "b" 'aiern-backward-word-begin)
(define-key aiern-motion-state-map "B" 'aiern-backward-WORD-begin)
(define-key aiern-motion-state-map "e" 'aiern-forward-word-end)
(define-key aiern-motion-state-map "E" 'aiern-forward-WORD-end)
(define-key aiern-motion-state-map "f" 'aiern-find-char)
(define-key aiern-motion-state-map "F" 'aiern-find-char-backward)
(define-key aiern-motion-state-map "G" 'aiern-goto-line)
(define-key aiern-motion-state-map "h" 'aiern-backward-char)
(define-key aiern-motion-state-map "H" 'aiern-window-top)
(define-key aiern-motion-state-map "j" 'aiern-next-line)
(define-key aiern-motion-state-map "k" 'aiern-previous-line)
(define-key aiern-motion-state-map "l" 'aiern-forward-char)
(define-key aiern-motion-state-map " " 'aiern-forward-char)
(define-key aiern-motion-state-map "K" 'aiern-lookup)
(define-key aiern-motion-state-map "L" 'aiern-window-bottom)
(define-key aiern-motion-state-map "M" 'aiern-window-middle)
(define-key aiern-motion-state-map "n" 'aiern-search-next)
(define-key aiern-motion-state-map "N" 'aiern-search-previous)
(define-key aiern-motion-state-map "t" 'aiern-find-char-to)
(define-key aiern-motion-state-map "T" 'aiern-find-char-to-backward)
(define-key aiern-motion-state-map "w" 'aiern-forward-word-begin)
(define-key aiern-motion-state-map "W" 'aiern-forward-WORD-begin)
(define-key aiern-motion-state-map "y" 'aiern-yank)
(define-key aiern-motion-state-map "Y" 'aiern-yank-line)
(define-key aiern-motion-state-map "gd" 'aiern-goto-definition)
(define-key aiern-motion-state-map "ge" 'aiern-backward-word-end)
(define-key aiern-motion-state-map "gE" 'aiern-backward-WORD-end)
(define-key aiern-motion-state-map "gg" 'aiern-goto-first-line)
(define-key aiern-motion-state-map "gj" 'aiern-next-visual-line)
(define-key aiern-motion-state-map "gk" 'aiern-previous-visual-line)
(define-key aiern-motion-state-map "g0" 'aiern-beginning-of-visual-line)
(define-key aiern-motion-state-map "g_" 'aiern-last-non-blank)
(define-key aiern-motion-state-map "g^" 'aiern-first-non-blank-of-visual-line)
(define-key aiern-motion-state-map "gm" 'aiern-middle-of-visual-line)
(define-key aiern-motion-state-map "go" 'aiern-goto-char)
(define-key aiern-motion-state-map "g$" 'aiern-end-of-visual-line)
(define-key aiern-motion-state-map "g\C-]" 'aiern-jump-to-tag)
(define-key aiern-motion-state-map "{" 'aiern-backward-paragraph)
(define-key aiern-motion-state-map "}" 'aiern-forward-paragraph)
(define-key aiern-motion-state-map "#" 'aiern-search-word-backward)
(define-key aiern-motion-state-map "g#" 'aiern-search-unbounded-word-backward)
(define-key aiern-motion-state-map "$" 'aiern-end-of-line)
(define-key aiern-motion-state-map [end] 'aiern-end-of-line)
(define-key aiern-motion-state-map [home] 'aiern-beginning-of-line)
(define-key aiern-motion-state-map "%" 'aiern-jump-item)
(define-key aiern-motion-state-map "`" 'aiern-goto-mark)
(define-key aiern-motion-state-map "'" 'aiern-goto-mark-line)
(define-key aiern-motion-state-map "(" 'aiern-backward-sentence-begin)
(define-key aiern-motion-state-map ")" 'aiern-forward-sentence-begin)
(define-key aiern-motion-state-map "]]" 'aiern-forward-section-begin)
(define-key aiern-motion-state-map "][" 'aiern-forward-section-end)
(define-key aiern-motion-state-map "[[" 'aiern-backward-section-begin)
(define-key aiern-motion-state-map "[]" 'aiern-backward-section-end)
(define-key aiern-motion-state-map "[(" 'aiern-previous-open-paren)
(define-key aiern-motion-state-map "])" 'aiern-next-close-paren)
(define-key aiern-motion-state-map "[{" 'aiern-previous-open-brace)
(define-key aiern-motion-state-map "]}" 'aiern-next-close-brace)
(define-key aiern-motion-state-map "]'" 'aiern-next-mark-line)
(define-key aiern-motion-state-map "]`" 'aiern-next-mark)
(define-key aiern-motion-state-map "['" 'aiern-previous-mark-line)
(define-key aiern-motion-state-map "[`" 'aiern-previous-mark)
(define-key aiern-motion-state-map "]s" 'aiern-next-flyspell-error)
(define-key aiern-motion-state-map "[s" 'aiern-prev-flyspell-error)
(define-key aiern-motion-state-map "*" 'aiern-search-word-forward)
(define-key aiern-motion-state-map "g*" 'aiern-search-unbounded-word-forward)
(define-key aiern-motion-state-map "," 'aiern-repeat-find-char-reverse)
(define-key aiern-motion-state-map "/" 'aiern-search-forward)
(define-key aiern-motion-state-map ";" 'aiern-repeat-find-char)
(define-key aiern-motion-state-map "?" 'aiern-search-backward)
(define-key aiern-motion-state-map "|" 'aiern-goto-column)
(define-key aiern-motion-state-map "^" 'aiern-first-non-blank)
(define-key aiern-motion-state-map "+" 'aiern-next-line-first-non-blank)
(define-key aiern-motion-state-map "_" 'aiern-next-line-1-first-non-blank)
(define-key aiern-motion-state-map "-" 'aiern-previous-line-first-non-blank)
(define-key aiern-motion-state-map "\C-w" 'aiern-window-map)
(define-key aiern-motion-state-map (kbd "C-6") 'aiern-switch-to-windows-last-buffer)
(define-key aiern-motion-state-map "\C-]" 'aiern-jump-to-tag)
(define-key aiern-motion-state-map (kbd "C-b") 'aiern-scroll-page-up)
(define-key aiern-motion-state-map (kbd "C-e") 'aiern-scroll-line-down)
(define-key aiern-motion-state-map (kbd "C-f") 'aiern-scroll-page-down)
(define-key aiern-motion-state-map (kbd "C-o") 'aiern-jump-backward)
(define-key aiern-motion-state-map (kbd "C-y") 'aiern-scroll-line-up)
(define-key aiern-motion-state-map (kbd "RET") 'aiern-ret)
(define-key aiern-motion-state-map "\\" 'aiern-execute-in-emacs-state)
(define-key aiern-motion-state-map "z^" 'aiern-scroll-top-line-to-bottom)
(define-key aiern-motion-state-map "z+" 'aiern-scroll-bottom-line-to-top)
(define-key aiern-motion-state-map "zt" 'aiern-scroll-line-to-top)
;; TODO: z RET has an advanced form taking an count before the RET
;; but this requires again a special state with a single command
;; bound to RET
(define-key aiern-motion-state-map (vconcat "z" [return]) "zt^")
(define-key aiern-motion-state-map (kbd "z RET") (vconcat "z" [return]))
(define-key aiern-motion-state-map "zz" 'aiern-scroll-line-to-center)
(define-key aiern-motion-state-map "z." "zz^")
(define-key aiern-motion-state-map "zb" 'aiern-scroll-line-to-bottom)
(define-key aiern-motion-state-map "z-" "zb^")
(define-key aiern-motion-state-map "v" 'aiern-visual-char)
(define-key aiern-motion-state-map "V" 'aiern-visual-line)
(define-key aiern-motion-state-map "\C-v" 'aiern-visual-block)
(define-key aiern-motion-state-map "gv" 'aiern-visual-restore)
(define-key aiern-motion-state-map (kbd "C-^") 'aiern-buffer)
(define-key aiern-motion-state-map [left] 'aiern-backward-char)
(define-key aiern-motion-state-map [right] 'aiern-forward-char)
(define-key aiern-motion-state-map [up] 'aiern-previous-line)
(define-key aiern-motion-state-map [down] 'aiern-next-line)
(define-key aiern-motion-state-map "zl" 'aiern-scroll-column-right)
(define-key aiern-motion-state-map [?z right] "zl")
(define-key aiern-motion-state-map "zh" 'aiern-scroll-column-left)
(define-key aiern-motion-state-map [?z left] "zh")
(define-key aiern-motion-state-map "zL" 'aiern-scroll-right)
(define-key aiern-motion-state-map "zH" 'aiern-scroll-left)
(define-key aiern-motion-state-map
  (read-kbd-macro aiern-toggle-key) 'aiern-emacs-state)

;; text objects
(define-key aiern-outer-text-objects-map "w" 'aiern-a-word)
(define-key aiern-outer-text-objects-map "W" 'aiern-a-WORD)
(define-key aiern-outer-text-objects-map "s" 'aiern-a-sentence)
(define-key aiern-outer-text-objects-map "p" 'aiern-a-paragraph)
(define-key aiern-outer-text-objects-map "b" 'aiern-a-paren)
(define-key aiern-outer-text-objects-map "(" 'aiern-a-paren)
(define-key aiern-outer-text-objects-map ")" 'aiern-a-paren)
(define-key aiern-outer-text-objects-map "[" 'aiern-a-bracket)
(define-key aiern-outer-text-objects-map "]" 'aiern-a-bracket)
(define-key aiern-outer-text-objects-map "B" 'aiern-a-curly)
(define-key aiern-outer-text-objects-map "{" 'aiern-a-curly)
(define-key aiern-outer-text-objects-map "}" 'aiern-a-curly)
(define-key aiern-outer-text-objects-map "<" 'aiern-an-angle)
(define-key aiern-outer-text-objects-map ">" 'aiern-an-angle)
(define-key aiern-outer-text-objects-map "'" 'aiern-a-single-quote)
(define-key aiern-outer-text-objects-map "\"" 'aiern-a-double-quote)
(define-key aiern-outer-text-objects-map "`" 'aiern-a-back-quote)
(define-key aiern-outer-text-objects-map "t" 'aiern-a-tag)
(define-key aiern-outer-text-objects-map "o" 'aiern-a-symbol)
(define-key aiern-inner-text-objects-map "w" 'aiern-inner-word)
(define-key aiern-inner-text-objects-map "W" 'aiern-inner-WORD)
(define-key aiern-inner-text-objects-map "s" 'aiern-inner-sentence)
(define-key aiern-inner-text-objects-map "p" 'aiern-inner-paragraph)
(define-key aiern-inner-text-objects-map "b" 'aiern-inner-paren)
(define-key aiern-inner-text-objects-map "(" 'aiern-inner-paren)
(define-key aiern-inner-text-objects-map ")" 'aiern-inner-paren)
(define-key aiern-inner-text-objects-map "[" 'aiern-inner-bracket)
(define-key aiern-inner-text-objects-map "]" 'aiern-inner-bracket)
(define-key aiern-inner-text-objects-map "B" 'aiern-inner-curly)
(define-key aiern-inner-text-objects-map "{" 'aiern-inner-curly)
(define-key aiern-inner-text-objects-map "}" 'aiern-inner-curly)
(define-key aiern-inner-text-objects-map "<" 'aiern-inner-angle)
(define-key aiern-inner-text-objects-map ">" 'aiern-inner-angle)
(define-key aiern-inner-text-objects-map "'" 'aiern-inner-single-quote)
(define-key aiern-inner-text-objects-map "\"" 'aiern-inner-double-quote)
(define-key aiern-inner-text-objects-map "`" 'aiern-inner-back-quote)
(define-key aiern-inner-text-objects-map "t" 'aiern-inner-tag)
(define-key aiern-inner-text-objects-map "o" 'aiern-inner-symbol)
(define-key aiern-motion-state-map "gn" 'aiern-next-match)
(define-key aiern-motion-state-map "gN" 'aiern-previous-match)

(when aiern-want-C-i-jump
  (define-key aiern-motion-state-map (kbd "C-i") 'aiern-jump-forward))

(when aiern-want-C-u-scroll
  (define-key aiern-motion-state-map (kbd "C-u") 'aiern-scroll-up))

(when aiern-want-C-d-scroll
  (define-key aiern-motion-state-map (kbd "C-d") 'aiern-scroll-down))

(when aiern-want-C-g-bindings
  (define-key aiern-motion-state-map "g\C-g" 'count-words))

;;; Visual state

(define-key aiern-visual-state-map "A" 'aiern-append)
(define-key aiern-visual-state-map "I" 'aiern-insert)
(define-key aiern-visual-state-map "o" 'exchange-point-and-mark)
(define-key aiern-visual-state-map "O" 'aiern-visual-exchange-corners)
(define-key aiern-visual-state-map "R" 'aiern-change-whole-line)
(define-key aiern-visual-state-map "u" 'aiern-downcase)
(define-key aiern-visual-state-map "U" 'aiern-upcase)
(define-key aiern-visual-state-map "z=" 'ispell-word)
(define-key aiern-visual-state-map "a" aiern-outer-text-objects-map)
(define-key aiern-visual-state-map "i" aiern-inner-text-objects-map)
(define-key aiern-visual-state-map (kbd "<insert>") 'undefined)
(define-key aiern-visual-state-map (kbd "<insertchar>") 'undefined)
(define-key aiern-visual-state-map [remap aiern-repeat] 'undefined)
(define-key aiern-visual-state-map [escape] 'aiern-exit-visual-state)
(define-key aiern-visual-state-map "gf" 'aiern-find-file-at-point-visual)

;;; Operator-Pending state

(define-key aiern-operator-state-map "a" aiern-outer-text-objects-map)
(define-key aiern-operator-state-map "i" aiern-inner-text-objects-map)
;; (define-key aiern-operator-state-map [escape] 'keyboard-quit)

;;; Insert state
(defvar aiern-insert-state-bindings
  `(;; ("\C-v" . quoted-insert)
    ;; ("\C-k" . aiern-insert-digraph)
    ;; ("\C-o" . aiern-execute-in-normal-state)
    ;; ("\C-r" . aiern-paste-from-register)
    ;; ("\C-y" . aiern-copy-from-above)
    ;; ("\C-e" . aiern-copy-from-below)
    ;; ("\C-n" . aiern-complete-next)
    ;; ("\C-p" . aiern-complete-previous)
    ;; ("\C-x\C-n" . aiern-complete-next-line)
    ;; ("\C-x\C-p" . aiern-complete-previous-line)
    ;; ("\C-t" . aiern-shift-right-line)
    ;; ("\C-d" . aiern-shift-left-line)
    ;; ("\C-a" . aiern-paste-last-insertion)
    ;; ([remap delete-backward-char] . aiern-delete-backward-char-and-join)
    ;; ,(if aiern-want-C-w-delete
    ;;      '("\C-w" . aiern-delete-backward-word)
    ;;    '("\C-w" . aiern-window-map))
    ;; ,@(when aiern-want-C-u-delete
    ;;     '(("\C-u" . aiern-delete-back-to-indentation)))
    ;; ([mouse-2] . mouse-yank-primary)
  )
  "aiern's bindings for insert state (for
`aiern-insert-state-map'), excluding <delete>, <escape>, and
`aiern-toggle-key'.")

(defun aiern-update-insert-state-bindings (&optional _option-name remove force)
  "Update bindings in `aiern-insert-state-map'.
If no arguments are given add the bindings specified in
`aiern-insert-state-bindings'. If REMOVE is non nil, remove only
these bindings. Unless FORCE is non nil, this will not
overwriting existing bindings, which means bindings will not be
added if one already exists for a key and only default bindings
are removed.

Note that <delete>, <escape> and `aiern-toggle-key' are not
included in `aiern-insert-state-bindings' by default."
  (interactive)
  (dolist (binding aiern-insert-state-bindings)
    (cond
     ((and remove
           (or force
               ;; Only remove if the default binding has not changed
               (eq (aiern-lookup-key aiern-insert-state-map (car binding))
                   (cdr binding))))
      (define-key aiern-insert-state-map (car binding) nil))
     ((and (null remove)
           (or force
               ;; Check to see that nothing is bound here before adding
               (not (aiern-lookup-key aiern-insert-state-map (car binding)))))
      (define-key aiern-insert-state-map (car binding) (cdr binding))))))

(define-key aiern-insert-state-map [delete] 'delete-char)
(define-key aiern-insert-state-map [escape] 'aiern-normal-state)
(define-key aiern-insert-state-map
  (read-kbd-macro aiern-toggle-key) 'aiern-emacs-state)

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/3883/31428
;; User: https://emacs.stackexchange.com/users/2454/alexander-shukaev 
(let ((c ?\s))
    (while (< c ?\d)
        (tag-def :keymaps 'aiern-insert-state-map (vector c) 'self-insert-command)
        (setq c (1+ c)))
    (when (eq system-type 'ms-dos)
        (setq c 128)
        (while (< c 160)
          (tag-def :keymaps 'aiern-insert-state-map (vector c) 'self-insert-command)
          (setq c (1+ c))))
    (setq c 160)
    (while (< c 256)
        (tag-def :keymaps 'aiern-insert-state-map (vector c) 'self-insert-command)
        (setq c (1+ c))))

(tag-def :keymaps 'aiern-insert-state-map
  ;; NOTE: When I couldn't find this, I used `command-log-mode':
  ;; http://ergoemacs.org/emacs/emacs_show_key_and_command.html
  [deletechar] 'delete-char

  ;; NOTE: This is actually backspace...
  ;; "DEL" 'delete-backward-char

  [?\d] 'delete-backward-char
  (naked "RET") 'newline-and-indent)

(tag-def :keymaps '(aiern-insert-state-map aiern-normal-state-map)
  "C-x C-c" 'save-buffers-kill-terminal
  [t] 'self-insert-command

  ;; Adapted From:
  ;; Answer: https://stackoverflow.com/a/4557027/10827766
  ;; User: https://stackoverflow.com/users/387076/gilles-so-stop-being-evil
  "\eOA" '[up]
  "\e[A" '[up]
  "\eOB" '[down]
  "\e[B" '[down]
  "\eOD" '[left]
  "\e[D" '[left]
  "\eOC" '[right]
  "\e[C" '[right]

  [up] 'previous-line
  [down] 'next-line
  [left] 'backward-char
  [right] 'forward-char)

(mapc #'(lambda (state) (interactive)
  (add-hook (intern (concat "aiern-" state "-state-entry-hook")) #'(lambda nil (interactive)
    `(use-global-map ,(intern (concat "aiern-" state "-state-map")))))
  (add-hook (intern (concat "aiern-" state "-state-exit-hook")) #'(lambda nil (interactive)
    (use-global-map global-map))))
  '("insert" "normal"))

;;; Replace state

(define-key aiern-replace-state-map (kbd "DEL") 'aiern-replace-backspace)
(define-key aiern-replace-state-map [escape] 'aiern-normal-state)

;;; Emacs state

(define-key aiern-emacs-state-map
  (read-kbd-macro aiern-toggle-key) 'aiern-exit-emacs-state)

(when aiern-want-C-w-in-emacs-state
  (define-key aiern-emacs-state-map "\C-w" 'aiern-window-map))

;;; Mouse
(define-key aiern-motion-state-map [down-mouse-1] 'aiern-mouse-drag-region)
(define-key aiern-visual-state-map [mouse-2] 'aiern-exit-visual-and-repeat)
(define-key aiern-normal-state-map [mouse-2] 'mouse-yank-primary)

;; Ex
(define-key aiern-motion-state-map ":" 'aiern-ex)
(define-key aiern-motion-state-map "!" 'aiern-shell-command)

;; (aiern-ex-define-cmd "e[dit]" 'aiern-edit)
(aiern-ex-define-cmd "w[rite]" 'aiern-write)
;; (aiern-ex-define-cmd "wa[ll]" 'aiern-write-all)
;; (aiern-ex-define-cmd "sav[eas]" 'aiern-save)
;; (aiern-ex-define-cmd "r[ead]" 'aiern-read)
;; (aiern-ex-define-cmd "b[uffer]" 'aiern-buffer)
;; (aiern-ex-define-cmd "bn[ext]" 'aiern-next-buffer)
;; (aiern-ex-define-cmd "bp[revious]" 'aiern-prev-buffer)
;; (aiern-ex-define-cmd "bN[ext]" "bprevious")
;; (aiern-ex-define-cmd "sb[uffer]" 'aiern-split-buffer)
;; (aiern-ex-define-cmd "sbn[ext]" 'aiern-split-next-buffer)
;; (aiern-ex-define-cmd "sbp[revious]" 'aiern-split-prev-buffer)
;; (aiern-ex-define-cmd "sbN[ext]" "sbprevious")
;; (aiern-ex-define-cmd "buffers" 'buffer-menu)
;; (aiern-ex-define-cmd "files" 'aiern-show-files)
;; (aiern-ex-define-cmd "ls" "buffers")

;; (aiern-ex-define-cmd "c[hange]" 'aiern-change)
;; (aiern-ex-define-cmd "co[py]" 'aiern-copy)
;; (aiern-ex-define-cmd "t" "copy")
;; (aiern-ex-define-cmd "m[ove]" 'aiern-move)
;; (aiern-ex-define-cmd "d[elete]" 'aiern-ex-delete)
;; (aiern-ex-define-cmd "y[ank]" 'aiern-ex-yank)
;; (aiern-ex-define-cmd "pu[t]" 'aiern-ex-put)
;; (aiern-ex-define-cmd "go[to]" 'aiern-goto-char)
;; (aiern-ex-define-cmd "j[oin]" 'aiern-ex-join)
;; (aiern-ex-define-cmd "le[ft]" 'aiern-align-left)
;; (aiern-ex-define-cmd "ri[ght]" 'aiern-align-right)
;; (aiern-ex-define-cmd "ce[nter]" 'aiern-align-center)
;; (aiern-ex-define-cmd "sp[lit]" 'aiern-window-split)
;; (aiern-ex-define-cmd "vs[plit]" 'aiern-window-vsplit)
;; (aiern-ex-define-cmd "new" 'aiern-window-new)
;; (aiern-ex-define-cmd "ene[w]" 'aiern-buffer-new)
;; (aiern-ex-define-cmd "vne[w]" 'aiern-window-vnew)
;; (aiern-ex-define-cmd "clo[se]" 'aiern-window-delete)
;; (aiern-ex-define-cmd "on[ly]" 'delete-other-windows)
(aiern-ex-define-cmd "q[uit]" 'aiern-quit)
;; (aiern-ex-define-cmd "wq" 'aiern-save-and-close)
;; (aiern-ex-define-cmd "quita[ll]" 'aiern-quit-all)
;; (aiern-ex-define-cmd "qa[ll]" "quitall")
;; (aiern-ex-define-cmd "cq[uit]" 'aiern-quit-all-with-error-code)
;; (aiern-ex-define-cmd "wqa[ll]" 'aiern-save-and-quit)
;; (aiern-ex-define-cmd "xa[ll]" "wqall")
;; (aiern-ex-define-cmd "x[it]" 'aiern-save-modified-and-close)
;; (aiern-ex-define-cmd "exi[t]" 'aiern-save-modified-and-close)
;; (aiern-ex-define-cmd "bd[elete]" 'aiern-delete-buffer)
;; (aiern-ex-define-cmd "bw[ipeout]" 'aiern-delete-buffer)
;; (aiern-ex-define-cmd "g[lobal]" 'aiern-ex-global)
;; (aiern-ex-define-cmd "v[global]" 'aiern-ex-global-inverted)
;; (aiern-ex-define-cmd "norm[al]" 'aiern-ex-normal)
;; (aiern-ex-define-cmd "s[ubstitute]" 'aiern-ex-substitute)
;; (aiern-ex-define-cmd "&" 'aiern-ex-repeat-substitute)
;; (aiern-ex-define-cmd "&&" 'aiern-ex-repeat-substitute-with-flags)
;; (aiern-ex-define-cmd "~" 'aiern-ex-repeat-substitute-with-search)
;; (aiern-ex-define-cmd "~&" 'aiern-ex-repeat-substitute-with-search-and-flags)
;; (aiern-ex-define-cmd "registers" 'aiern-show-registers)
;; (aiern-ex-define-cmd "di[splay]" "registers")
;; (aiern-ex-define-cmd "ma[rk]" 'aiern-set-col-0-mark)
;; (aiern-ex-define-cmd "marks" 'aiern-show-marks)
;; (aiern-ex-define-cmd "delm[arks]" 'aiern-delete-marks)
;; (aiern-ex-define-cmd "ju[mps]" 'aiern-show-jumps)
;; (aiern-ex-define-cmd "noh[lsearch]" 'aiern-ex-nohighlight)
;; (aiern-ex-define-cmd "f[ile]" 'aiern-show-file-info)
;; (aiern-ex-define-cmd "<" 'aiern-shift-left)
;; (aiern-ex-define-cmd ">" 'aiern-shift-right)
;; (aiern-ex-define-cmd "=" 'aiern-ex-line-number)
;; (aiern-ex-define-cmd "!" 'aiern-shell-command)
;; (aiern-ex-define-cmd "@:" 'aiern-ex-repeat)
;; (aiern-ex-define-cmd "mak[e]" 'aiern-make)
;; (aiern-ex-define-cmd "cc" 'aiern-goto-error)
;; (aiern-ex-define-cmd "cfir[st]" 'first-error)
;; (aiern-ex-define-cmd "cr[ewind]" 'first-error)
;; (aiern-ex-define-cmd "cn[ext]" 'next-error)
;; (aiern-ex-define-cmd "cp[revious]" 'previous-error)
;; (aiern-ex-define-cmd "set-initial-state" 'aiern-ex-set-initial-state)
;; (aiern-ex-define-cmd "show-digraphs" 'aiern-ex-show-digraphs)
;; (aiern-ex-define-cmd "sor[t]" 'aiern-ex-sort)
;; (aiern-ex-define-cmd "res[ize]" 'aiern-ex-resize)
;; (aiern-ex-define-cmd "u[ndo]" 'aiern-undo)
;; (aiern-ex-define-cmd "red[o]" 'aiern-redo)

;; (when (featurep 'tab-bar)
;;   (aiern-ex-define-cmd "tabnew" 'tab-bar-new-tab)
;;   (aiern-ex-define-cmd "tabn[ext]" 'tab-bar-switch-to-next-tab)
;;   (aiern-ex-define-cmd "tabp[revious]" 'tab-bar-switch-to-prev-tab))

;; search command line
(define-key aiern-ex-search-keymap "\d" #'aiern-ex-delete-backward-char)
(define-key aiern-ex-search-keymap "\C-b" 'move-beginning-of-line)
(define-key aiern-ex-search-keymap "\C-c" 'abort-recursive-edit)
(define-key aiern-ex-search-keymap "\C-g" 'abort-recursive-edit)
(define-key aiern-ex-search-keymap "\C-k" 'aiern-insert-digraph)
(define-key aiern-ex-search-keymap "\C-f" 'aiern-ex-search-command-window)
(define-key aiern-ex-search-keymap "\C-r" 'aiern-paste-from-register)
(define-key aiern-ex-search-keymap "\C-n" 'next-history-element)
(define-key aiern-ex-search-keymap "\C-p" 'previous-history-element)
(define-key aiern-ex-search-keymap "\C-u" 'aiern-delete-whole-line)
(define-key aiern-ex-search-keymap "\C-v" #'quoted-insert)
(define-key aiern-ex-search-keymap "\C-w" 'backward-kill-word)

;; ex command line
(define-key aiern-ex-completion-map "\d" #'aiern-ex-delete-backward-char)
(define-key aiern-ex-completion-map "\t" #'aiern-ex-completion)
(define-key aiern-ex-completion-map [tab] #'aiern-ex-completion)
(define-key aiern-ex-completion-map [remap completion-at-point] #'aiern-ex-completion)
(define-key aiern-ex-completion-map "\C-a" 'aiern-ex-completion)
(define-key aiern-ex-completion-map "\C-b" 'move-beginning-of-line)
(define-key aiern-ex-completion-map "\C-c" 'abort-recursive-edit)
(define-key aiern-ex-completion-map "\C-d" 'aiern-ex-completion)
(define-key aiern-ex-completion-map "\C-f" 'aiern-ex-command-window)
(define-key aiern-ex-completion-map "\C-g" 'abort-recursive-edit)
(define-key aiern-ex-completion-map "\C-k" 'aiern-insert-digraph)
(define-key aiern-ex-completion-map "\C-l" 'aiern-ex-completion)
(define-key aiern-ex-completion-map "\C-p" #'previous-complete-history-element)
(define-key aiern-ex-completion-map "\C-r" 'aiern-paste-from-register)
(define-key aiern-ex-completion-map "\C-n" #'next-complete-history-element)
(define-key aiern-ex-completion-map "\C-u" 'aiern-delete-whole-line)
(define-key aiern-ex-completion-map "\C-v" #'quoted-insert)
(define-key aiern-ex-completion-map "\C-w" 'backward-kill-word)
(define-key aiern-ex-completion-map [escape] 'abort-recursive-edit)
(define-key aiern-ex-completion-map [S-left] 'backward-word)
(define-key aiern-ex-completion-map [S-right] 'forward-word)
(define-key aiern-ex-completion-map [up] 'previous-complete-history-element)
(define-key aiern-ex-completion-map [down] 'next-complete-history-element)
(define-key aiern-ex-completion-map [prior] 'previous-history-element)
(define-key aiern-ex-completion-map [next] 'next-history-element)
(define-key aiern-ex-completion-map [return] 'exit-minibuffer)
(define-key aiern-ex-completion-map (kbd "RET") 'exit-minibuffer)

;; eval prompt (the `=' register)
(define-key aiern-eval-map "\C-b" 'move-beginning-of-line)
(define-key aiern-eval-map "\C-c" 'abort-recursive-edit)
(define-key aiern-eval-map "\C-g" 'abort-recursive-edit)
(define-key aiern-eval-map "\C-k" 'aiern-insert-digraph)
(define-key aiern-eval-map "\C-p" #'previous-complete-history-element)
(define-key aiern-eval-map "\C-r" 'aiern-paste-from-register)
(define-key aiern-eval-map "\C-n" #'next-complete-history-element)
(define-key aiern-eval-map "\C-u" 'aiern-delete-whole-line)
(define-key aiern-eval-map "\C-v" #'quoted-insert)
(define-key aiern-eval-map "\C-w" 'backward-kill-word)
(define-key aiern-eval-map [escape] 'abort-recursive-edit)
(define-key aiern-eval-map [S-left] 'backward-word)
(define-key aiern-eval-map [S-right] 'forward-word)
(define-key aiern-eval-map [up] 'previous-complete-history-element)
(define-key aiern-eval-map [down] 'next-complete-history-element)
(define-key aiern-eval-map [prior] 'previous-history-element)
(define-key aiern-eval-map [next] 'next-history-element)
(define-key aiern-eval-map [return] 'exit-minibuffer)
(define-key aiern-eval-map (kbd "RET") 'exit-minibuffer)

;; aiern-read-key
(define-key aiern-read-key-map (kbd "ESC") #'keyboard-quit)
(define-key aiern-read-key-map (kbd "C-]") #'keyboard-quit)
(define-key aiern-read-key-map (kbd "C-g") #'keyboard-quit)
(define-key aiern-read-key-map (kbd "C-q") #'aiern-read-quoted-char)
(define-key aiern-read-key-map (kbd "C-v") #'aiern-read-quoted-char)
(define-key aiern-read-key-map (kbd "C-k") #'aiern-read-digraph-char)
(define-key aiern-read-key-map "\r" "\n")

;; command line window
(aiern-define-key 'normal
  aiern-command-window-mode-map (kbd "RET") 'aiern-command-window-execute)
(aiern-define-key 'insert
  aiern-command-window-mode-map (kbd "RET") 'aiern-command-window-execute)

(provide 'aiern-maps)

;;; aiern-maps.el ends here
