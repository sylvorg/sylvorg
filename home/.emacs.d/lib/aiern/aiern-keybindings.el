;;; aiern-keybindings.el --- Add some aiern keybindings to other modules -*- lexical-binding: t -*-

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

;;; Commentary:

;; This provides a set of keybindings for other emacs modes. This also includes
;; setting up the initial aiern state of those other modes.

;;; Code:

(require 'aiern-maps)
(require 'aiern-core)
(require 'aiern-macros)
(require 'aiern-types)
(require 'aiern-repeat)

;; etags-select
;; FIXME: probably etags-select should be recomended in docs
(eval-after-load 'etags-select
  '(progn
     (define-key aiern-motion-state-map "g]" 'etags-select-find-tag-at-point)))

;;; Buffer-menu

(aiern-add-hjkl-bindings Buffer-menu-mode-map 'motion)

;; dictionary.el

(aiern-add-hjkl-bindings dictionary-mode-map 'motion
  "?" 'dictionary-help        ; "h"
  "C-o" 'dictionary-previous) ; "l"

;;; Dired

(eval-after-load 'dired
  '(progn
     ;; use the standard Dired bindings as a base
     (defvar dired-mode-map)
     (aiern-make-overriding-map dired-mode-map 'normal)
     (aiern-add-hjkl-bindings dired-mode-map 'normal
       "J" 'dired-goto-file                   ; "j"
       "K" 'dired-do-kill-lines               ; "k"
       "r" 'dired-do-redisplay                ; "l"
       ;; ":d", ":v", ":s", ":e"
       ";" (lookup-key dired-mode-map ":"))))

;;; ERT

(aiern-add-hjkl-bindings ert-results-mode-map 'motion)

;;; Info

(aiern-add-hjkl-bindings Info-mode-map 'motion
  "0" 'aiern-digit-argument-or-aiern-beginning-of-line
  (kbd "\M-h") 'Info-help   ; "h"
  "\C-t" 'Info-history-back ; "l"
  "\C-o" 'Info-history-back
  " " 'Info-scroll-up
  "\C-]" 'Info-follow-nearest-node
  (kbd "DEL") 'Info-scroll-down)

;;; Speedbar

(aiern-add-hjkl-bindings speedbar-key-map 'motion
  "h" 'backward-char
  "j" 'speedbar-next
  "k" 'speedbar-prev
  "l" 'forward-char
  "i" 'speedbar-item-info
  "r" 'speedbar-refresh
  "u" 'speedbar-up-directory
  "o" 'speedbar-toggle-line-expansion
  (kbd "RET") 'speedbar-edit-line)

;; Ibuffer
(eval-after-load 'ibuffer
  '(progn
     (defvar ibuffer-mode-map)
     (aiern-make-overriding-map ibuffer-mode-map 'normal)
     (aiern-define-key 'normal ibuffer-mode-map
       "j" 'aiern-next-line
       "k" 'aiern-previous-line
       "RET" 'ibuffer-visit-buffer)))

;;; ag.el
(eval-after-load 'ag
  '(progn
     (defvar ag-mode-map)
     (add-to-list 'aiern-motion-state-modes 'ag-mode)
     (aiern-add-hjkl-bindings ag-mode-map 'motion)))

;;; ELP

(eval-after-load 'elp
  '(defadvice elp-results (after aiern activate)
     (aiern-motion-state)))

(provide 'aiern-keybindings)

;;; aiern-keybindings.el ends here
