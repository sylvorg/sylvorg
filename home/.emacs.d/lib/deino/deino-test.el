;;; deino-test.el --- Tests for deino

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'deino)
(setq text-quoting-style 'grave)
(message "Emacs version: %s" emacs-version)

(ert-deftest deino-red-error ()
  (should
   (equal
    (macroexpand
     '(defdeino deino-error (global-map "M-g")
       "error"
       ("h" first-error "first")
       ("j" next-error "next")
       ("k" previous-error "prev")
       ("SPC" deino-repeat "rep" :bind nil)))
    '(progn
      (set
       (defvar deino-error/params nil
         "Params of deino-error.")
       (quote (global-map "M-g")))
      (set
       (defvar deino-error/docstring nil
         "Docstring of deino-error.")
       "error")
      (set
       (defvar deino-error/heads nil
         "Heads for deino-error.")
       (quote
        (("h"
          first-error
          "first"
          :exit nil)
         ("j"
          next-error
          "next"
          :exit nil)
         ("k"
          previous-error
          "prev"
          :exit nil)
         ("SPC"
          deino-repeat
          "rep"
          :bind nil
          :exit nil))))
      (set
       (defvar deino-error/keymap nil
         "Keymap for deino-error.")
       (quote
        (keymap
         (32 . deino-repeat)
         (107 . deino-error/previous-error)
         (106 . deino-error/next-error)
         (104 . deino-error/first-error)
         (kp-subtract . deino--negative-argument)
         (kp-9 . deino--digit-argument)
         (kp-8 . deino--digit-argument)
         (kp-7 . deino--digit-argument)
         (kp-6 . deino--digit-argument)
         (kp-5 . deino--digit-argument)
         (kp-4 . deino--digit-argument)
         (kp-3 . deino--digit-argument)
         (kp-2 . deino--digit-argument)
         (kp-1 . deino--digit-argument)
         (kp-0 . deino--digit-argument)
         (57 . deino--digit-argument)
         (56 . deino--digit-argument)
         (55 . deino--digit-argument)
         (54 . deino--digit-argument)
         (53 . deino--digit-argument)
         (52 . deino--digit-argument)
         (51 . deino--digit-argument)
         (50 . deino--digit-argument)
         (49 . deino--digit-argument)
         (48 . deino--digit-argument)
         (45 . deino--negative-argument)
         (21 . deino--universal-argument))))
      (set
       (defvar deino-error/hint nil
         "Dynamic hint for deino-error.")
       (quote
        (format
         #("error: [h]: first, [j]: next, [k]: prev, [SPC]: rep."
           8 9 (face deino-face-red)
           20 21 (face deino-face-red)
           31 32 (face deino-face-red)
           42 45 (face deino-face-red)))))
      (defun deino-error/first-error nil
        "Call the head `first-error' in the \"deino-error\" deino.

The heads for the associated deino are:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error',
\"SPC\":    `deino-repeat'

The body can be accessed via `deino-error/body', which is bound to \"M-g\"."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (let ((deino--ignore t))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-error/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote first-error))
              (deino--call-interactively-remap-maybe
               (function first-error)))
          ((quit error)
           (message
            (error-message-string err))))
        (deino-show-hint
         deino-error/hint
         (quote deino-error))
        (deino-set-transient-map
         deino-error/keymap
         (lambda nil
           (deino-keyboard-quit)
           nil)
         nil))
      (defun deino-error/next-error nil
        "Call the head `next-error' in the \"deino-error\" deino.

The heads for the associated deino are:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error',
\"SPC\":    `deino-repeat'

The body can be accessed via `deino-error/body', which is bound to \"M-g\"."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (let ((deino--ignore t))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-error/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote next-error))
              (deino--call-interactively-remap-maybe
               (function next-error)))
          ((quit error)
           (message
            (error-message-string err))))
        (deino-show-hint
         deino-error/hint
         (quote deino-error))
        (deino-set-transient-map
         deino-error/keymap
         (lambda nil
           (deino-keyboard-quit)
           nil)
         nil))
      (defun deino-error/previous-error nil
        "Call the head `previous-error' in the \"deino-error\" deino.

The heads for the associated deino are:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error',
\"SPC\":    `deino-repeat'

The body can be accessed via `deino-error/body', which is bound to \"M-g\"."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (let ((deino--ignore t))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-error/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote previous-error))
              (deino--call-interactively-remap-maybe
               (function previous-error)))
          ((quit error)
           (message
            (error-message-string err))))
        (deino-show-hint
         deino-error/hint
         (quote deino-error))
        (deino-set-transient-map
         deino-error/keymap
         (lambda nil
           (deino-keyboard-quit)
           nil)
         nil))
      (unless (keymapp
               (lookup-key
                global-map
                (kbd "M-g")))
        (define-key global-map (kbd "M-g")
          nil))
      (define-key global-map [134217831 104]
       (quote deino-error/first-error))
      (define-key global-map [134217831 106]
       (quote deino-error/next-error))
      (define-key global-map [134217831 107]
       (quote
        deino-error/previous-error))
      (defun deino-error/body nil
        "Call the body in the \"deino-error\" deino.

The heads for the associated deino are:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error',
\"SPC\":    `deino-repeat'

The body can be accessed via `deino-error/body', which is bound to \"M-g\"."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (let ((deino--ignore nil))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-error/body)))
        (deino-show-hint
         deino-error/hint
         (quote deino-error))
        (deino-set-transient-map
         deino-error/keymap
         (lambda nil
           (deino-keyboard-quit)
           nil)
         nil)
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest deino-blue-toggle ()
  (should
   (equal
    (macroexpand
     '(defdeino deino-toggle (:color blue)
       "toggle"
       ("t" toggle-truncate-lines "truncate")
       ("f" auto-fill-mode "fill")
       ("a" abbrev-mode "abbrev")
       ("q" nil "cancel")))
    '(progn
      (set
       (defvar deino-toggle/params nil
         "Params of deino-toggle.")
       (quote
        (nil
         nil
         :exit t
         :foreign-keys nil)))
      (set
       (defvar deino-toggle/docstring nil
         "Docstring of deino-toggle.")
       "toggle")
      (set
       (defvar deino-toggle/heads nil
         "Heads for deino-toggle.")
       (quote
        (("t"
          toggle-truncate-lines
          "truncate"
          :exit t)
         ("f"
          auto-fill-mode
          "fill"
          :exit t)
         ("a"
          abbrev-mode
          "abbrev"
          :exit t)
         ("q" nil "cancel" :exit t))))
      (set
       (defvar deino-toggle/keymap nil
         "Keymap for deino-toggle.")
       (quote
        (keymap
         (113 . deino-toggle/nil)
         (97 . deino-toggle/abbrev-mode-and-exit)
         (102 . deino-toggle/auto-fill-mode-and-exit)
         (116 . deino-toggle/toggle-truncate-lines-and-exit)
         (kp-subtract . deino--negative-argument)
         (kp-9 . deino--digit-argument)
         (kp-8 . deino--digit-argument)
         (kp-7 . deino--digit-argument)
         (kp-6 . deino--digit-argument)
         (kp-5 . deino--digit-argument)
         (kp-4 . deino--digit-argument)
         (kp-3 . deino--digit-argument)
         (kp-2 . deino--digit-argument)
         (kp-1 . deino--digit-argument)
         (kp-0 . deino--digit-argument)
         (57 . deino--digit-argument)
         (56 . deino--digit-argument)
         (55 . deino--digit-argument)
         (54 . deino--digit-argument)
         (53 . deino--digit-argument)
         (52 . deino--digit-argument)
         (51 . deino--digit-argument)
         (50 . deino--digit-argument)
         (49 . deino--digit-argument)
         (48 . deino--digit-argument)
         (45 . deino--negative-argument)
         (21 . deino--universal-argument))))
      (set
       (defvar deino-toggle/hint nil
         "Dynamic hint for deino-toggle.")
       (quote
        (format
         #("toggle: [t]: truncate, [f]: fill, [a]: abbrev, [q]: cancel."
           9 10 (face deino-face-blue)
           24 25 (face deino-face-blue)
           35 36 (face deino-face-blue)
           48 49 (face deino-face-blue)))))
      (defun deino-toggle/toggle-truncate-lines-and-exit nil
        "Call the head `toggle-truncate-lines' in the \"deino-toggle\" deino.

The heads for the associated deino are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `deino-toggle/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (deino-keyboard-quit)
        (setq deino-curr-body-fn
              (quote deino-toggle/body))
        (progn
          (setq this-command
                (quote toggle-truncate-lines))
          (deino--call-interactively-remap-maybe
           (function
            toggle-truncate-lines))))
      (defun deino-toggle/auto-fill-mode-and-exit nil
        "Call the head `auto-fill-mode' in the \"deino-toggle\" deino.

The heads for the associated deino are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `deino-toggle/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (deino-keyboard-quit)
        (setq deino-curr-body-fn
              (quote deino-toggle/body))
        (progn
          (setq this-command
                (quote auto-fill-mode))
          (deino--call-interactively-remap-maybe
           (function auto-fill-mode))))
      (defun deino-toggle/abbrev-mode-and-exit nil
        "Call the head `abbrev-mode' in the \"deino-toggle\" deino.

The heads for the associated deino are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `deino-toggle/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (deino-keyboard-quit)
        (setq deino-curr-body-fn
              (quote deino-toggle/body))
        (progn
          (setq this-command
                (quote abbrev-mode))
          (deino--call-interactively-remap-maybe
           (function abbrev-mode))))
      (defun deino-toggle/nil nil
        "Call the head `nil' in the \"deino-toggle\" deino.

The heads for the associated deino are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `deino-toggle/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (deino-keyboard-quit)
        (setq deino-curr-body-fn
              (quote deino-toggle/body)))
      (defun deino-toggle/body nil
        "Call the body in the \"deino-toggle\" deino.

The heads for the associated deino are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `deino-toggle/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (let ((deino--ignore nil))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-toggle/body)))
        (deino-show-hint
         deino-toggle/hint
         (quote deino-toggle))
        (deino-set-transient-map
         deino-toggle/keymap
         (lambda nil
           (deino-keyboard-quit)
           nil)
         nil)
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest deino-amaranth-vi ()
  (should
   (equal
    (macroexpand
     '(defdeino deino-vi
       (:pre
        (set-cursor-color "#e52b50")
        :post
        (set-cursor-color "#ffffff")
        :color amaranth)
       "vi"
       ("j" next-line)
       ("k" previous-line)
       ("q" nil "quit")))
    '(progn
      (set
       (defvar deino-vi/params nil
         "Params of deino-vi.")
       (quote
        (nil
         nil
         :exit nil
         :foreign-keys warn
         :post (set-cursor-color "#ffffff")
         :pre (set-cursor-color "#e52b50"))))
      (set
       (defvar deino-vi/docstring nil
         "Docstring of deino-vi.")
       "vi")
      (set
       (defvar deino-vi/heads nil
         "Heads for deino-vi.")
       (quote
        (("j" next-line "" :exit nil)
         ("k"
          previous-line
          ""
          :exit nil)
         ("q" nil "quit" :exit t))))
      (set
       (defvar deino-vi/keymap nil
         "Keymap for deino-vi.")
       (quote
        (keymap
         (113 . deino-vi/nil)
         (107 . deino-vi/previous-line)
         (106 . deino-vi/next-line)
         (kp-subtract . deino--negative-argument)
         (kp-9 . deino--digit-argument)
         (kp-8 . deino--digit-argument)
         (kp-7 . deino--digit-argument)
         (kp-6 . deino--digit-argument)
         (kp-5 . deino--digit-argument)
         (kp-4 . deino--digit-argument)
         (kp-3 . deino--digit-argument)
         (kp-2 . deino--digit-argument)
         (kp-1 . deino--digit-argument)
         (kp-0 . deino--digit-argument)
         (57 . deino--digit-argument)
         (56 . deino--digit-argument)
         (55 . deino--digit-argument)
         (54 . deino--digit-argument)
         (53 . deino--digit-argument)
         (52 . deino--digit-argument)
         (51 . deino--digit-argument)
         (50 . deino--digit-argument)
         (49 . deino--digit-argument)
         (48 . deino--digit-argument)
         (45 . deino--negative-argument)
         (21 . deino--universal-argument))))
      (set
       (defvar deino-vi/hint nil
         "Dynamic hint for deino-vi.")
       (quote
        (format
         #("vi: j, k, [q]: quit."
           4 5 (face deino-face-amaranth)
           7 8 (face deino-face-amaranth)
           11 12 (face deino-face-teal)))))
      (defun deino-vi/next-line nil
        "Call the head `next-line' in the \"deino-vi\" deino.

The heads for the associated deino are:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `deino-vi/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (set-cursor-color "#e52b50")
        (let ((deino--ignore t))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-vi/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote next-line))
              (deino--call-interactively-remap-maybe
               (function next-line)))
          ((quit error)
           (message
            (error-message-string err))))
        (deino-show-hint
         deino-vi/hint
         (quote deino-vi))
        (deino-set-transient-map
         deino-vi/keymap
         (lambda nil
           (deino-keyboard-quit)
           (set-cursor-color "#ffffff"))
         (quote warn)))
      (defun deino-vi/previous-line nil
        "Call the head `previous-line' in the \"deino-vi\" deino.

The heads for the associated deino are:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `deino-vi/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (set-cursor-color "#e52b50")
        (let ((deino--ignore t))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-vi/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote previous-line))
              (deino--call-interactively-remap-maybe
               (function previous-line)))
          ((quit error)
           (message
            (error-message-string err))))
        (deino-show-hint
         deino-vi/hint
         (quote deino-vi))
        (deino-set-transient-map
         deino-vi/keymap
         (lambda nil
           (deino-keyboard-quit)
           (set-cursor-color "#ffffff"))
         (quote warn)))
      (defun deino-vi/nil nil
        "Call the head `nil' in the \"deino-vi\" deino.

The heads for the associated deino are:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `deino-vi/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (set-cursor-color "#e52b50")
        (deino-keyboard-quit)
        (setq deino-curr-body-fn
              (quote deino-vi/body)))
      (defun deino-vi/body nil
        "Call the body in the \"deino-vi\" deino.

The heads for the associated deino are:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `deino-vi/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (set-cursor-color "#e52b50")
        (let ((deino--ignore nil))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-vi/body)))
        (deino-show-hint
         deino-vi/hint
         (quote deino-vi))
        (deino-set-transient-map
         deino-vi/keymap
         (lambda nil
           (deino-keyboard-quit)
           (set-cursor-color "#ffffff"))
         (quote warn))
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest deino-zoom-duplicate-1 ()
  (should
   (equal
    (macroexpand
     '(defdeino deino-zoom ()
       "zoom"
       ("r" (text-scale-set 0) "reset")
       ("0" (text-scale-set 0) :bind nil :exit t)
       ("1" (text-scale-set 0) nil :bind nil :exit t)))
    '(progn
      (set
       (defvar deino-zoom/params nil
         "Params of deino-zoom.")
       (quote (nil nil)))
      (set
       (defvar deino-zoom/docstring nil
         "Docstring of deino-zoom.")
       "zoom")
      (set
       (defvar deino-zoom/heads nil
         "Heads for deino-zoom.")
       (quote
        (("r"
          (text-scale-set 0)
          "reset"
          :exit nil)
         ("0"
          (text-scale-set 0)
          ""
          :bind nil
          :exit t)
         ("1"
          (text-scale-set 0)
          nil
          :bind nil
          :exit t))))
      (set
       (defvar deino-zoom/keymap nil
         "Keymap for deino-zoom.")
       (quote
        (keymap
         (114 . deino-zoom/lambda-r)
         (kp-subtract . deino--negative-argument)
         (kp-9 . deino--digit-argument)
         (kp-8 . deino--digit-argument)
         (kp-7 . deino--digit-argument)
         (kp-6 . deino--digit-argument)
         (kp-5 . deino--digit-argument)
         (kp-4 . deino--digit-argument)
         (kp-3 . deino--digit-argument)
         (kp-2 . deino--digit-argument)
         (kp-1 . deino--digit-argument)
         (kp-0 . deino--digit-argument)
         (57 . deino--digit-argument)
         (56 . deino--digit-argument)
         (55 . deino--digit-argument)
         (54 . deino--digit-argument)
         (53 . deino--digit-argument)
         (52 . deino--digit-argument)
         (51 . deino--digit-argument)
         (50 . deino--digit-argument)
         (49 . deino-zoom/lambda-0-and-exit)
         (48 . deino-zoom/lambda-0-and-exit)
         (45 . deino--negative-argument)
         (21 . deino--universal-argument))))
      (set
       (defvar deino-zoom/hint nil
         "Dynamic hint for deino-zoom.")
       (quote
        (format
         #("zoom: [r 0]: reset."
           7 8 (face deino-face-red)
           9 10 (face deino-face-blue)))))
      (defun deino-zoom/lambda-r nil
        "Call the head `(text-scale-set 0)' in the \"deino-zoom\" deino.

The heads for the associated deino are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `deino-zoom/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (let ((deino--ignore t))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-zoom/body)))
        (condition-case err
            (deino--call-interactively-remap-maybe
             (function
              (lambda nil
               (interactive)
               (text-scale-set 0))))
          ((quit error)
           (message
            (error-message-string err))))
        (deino-show-hint
         deino-zoom/hint
         (quote deino-zoom))
        (deino-set-transient-map
         deino-zoom/keymap
         (lambda nil
           (deino-keyboard-quit)
           nil)
         nil))
      (defun deino-zoom/lambda-0-and-exit nil
        "Call the head `(text-scale-set 0)' in the \"deino-zoom\" deino.

The heads for the associated deino are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `deino-zoom/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (deino-keyboard-quit)
        (setq deino-curr-body-fn
              (quote deino-zoom/body))
        (deino--call-interactively-remap-maybe
         (function
          (lambda nil
           (interactive)
           (text-scale-set 0)))))
      (defun deino-zoom/body nil
        "Call the body in the \"deino-zoom\" deino.

The heads for the associated deino are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `deino-zoom/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (let ((deino--ignore nil))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-zoom/body)))
        (deino-show-hint
         deino-zoom/hint
         (quote deino-zoom))
        (deino-set-transient-map
         deino-zoom/keymap
         (lambda nil
           (deino-keyboard-quit)
           nil)
         nil)
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest deino-zoom-duplicate-2 ()
  (should
   (equal
    (macroexpand
     '(defdeino deino-zoom ()
       "zoom"
       ("r" (text-scale-set 0) "reset")
       ("0" (text-scale-set 0) :bind nil :exit t)
       ("1" (text-scale-set 0) nil :bind nil)))
    '(progn
      (set
       (defvar deino-zoom/params nil
         "Params of deino-zoom.")
       (quote (nil nil)))
      (set
       (defvar deino-zoom/docstring nil
         "Docstring of deino-zoom.")
       "zoom")
      (set
       (defvar deino-zoom/heads nil
         "Heads for deino-zoom.")
       (quote
        (("r"
          (text-scale-set 0)
          "reset"
          :exit nil)
         ("0"
          (text-scale-set 0)
          ""
          :bind nil
          :exit t)
         ("1"
          (text-scale-set 0)
          nil
          :bind nil
          :exit nil))))
      (set
       (defvar deino-zoom/keymap nil
         "Keymap for deino-zoom.")
       (quote
        (keymap
         (114 . deino-zoom/lambda-r)
         (kp-subtract . deino--negative-argument)
         (kp-9 . deino--digit-argument)
         (kp-8 . deino--digit-argument)
         (kp-7 . deino--digit-argument)
         (kp-6 . deino--digit-argument)
         (kp-5 . deino--digit-argument)
         (kp-4 . deino--digit-argument)
         (kp-3 . deino--digit-argument)
         (kp-2 . deino--digit-argument)
         (kp-1 . deino--digit-argument)
         (kp-0 . deino--digit-argument)
         (57 . deino--digit-argument)
         (56 . deino--digit-argument)
         (55 . deino--digit-argument)
         (54 . deino--digit-argument)
         (53 . deino--digit-argument)
         (52 . deino--digit-argument)
         (51 . deino--digit-argument)
         (50 . deino--digit-argument)
         (49 . deino-zoom/lambda-r)
         (48 . deino-zoom/lambda-0-and-exit)
         (45 . deino--negative-argument)
         (21 . deino--universal-argument))))
      (set
       (defvar deino-zoom/hint nil
         "Dynamic hint for deino-zoom.")
       (quote
        (format
         #("zoom: [r 0]: reset."
           7 8 (face deino-face-red)
           9 10 (face deino-face-blue)))))
      (defun deino-zoom/lambda-r nil
        "Call the head `(text-scale-set 0)' in the \"deino-zoom\" deino.

The heads for the associated deino are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `deino-zoom/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (let ((deino--ignore t))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-zoom/body)))
        (condition-case err
            (deino--call-interactively-remap-maybe
             (function
              (lambda nil
               (interactive)
               (text-scale-set 0))))
          ((quit error)
           (message
            (error-message-string err))))
        (deino-show-hint
         deino-zoom/hint
         (quote deino-zoom))
        (deino-set-transient-map
         deino-zoom/keymap
         (lambda nil
           (deino-keyboard-quit)
           nil)
         nil))
      (defun deino-zoom/lambda-0-and-exit nil
        "Call the head `(text-scale-set 0)' in the \"deino-zoom\" deino.

The heads for the associated deino are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `deino-zoom/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (deino-keyboard-quit)
        (setq deino-curr-body-fn
              (quote deino-zoom/body))
        (deino--call-interactively-remap-maybe
         (function
          (lambda nil
           (interactive)
           (text-scale-set 0)))))
      (defun deino-zoom/body nil
        "Call the body in the \"deino-zoom\" deino.

The heads for the associated deino are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `deino-zoom/body'."
        (interactive)
        (require (quote deino))
        (deino-default-pre)
        (let ((deino--ignore nil))
          (deino-keyboard-quit)
          (setq deino-curr-body-fn
                (quote deino-zoom/body)))
        (deino-show-hint
         deino-zoom/hint
         (quote deino-zoom))
        (deino-set-transient-map
         deino-zoom/keymap
         (lambda nil
           (deino-keyboard-quit)
           nil)
         nil)
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest defdeinodio ()
  (should (equal
           (macroexpand
            '(defdeinodio deino-test ()
              (num "Num" [0 1 2 3 4 5 6 7 8 9 10])
              (str "Str" ["foo" "bar" "baz"])))
           '(progn
             (defvar deino-test/num 0
               "Num")
             (put 'deino-test/num 'range [0 1 2 3 4 5 6 7 8 9 10])
             (defun deino-test/num ()
               (deino--cycle-radio 'deino-test/num))
             (defvar deino-test/str "foo"
               "Str")
             (put 'deino-test/str 'range ["foo" "bar" "baz"])
             (defun deino-test/str ()
               (deino--cycle-radio 'deino-test/str))
             (defvar deino-test/names '(deino-test/num deino-test/str))))))

(ert-deftest deino-blue-compat ()
  (should
   (equal
    (macroexpand
     '(defdeino deino-toggle (:color blue)
       "toggle"
       ("t" toggle-truncate-lines "truncate")
       ("f" auto-fill-mode "fill")
       ("a" abbrev-mode "abbrev")
       ("q" nil "cancel")))
    (macroexpand
     '(defdeino deino-toggle (:exit t)
       "toggle"
       ("t" toggle-truncate-lines "truncate")
       ("f" auto-fill-mode "fill")
       ("a" abbrev-mode "abbrev")
       ("q" nil "cancel"))))))

(ert-deftest deino-amaranth-compat ()
  (should
   (equal
    (macroexpand
     '(defdeino deino-vi
       (:pre
        (set-cursor-color "#e52b50")
        :post
        (set-cursor-color "#ffffff")
        :color amaranth)
       "vi"
       ("j" next-line)
       ("k" previous-line)
       ("q" nil "quit")))
    (macroexpand
     '(defdeino deino-vi
       (:pre
        (set-cursor-color "#e52b50")
        :post
        (set-cursor-color "#ffffff")
        :foreign-keys warn)
       "vi"
       ("j" next-line)
       ("k" previous-line)
       ("q" nil "quit"))))))

(ert-deftest deino-pink-compat ()
  (should
   (equal
    (macroexpand
     '(defdeino deino-zoom (global-map "<f2>"
                            :color pink)
       "zoom"
       ("g" text-scale-increase "in")
       ("l" text-scale-decrease "out")
       ("q" nil "quit")))
    (macroexpand
     '(defdeino deino-zoom (global-map "<f2>"
                            :foreign-keys run)
       "zoom"
       ("g" text-scale-increase "in")
       ("l" text-scale-decrease "out")
       ("q" nil "quit"))))))

(ert-deftest deino-teal-compat ()
  (should
   (equal
    (macroexpand
     '(defdeino deino-zoom (global-map "<f2>"
                            :color teal)
       "zoom"
       ("g" text-scale-increase "in")
       ("l" text-scale-decrease "out")
       ("q" nil "quit")))
    (macroexpand
     '(defdeino deino-zoom (global-map "<f2>"
                            :foreign-keys warn
                            :exit t)
       "zoom"
       ("g" text-scale-increase "in")
       ("l" text-scale-decrease "out")
       ("q" nil "quit"))))))

(ert-deftest deino-format-1 ()
  (should (equal
           (let ((deino-fontify-head-function
                  'deino-fontify-head-greyscale))
             (deino--format
              'deino-toggle
              nil
              "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
" '(("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("g" golden-ratio-mode nil)
    ("t" toggle-truncate-lines nil)
    ("w" whitespace-mode nil)
    ("q" nil "quit"))))
           '(format
             "%s abbrev-mode:       %S
%s debug-on-error:    %S
%s auto-fill-mode:    %S
[{q}]: quit."
             "{a}" abbrev-mode
             "{d}" debug-on-error
             "{f}" auto-fill-function))))

(ert-deftest deino-format-2 ()
  (should (equal
           (let ((deino-fontify-head-function
                  'deino-fontify-head-greyscale))
             (deino--format
              'bar
              nil
              "\n  bar %s`foo\n"
              '(("a" (quote t) "" :cmd-name bar/lambda-a :exit nil)
                ("q" nil "" :cmd-name bar/nil :exit t))))
           '(format "  bar %s\n{a}, [q]." foo))))

(ert-deftest deino-format-3 ()
  (should (equal
           (let ((deino-fontify-head-function
                  'deino-fontify-head-greyscale))
             (deino--format
              'bar
              nil
              "\n_<SPC>_   ^^ace jump\n"
              '(("<SPC>" ace-jump-char-mode nil :cmd-name bar/ace-jump-char-mode))))
           '(format "%s   ace jump\n" "{<SPC>}"))))

(ert-deftest deino-format-4 ()
  (should
   (equal (deino--format
           nil
           '(nil nil :hint nil)
           "\n_j_,_k_"
           '(("j" nil nil :exit t) ("k" nil nil :exit t)))
          '(format "%s,%s"
            #("j" 0 1 (face deino-face-blue))
            #("k" 0 1 (face deino-face-blue))))))

(ert-deftest deino-format-5 ()
  (should
   (equal (deino--format
           nil nil "\n_-_: mark          _u_: unmark\n"
           '(("-" Buffer-menu-mark nil)
             ("u" Buffer-menu-unmark nil)))
          '(format
            "%s: mark          %s: unmark\n"
            #("-" 0 1 (face deino-face-red))
            #("u" 0 1 (face deino-face-red))))))

(ert-deftest deino-format-6 ()
  (should
   (equal (deino--format
           nil nil "\n[_]_] forward [_[_] backward\n"
           '(("]" forward-char nil)
             ("[" backward-char nil)))
          '(format
            "[%s] forward [%s] backward\n"
            #("]"
              0 1 (face
                   deino-face-red))
            #("["
              0 1 (face
                   deino-face-red))))))

(ert-deftest deino-format-7 ()
  (should
   (equal
    (deino--format nil nil "test"
                   '(("%" forward-char "" :exit nil)
                     ("b" backward-char "" :exit nil)))
    '(format
      #("test: %%%%, b."
        6 7 (face deino-face-red)
        7 8 (face deino-face-red)
        8 9 (face deino-face-red)
        9 10 (face deino-face-red)
        12 13 (face deino-face-red)))))
  (should
   (equal
    (deino--format nil nil "\n_%_ forward\n"
                   '(("%" forward-char nil :exit nil)))
    '(format
      "%s forward\n"
      #("%%"
        0 2 (face deino-face-red))))))

(ert-deftest deino-format-8 ()
  (should
   (equal
    (deino--format nil '(nil nil :hint nil) "test"
                   '(("f" forward-char nil :exit nil)
                     ("b" backward-char "back" :exit nil)))
    '(format
      #("test: [b]: back."
        7 8 (face deino-face-red))))))

(ert-deftest deino-format-9 ()
  (should
   (equal
    (deino--format nil '(nil nil :hint nil) "\n_f_(foo)"
                   '(("f" forward-char nil :exit nil)))
    '(format
      "%s(foo)"
      #("f" 0 1 (face deino-face-red))))))

(ert-deftest deino-format-10 ()
  (should
   (equal
    (deino--format nil '(nil nil) "Test:"
                   '(("j" next-line (format-time-string "%H:%M:%S" (current-time))
                      :exit nil)))
    '(concat
      (format "Test:\n")
      (mapconcat
       (function
        deino--eval-and-format)
       (quote
        ((#("j" 0 1 (face deino-face-red))
           format-time-string
           "%H:%M:%S"
           (current-time))))
       ", ")
      "."))))

(ert-deftest deino-format-11 ()
  (should
   (equal
    (deino--format nil '(nil nil :hint nil) "\n_f_ #+begin__src/#+end__src"
                   '(("f" forward-char nil :exit nil)))
    '(format
      "%s #+begin_src/#+end_src"
      #("f" 0 1 (face deino-face-red))))))

(ert-deftest deino-format-with-sexp-1 ()
  (should (equal
           (let ((deino-fontify-head-function
                  'deino-fontify-head-greyscale))
             (deino--format
              'deino-toggle nil
              "\n_n_ narrow-or-widen-dwim %(progn (message \"checking\")(buffer-narrowed-p))asdf\n"
              '(("n" narrow-to-region nil) ("q" nil "cancel" :exit t))))
           '(format
             "%s narrow-or-widen-dwim %Sasdf\n[[q]]: cancel."
             "{n}"
             (progn
               (message "checking")
               (buffer-narrowed-p))))))

(ert-deftest deino-format-with-sexp-2 ()
  (should (equal
           (let ((deino-fontify-head-function
                  'deino-fontify-head-greyscale))
             (deino--format
              'deino-toggle nil
              "\n_n_ narrow-or-widen-dwim %s(progn (message \"checking\")(buffer-narrowed-p))asdf\n"
              '(("n" narrow-to-region nil) ("q" nil "cancel" :exit t))))
           '(format
             "%s narrow-or-widen-dwim %sasdf\n[[q]]: cancel."
             "{n}"
             (progn
               (message "checking")
               (buffer-narrowed-p))))))

(ert-deftest deino-compat-colors-2 ()
  (should
   (equal
    (cddr (macroexpand
           '(defdeino deino-test (:color amaranth)
             ("a" fun-a)
             ("b" fun-b :color blue)
             ("c" fun-c :color blue)
             ("d" fun-d :color blue)
             ("e" fun-e :color blue)
             ("f" fun-f :color blue))))
    (cddr (macroexpand
           '(defdeino deino-test (:color teal)
             ("a" fun-a :color red)
             ("b" fun-b)
             ("c" fun-c)
             ("d" fun-d)
             ("e" fun-e)
             ("f" fun-f)))))))

(ert-deftest deino-compat-colors-3 ()
  (should
   (equal
    (cddr (macroexpand
           '(defdeino deino-test ()
             ("a" fun-a)
             ("b" fun-b :color blue)
             ("c" fun-c :color blue)
             ("d" fun-d :color blue)
             ("e" fun-e :color blue)
             ("f" fun-f :color blue))))
    (cddr (macroexpand
           '(defdeino deino-test (:color blue)
             ("a" fun-a :color red)
             ("b" fun-b)
             ("c" fun-c)
             ("d" fun-d)
             ("e" fun-e)
             ("f" fun-f)))))))

(ert-deftest deino-compat-colors-4 ()
  (should
   (equal
    (cddr (macroexpand
           '(defdeino deino-test ()
             ("a" fun-a)
             ("b" fun-b :exit t)
             ("c" fun-c :exit t)
             ("d" fun-d :exit t)
             ("e" fun-e :exit t)
             ("f" fun-f :exit t))))
    (cddr (macroexpand
           '(defdeino deino-test (:exit t)
             ("a" fun-a :exit nil)
             ("b" fun-b)
             ("c" fun-c)
             ("d" fun-d)
             ("e" fun-e)
             ("f" fun-f)))))))

(ert-deftest deino--pad ()
  (should (equal (deino--pad '(a b c) 3)
                 '(a b c)))
  (should (equal (deino--pad '(a) 3)
                 '(a nil nil))))

(ert-deftest deino--matrix ()
  (should (equal (deino--matrix '(a b c) 2 2)
                 '((a b) (c nil))))
  (should (equal (deino--matrix '(a b c d e f g h i) 4 3)
                 '((a b c d) (e f g h) (i nil nil nil)))))

(ert-deftest deino--cell ()
  (should (equal (deino--cell "% -75s %%`%s" '(deino-hint-display-type deino-verbose))
                 "The utility to show deino hint                                              %`deino-hint-display-type
When non-nil, deino will issue some non essential style warnings.           %`deino-verbose^^^^^^^^^^")))

(ert-deftest deino--vconcat ()
  (should (equal (deino--vconcat '("abc\ndef" "012\n34" "def\nabc"))
                 "abc012def\ndef34abc")))

(defdeinodio deino-tng ()
  (picard "_p_ Captain Jean Luc Picard:")
  (riker "_r_ Commander William Riker:")
  (data "_d_ Lieutenant Commander Data:")
  (worf "_w_ Worf:")
  (la-forge "_f_ Geordi La Forge:")
  (troi "_t_ Deanna Troi:")
  (dr-crusher "_c_ Doctor Beverly Crusher:")
  (phaser "_h_ Set phasers to " [stun kill]))

(ert-deftest deino--table ()
  (let ((deino-cell-format "% -30s %% -8`%s"))
    (should (equal (deino--table deino-tng/names 5 2)
                   (substring "
_p_ Captain Jean Luc Picard:   % -8`deino-tng/picard^^    _t_ Deanna Troi:               % -8`deino-tng/troi^^^^^^
_r_ Commander William Riker:   % -8`deino-tng/riker^^^    _c_ Doctor Beverly Crusher:    % -8`deino-tng/dr-crusher
_d_ Lieutenant Commander Data: % -8`deino-tng/data^^^^    _h_ Set phasers to             % -8`deino-tng/phaser^^^^
_w_ Worf:                      % -8`deino-tng/worf^^^^
_f_ Geordi La Forge:           % -8`deino-tng/la-forge" 1)))
    (should (equal (deino--table deino-tng/names 4 3)
                   (substring "
_p_ Captain Jean Luc Picard:   % -8`deino-tng/picard    _f_ Geordi La Forge:           % -8`deino-tng/la-forge^^
_r_ Commander William Riker:   % -8`deino-tng/riker^    _t_ Deanna Troi:               % -8`deino-tng/troi^^^^^^
_d_ Lieutenant Commander Data: % -8`deino-tng/data^^    _c_ Doctor Beverly Crusher:    % -8`deino-tng/dr-crusher
_w_ Worf:                      % -8`deino-tng/worf^^    _h_ Set phasers to             % -8`deino-tng/phaser^^^^" 1)))))

(ert-deftest deino--make-funcall ()
  (should (equal (let ((body-pre 'foo))
                   (deino--make-funcall body-pre)
                   body-pre)
                 '(funcall (function foo)))))

(defdeino deino-simple-1 (global-map "C-c")
  ("a" (insert "j"))
  ("b" (insert "k"))
  ("q" nil))

(defdeino deino-simple-2 (global-map "C-c" :color amaranth)
  ("c" self-insert-command)
  ("d" self-insert-command)
  ("q" nil))

(defdeino deino-simple-3 (global-map "C-c")
  ("g" goto-line)
  ("1" find-file)
  ("q" nil))

(defun remapable-print ()
  (interactive)
  (insert "remapable print was called"))
(defun remaped-print ()
  (interactive)
  (insert "*remaped* print was called"))
(define-key global-map (kbd "C-=") 'remapable-print)
(define-key global-map [remap remapable-print] 'remaped-print)

(defdeino deino-simple-with-remap (global-map "C-c")
  ("r" remapable-print)
  ("q" nil))

(defmacro deino-with (in &rest body)
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (unwind-protect
            (progn
              (switch-to-buffer temp-buffer)
              (transient-mark-mode 1)
              (insert ,in)
              (goto-char (point-min))
              (when (search-forward "~" nil t)
                (backward-delete-char 1)
                (set-mark (point)))
              (goto-char (point-max))
              (search-backward "|")
              (delete-char 1)
              (setq current-prefix-arg nil)
              ,@body
              (insert "|")
              (when (region-active-p)
                (exchange-point-and-mark)
                (insert "~"))
              (buffer-substring-no-properties
               (point-min)
               (point-max)))
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))))))

(ert-deftest deino-integration-1 ()
  (should (string= (deino-with "|"
                               (execute-kbd-macro
                                (kbd "C-c aabbaaqaabbaa")))
                   "jjkkjjaabbaa|"))
  (should (string= (deino-with "|"
                               (condition-case nil
                                   (execute-kbd-macro
                                    (kbd "C-c aabb C-g"))
                                 (quit nil))
                               (execute-kbd-macro "aaqaabbaa"))
                   "jjkkaaqaabbaa|")))

(ert-deftest deino-integration-2 ()
  (should (string= (deino-with "|"
                               (execute-kbd-macro
                                (kbd "C-c c 1 c 2 d 4 c q")))
                   "ccddcccc|"))
  (should (string= (deino-with "|"
                               (execute-kbd-macro
                                (kbd "C-c c 1 c C-u d C-u 10 c q")))
                   "ccddddcccccccccc|")))

(ert-deftest deino-integration-3 ()
  (should (string= (deino-with "foo\nbar|"
                               (execute-kbd-macro
                                (kbd "C-c g 1 RET q")))
                   "|foo\nbar")))

(ert-deftest deino-remap-lookup-1 ()
  "try calling a remapped command while option is disabled "
  (setq deino-look-for-remap nil)
  (should (string= (deino-with "|"
                               (execute-kbd-macro
                                (kbd "C-c rq")))
                   "remapable print was called|")))
(ert-deftest deino-remap-lookup-2 ()
  "try calling a remapped command while option is enabled"
  (setq deino-look-for-remap t)
  (should (string= (deino-with "|"
                               (execute-kbd-macro
                                (kbd "C-c rq")))
                   "*remaped* print was called|")))

(ert-deftest deino-columns-1 ()
  (should (equal (eval
                  (cadr
                   (nth 2
                        (nth 5
                             (macroexpand
                              '(defdeino deino-info (:color blue
                                                     :columns 3)
                                "Info-mode"
                                ("?" Info-summary "summary")
                                ("]" Info-forward-node "forward")
                                ("[" Info-backward-node "backward")
                                ("<" Info-top-node "top node")
                                (">" Info-final-node "final node")
                                ("h" Info-help "help")
                                ("d" Info-directory "info dir")
                                ("f" Info-follow-reference "follow ref")
                                ("g" Info-goto-node "goto node")
                                ("l" Info-history-back "hist back")
                                ("r" Info-history-forward "hist forward")
                                ("i" Info-index "index")
                                ("I" Info-virtual-index "virtual index")
                                ("L" Info-history "hist")
                                ("n" Info-next "next")
                                ("p" Info-prev "previous")
                                ("s" Info-search "search")
                                ("S" Info-search-case-sensitively "case-search")
                                ("T" Info-toc "TOC")
                                ("u" Info-up "up")
                                ("m" Info-menu "menu")
                                ("t" deino-info-to/body "info-to")))))))
                 #("Info-mode:
?: summary       ]: forward       [: backward
<: top node      >: final node    h: help
d: info dir      f: follow ref    g: goto node
l: hist back     r: hist forward  i: index
I: virtual index L: hist          n: next
p: previous      s: search        S: case-search
T: TOC           u: up            m: menu
t: info-to"
                   11 12 (face deino-face-blue)
                   28 29 (face deino-face-blue)
                   45 46 (face deino-face-blue)
                   57 58 (face deino-face-blue)
                   74 75 (face deino-face-blue)
                   91 92 (face deino-face-blue)
                   99 100 (face deino-face-blue)
                   116 117 (face deino-face-blue)
                   133 134 (face deino-face-blue)
                   146 147 (face deino-face-blue)
                   163 164 (face deino-face-blue)
                   180 181 (face deino-face-blue)
                   189 190 (face deino-face-blue)
                   206 207 (face deino-face-blue)
                   223 224 (face deino-face-blue)
                   231 232 (face deino-face-blue)
                   248 249 (face deino-face-blue)
                   265 266 (face deino-face-blue)
                   280 281 (face deino-face-blue)
                   297 298 (face deino-face-blue)
                   314 315 (face deino-face-blue)
                   322 323 (face deino-face-blue)))))

(ert-deftest deino-columns-2 ()
  (should (equal (eval
                  (cadr
                   (nth 2
                        (nth 5
                             (macroexpand
                              '(defdeino deino-foo (:color blue)
                                "Silly deino"
                                ("x" forward-char "forward" :column "sideways")
                                ("y" backward-char "back")
                                ("a" next-line "down" :column "vertical")
                                ("b" previous-line "up")))))))
                 #("Silly deino:
sideways    | vertical
----------- | -----------
x: forward  | a: down
y: back     | b: up
"
                   62 63 (face deino-face-blue)
                   76 77 (face deino-face-blue)
                   84 85 (face deino-face-blue)
                   98 99 (face deino-face-blue)))))

;; checked:
;; basic rendering
;; column compatibility with ruby style and no column specified
;; column declared several time
;; nil column
(ert-deftest deino-column-basic ()
  (should (equal (eval
                  (cadr
                   (nth 2
                        (nth 5
                             (macroexpand
                              '(defdeino deino-rectangle (:body-pre (rectangle-mark-mode 1)
                                                          :color pink
                                                          :post (deactivate-mark))
                                "
  ^_k_^         ()()
_h_   _l_       (O)(o)
  ^_j_^         (  O )
^^^^            (’’)(’’)
^^^^
"
                                ("h" backward-char nil)
                                ("l" forward-char nil)
                                ("k" previous-line nil)
                                ("j" next-line nil)
                                ("Of" 5x5 "outside of table 1")
                                ("e" exchange-point-and-mark "exchange" :column "firstcol")
                                ("n" copy-rectangle-as-kill "new-copy")
                                ("d" delete-rectangle "delete")
                                ("r" (if (region-active-p)
                                         (deactivate-mark)
                                       (rectangle-mark-mode 1)) "reset" :column "secondcol")
                                ("y" yank-rectangle "yank")
                                ("u" undo "undo")
                                ("s" string-rectangle "string")
                                ("p" kill-rectangle "paste")
                                ("o" nil "ok" :column "firstcol")
                                ("Os" 5x5-bol "outside of table 2" :column nil)
                                ("Ot" 5x5-eol "outside of table 3")))))))
                 #("  k         ()()
h   l       (O)(o)
  j         (  O )
            (’’)(’’)

firstcol    | secondcol
----------- | ------------
e: exchange | r: reset
n: new-copy | y: yank
d: delete   | u: undo
o: ok       | s: string
            | p: paste
[Of]: outside of table 1, [Os]: outside of table 2, [Ot]: outside of table 3."
                   2 3 (face deino-face-pink)
                   17 18 (face deino-face-pink)
                   21 22 (face deino-face-pink)
                   38 39 (face deino-face-pink)
                   128 129 (face deino-face-pink)
                   142 143 (face deino-face-pink)
                   151 152 (face deino-face-pink)
                   165 166 (face deino-face-pink)
                   173 174 (face deino-face-pink)
                   187 188 (face deino-face-pink)
                   195 196 (face deino-face-blue)
                   209 210 (face deino-face-pink)
                   233 234 (face deino-face-pink)
                   243 245 (face deino-face-pink)
                   269 271 (face deino-face-pink)
                   295 297 (face deino-face-pink)))))

;; check column order is the same as they appear in defdeino
(ert-deftest deino-column-order ()
  (should (equal (eval
                  (cadr
                   (nth 2
                        (nth 5
                             (macroexpand
                              '(defdeino deino-window-order
                                (:color red :timeout 4)
                                ("z" ace-window "ace" :color blue :column "Switch")
                                ("h" windmove-left "← window")
                                ("j" windmove-down "↓ window")
                                ("l" windmove-right "→ window")
                                ("s" split-window-below "split window" :color blue :column "Split Management")
                                ("v" split-window-right "split window vertically" :color blue)
                                ("d" delete-window "delete current window")
                                ("f" follow-mode "toggle follow mode")
                                ("u" winner-undo "undo window conf" :column "Undo/Redo")
                                ("r" winner-redo "redo window conf")
                                ("b" balance-windows "balance window height" :column "1-Sizing")
                                ("m" maximize-window "maximize current window")
                                ("k" windmove-up "↑ window" :column "Switch")
                                ("M" minimize-window "minimize current window" :column "1-Sizing")
                                ("q" nil "quit menu" :color blue :column nil)))))))
                 #("Switch      | Split Management           | Undo/Redo           | 1-Sizing
----------- | -------------------------- | ------------------- | --------------------------
z: ace      | s: split window            | u: undo window conf | b: balance window height
h: ← window | v: split window vertically | r: redo window conf | m: maximize current window
j: ↓ window | d: delete current window   |                     | M: minimize current window
l: → window | f: toggle follow mode      |                     |
k: ↑ window |                            |                     |
[q]: quit menu."
                   173 174 (face deino-face-blue)
                   187 188 (face deino-face-blue)
                   216 217 (face deino-face-red)
                   238 239 (face deino-face-red)
                   263 264 (face deino-face-red)
                   277 278 (face deino-face-blue)
                   306 307 (face deino-face-red)
                   328 329 (face deino-face-red)
                   355 356 (face deino-face-red)
                   369 370 (face deino-face-red)
                   420 421 (face deino-face-red)
                   447 448 (face deino-face-red)
                   461 462 (face deino-face-red)
                   512 513 (face deino-face-red)
                   578 579 (face deino-face-blue)))))

(ert-deftest deino-column-sexp ()
  (should (equal
           (eval (nth 5
                      (macroexpand
                       '(defdeino deino-toggle-stuff ()
                         "Toggle"
                         ("d" toggle-debug-on-error "debug-on-error" :column "Misc")
                         ("a" abbrev-mode
                          (format "abbrev: %s"
                           (if (bound-and-true-p abbrev-mode)
                               "[x]"
                             "[ ]")))))))
           '(concat
             (format "Toggle:\n")
             "Misc"
             "\n"
             "-----------------"
             "\n"
             #("d: debug-on-error"
               0 1 (face deino-face-red))
             "\n"
             (format
              "%1s: %-15s"
              #("a" 0 1 (face deino-face-red))
              (format
               "abbrev: %s"
               (if (bound-and-true-p abbrev-mode)
                   "[x]"
                 "[ ]")))
             "\n"))))

(defdeino deino-extendable ()
  "extendable"
  ("j" next-line "down"))

(ert-deftest deino-extend ()
  (should (equal (macroexpand
                  '(defdeino+ deino-extendable ()
                    ("k" previous-line "up")))
                 (macroexpand
                  '(defdeino deino-extendable ()
                    "extendable"
                    ("j" next-line "down")
                    ("k" previous-line "up")))))
  (should (equal (macroexpand
                  '(defdeino+ deino-extendable ()
                    ("k" previous-line "up" :exit t)))
                 (macroexpand
                  '(defdeino deino-extendable ()
                    "extendable"
                    ("j" next-line "down")
                    ("k" previous-line "up" :exit t))))))

(provide 'deino-test)

;;; deino-test.el ends here
