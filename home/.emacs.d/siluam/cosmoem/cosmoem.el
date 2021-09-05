;;; cosmoem.el --- An auto-magical, which-key-based hydra banisher. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/cosmoem

;; Version: 0.3
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4") (which-key "3.3.2"))

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; An auto-magical, which-key-based hydra banisher.

;; With almost no set-up code, cosmoem lets you call any group of
;; related command sequentially with no prefix keys, while showing a
;; handy popup to remember the bindings for those commands.  It can
;; create both of these (the grouped commands, and the popup) from any
;; keymap.

;;; Code:
(require 'which-key)

(defvar cosmoem--popup-showing-p nil
  "Whether or not cosmoem.el has been summoned.
Used in addition to `which-key-persistent-popup' in case other
packages start relying on it.")

;;;###autoload
(defun cosmoem-any-popup-showing-p nil (interactive) (or cosmoem--popup-showing-p (which-key--popup-showing-p)))

(defvar cosmoem-show-prefix nil
  "One of `which-key-show-prefix'.
Used as value of `which-key-show-prefix' in cosmoem.el
pop-ups.")

;;;###autoload
(defun cosmoem--hide (&optional keymap flatten &rest _)
        "Dismiss cosmoem.el.
    Pop KEYMAP from `overriding-terminal-local-map' when it is not
    nil.  If FLATTEN is t, `cosmoem--show' was called with the same
    argument.  Restore `which-key--update' after such a call."
        (interactive)
        (setq cosmoem--popup-showing-p nil)
        (setq overriding-terminal-local-map nil)
        (when flatten (advice-remove #'which-key--update #'ignore))
        (meq/which-key-show-top-level))

;;;###autoload
(defun cosmoem--show (&optional keymap flatten transient &rest _)
    "Summon cosmoem.el showing KEYMAP.
    Push KEYMAP onto `overriding-terminal-local-map' when TRANSIENT
    is nil.  Otherwise use `set-transient-map'.  If FLATTEN is t,
    show full keymap \(including sub-maps\), and prevent redrawing on
    prefix-key press by overriding `which-key--update'."
    (interactive)
    (when which-key-persistent-popup
        (setq cosmoem--popup-showing-p t)
        (when keymap
            (let ((which-key-show-prefix cosmoem-show-prefix))
            (if flatten
                (progn
                    (which-key--show-keymap
                    (symbol-name keymap) (symbol-value keymap) nil t t)
                    (advice-add #'which-key--update :override #'ignore))
                (which-key--show-keymap
                (symbol-name keymap) (symbol-value keymap) nil nil t)))
            (if transient
                (set-transient-map (symbol-value keymap)
                                t #'cosmoem--hide)
            (internal-push-keymap (symbol-value keymap)
                                    'overriding-terminal-local-map)))))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/42240
;; User: user12563
;;;###autoload
(defun cosmoem-hide-all-modal-modes (&optional keymap include-ignored) (interactive)
    (when overriding-terminal-local-map (mapc #'(lambda (prefix) (interactive)
        (unless (member prefix meq/var/ignored-modal-prefixes)
          (message (format "Hiding %s" prefix))
          (ignore-errors (funcall (meq/inconcat "meq/" prefix "-cosmoem-hide")))
          ;; (internal-push-keymap 'global-map 'overriding-terminal-local-map)
          ;; (internal-push-keymap nil 'overriding-terminal-local-map)
          (setq overriding-terminal-local-map nil))) meq/var/modal-prefixes)
      (mapc #'(lambda (prefix) (interactive) (if include-ignored
            (progn (message (format "Hiding %s" prefix))
              (ignore-errors (funcall (meq/inconcat "meq/" prefix "-cosmoem-hide")))
              ;; (internal-push-keymap 'global-map 'overriding-terminal-local-map)
              ;; (internal-push-keymap nil 'overriding-terminal-local-map)
              (setq overriding-terminal-local-map nil))
            (message (format "Showing %s" prefix))
            (ignore-errors (funcall (meq/inconcat "meq/" prefix "-cosmoem-show")))
            ;; (internal-push-keymap 'global-map 'overriding-terminal-local-map)
            ;; (internal-push-keymap nil 'overriding-terminal-local-map)
            ;; (setq overriding-terminal-local-map nil)
            )
          ) meq/var/ignored-modal-prefixes))
    (meq/which-key-show-top-level keymap))

(defun cosmoem--toggle (&optional keymap flatten transient &rest _)
  "Toggle cosmoem.el showing KEYMAP.
Pass TRANSIENT and FLATTEN to `cosmoem--hide', and
`cosmoem--show'."
  (if cosmoem--popup-showing-p
      (cosmoem--hide keymap)
    (cosmoem--show keymap flatten transient)))

(defun cosmoem--enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun cosmoem--advise (funs hst &optional keymap flatten transient)
  "Either `hide', `show' or `toggle' cosmoem.el depending on HST.
Do so when calling FUNS showing KEYMAP.  Pass TRANSIENT to
`cosmoem--hide', `cosmoem--show', or `cosmoem--toggle'."
  (cl-loop
   for fun in (cosmoem--enlist funs) do
   (progn
     (unless (symbol-function fun)
       (fset fun (lambda () (interactive))))
     (advice-add fun :before
                 (pcase hst
                   ('toggle (apply-partially #'cosmoem--toggle keymap flatten transient))
                   ('show (apply-partially #'cosmoem--show keymap flatten transient))
                   ('hide (apply-partially #'cosmoem--hide keymap flatten)))))))

(defun cosmoem--graylist (keys funs keymap &optional whitelist)
  "Unbind KEYS and keys bound to FUNS from KEYMAP.
If WHITELIST is t, Unbind all keys not in KEYS or bound to FUNS
from KEYMAP."
  (let ((keymap-alist
         (cl-loop for (key . fun-name)
                  in (which-key--get-keymap-bindings
                      (symbol-value keymap))
                  as fun = (intern fun-name)
                  when
                  (or (member key (cosmoem--enlist keys))
                      (member fun (cosmoem--enlist funs)))
                  collect (cons key fun))))

    (if whitelist
        (progn
          (set keymap (make-sparse-keymap))
          (cl-loop for (key . fun) in keymap-alist do
                   (define-key (symbol-value keymap) (kbd key) fun)))
      (cl-loop for (key . fun) in keymap-alist do
               (define-key (symbol-value keymap) (kbd key) nil)))))

(defun cosmoem--graylist-after-load (keys funs keymap &optional
                                           package whitelist)
  "Call `cosmoem--graylist' after PACKAGE has been loaded.
Pass KEYS, FUNS, KEYMAP, and WHITELIST directly to it.  If
PACKAGE is nil, simply call `cosmoem--graylist'."
  (if package
      (with-eval-after-load package
        (cosmoem--graylist keys funs keymap whitelist))
    (cosmoem--graylist keys funs keymap whitelist)))

;;;###autoload
(cl-defun cosmoem-def
    (&key toggle-funs
          show-funs
          hide-funs
          keymap
          flatten
          transient
          blacklist-keys
          whitelist-keys
          blacklist-funs
          whitelist-funs
          package)
  "Summon cosmoem.el to banish your hydras.

TOGGLE-FUNS, SHOW-FUNS, and HIDE-FUNS define entry and exit
points for cosmoem.el to show KEYMAP. Both single functions and
lists work. As all other arguments to `cosmoem-def', these must
be quoted.

KEYMAP specifies the keymap for cosmoem.el to make a pop-up out
of.  If KEYMAP is nil, it is assumed that one of SHOW-FUNS or
TOGGLE-FUNS results in a `which-key--show-popup' call. This may
be useful for functions such as `which-key-show-top-level'. I use
it to remind myself of some obscure Evil commands from time to
time.

FLATTEN displays all maps and sub-maps without redrawing on
prefix-key presses. This allows for multi-key combinations in a
single cosmoem.el buffer.

BLACKLIST-KEYS and WHITELIST-KEYS specify
which (`kbd'-interpretable) keys should removed from/allowed to
remain on KEYMAP. Handy if you want to unbind things in bulk and
don't want to get your hands dirty with keymaps. Both single
characters and lists work. Blacklists take precedence over
whitelists.

BLACKLIST-FUNS and WHITELIST-FUNS are analogous to BLACKLIST-KEYS
and WHITELIST-KEYS except that they operate on function
symbols. These might be useful if a keymap specifies multiple
bindings for a commands and pruning it is more efficient this
way. Blacklists again take precedence over whitelists.

PACKAGE must be passed along with BLACKLIST-KEYS, WHITELIST-KEYS,
BLACKLIST-FUNS, or WHITELIST-FUNS if KEYMAP belongs to a lazy
loaded package. Its contents should be the package name as a
quoted symbol.

Setting TRANSIENT to t allows you to get away with not setting
HIDE-FUNS or TOGGLE-FUNS by dismissing cosmoem.el whenever you
press a key not on KEYMAP."
  ;; tweak keymaps
  (when keymap
    (when (or whitelist-keys whitelist-funs)
      (cosmoem--graylist-after-load
       whitelist-keys whitelist-funs
       keymap package t))
    (when (or blacklist-keys blacklist-funs)
      (cosmoem--graylist-after-load
       blacklist-keys blacklist-funs
       keymap package nil)))

  ;; define entry points
  (cosmoem--advise toggle-funs 'toggle keymap flatten transient)
  (cosmoem--advise show-funs 'show keymap flatten transient)
  (cosmoem--advise hide-funs 'hide keymap flatten))

(provide 'cosmoem)
;;; cosmoem.el ends here
