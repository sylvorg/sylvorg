;;; deino.el --- Make bindings that stick around. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Maintainer: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/deino
;; Version: 0.15.0
;; Keywords: bindings
;; Package-Requires: ((cl-lib "0.5") (lv "0"))

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
;; This package can be used to tie related commands into a family of
;; short bindings with a common prefix - a deino.
;;
;; Once you summon the deino (through the prefixed binding), all the
;; heads can be called in succession with only a short extension.
;; The deino is vanquished once Hercules, any binding that isn't the
;; deino's head, arrives.  Note that Hercules, besides vanquishing the
;; deino, will still serve his original purpose, calling his proper
;; command.  This makes the deino very seamless, it's like a minor
;; mode that disables itself automagically.
;;
;; Here's an example deino, bound in the global map (you can use any
;; keymap in place of `global-map'):
;;
;;     (defdeino deino-zoom (global-map "<f2>")
;;       "zoom"
;;       ("g" text-scale-increase "in")
;;       ("l" text-scale-decrease "out"))
;;
;; It allows to start a command chain either like this:
;; "<f2> gg4ll5g", or "<f2> lgllg".
;;
;; Here's another approach, when you just want a "callable keymap":
;;
;;     (defdeino deino-toggle (:color blue)
;;       "toggle"
;;       ("a" abbrev-mode "abbrev")
;;       ("d" toggle-debug-on-error "debug")
;;       ("f" auto-fill-mode "fill")
;;       ("t" toggle-truncate-lines "truncate")
;;       ("w" whitespace-mode "whitespace")
;;       ("q" nil "cancel"))
;;
;; This binds nothing so far, but if you follow up with:
;;
;;     (global-set-key (naked "C-c C-v") 'deino-toggle/body)
;;
;; you will have bound "C-c C-v a", "C-c C-v d" etc.
;;
;; Knowing that `defdeino' defines e.g. `deino-toggle/body' command,
;; you can nest deinos if you wish, with `deino-toggle/body' possibly
;; becoming a blue head of another deino.
;;
;; If you want to learn all intricacies of using `defdeino' without
;; having to figure it all out from this source code, check out the
;; wiki: https://github.com/abo-abo/deino/wiki. There's a wealth of
;; information there. Everyone is welcome to bring the existing pages
;; up to date and add new ones.
;;
;; Additionally, the file deino-examples.el serves to demo most of the
;; functionality.

;;; Code:
;;* Requires
(require 'cl-lib)
(require 'lv)
(require 'ring)
(require 'meq)

(defvar deino-curr-map nil
  "The keymap of the current deino called.")

(defvar deino-curr-on-exit nil
  "The on-exit predicate for the current deino.")

(defvar deino-curr-foreign-keys nil
  "The current :foreign-keys behavior.")

(defvar deino-curr-body-fn nil
  "The current deino-.../body function.")

(defvar deino-deactivate nil
  "If a deino head sets this to t, exit the deino.
This will be done even if the head wasn't designated for exiting.")

(defvar deino-amaranth-warn-message "An amaranth deino can only exit through a blue head"
  "Amaranth Warning message.  Shown when the user tries to press an unbound/non-exit key while in an amaranth head.")

(defun deino-set-transient-map (keymap on-exit &optional foreign-keys)
  "Set KEYMAP to the highest priority.

Call ON-EXIT when the KEYMAP is deactivated.

FOREIGN-KEYS determines the deactivation behavior, when a command
that isn't in KEYMAP is called:

nil: deactivate KEYMAP and run the command.
run: keep KEYMAP and run the command.
warn: keep KEYMAP and issue a warning instead of running the command."
  (if deino-deactivate
      (deino-keyboard-quit)
    (setq deino-curr-map keymap)
    (setq deino-curr-on-exit on-exit)
    (setq deino-curr-foreign-keys foreign-keys)
    (add-hook 'pre-command-hook 'deino--clearfun)
    (internal-push-keymap keymap 'overriding-terminal-local-map)))

(defun deino--clearfun ()
  "Disable the current deino unless `this-command' is a head."
  (unless (eq this-command 'deino-pause-resume)
    (when (or
           (memq this-command '(handle-switch-frame
                                keyboard-quit))
           (null overriding-terminal-local-map)
           (not (or (eq this-command
                        (lookup-key deino-curr-map (this-single-command-keys)))
                    (cl-case deino-curr-foreign-keys
                      (warn
                       (setq this-command 'deino-amaranth-warn))
                      (run
                       t)
                      (t nil)))))
      (deino-disable))))

(defvar deino--ignore nil
  "When non-nil, don't call `deino-curr-on-exit'.")

(defvar deino--input-method-function nil
  "Store overridden `input-method-function' here.")

(defun deino-disable ()
  "Disable the current deino."
  (setq deino-deactivate nil)
  (remove-hook 'pre-command-hook 'deino--clearfun)
  (unless deino--ignore
    (if (fboundp 'remove-function)
        (remove-function input-method-function #'deino--imf)
      (when deino--input-method-function
        (setq input-method-function deino--input-method-function)
        (setq deino--input-method-function nil))))
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (when overriding-terminal-local-map
        (internal-pop-keymap deino-curr-map 'overriding-terminal-local-map))))
  (setq deino-curr-map nil)
  (unless deino--ignore
    (when deino-curr-on-exit
      (let ((on-exit deino-curr-on-exit))
        (setq deino-curr-on-exit nil)
        (funcall on-exit)))))

(unless (fboundp 'internal-push-keymap)
  (defun internal-push-keymap (keymap symbol)
    (let ((map (symbol-value symbol)))
      (unless (memq keymap map)
        (unless (memq 'add-keymap-witness (symbol-value symbol))
          (setq map (make-composed-keymap nil (symbol-value symbol)))
          (push 'add-keymap-witness (cdr map))
          (set symbol map))
        (push keymap (cdr map))))))

(unless (fboundp 'internal-pop-keymap)
  (defun internal-pop-keymap (keymap symbol)
    (let ((map (symbol-value symbol)))
      (when (memq keymap map)
        (setf (cdr map) (delq keymap (cdr map))))
      (let ((tail (cddr map)))
        (and (or (null tail) (keymapp tail))
             (eq 'add-keymap-witness (nth 1 map))
             (set symbol tail))))))

(defun deino-amaranth-warn ()
  "Issue a warning that the current input was ignored."
  (interactive)
  (message deino-amaranth-warn-message))

;;* Customize
(defgroup deino nil
  "Make bindings that stick around."
  :group 'bindings
  :prefix "deino-")

(defcustom deino-is-helpful t
  "When t, display a hint with possible bindings in the echo area."
  :type 'boolean
  :group 'deino)

(defcustom deino-default-hint ""
  "Default :hint property to use for heads when not specified in
the body or the head."
  :type 'sexp
  :group 'deino)

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-poshandler-window-center "posframe")

(defvar deino-posframe-show-params
  '(:internal-border-width 1
    :internal-border-color "red"
    :poshandler posframe-poshandler-window-center)
  "List of parameters passed to `posframe-show'.")

(defvar deino--posframe-timer nil
  "Timer for hiding posframe hint.")

(defun deino-posframe-show (str)
  (require 'posframe)
  (when deino--posframe-timer
    (cancel-timer deino--posframe-timer))
  (setq deino--posframe-timer nil)
  (apply #'posframe-show
         " *deino-posframe*"
         :string str
         deino-posframe-show-params))

(defun deino-posframe-hide ()
  (require 'posframe)
  (unless deino--posframe-timer
    (setq deino--posframe-timer
          (run-with-idle-timer
           0 nil (lambda ()
                   (setq deino--posframe-timer nil)
                   (posframe-hide " *deino-posframe*"))))))

(defvar deino-hint-display-alist
  (list (list 'lv #'lv-message #'lv-delete-window)
        (list 'message (lambda (str) (message "%s" str)) (lambda () (message "")))
        (list 'posframe #'deino-posframe-show #'deino-posframe-hide))
  "Store the functions for `deino-hint-display-type'.")

(defcustom deino-hint-display-type 'lv
  "The utility to show deino hint"
  :type '(choice
          (const message)
          (const lv)
          (const posframe))
  :group 'deino)

(defcustom deino-verbose nil
  "When non-nil, deino will issue some non essential style warnings."
  :type 'boolean)

(defcustom deino-key-format-spec "%s"
  "Default `format'-style specifier for _a_  syntax in docstrings.
When nil, you can specify your own at each location like this: _ 5a_."
  :type 'string)

(defcustom deino-doc-format-spec "%s"
  "Default `format'-style specifier for ?a?  syntax in docstrings."
  :type 'string)

(defcustom deino-look-for-remap nil
  "When non-nil, deino binding behaves as keymap binding with [remap].
When calling a head with a simple command, deino will lookup for a potential
remap command according to the current active keymap and call it instead if
found"
  :type 'boolean)

(make-obsolete-variable
 'deino-key-format-spec
 "Since the docstrings are aligned by hand anyway, this isn't very useful."
 "0.13.1")

(defface deino-face-red
  '((t (:foreground "#FF0000" :bold t)))
  "Red deino heads don't exit the deino.
Every other command exits the deino."
  :group 'deino)

(defface deino-face-blue
  '((((class color) (background light))
     :foreground "#0000FF" :bold t)
    (((class color) (background dark))
     :foreground "#8ac6f2" :bold t))
  "Blue deino heads exit the deino.
Every other command exits as well.")

(defface deino-face-amaranth
  '((t (:foreground "#E52B50" :bold t)))
  "Amaranth body has red heads and warns on intercepting non-heads.
Exitable only through a blue head.")

(defface deino-face-pink
  '((t (:foreground "#FF6EB4" :bold t)))
  "Pink body has red heads and runs intercepted non-heads.
Exitable only through a blue head.")

(defface deino-face-teal
  '((t (:foreground "#367588" :bold t)))
  "Teal body has blue heads and warns on intercepting non-heads.
Exitable only through a blue head.")

;;* Fontification
(defun deino-add-font-lock ()
  "Fontify `defdeino' statements."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(defdeino\\)\\_> +\\(.*?\\)\\_>"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
     ("(\\(defdeinodio\\)\\_> +\\(.*?\\)\\_>"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face)))))

;;* Imenu
(defun deino-add-imenu ()
  "Add this to `emacs-lisp-mode-hook' to have deinos in `imenu'."
  (add-to-list
   'imenu-generic-expression
   '("deinos"
     "^.*(\\(defdeino\\) \\([a-zA-Z-]+\\)"
     2)))

;;* Find Function
(eval-after-load 'find-func
  '(defadvice find-function-search-for-symbol
    (around deino-around-find-function-search-for-symbol-advice
     (symbol type library) activate)
    "Navigate to deinos with `find-function-search-for-symbol'."
    (prog1 ad-do-it
      (when (symbolp symbol)
        ;; The original function returns (cons (current-buffer) (point))
        ;; if it found the point.
        (unless (cdr ad-return-value)
          (with-current-buffer (find-file-noselect library)
            (let ((sn (symbol-name symbol)))
              (when (and (null type)
                         (string-match "\\`\\(deino-[a-z-A-Z0-9]+\\)/\\(.*\\)\\'" sn)
                         (re-search-forward (concat "(defdeino " (match-string 1 sn))
                                            nil t))
                (goto-char (match-beginning 0)))
              (cons (current-buffer) (point)))))))))

;;* Universal Argument
(defvar deino-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-u] 'deino--universal-argument)
    (define-key map [?-] 'deino--negative-argument)
    (define-key map [?0] 'deino--digit-argument)
    (define-key map [?1] 'deino--digit-argument)
    (define-key map [?2] 'deino--digit-argument)
    (define-key map [?3] 'deino--digit-argument)
    (define-key map [?4] 'deino--digit-argument)
    (define-key map [?5] 'deino--digit-argument)
    (define-key map [?6] 'deino--digit-argument)
    (define-key map [?7] 'deino--digit-argument)
    (define-key map [?8] 'deino--digit-argument)
    (define-key map [?9] 'deino--digit-argument)
    (define-key map [kp-0] 'deino--digit-argument)
    (define-key map [kp-1] 'deino--digit-argument)
    (define-key map [kp-2] 'deino--digit-argument)
    (define-key map [kp-3] 'deino--digit-argument)
    (define-key map [kp-4] 'deino--digit-argument)
    (define-key map [kp-5] 'deino--digit-argument)
    (define-key map [kp-6] 'deino--digit-argument)
    (define-key map [kp-7] 'deino--digit-argument)
    (define-key map [kp-8] 'deino--digit-argument)
    (define-key map [kp-9] 'deino--digit-argument)
    (define-key map [kp-subtract] 'deino--negative-argument)
    map)
  "Keymap that all deinos inherit.  See `universal-argument-map'.")

(defun deino--universal-argument (arg)
  "Forward to (`universal-argument' ARG)."
  (interactive "P")
  (setq prefix-arg (if (consp arg)
                       (list (* 4 (car arg)))
                     (if (eq arg '-)
                         (list -4)
                       '(4)))))

(defun deino--digit-argument (arg)
  "Forward to (`digit-argument' ARG)."
  (interactive "P")
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (setq prefix-arg (cond ((integerp arg)
                            (+ (* arg 10)
                               (if (< arg 0)
                                   (- digit)
                                 digit)))
                           ((eq arg '-)
                            (if (zerop digit)
                                '-
                              (- digit)))
                           (t
                            digit)))))

(defun deino--negative-argument (arg)
  "Forward to (`negative-argument' ARG)."
  (interactive "P")
  (setq prefix-arg (cond ((integerp arg) (- arg))
                         ((eq arg '-) nil)
                         (t '-))))

;;* Repeat
(defvar deino-repeat--prefix-arg nil
  "Prefix arg to use with `deino-repeat'.")

(defvar deino-repeat--command nil
  "Command to use with `deino-repeat'.")

(defun deino-repeat (&optional arg)
  "Repeat last command with last prefix arg.
When ARG is non-nil, use that instead."
  (interactive "p")
  (if (eq arg 1)
      (unless (string-match "deino-repeat$" (symbol-name last-command))
        (setq deino-repeat--command last-command)
        (setq deino-repeat--prefix-arg last-prefix-arg))
    (setq deino-repeat--prefix-arg arg))
  (setq current-prefix-arg deino-repeat--prefix-arg)
  (funcall deino-repeat--command))

;;* Misc internals
(defun deino--callablep (x)
  "Test if X is callable."
  (or (functionp x)
      (and (consp x)
           (memq (car x) '(function quote)))))

(defun deino--make-callable (x)
  "Generate a callable symbol from X.
If X is a function symbol or a lambda, return it.  Otherwise, it
should be a single statement.  Wrap it in an interactive lambda."
  (cond ((or (symbolp x) (functionp x))
         x)
        ((and (consp x) (eq (car x) 'function))
         (cadr x))
        (t
         `(lambda ()
            (interactive)
            ,x))))

(defun deino-plist-get-default (plist prop default)
  "Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).

Return the value corresponding to PROP, or DEFAULT if PROP is not
one of the properties on the list."
  (if (memq prop plist)
      (plist-get plist prop)
    default))

(defun deino--head-property (h prop &optional default)
  "Return for deino head H the value of property PROP.
Return DEFAULT if PROP is not in H."
  (deino-plist-get-default (cl-cdddr h) prop default))

(defun deino--head-set-property (h prop value)
  "In deino Head H, set a property PROP to the value VALUE."
  (cons (car h) (plist-put (cdr h) prop value)))

(defun deino--head-has-property (h prop)
  "Return non nil if heads H has the property PROP."
  (plist-member (cdr h) prop))

(defun deino--body-foreign-keys (body)
  "Return what BODY does with a non-head binding."
  (or
   (plist-get (cddr body) :foreign-keys)
   (let ((color (plist-get (cddr body) :color)))
     (cl-case color
       ((amaranth teal) 'warn)
       (pink 'run)))))

(defun deino--body-exit (body)
  "Return the exit behavior of BODY."
  (or
   (plist-get (cddr body) :exit)
   (let ((color (plist-get (cddr body) :color)))
     (cl-case color
       ((blue teal) t)
       (t nil)))))

(defun deino--normalize-body (body)
  "Put BODY in a normalized format.
Add :exit and :foreign-keys if they are not there.
Remove :color key. And sort the plist alphabetically."
  (let ((plist (cddr body)))
    (plist-put plist :exit (deino--body-exit body))
    (plist-put plist :foreign-keys (deino--body-foreign-keys body))
    (let* ((alist0 (cl-loop for (k v) on plist
                      by #'cddr collect (cons k v)))
           (alist1 (assq-delete-all :color alist0))
           (alist2 (cl-sort alist1 #'string<
                            :key (lambda (x) (symbol-name (car x))))))
      (append (list (car body) (cadr body))
              (cl-mapcan (lambda (x) (list (car x) (cdr x))) alist2)))))

(defalias 'deino--imf #'list)

(defun deino-default-pre ()
  "Default setup that happens in each head before :pre."
  (when (eq input-method-function 'key-chord-input-method)
    (if (fboundp 'add-function)
        (add-function :override input-method-function #'deino--imf)
      (unless deino--input-method-function
        (setq deino--input-method-function input-method-function)
        (setq input-method-function nil)))))

(defvar deino-timeout-timer (timer-create)
  "Timer for `deino-timeout'.")

(defvar deino-message-timer (timer-create)
  "Timer for the hint.")

(defvar deino--work-around-dedicated t
  "When non-nil, assume there's no bug in `pop-to-buffer'.
`pop-to-buffer' should not select a dedicated window.")

(defun deino-keyboard-quit ()
  "Quitting function similar to `keyboard-quit'."
  (interactive)
  (deino-disable)
  (cancel-timer deino-timeout-timer)
  (cancel-timer deino-message-timer)
  (unless (and deino--ignore
               (null deino--work-around-dedicated))
    (funcall
     (nth 2 (assoc deino-hint-display-type deino-hint-display-alist))))
  nil)

(defvar deino-head-format "[%s]: "
  "The formatter for each head of a plain docstring.")

(defvar deino-key-doc-function 'deino-key-doc-function-default
  "The function for formatting key-doc pairs.")

(defun deino-key-doc-function-default (key key-width doc doc-width)
  (cond
    ((equal key " ") (format (format "%%-%ds" (+ 3 key-width doc-width)) doc))
    ((listp doc)
     `(format ,(format "%%%ds: %%%ds" key-width (- -1 doc-width)) ,key ,doc))
    (t (format (format "%%%ds: %%%ds" key-width (- -1 doc-width)) key doc))))

(defun deino--to-string (x)
  (if (stringp x)
      x
    (eval x)))

(defun deino--eval-and-format (x)
  (let ((str (deino--to-string (cdr x))))
    (format
     (if (> (length str) 0)
         (concat deino-head-format str)
       "%s")
     (car x))))

(defun deino--hint-heads-wocol (body heads)
  "Generate a hint for the echo area.
BODY, and HEADS are parameters to `defdeino'.
Works for heads without a property :column."
  (let (alist)
    (dolist (h heads)
      (let ((val (assoc (cadr h) alist))
            (pstr (deino-fontify-head h body)))
        (if val
            (setf (cadr val)
                  (concat (cadr val) " " pstr))
          (push
           (cons (cadr h)
                 (cons pstr (cl-caddr h)))
           alist))))
    (let ((keys (nreverse (mapcar #'cdr alist)))
          (n-cols (plist-get (cddr body) :columns))
          res)
      (setq res
            (if n-cols
                (let ((n-rows (1+ (/ (length keys) n-cols)))
                      (max-key-len (apply #'max (mapcar (lambda (x) (length (car x))) keys)))
                      (max-doc-len (apply #'max (mapcar (lambda (x)
                                                          (length (deino--to-string (cdr x)))) keys))))
                  `(concat
                    "\n"
                    (mapconcat #'identity
                               (mapcar
                                (lambda (x)
                                  (mapconcat
                                   (lambda (y)
                                     (and y
                                          (funcall deino-key-doc-function
                                                   (car y)
                                                   ,max-key-len
                                                   (deino--to-string (cdr y))
                                                   ,max-doc-len))) x ""))
                                ',(deino--matrix keys n-cols n-rows))
                               "\n")))


              `(concat
                (mapconcat
                 #'deino--eval-and-format
                 ',keys
                 ", ")
                ,(if keys "." ""))))
      (if (cl-every #'stringp
                    (mapcar 'cddr alist))
          (eval res)
        res))))

(defun deino--hint (body heads)
  "Generate a hint for the echo area.
BODY, and HEADS are parameters to `defdeino'."
  (let* ((sorted-heads (deino--sort-heads (deino--normalize-heads heads)))
         (heads-w-col (cl-remove-if-not (lambda (heads) (deino--head-property (nth 0 heads) :column)) sorted-heads))
         (heads-wo-col (cl-remove-if (lambda (heads) (deino--head-property (nth 0 heads) :column)) sorted-heads))
         (hint-w-col (when heads-w-col
                       (deino--hint-from-matrix body (deino--generate-matrix heads-w-col))))
         (hint-wo-col (when heads-wo-col
                        (deino--hint-heads-wocol body (car heads-wo-col)))))
    (if (null hint-w-col)
        hint-wo-col
      (if (stringp hint-wo-col)
          `(concat ,@hint-w-col ,hint-wo-col)
        `(concat ,@hint-w-col ,@(cdr hint-wo-col))))))

(defvar deino-fontify-head-function nil
  "Possible replacement for `deino-fontify-head-default'.")

(defun deino-fontify-head-default (head body)
  "Produce a pretty string from HEAD and BODY.
HEAD's binding is returned as a string with a colored face."
  (let* ((foreign-keys (deino--body-foreign-keys body))
         (head-exit (deino--head-property head :exit))
         (head-color
          (if head-exit
              (if (eq foreign-keys 'warn)
                  'teal
                'blue)
            (cl-case foreign-keys
              (warn 'amaranth)
              (run 'pink)
              (t 'red)))))
    (when (and (null (cadr head))
               (not head-exit))
      (deino--complain "nil cmd can only be blue"))
    (propertize
     (replace-regexp-in-string "%" "%%" (car head))
     'face
     (or (deino--head-property head :face)
         (cl-case head-color
           (blue 'deino-face-blue)
           (red 'deino-face-red)
           (amaranth 'deino-face-amaranth)
           (pink 'deino-face-pink)
           (teal 'deino-face-teal)
           (t (error "Unknown color for %S" head)))))))

(defun deino-fontify-head-greyscale (head _body)
  "Produce a pretty string from HEAD and BODY.
HEAD's binding is returned as a string wrapped with [] or {}."
  (format
   (if (deino--head-property head :exit)
       "[%s]"
     "{%s}") (car head)))

(defun deino-fontify-head (head body)
  "Produce a pretty string from HEAD and BODY."
  (funcall (or deino-fontify-head-function 'deino-fontify-head-default)
           head body))

(defun deino--strip-align-markers (str)
  "Remove ^ from STR, unless they're escaped: \\^."
  (let ((start 0))
    (while (setq start (string-match "\\\\?\\^" str start))
      (if (eq (- (match-end 0) (match-beginning 0)) 2)
          (progn
            (setq str (replace-match "^" nil nil str))
            (cl-incf start))
        (setq str (replace-match "" nil nil str))))
    str))

(defvar deino-docstring-keys-translate-alist
  '(("↑" . "<up>")
    ("↓" . "<down>")
    ("→" . "<right>")
    ("←" . "<left>")
    ("⌫" . "DEL")
    ("⌦" . "<deletechar>")
    ("⏎" . "RET")))

(defconst deino-width-spec-regex " ?-?[0-9]*?"
  "Regex for the width spec in keys and %` quoted sexps.")

(defvar deino-key-regex "[][\\[:alnum:] ~.,;:/|?<>={}*+#%@!&^↑↓←→⌫⌦⏎'`()\"$-]+?"
  "Regex for the key quoted in the docstring.")

(defun deino--format (_name body docstring heads)
  "Generate a `format' statement from STR.
\"%`...\" expressions are extracted into \"%S\".
_NAME, BODY, DOCSTRING and HEADS are parameters of `defdeino'.
The expressions can be auto-expanded according to NAME."
  (unless (memq 'elisp--witness--lisp (mapcar #'cadr heads))
    (setq docstring (deino--strip-align-markers docstring))
    (setq docstring (replace-regexp-in-string "___" "_β_" docstring))
    (let ((rest (if (eq (plist-get (cddr body) :hint) 'none)
                    ""
                  (deino--hint body heads)))
          (start 0)
          (inner-regex (format "\\(%s\\)\\(%s\\)" deino-width-spec-regex deino-key-regex))
          varlist
          offset)
      (while (setq start
                   (string-match
                    (format
                     "\\(?:%%\\( ?-?[0-9]*s?\\)\\(`[a-z-A-Z/0-9]+\\|(\\)\\)\\|\\(?:_%s_\\)\\|\\(?:[?]%s[?]\\)\\|__"
                     inner-regex
                     inner-regex)
                    docstring start))
        (cond ((string= "__" (match-string 0 docstring))
               (setq docstring (replace-match "_" nil t docstring))
               (setq start (1- (match-end 0))))
              ((eq ?? (aref (match-string 0 docstring) 0))
               (let* ((key (match-string 6 docstring))
                      (head (assoc key heads)))
                 (if head
                     (progn
                       (push (nth 2 head) varlist)
                       (setq docstring
                             (replace-match
                              (or
                               deino-doc-format-spec
                               (concat "%" (match-string 3 docstring) "s"))
                              t nil docstring)))
                   (setq start (match-end 0))
                   (warn "Unrecognized key: ?%s?" key))))
              ((eq ?_ (aref (match-string 0 docstring) 0))
               (let* ((key (match-string 4 docstring))
                      (key (if (equal key "β") "_" key))
                      normal-key
                      (head (or (assoc key heads)
                                (when (setq normal-key
                                            (cdr (assoc
                                                  key deino-docstring-keys-translate-alist)))
                                  (assoc normal-key heads)))))
                 (if head
                     (progn
                       (push (deino-fontify-head (if normal-key
                                                     (cons key (cdr head))
                                                   head)
                                                 body)
                             varlist)
                       (let ((replacement
                              (or
                               deino-key-format-spec
                               (concat "%" (match-string 3 docstring) "s"))))
                         (setq docstring
                               (replace-match replacement t nil docstring))
                         (setq start (+ start (length replacement)))))
                   (setq start (match-end 0))
                   (warn "Unrecognized key: _%s_" key))))

              (t
               (let* ((varp (if (eq ?` (aref (match-string 2 docstring) 0)) 1 0))
                      (spec (match-string 1 docstring))
                      (lspec (length spec)))
                 (setq offset
                       (with-temp-buffer
                         (insert (substring docstring (+ 1 start varp
                                                         (length spec))))
                         (goto-char (point-min))
                         (push (read (current-buffer)) varlist)
                         (- (point) (point-min))))
                 (when (or (zerop lspec)
                           (/= (aref spec (1- (length spec))) ?s))
                   (setq spec (concat spec "S")))
                 (setq docstring
                       (concat
                        (substring docstring 0 start)
                        "%" spec
                        (substring docstring (+ start offset 1 lspec varp))))))))
      (deino--format-1 docstring rest varlist))))

(defun deino--format-1 (docstring rest varlist)
  (cond
    ((string= docstring "")
     rest)
    ((listp rest)
     (unless (string-match-p "[:\n]" docstring)
       (setq docstring (concat docstring ":\n")))
     (unless (or (string-match-p "\n\\'" docstring)
                 (equal (cadr rest) "\n"))
       (setq docstring (concat docstring "\n")))
     `(concat (format ,(replace-regexp-in-string "\\`\n" "" docstring) ,@(nreverse varlist))
              ,@(cdr rest)))
    ((eq ?\n (aref docstring 0))
     `(format ,(concat (substring docstring 1) rest) ,@(nreverse varlist)))
    (t
     (let ((r `(replace-regexp-in-string
                " +$" ""
                (concat ,docstring
                        ,(cond ((string-match-p "\\`\n" rest)
                                ":")
                               ((string-match-p "\n" rest)
                                ":\n")
                               (t
                                ": "))
                        (replace-regexp-in-string
                         "\\(%\\)" "\\1\\1" ,rest)))))
       (if (stringp rest)
           `(format ,(eval r))
         `(format ,r))))))

(defun deino--complain (format-string &rest args)
  "Forward to (`message' FORMAT-STRING ARGS) unless `deino-verbose' is nil."
  (if deino-verbose
      (apply #'error format-string args)
    (apply #'message format-string args)))

(defun deino--doc (body-key body-name heads)
  "Generate a part of deino docstring.
BODY-KEY is the body key binding.
BODY-NAME is the symbol that identifies the deino.
HEADS is a list of heads."
  (format
   "The heads for the associated deino are:\n\n%s\n\n%s%s."
   (mapconcat
    (lambda (x)
      (format "\"%s\":    `%S'" (car x) (cadr x)))
    heads ",\n")
   (format "The body can be accessed via `%S'" body-name)
   (if body-key
       (format ", which is bound to \"%s\"" body-key)
     "")))

(defun deino--call-interactively-remap-maybe (cmd)
  "`call-interactively' the given CMD or its remapped equivalent.
Only when `deino-look-for-remap' is non nil."
  (let ((remapped-cmd (if deino-look-for-remap
                          (command-remapping `,cmd)
                        nil)))
    (if remapped-cmd
        (call-interactively `,remapped-cmd)
      (call-interactively `,cmd))))

(defun deino--call-interactively (cmd name)
  "Generate a `call-interactively' statement for CMD.
Set `this-command' to NAME."
  (if (and (symbolp name)
           (not (memq name '(nil body))))
      `(progn
         (setq this-command ',name)
         (deino--call-interactively-remap-maybe #',cmd))
    `(deino--call-interactively-remap-maybe #',cmd)))

(defun deino--make-defun (name body doc head
                          keymap body-pre body-before-exit
                          &optional body-after-exit)
  "Make a defun wrapper, using NAME, BODY, DOC, HEAD, and KEYMAP.
NAME and BODY are the arguments to `defdeino'.
DOC was generated with `deino--doc'.
HEAD is one of the HEADS passed to `defdeino'.
BODY-PRE is added to the start of the wrapper.
BODY-BEFORE-EXIT will be called before the deino quits.
BODY-AFTER-EXIT is added to the end of the wrapper."
  (let* ((cmd-name (deino--head-name head name))
         (cmd (when (car head)
                (deino--make-callable
                 (cadr head))))
         (doc (if (car head)
                  (format "Call the head `%S' in the \"%s\" deino.\n\n%s"
                          (cadr head) name doc)
                (format "Call the body in the \"%s\" deino.\n\n%s"
                        name doc)))
         (hint (intern (format "%S/hint" name)))
         (body-foreign-keys (deino--body-foreign-keys body))
         (body-timeout (plist-get body :timeout))
         (idle (or (and (eq (cadr head) 'body) (plist-get body :idle))
                   (plist-get (nthcdr 3 head) :idle)))
         (curr-body-fn-sym (intern (format "%S/body" name)))
         (body-on-exit-t
          `((deino-keyboard-quit)
            (setq deino-curr-body-fn ',curr-body-fn-sym)
            ,@(if body-after-exit
                  `((unwind-protect
                         ,(when cmd
                            (deino--call-interactively cmd (cadr head)))
                      ,body-after-exit))
                (when cmd
                  `(,(deino--call-interactively cmd (cadr head)))))
            (unless deino-curr-map (meq/which-key--show-popup))))
         (body-on-exit-nil
          (delq
           nil
           `((let ((deino--ignore ,(not (eq (cadr head) 'body))))
               (deino-keyboard-quit)
               (setq deino-curr-body-fn ',curr-body-fn-sym))
             ,(when cmd
                `(condition-case err
                     ,(deino--call-interactively cmd (cadr head))
                   ((quit error)
                    (message (error-message-string err)))))
             ,(if idle
                  `(deino-idle-message ,idle ,hint ',name)
                `(deino-show-hint ,hint ',name))
             (deino-set-transient-map
              ,keymap
              (lambda () (deino-keyboard-quit) ,body-before-exit)
              ,(when body-foreign-keys
                 (list 'quote body-foreign-keys)))
             ,body-after-exit
             ,(when body-timeout
                `(deino-timeout ,body-timeout))))))
    `(defun ,cmd-name ()
       ,doc
       (interactive)
       (require 'deino)
       (when (meq/any-popup-showing-p) (meq/which-key--hide-popup))
       (deino-default-pre)
       ,@(when body-pre (list body-pre))
       ,@(cond ((eq (deino--head-property head :exit) t)
                body-on-exit-t)
               ((eq (deino--head-property head :exit) nil)
                body-on-exit-nil)
               (t
                `((if ,(deino--head-property head :exit)
                      (progn
                        ,@body-on-exit-t)
                    ,@body-on-exit-nil)))))))

(defvar deino-props-alist nil)

(defun deino-set-property (name key val)
  "Set deino property.
NAME is the symbolic name of the deino.
KEY and VAL are forwarded to `plist-put'."
  (let ((entry (assoc name deino-props-alist))
        plist)
    (when (null entry)
      (add-to-list 'deino-props-alist (list name))
      (setq entry (assoc name deino-props-alist)))
    (setq plist (cdr entry))
    (setcdr entry (plist-put plist key val))))

(defun deino-get-property (name key)
  "Get deino property.
NAME is the symbolic name of the deino.
KEY is forwarded to `plist-get'."
  (let ((entry (assoc name deino-props-alist)))
    (when entry
      (plist-get (cdr entry) key))))

(defun deino-show-hint (hint caller)
  (let ((verbosity (plist-get (cdr (assoc caller deino-props-alist))
                              :verbosity)))
    (cond ((eq verbosity 0))
          ((eq verbosity 1)
           (message (eval hint)))
          (t
           (when deino-is-helpful
             (funcall
              (nth 1 (assoc deino-hint-display-type deino-hint-display-alist))
              (eval hint)))))))

(defmacro deino--make-funcall (sym)
  "Transform SYM into a `funcall' to call it."
  `(when (and ,sym (symbolp ,sym))
     (setq ,sym `(funcall #',,sym))))

(defun deino--head-name (h name)
  "Return the symbol for head H of deino with NAME."
  (let ((str (format "%S/%s" name
                     (cond ((symbolp (cadr h))
                            (cadr h))
                           ((and (consp (cadr h))
                                 (eq (cl-caadr h) 'function))
                            (cadr (cadr h)))
                           (t
                            (concat "lambda-" (car h)))))))
    (when (and (deino--head-property h :exit)
               (not (memq (cadr h) '(body nil))))
      (setq str (concat str "-and-exit")))
    (intern str)))

(defun deino--delete-duplicates (heads)
  "Return HEADS without entries that have the same CMD part.
In duplicate HEADS, :cmd-name is modified to whatever they duplicate."
  (let ((ali '(((deino-repeat . nil) . deino-repeat)))
        res entry)
    (dolist (h heads)
      (if (setq entry (assoc (cons (cadr h)
                                   (deino--head-property h :exit))
                             ali))
          (setf (cl-cdddr h) (plist-put (cl-cdddr h) :cmd-name (cdr entry)))
        (push (cons (cons (cadr h)
                          (deino--head-property h :exit))
                    (plist-get (cl-cdddr h) :cmd-name))
              ali)
        (push h res)))
    (nreverse res)))

(defun deino--pad (lst n)
  "Pad LST with nil until length N."
  (let ((len (length lst)))
    (if (= len n)
        lst
      (append lst (make-list (- n len) nil)))))

(defmacro deino-multipop (lst n)
  "Return LST's first N elements while removing them."
  `(if (<= (length ,lst) ,n)
       (prog1 ,lst
         (setq ,lst nil))
     (prog1 ,lst
       (setcdr
        (nthcdr (1- ,n) (prog1 ,lst (setq ,lst (nthcdr ,n ,lst))))
        nil))))

(defun deino--matrix (lst rows cols)
  "Create a matrix from elements of LST.
The matrix size is ROWS times COLS."
  (let ((ls (copy-sequence lst))
        res)
    (dotimes (_c cols)
      (push (deino--pad (deino-multipop ls rows) rows) res))
    (nreverse res)))

(defun deino--cell (fstr names)
  "Format a rectangular cell based on FSTR and NAMES.
FSTR is a format-style string with two string inputs: one for the
doc and one for the symbol name.
NAMES is a list of variables."
  (let ((len (cl-reduce
              (lambda (acc it) (max (length (symbol-name it)) acc))
              names
              :initial-value 0)))
    (mapconcat
     (lambda (sym)
       (if sym
           (format fstr
                   (documentation-property sym 'variable-documentation)
                   (let ((name (symbol-name sym)))
                     (concat name (make-string (- len (length name)) ?^)))
                   sym)
         ""))
     names
     "\n")))

(defun deino--vconcat (strs &optional joiner)
  "Glue STRS vertically.  They must be the same height.
JOINER is a function similar to `concat'."
  (setq joiner (or joiner #'concat))
  (mapconcat
   (lambda (s)
     (if (string-match " +$" s)
         (replace-match "" nil nil s)
       s))
   (apply #'cl-mapcar joiner
          (mapcar
           (lambda (s) (split-string s "\n"))
           strs))
   "\n"))

(defvar deino-cell-format "% -20s %% -8`%s"
  "The default format for docstring cells.")

(defun deino--table (names rows cols &optional cell-formats)
  "Format a `format'-style table from variables in NAMES.
The size of the table is ROWS times COLS.
CELL-FORMATS are `format' strings for each column.
If CELL-FORMATS is a string, it's used for all columns.
If CELL-FORMATS is nil, `deino-cell-format' is used for all columns."
  (setq cell-formats
        (cond ((null cell-formats)
               (make-list cols deino-cell-format))
              ((stringp cell-formats)
               (make-list cols cell-formats))
              (t
               cell-formats)))
  (deino--vconcat
   (cl-mapcar
    #'deino--cell
    cell-formats
    (deino--matrix names rows cols))
   (lambda (&rest x)
     (mapconcat #'identity x "    "))))

(defun deino-reset-radios (names)
  "Set variables NAMES to their defaults.
NAMES should be defined by `defdeinodio' or similar."
  (dolist (n names)
    (set n (aref (get n 'range) 0))))

;; Following functions deal with automatic docstring table generation from :column head property
(defun deino--normalize-heads (heads)
  "Ensure each head from HEADS have a property :column.
Set it to the same value as preceding head or nil if no previous value
was defined."
  (let ((current-col nil))
    (mapcar (lambda (head)
              (if (deino--head-has-property head :column)
                  (setq current-col (deino--head-property head :column)))
              (deino--head-set-property head :column current-col))
            heads)))

(defun deino--sort-heads (normalized-heads)
  "Return a list of heads with non-nil doc grouped by column property.
Each head of NORMALIZED-HEADS must have a column property."
  (let* ((heads-wo-nil-doc (cl-remove-if-not (lambda (head) (nth 2 head)) normalized-heads))
         (columns-list (delete-dups (mapcar (lambda (head) (deino--head-property head :column))
                                            normalized-heads)))
         (get-col-index-fun (lambda (head) (cl-position (deino--head-property head :column)
                                                        columns-list
                                                        :test 'equal)))
         (heads-sorted (cl-sort heads-wo-nil-doc (lambda (it other)
                                                   (< (funcall get-col-index-fun it)
                                                      (funcall get-col-index-fun other))))))
    ;; this operation partition the sorted head list into lists of heads with same column property
    (cl-loop for head in heads-sorted
       for column-name = (deino--head-property head :column)
       with prev-column-name = (deino--head-property (nth 0 heads-sorted) :column)
       unless (equal prev-column-name column-name) collect heads-one-column into heads-all-columns
       and do (setq heads-one-column nil)
       collect head into heads-one-column
       do (setq prev-column-name column-name)
       finally return (append heads-all-columns (list heads-one-column)))))

(defun deino--pad-heads (heads-groups padding-head)
  "Return a copy of HEADS-GROUPS padded where applicable with PADDING-HEAD."
  (cl-loop for heads-group in heads-groups
     for this-head-group-length = (length heads-group)
     with head-group-max-length = (apply #'max (mapcar (lambda (heads) (length heads)) heads-groups))
     if (<= this-head-group-length head-group-max-length)
     collect (append heads-group (make-list (- head-group-max-length this-head-group-length) padding-head))
     into balanced-heads-groups
     else collect heads-group into balanced-heads-groups
     finally return balanced-heads-groups))

(defun deino--generate-matrix (heads-groups)
  "Return a copy of HEADS-GROUPS decorated with table formatting information.
Details of modification:
2 virtual heads acting as table header were added to each heads-group.
Each head is decorated with 2 new properties max-doc-len and max-key-len
representing the maximum dimension of their owning group.
 Every heads-group have equal length by adding padding heads where applicable."
  (when heads-groups
    (let ((res nil))
      (dolist (heads-group (deino--pad-heads heads-groups '(" " nil " " :exit t)))
        (let* ((column-name (deino--head-property (nth 0 heads-group) :column))
               (max-key-len (apply #'max (mapcar (lambda (x) (length (car x))) heads-group)))
               (max-doc-len (apply #'max
                                   (length column-name)
                                   (mapcar (lambda (x) (length (deino--to-string (nth 2 x)))) heads-group)))
               (header-virtual-head `(" " nil ,column-name :column ,column-name :exit t))
               (separator-virtual-head `(" " nil ,(make-string (+ 2 max-doc-len max-key-len) ?-) :column ,column-name :exit t))
               (decorated-heads (copy-tree (apply 'list header-virtual-head separator-virtual-head heads-group))))
          (push (mapcar (lambda (it)
                          (deino--head-set-property it :max-key-len max-key-len)
                          (deino--head-set-property it :max-doc-len max-doc-len))
                        decorated-heads) res)))
      (nreverse res))))

(defun deino-interpose (x lst)
  "Insert X in between each element of LST."
  (let (res y)
    (while (setq y (pop lst))
      (push y res)
      (push x res))
    (nreverse (cdr res))))

(defun deino--hint-row (heads body)
  (let ((lst (deino-interpose
              "| "
              (mapcar (lambda (head)
                        (funcall deino-key-doc-function
                                 (deino-fontify-head head body)
                                 (let ((n (deino--head-property head :max-key-len)))
                                   (+ n (cl-count ?% (car head))))
                                 (nth 2 head) ;; doc
                                 (deino--head-property head :max-doc-len)))
                      heads))))
    (when (stringp (car (last lst)))
      (let ((len (length lst))
            (new-last (replace-regexp-in-string "\s+$" "" (car (last lst)))))
        (when (= 0 (length (setf (nth (- len 1) lst) new-last)))
          (setf (nth (- len 2) lst) "|"))))
    lst))


(defun deino--hint-from-matrix (body heads-matrix)
  "Generate a formatted table-style docstring according to BODY and HEADS-MATRIX.
HEADS-MATRIX is expected to be a list of heads with following features:
Each heads must have the same length
Each head must have a property max-key-len and max-doc-len."
  (when heads-matrix
    (let ((lines (deino--hint-from-matrix-1 body heads-matrix)))
      `(,@(apply #'append (deino-interpose '("\n") lines))
          "\n"))))

(defun deino--hint-from-matrix-1 (body heads-matrix)
  (let* ((first-heads-col (nth 0 heads-matrix))
         (last-row-index (- (length first-heads-col) 1))
         (lines nil))
    (dolist (row-index (number-sequence 0 last-row-index))
      (let ((heads-in-row (mapcar
                           (lambda (heads) (nth row-index heads))
                           heads-matrix)))
        (push (deino--hint-row heads-in-row body)
              lines)))
    (nreverse lines)))

(defun deino-idle-message (secs hint name)
  "In SECS seconds display HINT."
  (cancel-timer deino-message-timer)
  (setq deino-message-timer (timer-create))
  (timer-set-time deino-message-timer
                  (timer-relative-time (current-time) secs))
  (timer-set-function
   deino-message-timer
   (lambda ()
     (deino-show-hint hint name)
     (cancel-timer deino-message-timer)))
  (timer-activate deino-message-timer))

(defun deino-timeout (secs &optional function)
  "In SECS seconds call FUNCTION, then function `deino-keyboard-quit'.
Cancel the previous `deino-timeout'."
  (cancel-timer deino-timeout-timer)
  (setq deino-timeout-timer (timer-create))
  (timer-set-time deino-timeout-timer
                  (timer-relative-time (current-time) secs))
  (timer-set-function
   deino-timeout-timer
   `(lambda ()
      ,(when function
         `(funcall ,function))
      (deino-keyboard-quit)))
  (timer-activate deino-timeout-timer))

;;* Macros
;;;###autoload
(defmacro defdeino (name body &optional docstring &rest heads)
  "Create a deino - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
deino.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `deino--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `naked'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `deino-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit, :bind, and :column.
:exit can be:

- nil (default): this head will continue the deino state.
- t: this head will stop the deino state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

:column is a string that sets the column for all subsequent heads.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defdeino'."
  (declare (indent defun) (doc-string 3))
  (setq heads (copy-tree heads))
  (cond ((stringp docstring))
        ((and (consp docstring)
              (memq (car docstring) '(deino--table concat format)))
         (setq docstring (concat "\n" (eval docstring))))
        (t
         (setq heads (cons docstring heads))
         (setq docstring "")))
  (when (keywordp (car body))
    (setq body (cons nil (cons nil body))))
  (setq body (deino--normalize-body body))
  (condition-case-unless-debug err
      (let* ((keymap-name (intern (format "%S/keymap" name)))
             (body-name (intern (format "%S/body" name)))
             (body-key (cadr body))
             (body-plist (cddr body))
             (base-map (or (eval (plist-get body-plist :base-map))
                           deino-base-map))
             (keymap (copy-keymap base-map))
             (body-map (or (car body)
                           (plist-get body-plist :bind)))
             (body-pre (plist-get body-plist :pre))
             (body-body-pre (plist-get body-plist :body-pre))
             (body-before-exit (or (plist-get body-plist :post)
                                   (plist-get body-plist :before-exit)))
             (body-after-exit (plist-get body-plist :after-exit))
             (body-inherit (plist-get body-plist :inherit))
             (body-foreign-keys (deino--body-foreign-keys body))
             (body-exit (deino--body-exit body)))
        (dolist (base body-inherit)
          (setq heads (append heads (copy-sequence (eval base)))))
        (dolist (h heads)
          (let ((len (length h)))
            (cond ((< len 2)
                   (error "Each head should have at least two items: %S" h))
                  ((= len 2)
                   (setcdr (cdr h)
                           (list
                            (deino-plist-get-default
                             body-plist :hint deino-default-hint)))
                   (setcdr (nthcdr 2 h) (list :exit body-exit)))
                  (t
                   (let ((hint (cl-caddr h)))
                     (unless (or (null hint)
                                 (stringp hint)
                                 (consp hint))
                       (let ((inherited-hint
                              (deino-plist-get-default
                               body-plist :hint deino-default-hint)))
                         (setcdr (cdr h) (cons
                                          (if (eq 'none inherited-hint)
                                              nil
                                            inherited-hint)
                                          (cddr h))))))
                   (let ((hint-and-plist (cddr h)))
                     (if (null (cdr hint-and-plist))
                         (setcdr hint-and-plist (list :exit body-exit))
                       (let* ((plist (cl-cdddr h))
                              (h-color (plist-get plist :color)))
                         (if h-color
                             (progn
                               (plist-put plist :exit
                                          (cl-case h-color
                                            ((blue teal) t)
                                            (t nil)))
                               (cl-remf (cl-cdddr h) :color))
                           (let ((h-exit (deino-plist-get-default plist :exit 'default)))
                             (plist-put plist :exit
                                        (if (eq h-exit 'default)
                                            body-exit
                                          h-exit))))))))))
          (plist-put (cl-cdddr h) :cmd-name (deino--head-name h name))
          (when (null (cadr h)) (plist-put (cl-cdddr h) :exit t)))
        (let ((doc (deino--doc body-key body-name heads))
              (heads-nodup (deino--delete-duplicates heads)))
          (mapc
           (lambda (x)
             (define-key keymap (naked (car x))
               (plist-get (cl-cdddr x) :cmd-name)))
           heads)
          (deino--make-funcall body-pre)
          (deino--make-funcall body-body-pre)
          (deino--make-funcall body-before-exit)
          (deino--make-funcall body-after-exit)
          (when (memq body-foreign-keys '(run warn))
            (unless (cl-some
                     (lambda (h)
                       (deino--head-property h :exit))
                     heads)
              (error
               "An %S deino must have at least one blue head in order to exit"
               body-foreign-keys)))
          `(progn
             (set (defvar ,(intern (format "%S/params" name))
                    nil
                    ,(format "Params of %S." name))
                  ',body)
             (set (defvar ,(intern (format "%S/docstring" name))
                    nil
                    ,(format "Docstring of %S." name))
                  ,docstring)
             (set (defvar ,(intern (format "%S/heads" name))
                    nil
                    ,(format "Heads for %S." name))
                  ',(mapcar (lambda (h)
                              (let ((j (copy-sequence h)))
                                (cl-remf (cl-cdddr j) :cmd-name)
                                j))
                            heads))
             ;; create keymap
             (set (defvar ,keymap-name
                    nil
                    ,(format "Keymap for %S." name))
                  ',keymap)
             ;; declare heads
             (set
              (defvar ,(intern (format "%S/hint" name)) nil
                ,(format "Dynamic hint for %S." name))
              ',(deino--format name body docstring heads))
             ;; create defuns
             ,@(mapcar
                (lambda (head)
                  (deino--make-defun name body doc head keymap-name
                                     body-pre
                                     body-before-exit
                                     body-after-exit))
                heads-nodup)
             ;; free up keymap prefix
             ,@(unless (or (null body-key)
                           (null body-map)
                           (deino--callablep body-map))
                 `((unless (keymapp (lookup-key ,body-map (naked ,body-key)))
                     (define-key ,body-map (naked ,body-key) nil))))
             ;; bind keys
             ,@(delq nil
                     (mapcar
                      (lambda (head)
                        (let ((name (deino--head-property head :cmd-name)))
                          (when (and (cadr head)
                                     (or body-key body-map))
                            (let ((bind (deino--head-property head :bind body-map))
                                  (final-key
                                   (if body-key
                                       (vconcat (naked body-key) (naked (car head)))
                                     (naked (car head)))))
                              (cond ((null bind) nil)
                                    ((deino--callablep bind)
                                     `(funcall ,bind ,final-key (function ,name)))
                                    ((and (symbolp bind)
                                          (if (boundp bind)
                                              (keymapp (symbol-value bind))
                                            t))
                                     `(define-key ,bind ,final-key (quote ,name)))
                                    (t
                                     (error "Invalid :bind property `%S' for head %S" bind head)))))))
                      heads))
             ,(deino--make-defun
               name body doc '(nil body)
               keymap-name
               (or body-body-pre body-pre) body-before-exit
               '(setq prefix-arg current-prefix-arg)))))
    (error
     (deino--complain "Error in defdeino %S: %s" name (cdr err))
     nil)))

(defmacro defdeino+ (name body &optional docstring &rest heads)
  "Redefine an existing deino by adding new heads.
Arguments are same as of `defdeino'."
  (declare (indent defun) (doc-string 3))
  (unless (stringp docstring)
    (setq heads
          (cons docstring heads))
    (setq docstring nil))
  `(defdeino ,name ,(or body (deino--prop name "/params"))
     ,(or docstring (deino--prop name "/docstring"))
     ,@(cl-delete-duplicates
        (append (deino--prop name "/heads") heads)
        :key #'car
        :test #'equal)))

(defun deino--prop (name prop-name)
  (symbol-value (intern (concat (symbol-name name) prop-name))))

(defmacro defdeinodio (name _body &rest heads)
  "Create radios with prefix NAME.
_BODY specifies the options; there are none currently.
HEADS have the format:

    (TOGGLE-NAME &optional VALUE DOC)

TOGGLE-NAME will be used along with NAME to generate a variable
name and a function that cycles it with the same name.  VALUE
should be an array.  The first element of VALUE will be used to
inialize the variable.
VALUE defaults to [nil t].
DOC defaults to TOGGLE-NAME split and capitalized."
  (declare (indent defun))
  `(progn
     ,@(apply #'append
              (mapcar (lambda (h)
                        (deino--radio name h))
                      heads))
     (defvar ,(intern (format "%S/names" name))
       ',(mapcar (lambda (h) (intern (format "%S/%S" name (car h))))
                 heads))))

(defun deino--radio (parent head)
  "Generate a deinodio with PARENT from HEAD."
  (let* ((name (car head))
         (full-name (intern (format "%S/%S" parent name)))
         (doc (cadr head))
         (val (or (cl-caddr head) [nil t])))
    `((defvar ,full-name ,(deino--quote-maybe (aref val 0)) ,doc)
      (put ',full-name 'range ,val)
      (defun ,full-name ()
        (deino--cycle-radio ',full-name)))))

(defun deino--quote-maybe (x)
  "Quote X if it's a symbol."
  (cond ((null x)
         nil)
        ((symbolp x)
         (list 'quote x))
        (t
         x)))

(defun deino--cycle-radio (sym)
  "Set SYM to the next value in its range."
  (let* ((val (symbol-value sym))
         (range (get sym 'range))
         (i 0)
         (l (length range)))
    (setq i (catch 'done
              (while (< i l)
                (if (equal (aref range i) val)
                    (throw 'done (1+ i))
                  (cl-incf i)))
              (error "Val not in range for %S" sym)))
    (set sym
         (aref range
               (if (>= i l)
                   0
                 i)))))

(defvar deino-pause-ring (make-ring 10)
  "Ring for paused deinos.")

(defun deino-pause-resume ()
  "Quit the current deino and save it to the stack.
If there's no active deino, pop one from the stack and call its body.
If the stack is empty, call the last deino's body."
  (interactive)
  (cond (deino-curr-map
         (ring-insert deino-pause-ring deino-curr-body-fn)
         (deino-keyboard-quit))
        ((zerop (ring-length deino-pause-ring))
         (funcall deino-curr-body-fn))
        (t
         (funcall (ring-remove deino-pause-ring 0)))))

;; Local Variables:
;; outline-regexp: ";;\\([;*]+ [^\s\t\n]\\|###autoload\\)\\|("
;; indent-tabs-mode: nil
;; End:

(provide 'deino)

;;; deino.el ends here
