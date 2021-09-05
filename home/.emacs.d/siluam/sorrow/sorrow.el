;;; sorrow.el --- Roll your own modal mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2016--present Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/sorrow
;; Keywords: convenience, modal, keys
;; Version: 0.45
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; sorrow provides a convenient way of defining modal keybindings in Emacs.
;; The primary way of binding keys is using `sorrow-key' and `sorrow-keys'.
;; Both of these functions provide useful keyword arguments.
;; `sorrow-mode' is used to toggle the modal editing environment.
;; sorrow does not come with any predefined bindings!
;;
;; If you want bindings that only should be active when the region is active,
;; please have a look at `selected-minor-mode' (https://github.com/Kungsgeten/selected.el)

;;; Code:
(require 'cl-lib)
(require 'org-macs)
(require 'seq)
(require 'subr-x)

(defvar sorrow-mode-map (make-sparse-keymap)
  "General bindings in sorrow-mode.
Major mode specific bindings will be bound to sorrow-<major-mode>-map instead.")

(defvar sorrow-cursor-type t
  "Cursor type used in `sorrow-mode'.  See description of `cursor-type'.")

(defvar sorrow-cursor-color "red"
  "The cursor color used in `sorrow-mode'.  If nil then use default color.")

(defconst sorrow-default-cursor-color (face-attribute 'cursor :background)
  "Default color of cursor.")

(defvar sorrow-bindings-list ()
  "A list of all the bindings in `sorrow-mode'.
It is more convenient to view this using `sorrow-bindings'.")

(defvar sorrow--last-command nil)

(defun sorrow-repeat ()
  "Repeat last executed command in `sorrow-map' (or major mode variant).

If you do not want a command to be remembered by `sorrow-repeat',
add :norepeat t as a keyword."
  (interactive)
  (when sorrow--last-command
    (command-execute sorrow--last-command nil nil t)))

(defvar sorrow--non-repeating-commands '(sorrow-repeat))

(defvar sorrow-mode-keymaps nil
  "Holds a list of all sorrow major mode specific keymaps.")

;; For compability with multiple-cursors
(defvar mc/cmds-to-run-for-all nil)
(defvar mc/cmds-to-run-once nil)

(defun sorrow-derived-keymaps ()
  "Get sorrow mode keymaps relevant to the current `major-mode' and/or minor-modes."
  (mapcar (lambda (mode)
            (eval (intern-soft (concat "sorrow-" (symbol-name mode) "-map"))))
          (seq-filter (lambda (elt) (or (derived-mode-p elt)
                                        (and (boundp elt) (symbol-value elt))))
                      sorrow-mode-keymaps)))

(defun sorrow-maybe-store-last-command ()
  "Update `sorrow--last-command', if `this-command' is repeatable."
  (when sorrow-mode
    (let ((cmd (lookup-key (apply 'append sorrow-mode-map
                                  (sorrow-derived-keymaps))
                           (this-command-keys))))
      (if (and (commandp cmd)
               (not (member cmd sorrow--non-repeating-commands)))
          (setq sorrow--last-command cmd)))))

(defun sorrow--translate-keymap (keymap)
  "Translate keymap to equivalent list of pairs (key command).

If KEYMAP contains keybinding to other keymaps these inner keymaps
will be translated as well."
  (let* ((entries (cdr keymap))
         (translate-entry
          '(lambda (entry)
             (let ((key (car entry))
                   (binding (cdr entry)))
             (list (key-description (vconcat (list key)))
                   (if (keymapp binding)
                       (sorrow--translate-keymap binding)
                     binding))))))
    (reverse (mapcar translate-entry entries))))

;;;###autoload
(defun sorrow-key (key target &rest args)
  "Bind KEY to TARGET in `sorrow-mode'.

TARGET can be one of:

naked-string Pressing KEY will simulate TARGET as a keypress.
command      Calls TARGET interactively.
list         Each element of TARGET is sent to `sorrow-key' again, with
             KEY as a prefix key.  ARGS are copied, except for :name.
             :name will be used by `which-key' (if installed) to name
             the prefix key, if `which-key-enable-extended-define-key'
             is t.
keymap       Similarly to list, each keybinding of provided keymap
             is sent to `sorrow-key' again with all keyword arguments applied.
             It also works with keymap that bind other keymaps like `ctl-x-map'.
:hydra       If you have hydra installed, a new hydra will be created and
             bound to KEY.  The first element of ARGS should be a list
             containing the arguments sent to `defhydra'.
:hydra+      If you have hydra installed, an old hydra will be added to.
             The first element of ARGS should be a list containing the
             arguments sent to `defhydra+'.
:deino       If you have deino installed, a new deino will be created and
             bound to KEY.  The first element of ARGS should be a list
             containing the arguments sent to `defdeino'.
:deino+      If you have deino installed, an old deino will be added to.
             The first element of ARGS should be a list containing the
             arguments sent to `defdeino+'.

ARGS should be of the form [:keyword option]... if TARGET is a naked-string
or a command.  The following keywords exist:

:name      A string, naming the binding.  If ommited get name from TARGET.
:exit      If t then exit `sorrow-mode' after the command.
:read      If t then prompt for a string to insert after the command.
:mode      If set to a major or minor mode symbol (e.g. 'org-mode) the key will
           only be bound in that mode.
:norepeat  If t then do not become a target of `sorrow-repeat'.
:then      Can be a quoted list of additional commands that will be run after
           the TARGET.  These will not be shown in the name of the binding.
           (use :name to give it a nickname).
:first     Similar to :then, but is run before the TARGET.
:mc-all    If t the binding's command will be added to `mc/cmds-to-run-for-all'.
           If 0 the binding's command will be added to `mc/cmds-to-run-once'.

If any ARGS other han :mode, :norepeat or :mc-all are given, a
new command named sorrow:<hash>:<name> will be created. This is to
make sure the name of the created command is unique."
  (cond
   ((listp target)
    (when (and (require 'which-key nil t)
               which-key-enable-extended-define-key
               (plist-get args :name))
      (let ((mode (plist-get args :mode)))
        (if mode
            (let ((map-name (format "sorrow-%s-map" mode)))
              (unless (intern-soft map-name)
                (set (intern map-name) (make-sparse-keymap))
                (set-keymap-parent (eval (intern map-name))
                                   sorrow-mode-map)
                (add-to-list 'sorrow-mode-keymaps mode))
              (define-key (eval (intern map-name)) (naked key) `(,(plist-get args :name))))
          (define-key sorrow-mode-map (naked key) `(,(plist-get args :name))))))
    (mapc (lambda (x)
            ;; Merge :then lists
            (when (and (plist-get (cddr x) :then)
                       (plist-get args :then))
              (setf (cddr x) (plist-put (cddr x) :then (append (plist-get (cddr x) :then)
                                                               (plist-get args :then)))))
            ;; Merge :first lists
            (when (and (plist-get (cddr x) :first)
                       (plist-get args :first))
              (setf (cddr x) (plist-put (cddr x) :first (append (plist-get (cddr x) :first)
                                                                (plist-get args :first)))))
            (apply #'sorrow-key `(,(concat key " " (car x))
                                     ,@(cdr x)
                                     ,@(org-plist-delete args :name))))
          target))
   ((and (require 'hydra nil t)
         (equal target :hydra))
    (apply #'sorrow-key `(,key ,(eval `(defhydra ,@(car args))) ,@(cdr args))))
   ((and (require 'hydra nil t)
         (equal target :hydra+))
    (apply #'sorrow-key `(,key ,(eval `(defhydra+ ,@(car args))) ,@(cdr args))))
   ((and (require 'deino nil t)
         (equal target :deino))
    (apply #'sorrow-key `(,key ,(eval `(defdeino ,@(car args))) ,@(cdr args))))
   ((and (require 'deino nil t)
         (equal target :deino+))
    (apply #'sorrow-key `(,key ,(eval `(defdeino+ ,@(car args))) ,@(cdr args))))
   ((and (stringp target) (keymapp (key-binding (naked target))))
    (let* ((binding (key-binding (naked target)))
           (map-to-translate (if (symbolp binding) (symbol-function binding) binding)))
      (let ((translated-keymap (sorrow--translate-keymap map-to-translate)))
        (apply #'sorrow-key `(,key ,translated-keymap ,@args)))))
   ((and (not (stringp target)) (not (symbol-function target)) (boundp target) (keymapp (symbol-value target)))
    (let ((translated-keymap (sorrow--translate-keymap (symbol-value target))))
      (apply #'sorrow-key `(,key ,translated-keymap ,@args))))
   ((and (not (stringp target)) (keymapp target))
    (let ((translated-keymap (sorrow--translate-keymap (symbol-function target))))
      (apply #'sorrow-key `(,key ,translated-keymap ,@args))))
   ((and (symbolp target) (not (functionp target)))
    (error "`%s' isn't a function" (symbol-name target)))
   (t
    (let* ((name (or (plist-get args :name)
                     (if (stringp target)
                         target
                       (symbol-name target))))
           (hash (secure-hash 'md5 (format "%s%s" target args)))
           (docs
            (if (stringp target)
                (if (keymapp (key-binding (naked target)))
                    (concat "Call keymap " target)
                  (format "%s → %s (`%s')\n\n%s%s"
                          (key-description (naked key))
                          (key-description (naked target))
                          (key-binding (naked target))
                          (documentation (key-binding (naked target)))
                          (mapconcat #'documentation (plist-get args :then) "\n")))
              (concat (documentation target)
                      (mapconcat #'documentation (plist-get args :then) "\n"))))
           (func
            (cond
             ((thread-first (org-plist-delete args :mode)
                (org-plist-delete :norepeat)
                (org-plist-delete :mc-all))
              (eval
               `(defun ,(intern (concat "sorrow:" hash ":" name)) ()
                  ,docs
                  (interactive)
                  (dolist (f (quote ,(plist-get args :first)))
                    (if (commandp f)
                        (let ((real-this-command f))
                          (call-interactively f))
                      (apply f nil)))
                  (if (and (stringp ',target)
                           (keymapp (key-binding (naked ,target))))
                      (progn
                        (when ,(plist-get args :exit) (sorrow-mode -1))
                        (setq unread-command-events (listify-key-sequence (naked ',target))))
                    (let ((real-this-command
                           (if (stringp ',target)
                               (key-binding (naked ,target))
                             ',target)))
                      (call-interactively real-this-command))
                    (dolist (f (quote ,(plist-get args :then)))
                      (if (commandp f)
                          (let ((real-this-command f))
                            (call-interactively f))
                        (apply f nil)))
                    (when ,(plist-get args :exit) (sorrow-mode -1))
                    (when ,(plist-get args :read) (insert (read-string "Insert: ")))))))
             ((stringp target)
              (if (keymapp (key-binding (naked target)))
                  ;; TODO: This doesn't seem to work with "keymaps inside of keymaps"
                  (lambda () (interactive)
                    (setq unread-command-events (listify-key-sequence (naked target))))
                (key-binding (naked target))))
             (t
              target)))
           (mode (plist-get args :mode)))
      (when (plist-get args :norepeat)
        (add-to-list 'sorrow--non-repeating-commands func))
      (let ((mc-all (plist-get args :mc-all)))
        (when mc-all
          (if (and (equal mc-all 0) (not (memq func mc/cmds-to-run-for-all)))
              (add-to-list 'mc/cmds-to-run-once func)
            (and (not (memq func mc/cmds-to-run-once))
                 (add-to-list 'mc/cmds-to-run-for-all func)))))
      (if mode
          (let ((map-name (format "sorrow-%s-map" mode)))
            (unless (intern-soft map-name)
              (set (intern map-name) (make-sparse-keymap))
              (set-keymap-parent (eval (intern map-name))
                                 sorrow-mode-map)
              (add-to-list 'sorrow-mode-keymaps mode))
            (define-key (eval (intern map-name)) (naked key) func))
        (define-key sorrow-mode-map (naked key) func))
      (add-to-list 'sorrow-bindings-list `(,key ,name ,@args))))))

;;;###autoload
(defmacro sorrow-keys (&rest args)
  "Bind several keys in `sorrow-mode'.
Typically each element in ARGS should be of the form (key target [keywords]).
The target should not be quoted.
The first argument may be a list of keywords; they're applied to all keys:

  \(:exit t :then '(kill-region)).

See `sorrow-key' for more information."
  (let ((kw-list
         (if (symbolp (caar args))
             (pop args)
           nil)))
    `(progn
       ,@(mapcar (lambda (x)
                   `(sorrow-key ,(car x)
                                   ,(if (stringp (cadr x))
                                        (cadr x)
                                      `(quote ,(cadr x)))
                                   ,@(nthcdr 2 x)
                                   ,@kw-list))
                 args))))

;;;###autoload
(defmacro sorrow-major-mode-keys (mode &rest args)
  "Bind several keys in `sorrow-mode', but only if major mode is MODE.
ARGS is the same as `sorrow-keys'."
  `(progn
     ,@(mapcar (lambda (x)
                 `(sorrow-key ,(car x)
                                 (if ,(stringp (cadr x))
                                     ,(cadr x)
                                   (quote ,(cadr x)))
                                 ,@(nthcdr 2 x)
                                 :mode ,mode))
               args)))

;;;###autoload
(defun sorrow-command-then-sorrow (binding &optional command keymap)
  "Define key BINDING to COMMAND in KEYMAP. Then activate `sorrow-mode'.
If COMMAND is excluded, use what is bound to right now in KEYMAP.
If KEYMAP is excluded, use `current-global-map'."
  (let* ((keymap (or keymap (current-global-map)))
         (command (or command
                      (lookup-key keymap (naked binding))
                      (user-error "No binding for '%s'" binding)))
         (name (symbol-name command))
         (hash (secure-hash 'md5 (format "%s-then-sorrow" command)))
         (docs (concat (documentation command)
                       "\n\nThen enter `sorrow-mode'."))
         (func
          (eval
           `(defun ,(intern (concat "sorrow:" hash ":" name)) ()
              ,docs
              (interactive)
              (call-interactively ',command)
              (sorrow-mode 1)))))
    (define-key keymap (naked binding) func)))

;;;###autoload
(defun sorrow-set-key (key command)
  "Give KEY a binding as COMMAND in `sorrow-mode-map'.

This function is meant to be used interactively, if you want to
temporarily bind a key in sorrow.

See `global-set-key' for more info."
  (interactive "KSet sorrow key: \nCSet sorrow key %s to command: ")
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key sorrow-mode-map key command))

;;;###autoload
(defun sorrow-unset-key (key)
  "Remove `sorrow-mode-map' binding of KEY.
KEY is a string or vector representing a sequence of keystrokes.

This function is meant to unbind keys set with `sorrow-set-key'."
  (interactive "kUnset sorrow key: ")
  (define-key sorrow-mode-map key nil))

;;;###autoload
(defun sorrow-bindings ()
  "Display a buffer of all bindings in `sorrow-mode'."
  (interactive)
  (let ((key-column-width 18)
        (command-column-width 40))
    (cl-flet ((sorrow-princ-bindings
               (bindings)
               (mapc (lambda (x)
                       (let ((keywords (nthcdr 2 x)))
                         (princ (concat (car x)
                                        (make-string (- key-column-width (length (car x))) ? )
                                        (cadr x)
                                        (if (plist-get keywords :exit) " → EXIT")
                                        (if (plist-get keywords :read) " → READ")
                                        "\n"))
                         (when (and (not (plist-get keywords :name))
                                    (plist-get keywords :then))
                           (princ (concat (make-string key-column-width ? )
                                          (mapconcat (lambda (x)
                                                       (concat " → " (symbol-name x)))
                                                     (plist-get keywords :then)
                                                     (concat "\n" (make-string key-column-width ? )))
                                          "\n")))))
                     bindings)))
      (with-output-to-temp-buffer "*sorrow-bindings*"
        (princ (format "Key%s Bound to\n%s %s\n"
                       (make-string (- key-column-width 4) ? )
                       (make-string (1- key-column-width) ?-)
                       (make-string (1- command-column-width) ?-)))
        (setq sorrow-bindings-list
              (sort sorrow-bindings-list
                    (lambda (l r)
                      (string< (car l) (car r)))))
        (sorrow-princ-bindings (cl-remove-if (lambda (x)
                                            (plist-get (nthcdr 2 x) :mode))
                                          sorrow-bindings-list))
        (let ((modes))
          (mapc (lambda (x)
                  (add-to-list 'modes (plist-get (nthcdr 2 x) :mode)))
                sorrow-bindings-list)
          (mapc (lambda (x)
                  (when x
                    (princ (format "\n\n%s specific bindings\n%s\n"
                                   (symbol-name x)
                                   (make-string (+ key-column-width command-column-width) ?-)))
                    (sorrow-princ-bindings (cl-remove-if-not (lambda (binding)
                                                            (equal x (plist-get (nthcdr 2 binding) :mode)))
                                                          sorrow-bindings-list))))
                modes))))))

;;;###autoload
(define-minor-mode sorrow-mode
  "Toggle `sorrow-mode'."
  nil " sorrow" sorrow-mode-map
  (if sorrow-mode
      (progn
        (add-hook 'post-command-hook #'sorrow-maybe-store-last-command)
        (when sorrow-cursor-color
          (add-hook 'post-command-hook #'sorrow--cursor-color-update))
        (setq-local cursor-type sorrow-cursor-type)
        (dolist (map (sorrow-derived-keymaps))
          (make-local-variable 'minor-mode-overriding-map-alist)
          (push `(sorrow-mode . ,map) minor-mode-overriding-map-alist)))
    (remove-hook 'post-command-hook #'sorrow-maybe-store-last-command)
    (remove-hook 'post-command-hook #'sorrow--cursor-color-update)
    (setq minor-mode-overriding-map-alist
          (assq-delete-all 'sorrow-mode minor-mode-overriding-map-alist))
    (set-cursor-color sorrow-default-cursor-color)
    (setq-local cursor-type (default-value 'cursor-type))))

(defun sorrow--cursor-color-update ()
  "Set cursor color depending on if `sorrow-mode' is active or not."
  (if sorrow-mode
      (set-cursor-color sorrow-cursor-color)
    (set-cursor-color sorrow-default-cursor-color)))

;; use-package integration
(defun sorrow--extract-commands-from (args)
  "Extract commands from ARGS to enable lazy loading for :sorrow."
  (let (commands)
    (cl-remove-duplicates
     (dolist (arg args commands)
       (let ((target (cadr arg)))
         (cond
          ((listp target)
           (setq commands (append (sorrow--extract-commands-from target) commands)))
          ((equal target :hydra)
           (dolist (hydra-term (cadr (cl-third arg)))
             (when (and hydra-term
                        (listp hydra-term)
                        (cadr hydra-term))
               (push (cadr hydra-term) commands))))
          ((equal target :hydra+)
           (dolist (hydra-term (cadr (cl-third arg)))
             (when (and hydra-term
                        (listp hydra-term)
                        (cadr hydra-term))
               (push (cadr hydra-term) commands))))
          ((equal target :deino)
           (dolist (deino-term (cadr (cl-third arg)))
             (when (and deino-term
                        (listp deino-term)
                        (cadr deino-term))
               (push (cadr deino-term) commands))))
          ((equal target :deino+)
           (dolist (deino-term (cadr (cl-third arg)))
             (when (and deino-term
                        (listp deino-term)
                        (cadr deino-term))
               (push (cadr deino-term) commands))))
          ((not (stringp target))
           (push target commands))))))))

(with-eval-after-load 'use-package-core
  ;; step 1: introduce sorrow keyword before :bind
  (unless (member :sorrow use-package-keywords)
    (setq use-package-keywords (use-package-list-insert :sorrow use-package-keywords :bind)))

  ;; ensure deferred loading
  (when (boundp 'use-package-deferring-keywords)
    (add-to-list 'use-package-deferring-keywords :sorrow t))

  ;; step 2: normalize
  (defun use-package-normalize/:sorrow (_name _keyword args)
    "Apply lists of keywords to all keys following that list."
    (let (kwlist sanitized-args)
      (dolist (arg args sanitized-args)
        (cond
         ((symbolp (car arg))
          (setq kwlist arg))
         ((stringp (car arg))
          (push (append arg kwlist) sanitized-args))))))

  ;; step 3: handler
  (defun use-package-handler/:sorrow (name _keyword arglists rest state)
    "Use-package handler for :sorrow."

    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-append rest :commands
                                  (sorrow--extract-commands-from arglists)))
       state)
     `((ignore ,@(mapcar (lambda (arglist)
                           (if (stringp (cadr arglist))
                               `(sorrow-key ,(car arglist)
                                               ,(cadr arglist)
                                               ,@(nthcdr 2 arglist)
                                               :package ',name)
                             `(sorrow-key ,(car arglist)
                                             (quote ,(cadr arglist))
                                             ,@(nthcdr 2 arglist)
                                             :package ',name)))
                         arglists))))))

(provide 'sorrow)
;;; sorrow.el ends here
