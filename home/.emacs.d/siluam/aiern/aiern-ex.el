;;; aiern-ex.el --- Ex-mode -*- lexical-binding: nil -*-

;; Author: Frank Fischer <frank fischer at mathematik.tu-chemnitz.de>
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

;; Ex is implemented as an extensible minilanguage, whose grammar
;; is stored in `aiern-ex-grammar'.  Ex commands are defined with
;; `aiern-ex-define-cmd', which creates a binding from a string
;; to an interactive function.  It is also possible to define key
;; sequences which execute a command immediately when entered:
;; such shortcuts go in `aiern-ex-map'.
;;
;; To provide buffer and filename completion, as well as interactive
;; feedback, Ex defines the concept of an argument handler, specified
;; with `aiern-ex-define-argument-type'.  In the case of the
;; substitution command (":s/foo/bar"), the handler incrementally
;; highlights matches in the buffer as the substitution is typed.

(require 'aiern-common)
(require 'aiern-states)
(require 'shell)

;;; Code:

(defconst aiern-ex-grammar
  '((expression
     (count command argument #'aiern-ex-call-command)
     ((\? range) command argument #'aiern-ex-call-command)
     (line #'aiern-goto-line)
     (sexp #'eval-expression))
    (count
     number)
    (command #'aiern-ex-parse-command)
    (binding
     "[~&*@<>=:]+\\|[[:alpha:]_]+\\|!")
    (emacs-binding
     "[[:alpha:]-][[:alnum:][:punct:]-]+")
    (bang
     (\? (! space) "!" #'$1))
    (argument
     ((\? space) (\? "\\(?:.\\|\n\\)+") #'$2))
    (range
     ("%" #'(aiern-ex-full-range))
     (line ";" line #'(let ((tmp1 $1))
                        (save-excursion
                          (goto-line tmp1)
                          (aiern-ex-range tmp1 $3))))
     (line "," line #'(aiern-ex-range $1 $3))
     (line #'(aiern-ex-range $1 nil))
     ("`" "[-a-zA-Z_<>']" ",`" "[-a-zA-Z_<>']"
      #'(aiern-ex-char-marker-range $2 $4)))
    (line
     (base (\? offset) search (\? offset)
           #'(let ((tmp (aiern-ex-line $1 $2)))
               (save-excursion
                 (goto-line tmp)
                 (aiern-ex-line $3 $4))))
     ((\? base) offset search (\? offset)
      #'(let ((tmp (aiern-ex-line $1 $2)))
          (save-excursion
            (goto-line tmp)
            (aiern-ex-line $3 $4))))
     (base (\? offset) #'aiern-ex-line)
     ((\? base) offset #'aiern-ex-line))
    (base
     number
     marker
     search
     ("\\^" #'(aiern-ex-first-line))
     ("\\$" #'(aiern-ex-last-line))
     ("\\." #'(aiern-ex-current-line)))
    (offset
     (+ signed-number #'+))
    (marker
     ("'" "[-a-zA-Z_<>']" #'(aiern-ex-marker $2)))
    (search
     forward
     backward
     next
     prev
     subst)
    (forward
     ("/" "\\(?:[\\].\\|[^/,; ]\\)+" (! "/")
      #'(aiern-ex-re-fwd $2))
     ("/" "\\(?:[\\].\\|[^/]\\)+" "/"
      #'(aiern-ex-re-fwd $2)))
    (backward
     ("\\?" "\\(?:[\\].\\|[^?,; ]\\)+" (! "\\?")
      #'(aiern-ex-re-bwd $2))
     ("\\?" "\\(?:[\\].\\|[^?]\\)+" "\\?"
      #'(aiern-ex-re-bwd $2)))
    (next
     "\\\\/" #'(aiern-ex-prev-search))
    (prev
     "\\\\\\?" #'(aiern-ex-prev-search))
    (subst
     "\\\\&" #'(aiern-ex-prev-search))
    (signed-number
     (sign (\? number) #'aiern-ex-signed-number))
    (sign
     "\\+\\|-" #'intern)
    (number
     "[0-9]+" #'string-to-number)
    (space
     "[ ]+")
    (sexp
     "(.*)" #'(car-safe (read-from-string $1))))
  "Grammar for Ex.
An association list of syntactic symbols and their definitions.
The first entry is the start symbol.  A symbol's definition may
reference other symbols, but the grammar cannot contain
left recursion.  See `aiern-parser' for a detailed explanation
of the syntax.")

(defvar aiern-ex-echo-overlay nil
  "Overlay used for displaying info messages during ex.")

(defun aiern-ex-p ()
  "Whether Ex is currently active."
  (and aiern-ex-current-buffer t))

(aiern-define-command aiern-ex (&optional initial-input)
  "Enter an Ex command.
The ex command line is initialized with the value of
INITIAL-INPUT. If the command is called interactively the initial
input depends on the current state. If the current state is
normal state and no count argument is given then the initial
input is empty. If a prefix count is given the initial input is
.,.+count. If the current state is visual state then the initial
input is the visual region '<,'> or `<,`>. If the value of the
global variable `aiern-ex-initial-input' is non-nil, its content
is appended to the line."
  :keep-visual t
  :repeat abort
  (interactive
   (list
    (let ((s (concat
              (cond
               ((and (aiern-visual-state-p)
                     aiern-ex-visual-char-range
                     (memq (aiern-visual-type) '(inclusive exclusive)))
                "`<,`>")
               ((aiern-visual-state-p)
                "'<,'>")
               (current-prefix-arg
                (let ((arg (prefix-numeric-value current-prefix-arg)))
                  (cond ((< arg 0) (setq arg (1+ arg)))
                        ((> arg 0) (setq arg (1- arg))))
                  (if (= arg 0) "."
                    (format ".,.%+d" arg)))))
              aiern-ex-initial-input)))
      (and (> (length s) 0) s))))
  (let ((aiern-ex-current-buffer (current-buffer))
        (aiern-ex-previous-command (unless initial-input
                                    (car-safe aiern-ex-history)))
        aiern-ex-argument-handler
        aiern-ex-info-string
        result)
    (minibuffer-with-setup-hook
        (if initial-input #'aiern-ex-setup-and-update #'aiern-ex-setup)
      (setq result
            (read-from-minibuffer
             ":"
             (or initial-input
                 (and aiern-ex-previous-command
                      (propertize aiern-ex-previous-command 'face 'shadow)))
             aiern-ex-completion-map
             nil
             'aiern-ex-history
             aiern-ex-previous-command
             t)))
    (aiern-ex-execute result)))

(defun aiern-ex-execute (result)
  "Execute RESULT as an ex command on `aiern-ex-current-buffer'."
  ;; empty input means repeating the previous command
  (when (zerop (length result))
    (setq result aiern-ex-previous-command))
  ;; parse data
  (aiern-ex-update nil nil nil result)
  ;; execute command
  (unless (zerop (length result))
    (if aiern-ex-expression
        (eval aiern-ex-expression)
      (user-error "Ex: syntax error"))))

(defun aiern-ex-delete-backward-char ()
  "Close the minibuffer if it is empty.
Otherwise behaves like `delete-backward-char'."
  (interactive)
  (call-interactively
   (if (zerop (length (minibuffer-contents)))
       #'abort-recursive-edit
     #'delete-backward-char)))

(defun aiern-ex-abort ()
  "Cancel ex state when another buffer is selected."
  (unless (minibufferp)
    (abort-recursive-edit)))

(defun aiern-ex-command-window-execute (config result)
  (select-window (active-minibuffer-window) t)
  (set-window-configuration config)
  (delete-minibuffer-contents)
  (insert result)
  (exit-minibuffer))

(defun aiern-ex-setup ()
  "Initialize Ex minibuffer.
This function registers several hooks that are used for the
interactive actions during ex state."
  (add-hook 'post-command-hook #'aiern-ex-abort)
  (add-hook 'after-change-functions #'aiern-ex-update nil t)
  (add-hook 'minibuffer-exit-hook #'aiern-ex-teardown nil t)
  (when aiern-ex-previous-command
    (add-hook 'pre-command-hook #'aiern-ex-remove-default))
  (remove-hook 'minibuffer-setup-hook #'aiern-ex-setup)
  (with-no-warnings
    (make-variable-buffer-local 'completion-at-point-functions))
  (setq completion-at-point-functions
        '(aiern-ex-command-completion-at-point
          aiern-ex-argument-completion-at-point)))
(put 'aiern-ex-setup 'permanent-local-hook t)

(defun aiern-ex-setup-and-update ()
  "Initialize Ex minibuffer with `aiern-ex-setup', then call `aiern-ex-update'."
  (aiern-ex-setup)
  (aiern-ex-update))

(defun aiern-ex-teardown ()
  "Deinitialize Ex minibuffer.
Clean up everything set up by `aiern-ex-setup'."
  (remove-hook 'post-command-hook #'aiern-ex-abort)
  (remove-hook 'minibuffer-exit-hook #'aiern-ex-teardown t)
  (remove-hook 'after-change-functions #'aiern-ex-update t)
  (when aiern-ex-argument-handler
    (let ((runner (aiern-ex-argument-handler-runner
                   aiern-ex-argument-handler)))
      (when runner
        (funcall runner 'stop)))))
(put 'aiern-ex-teardown 'permanent-local-hook t)

(defvar aiern-paste-clear-minibuffer-first nil
  "`aiern-paste-before' cannot have `delete-minibuffer-contents' called
before it fetches certain registers becuase this would trigger various ex-updates,
sometimes moving point, so `C-a' `C-w' etc. would miss their intended target.")

(defun aiern-ex-remove-default ()
  "Remove the default text shown in the ex minibuffer.
When ex starts, the previous command is shown enclosed in
parenthesis. This function removes this text when the first key
is pressed."
  (when (and (not (eq this-command 'exit-minibuffer))
             (/= (minibuffer-prompt-end) (point-max)))
    (if (eq this-command 'aiern-ex-delete-backward-char)
        (setq this-command 'ignore))
    (if (eq this-original-command 'aiern-paste-from-register)
        (setq aiern-paste-clear-minibuffer-first t)
      (delete-minibuffer-contents)))
  (remove-hook 'pre-command-hook #'aiern-ex-remove-default))
(put 'aiern-ex-remove-default 'permanent-local-hook t)

(defun aiern-ex-update (&optional beg end len string)
  "Update Ex variables when the minibuffer changes.
This function is usually called from `after-change-functions'
hook. If BEG is non-nil (which is the case when called from
`after-change-functions'), then an error description is shown
in case of incomplete or unknown commands."
  (let* ((prompt (minibuffer-prompt-end))
         (string (or string (buffer-substring prompt (point-max))))
         arg bang cmd count expr func handler range tree type)
    (cond
     ((and (eq this-command #'self-insert-command)
           (commandp (setq cmd (lookup-key aiern-ex-map string))))
      (setq aiern-ex-expression `(call-interactively #',cmd))
      (when (minibufferp)
        (exit-minibuffer)))
     (t
      (setq cmd nil)
      ;; store the buffer position of each character
      ;; as the `ex-index' text property
      (dotimes (i (length string))
        (add-text-properties
         i (1+ i) (list 'ex-index (+ i prompt)) string))
      (with-current-buffer aiern-ex-current-buffer
        (setq tree (aiern-ex-parse string t)
              expr (aiern-ex-parse string))
        (when (eq (car-safe expr) 'aiern-ex-call-command)
          (setq count (eval (nth 1 expr))
                cmd (eval (nth 2 expr))
                arg (eval (nth 3 expr))
                range (cond
                       ((aiern-range-p count)
                        count)
                       ((numberp count)
                        (aiern-ex-range count count)))
                bang (and (save-match-data (string-match ".!$" cmd)) t))))
      (setq aiern-ex-tree tree
            aiern-ex-expression expr
            aiern-ex-range range
            aiern-ex-cmd cmd
            aiern-ex-bang bang
            aiern-ex-argument arg)
      ;; test the current command
      (when (and cmd (minibufferp))
        (setq func (aiern-ex-completed-binding cmd t))
        (cond
         ;; update argument-handler
         (func
          (when (setq type (aiern-get-command-property
                            func :ex-arg))
            (setq handler (cdr-safe
                           (assoc type
                                  aiern-ex-argument-types))))
          (unless (eq handler aiern-ex-argument-handler)
            (let ((runner (and aiern-ex-argument-handler
                               (aiern-ex-argument-handler-runner
                                aiern-ex-argument-handler))))
              (when runner (funcall runner 'stop)))
            (setq aiern-ex-argument-handler handler)
            (let ((runner (and aiern-ex-argument-handler
                               (aiern-ex-argument-handler-runner
                                aiern-ex-argument-handler))))
              (when runner (funcall runner 'start aiern-ex-argument))))
          (let ((runner (and aiern-ex-argument-handler
                             (aiern-ex-argument-handler-runner
                              aiern-ex-argument-handler))))
            (when runner (funcall runner 'update aiern-ex-argument))))
         (beg
          ;; show error message only when called from `after-change-functions'
          (let ((n (length (all-completions cmd (aiern-ex-completion-table)))))
            (cond
             ((> n 1) (aiern-ex-echo "Incomplete command"))
             ((= n 0) (aiern-ex-echo "Unknown command")))))))))))
(put 'aiern-ex-update 'permanent-local-hook t)

(defun aiern-ex-echo (string &rest args)
  "Display a message after the current Ex command."
  (with-selected-window (minibuffer-window)
    (with-current-buffer (window-buffer (minibuffer-window))
      (unless (or aiern-no-display
                  (zerop (length string)))
        (let ((string (format " [%s]" (apply #'format string args)))
              (ov (or aiern-ex-echo-overlay
                      (setq aiern-ex-echo-overlay (make-overlay (point-min) (point-max) nil t t))))
              after-change-functions before-change-functions)
          (put-text-property 0 (length string) 'face 'aiern-ex-info string)
          ;; The following 'trick' causes point to be shown before the
          ;; message instead behind. It is shamelessly stolen from the
          ;; implementation of `minibuffer-message`.
          (put-text-property 0 1 'cursor t string)
          (move-overlay ov (point-max) (point-max))
          (overlay-put ov 'after-string string)
          (add-hook 'pre-command-hook #'aiern--ex-remove-echo-overlay nil t))))))

(defun aiern--ex-remove-echo-overlay ()
  "Remove echo overlay from ex minibuffer."
  (when aiern-ex-echo-overlay
    (delete-overlay aiern-ex-echo-overlay)
    (setq aiern-ex-echo-overlay nil))
  (remove-hook 'pre-command-hook 'aiern--ex-remove-echo-overlay t))

(defun aiern-ex-completion ()
  "Completes the current ex command or argument."
  (interactive)
  (let (after-change-functions)
    (aiern-ex-update)
    (completion-at-point)
    (remove-text-properties (minibuffer-prompt-end) (point-max) '(face nil aiern))))

(defun aiern-ex-command-completion-at-point ()
  (let ((beg (or (get-text-property 0 'ex-index aiern-ex-cmd)
                 (point)))
        (end (point)))
    (list beg end (aiern-ex-completion-table) :exclusive 'no)))

(defun aiern-ex-completion-table ()
  (cond
   ((eq aiern-ex-complete-emacs-commands nil)
    #'aiern-ex-command-collection)
   ((eq aiern-ex-complete-emacs-commands 'in-turn)
    (completion-table-in-turn
     #'aiern-ex-command-collection
     #'(lambda (str pred flag)
         (completion-table-with-predicate
          obarray #'commandp t str pred flag))))
   (t
    #'(lambda (str pred flag)
        (aiern-completion-table-concat
         #'aiern-ex-command-collection
         #'(lambda (str pred flag)
             (completion-table-with-predicate
              obarray #'commandp t str pred flag))
         str pred flag)))))

(defun aiern-completion-table-concat (table1 table2 string pred flag)
  (cond
   ((eq flag nil)
    (let ((result1 (try-completion string table1 pred))
          (result2 (try-completion string table2 pred)))
      (cond
       ((null result1) result2)
       ((null result2) result1)
       ((and (eq result1 t) (eq result2 t)) t)
       (t result1))))
   ((eq flag t)
    (delete-dups
     (append (all-completions string table1 pred)
             (all-completions string table2 pred))))
   ((eq flag 'lambda)
    (and (or (eq t (test-completion string table1 pred))
             (eq t (test-completion string table2 pred)))
         t))
   ((eq (car-safe flag) 'boundaries)
    (or (completion-boundaries string table1 pred (cdr flag))
        (completion-boundaries string table2 pred (cdr flag))))
   ((eq flag 'metadata)
    '(metadata (display-sort-function . aiern-ex-sort-completions)))))

(defun aiern-ex-sort-completions (completions)
  (sort completions
        #'(lambda (str1 str2)
            (let ((p1 (eq 'aiern-ex-commands (get-text-property 0 'face str1)))
                  (p2 (eq 'aiern-ex-commands (get-text-property 0 'face str2))))
              (if (equal p1 p2)
                  (string< str1 str2)
                p1)))))

(defun aiern-ex-command-collection (cmd predicate flag)
  "Called to complete a command."
  (let (commands)
    ;; append ! to all commands that may take a bang argument
    (dolist (cmd (mapcar #'car aiern-ex-commands))
      (push cmd commands)
      (if (aiern-ex-command-force-p cmd)
          (push (concat cmd "!") commands)))
    (when (eq aiern-ex-complete-emacs-commands t)
      (setq commands
            (mapcar #'(lambda (str) (propertize str 'face 'aiern-ex-commands))
                    commands)))
    (cond
     ((eq flag nil) (try-completion cmd commands predicate))
     ((eq flag t) (all-completions cmd commands predicate))
     ((eq flag 'lambda) (test-completion cmd commands))
     ((eq (car-safe flag) 'boundaries)
      `(boundaries 0 . ,(length (cdr flag)))))))

(defun aiern-ex-argument-completion-at-point ()
  (let ((context (aiern-ex-syntactic-context (1- (point)))))
    (when (memq 'argument context)
      ;; if it's an autoload, load the function; this allows external
      ;; packages to register autoloaded ex commands which will be
      ;; loaded when ex argument completion is triggered
      (let ((binding-definition (symbol-function (aiern-ex-binding aiern-ex-cmd))))
        (when (autoloadp binding-definition)
          (autoload-do-load binding-definition)))

      (let* ((beg (or (and aiern-ex-argument
                           (get-text-property 0 'ex-index aiern-ex-argument))
                      (point)))
             (end (1+ (or (and aiern-ex-argument
                               (get-text-property (1- (length aiern-ex-argument))
                                                  'ex-index
                                                  aiern-ex-argument))
                          (1- (point)))))
             (binding (aiern-ex-completed-binding aiern-ex-cmd))
             (arg-type (aiern-get-command-property binding :ex-arg))
             (arg-handler (assoc arg-type aiern-ex-argument-types))
             (completer (and arg-handler
                             (aiern-ex-argument-handler-completer
                              (cdr arg-handler)))))
        (when completer
          (if (eq (car completer) 'collection)
              (list beg end (cdr completer))
            (save-restriction
              (narrow-to-region beg (point-max))
              (funcall (cdr completer)))))))))

(defun aiern-ex-define-cmd (cmd function)
  "Binds the function FUNCTION to the command CMD."
  (save-match-data
    (if (string-match "^[^][]*\\(\\[\\(.*\\)\\]\\)[^][]*$" cmd)
        (let ((abbrev (replace-match "" nil t cmd 1))
              (full (replace-match "\\2" nil nil cmd 1)))
          (aiern--add-to-alist 'aiern-ex-commands full function)
          (aiern--add-to-alist 'aiern-ex-commands abbrev full))
      (aiern--add-to-alist 'aiern-ex-commands cmd function))))

(defun aiern-ex-make-argument-handler (runner completer)
  (list runner completer))

(defun aiern-ex-argument-handler-runner (arg-handler)
  (car arg-handler))

(defun aiern-ex-argument-handler-completer (arg-handler)
  (cadr arg-handler))

(defmacro aiern-ex-define-argument-type (arg-type doc &rest body)
  "Defines a new handler for argument-type ARG-TYPE.
DOC is the documentation string. It is followed by a list of
keywords and function:

:collection COLLECTION

  A collection for completion as required by `all-completions'.

:completion-at-point FUNC

  Function to be called to initialize a potential
  completion. FUNC must match the requirements as described for
  the variable `completion-at-point-functions'. When FUNC is
  called the minibuffer content is narrowed to exactly match the
  argument.

:runner FUNC

  Function to be called when the type of the current argument
  changes or when the content of this argument changes. This
  function should take one obligatory argument FLAG followed by
  an optional argument ARG. FLAG is one of three symbol 'start,
  'stop or 'update. When the argument type is recognized for the
  first time and this handler is started the FLAG is 'start. If
  the argument type changes to something else or ex state
  finished the handler FLAG is 'stop. If the content of the
  argument has changed FLAG is 'update. If FLAG is either 'start
  or 'update then ARG is the current value of this argument. If
  FLAG is 'stop then arg is nil."
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (unless (stringp doc) (push doc body))
  (let (runner completer)
    (while (keywordp (car-safe body))
      (let ((key (pop body))
            (func (pop body)))
        (cond
         ((eq key :runner)
          (setq runner func))
         ((eq key :collection)
          (setq completer (cons 'collection func)))
         ((eq key :completion-at-point)
          (setq completer (cons 'completion-at-point func))))))
    `(eval-and-compile
       (aiern--add-to-alist
        'aiern-ex-argument-types
        ',arg-type
        '(,runner ,completer)))))

(aiern-ex-define-argument-type file
  "Handles a file argument."
  :collection read-file-name-internal)

(aiern-ex-define-argument-type buffer
  "Called to complete a buffer name argument."
  :collection internal-complete-buffer)

(declare-function shell-completion-vars "shell" ())

(defun aiern-ex-init-shell-argument-completion (flag &optional arg)
  "Prepares the current minibuffer for completion of shell commands.
This function must be called from the :runner function of some
argument handler that requires shell completion."
  (when (and (eq flag 'start)
             (not aiern-ex-shell-argument-initialized))
    (set (make-local-variable 'aiern-ex-shell-argument-initialized) t)
    (cond
     ;; Emacs 24
     ((fboundp 'comint-completion-at-point)
      (shell-completion-vars))
     (t
      (set (make-local-variable 'minibuffer-default-add-function)
           'minibuffer-default-add-shell-commands)))
    (setq completion-at-point-functions
          '(aiern-ex-command-completion-at-point
            aiern-ex-argument-completion-at-point))))

(define-obsolete-function-alias
  'aiern-ex-shell-command-completion-at-point
  'comint-completion-at-point "1.2.13")

(aiern-ex-define-argument-type shell
  "Shell argument type, supports completion."
  :completion-at-point comint-completion-at-point
  :runner aiern-ex-init-shell-argument-completion)

(defun aiern-ex-file-or-shell-command-completion-at-point ()
  (if (and (< (point-min) (point-max))
           (= (char-after (point-min)) ?!))
      (save-restriction
        (narrow-to-region (1+ (point-min)) (point-max))
        (comint-completion-at-point))
    (list (point-min) (point-max) #'read-file-name-internal)))

(aiern-ex-define-argument-type file-or-shell
  "File or shell argument type.
If the current argument starts with a ! the rest of the argument
is considered a shell command, otherwise a file-name. Completion
works accordingly."
  :completion-at-point aiern-ex-file-or-shell-command-completion-at-point
  :runner aiern-ex-init-shell-argument-completion)

(defun aiern-ex-binding (command &optional noerror)
  "Returns the final binding of COMMAND."
  (save-match-data
    (let ((binding command))
      (when binding
        (string-match "^\\(.+?\\)\\!?$" binding)
        (setq binding (match-string 1 binding))
        (while (progn
                 (setq binding (cdr (assoc binding aiern-ex-commands)))
                 (stringp binding)))
        (unless binding
          (setq binding (intern command)))
        (if (commandp binding)
            ;; check for remaps
            (or (command-remapping binding) binding)
          (unless noerror
            (user-error "Unknown command: `%s'" command)))))))

(defun aiern-ex-completed-binding (command &optional noerror)
  "Returns the final binding of the completion of COMMAND."
  (let ((completion (try-completion command aiern-ex-commands)))
    (aiern-ex-binding (if (eq completion t) command
                       (or completion command))
                     noerror)))

;;; TODO: extensions likes :p :~ <cfile> ...
(defun aiern-ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME.
Replaces % by the current file-name,
Replaces # by the alternate file-name in FILE-NAME."
  (let ((remote (file-remote-p file-name))
        (current-fname (buffer-file-name))
        (alternate-fname (and (other-buffer)
                              (buffer-file-name (other-buffer)))))
    (setq file-name (or (file-remote-p file-name 'localname) file-name))
    (when current-fname
      (setq current-fname (or (file-remote-p current-fname 'localname)
                              current-fname))
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%\\)"
                                      current-fname file-name
                                      t t 2)))
    (when alternate-fname
      (setq alternate-fname (or (file-remote-p alternate-fname 'localname)
                                alternate-fname))
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(#\\)"
                                      alternate-fname file-name
                                      t t 2)))
    (setq file-name
          (replace-regexp-in-string "\\\\\\([#%]\\)"
                                    "\\1" file-name t))
    (setq file-name (concat remote file-name)))
  file-name)

(defun aiern-ex-file-arg ()
  "Returns the current Ex argument as a file name.
This function interprets special file names like # and %."
  (unless (zerop (length aiern-ex-argument))
    (aiern-ex-replace-special-filenames aiern-ex-argument)))

(defun aiern-ex-repeat (count)
  "Repeats the last ex command."
  (interactive "P")
  (when count
    (goto-char (point-min))
    (forward-line (1- count)))
  (let ((aiern-ex-current-buffer (current-buffer))
        (hist aiern-ex-history))
    (while hist
      (let ((aiern-ex-last-cmd (pop hist)))
        (when aiern-ex-last-cmd
          (aiern-ex-update nil nil nil aiern-ex-last-cmd)
          (let ((binding (aiern-ex-binding aiern-ex-cmd)))
            (unless (eq binding #'aiern-ex-repeat)
              (setq hist nil)
              (if aiern-ex-expression
                  (eval aiern-ex-expression)
                (user-error "Ex: syntax error")))))))))

(defun aiern-ex-call-command (range command argument)
  "Execute the given command COMMAND."
  (let* ((count (when (numberp range) range))
         (range (when (aiern-range-p range) range))
         (bang (and (save-match-data (string-match ".!$" command)) t))
         (aiern-ex-point (point))
         (aiern-ex-range
          (or range (and count (aiern-ex-range count count))))
         (aiern-ex-command (aiern-ex-completed-binding command))
         (aiern-ex-bang (and bang t))
         (aiern-ex-argument (copy-sequence argument))
         (aiern-this-type (aiern-type aiern-ex-range))
         (current-prefix-arg count)
         (prefix-arg current-prefix-arg))
    (when (stringp aiern-ex-argument)
      (set-text-properties
       0 (length aiern-ex-argument) nil aiern-ex-argument))
    (let ((buf (current-buffer)))
      (unwind-protect
          (cond
           ((not aiern-ex-range)
            (setq this-command aiern-ex-command)
            (run-hooks 'pre-command-hook)
            (call-interactively aiern-ex-command)
            (run-hooks 'post-command-hook))
           (t
            ;; set visual selection to match the region if an explicit
            ;; range has been specified
            (let ((ex-range (aiern-copy-range aiern-ex-range))
                  beg end)
              (aiern-expand-range ex-range)
              (setq beg (aiern-range-beginning ex-range)
                    end (aiern-range-end ex-range))
              (aiern-sort beg end)
              (setq this-command aiern-ex-command)
              (run-hooks 'pre-command-hook)
              (set-mark end)
              (goto-char beg)
              (activate-mark)
              (call-interactively aiern-ex-command)
              (run-hooks 'post-command-hook))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (deactivate-mark)))))))

(defun aiern-ex-line (base &optional offset)
  "Return the line number of BASE plus OFFSET."
  (+ (or base (line-number-at-pos))
     (or offset 0)))

(defun aiern-ex-first-line ()
  "Return the line number of the first line."
  (line-number-at-pos (point-min)))

(defun aiern-ex-current-line ()
  "Return the line number of the current line."
  (line-number-at-pos (point)))

(defun aiern-ex-last-line ()
  "Return the line number of the last line."
  (save-excursion
    (goto-char (point-max))
    (when (bolp)
      (forward-line -1))
    (line-number-at-pos)))

(defun aiern-ex-range (beg-line &optional end-line)
  "Returns the first and last position of the current range."
  (aiern-range
   (aiern-line-position beg-line)
   (aiern-line-position (or end-line beg-line) -1)
   'line
   :expanded t))

(defun aiern-ex-full-range ()
  "Return a range encompassing the whole buffer."
  (aiern-range (point-min) (point-max) 'line))

(defun aiern-ex-marker (marker)
  "Return MARKER's line number in the current buffer.
Signal an error if MARKER is in a different buffer."
  (when (stringp marker)
    (setq marker (aref marker 0)))
  (setq marker (aiern-get-marker marker))
  (if (numberp marker)
      (line-number-at-pos marker)
    (user-error "Ex does not support markers in other files")))

(defun aiern-ex-char-marker-range (beg end)
  (when (stringp beg) (setq beg (aref beg 0)))
  (when (stringp end) (setq end (aref end 0)))
  (setq beg (aiern-get-marker beg)
        end (aiern-get-marker end))
  (if (and (numberp beg) (numberp end))
      (aiern-expand-range
       (aiern-range beg end
                   (if (aiern-visual-state-p)
                       (aiern-visual-type)
                     'inclusive)))
    (user-error "Ex does not support markers in other files")))

(defun aiern-ex-re-fwd (pattern)
  "Search forward for PATTERN.
Returns the line number of the match."
  (condition-case err
      (save-match-data
        (save-excursion
          (set-text-properties 0 (length pattern) nil pattern)
          (aiern-move-end-of-line)
          (and (re-search-forward pattern nil t)
               (line-number-at-pos (1- (match-end 0))))))
    (invalid-regexp
     (aiern-ex-echo (cadr err))
     nil)))

(defun aiern-ex-re-bwd (pattern)
  "Search backward for PATTERN.
Returns the line number of the match."
  (condition-case err
      (save-match-data
        (save-excursion
          (set-text-properties 0 (length pattern) nil pattern)
          (aiern-move-beginning-of-line)
          (and (re-search-backward pattern nil t)
               (line-number-at-pos (match-beginning 0)))))
    (invalid-regexp
     (aiern-ex-echo (cadr err))
     nil)))

(defun aiern-ex-prev-search ()
  (error "Previous search not yet implemented"))

(defun aiern-ex-signed-number (sign &optional number)
  "Return a signed number like -3 and +1.
NUMBER defaults to 1."
  (funcall sign (or number 1)))

;; function `aiern-ex-eval' has been superseded by `aiern-ex-parse' plus `eval'
(make-obsolete 'aiern-ex-eval 'aiern-ex-parse "1.2.14")

(defun aiern-ex-parse (string &optional syntax start)
  "Parse STRING as an Ex expression and return an evaluation tree.
If SYNTAX is non-nil, return a syntax tree instead.
START is the start symbol, which defaults to `expression'."
  (let* ((start (or start (car-safe (car-safe aiern-ex-grammar))))
         (match (aiern-parser
                 string start aiern-ex-grammar t syntax)))
    (car-safe match)))

(defun aiern-ex-parse-command (string)
  "Parse STRING as an Ex binding."
  (let ((result (aiern-parser string 'binding aiern-ex-grammar))
        bang command)
    (when result
      (setq command (car-safe result)
            string (cdr-safe result))
      ;; check whether the parsed command is followed by a slash, dash
      ;; or number and either the part before is NOT known to be a binding,
      ;; or the complete string IS known to be a binding
      (when (and (> (length string) 0)
                 (string-match-p "^[-/[:digit:]]" string)
                 (or (aiern-ex-binding (concat command string) t)
                     (not (aiern-ex-binding command t))))
        (setq result (aiern-parser (concat command string)
                                  'emacs-binding
                                  aiern-ex-grammar)
              command (car-safe result)
              string (cdr-safe result)))
      ;; parse a following "!" as bang only if
      ;; the command has the property :ex-bang t
      (when (aiern-ex-command-force-p command)
        (setq result (aiern-parser string 'bang aiern-ex-grammar)
              bang (or (car-safe result) "")
              string (cdr-safe result)
              command (concat command bang)))
      (cons command string))))

(defun aiern-ex-command-force-p (command)
  "Whether COMMAND accepts the bang argument."
  (let ((binding (aiern-ex-completed-binding command t)))
    (when binding
      (aiern-get-command-property binding :ex-bang))))

(defun aiern-flatten-syntax-tree (tree)
  "Find all paths from the root of TREE to its leaves.
TREE is a syntax tree, i.e., all its leave nodes are strings.
The `nth' element in the result is the syntactic context
for the corresponding string index (counted from zero)."
  (let* ((result nil)
         (traverse nil)
         (traverse
          #'(lambda (tree path)
              (if (stringp tree)
                  (dotimes (char (length tree))
                    (push path result))
                (let ((path (cons (car tree) path)))
                  (dolist (subtree (cdr tree))
                    (funcall traverse subtree path)))))))
    (funcall traverse tree nil)
    (nreverse result)))

(defun aiern-ex-syntactic-context (&optional pos)
  "Return the syntactical context of the character at POS.
POS defaults to the current position of point."
  (let* ((contexts (aiern-flatten-syntax-tree aiern-ex-tree))
         (length (length contexts))
         (pos (- (or pos (point)) (minibuffer-prompt-end))))
    (when (>= pos length)
      (setq pos (1- length)))
    (when (< pos 0)
      (setq pos 0))
    (when contexts
      (nth pos contexts))))

(defun aiern-parser--dexp (obj)
  "Parse a numerical dollar-sign symbol.
Given e.g. $4, return 4."
  (when (symbolp obj)
    (let ((str (symbol-name obj)))
      (save-match-data
        (when (string-match "\\$\\([0-9]+\\)" str)
          (string-to-number (match-string 1 str)))))))

(defun aiern-parser--dval (obj result)
  "Substitute all dollar-sign symbols in OBJ.
Each dollar-sign symbol is replaced with the corresponding
element in RESULT, so that $1 becomes the first element, etc.
The special value $0 is substituted with the whole list RESULT.
If RESULT is not a list, all dollar-sign symbols are substituted with
RESULT."
  (if (listp obj)
      (mapcar (lambda (obj) (aiern-parser--dval obj result)) obj)
    (let ((num (aiern-parser--dexp obj)))
      (if num
          (if (not (listp result))
              result
            (if (eq num 0)
                `(list ,@result)
              (nth (1- num) result)))
        obj))))

(defun aiern-parser (string symbol grammar &optional greedy syntax)
  "Parse STRING as a SYMBOL in GRAMMAR.
If GREEDY is non-nil, the whole of STRING must match.
If the parse succeeds, the return value is a cons cell
\(RESULT . TAIL), where RESULT is a parse tree and TAIL is
the remainder of STRING. Otherwise, the return value is nil.

GRAMMAR is an association list of symbols and their definitions.
A definition is either a list of production rules, which are
tried in succession, or a #'-quoted function, which is called
to parse the input.

A production rule can be one of the following:

    nil matches the empty string.
    A regular expression matches a substring.
    A symbol matches a production for that symbol.
    (X Y) matches X followed by Y.
    (\\? X) matches zero or one of X.
    (* X) matches zero or more of X.
    (+ X) matches one or more of X.
    (& X) matches X, but does not consume.
    (! X) matches anything but X, but does not consume.

Thus, a simple grammar may look like:

    ((plus \"\\\\+\")           ; plus <- \"+\"
     (minus \"-\")            ; minus <- \"-\"
     (operator plus minus)) ; operator <- plus / minus

All input-consuming rules have a value. A regular expression evaluates
to the text matched, while a list evaluates to a list of values.
The value of a list may be overridden with a semantic action, which is
specified with a #'-quoted expression at the end:

    (X Y #'foo)

The value of this rule is the result of calling foo with the values
of X and Y as arguments. Alternatively, the function call may be
specified explicitly:

    (X Y #'(foo $1 $2))

Here, $1 refers to X and $2 refers to Y. $0 refers to the whole list.
Dollar expressions can also be used directly:

    (X Y #'$1)

This matches X followed by Y, but ignores the value of Y;
the value of the list is the same as the value of X.

If the SYNTAX argument is non-nil, then all semantic actions
are ignored, and a syntax tree is constructed instead. The
syntax tree obeys the property that all the leave nodes are
parts of the input string. Thus, by traversing the syntax tree,
one can determine how each character was parsed.

The following symbols have reserved meanings within a grammar:
`\\?', `*', `+', `&', `!', `function', `alt', `seq' and nil."
  (let ((string (or string ""))
        func pair result rules tail)
    (cond
     ;; epsilon
     ((member symbol '("" nil))
      (setq pair (cons (if syntax "" nil) string)))
     ;; token
     ((stringp symbol)
      (save-match-data
        (when (or (eq (string-match symbol string) 0)
                  ;; ignore leading whitespace
                  (and (eq (string-match "^[ \f\t\n\r\v]+" string) 0)
                       (eq (match-end 0)
                           (string-match
                            symbol string (match-end 0)))))
          (setq result (match-string 0 string)
                tail (substring string (match-end 0))
                pair (cons result tail))
          (when (and syntax pair)
            (setq result (substring string 0
                                    (- (length string)
                                       (length tail))))
            (setcar pair result)))))
     ;; symbol
     ((symbolp symbol)
      (let ((context symbol))
        (setq rules (cdr-safe (assq symbol grammar)))
        (setq pair (aiern-parser string `(alt ,@rules)
                                grammar greedy syntax))
        (when (and syntax pair)
          (setq result (car pair))
          (if (and (listp result) (sequencep (car result)))
              (setq result `(,symbol ,@result))
            (setq result `(,symbol ,result)))
          (setcar pair result))))
     ;; function
     ((eq (car-safe symbol) 'function)
      (setq symbol (cadr symbol)
            pair (funcall symbol string))
      (when (and syntax pair)
        (setq tail (or (cdr pair) "")
              result (substring string 0
                                (- (length string)
                                   (length tail))))
        (setcar pair result)))
     ;; list
     ((listp symbol)
      (setq rules symbol
            symbol (car-safe rules))
      (if (memq symbol '(& ! \? * + alt seq))
          (setq rules (cdr rules))
        (setq symbol 'seq))
      (when (and (memq symbol '(+ alt seq))
                 (> (length rules) 1))
        (setq func (car (last rules)))
        (if (eq (car-safe func) 'function)
            (setq rules (delq func (copy-sequence rules))
                  func (cadr func))
          (setq func nil)))
      (cond
       ;; positive lookahead
       ((eq symbol '&)
        (when (aiern-parser string rules grammar greedy syntax)
          (setq pair (aiern-parser string nil grammar nil syntax))))
       ;; negative lookahead
       ((eq symbol '!)
        (unless (aiern-parser string rules grammar greedy syntax)
          (setq pair (aiern-parser string nil grammar nil syntax))))
       ;; zero or one
       ((eq symbol '\?)
        (setq rules (if (> (length rules) 1)
                        `(alt ,rules nil)
                      `(alt ,@rules nil))
              pair (aiern-parser string rules grammar greedy syntax)))
       ;; zero or more
       ((eq symbol '*)
        (setq rules `(alt (+ ,@rules) nil)
              pair (aiern-parser string rules grammar greedy syntax)))
       ;; one or more
       ((eq symbol '+)
        (let (current results)
          (catch 'done
            (while (setq current (aiern-parser
                                  string rules grammar nil syntax))
              (setq result (car-safe current)
                    tail (or (cdr-safe current) "")
                    results (append results (if syntax result
                                              (cdr-safe result))))
              ;; stop if stuck
              (if (equal string tail)
                  (throw 'done nil)
                (setq string tail))))
          (when results
            (setq func (or func 'list)
                  pair (cons results tail)))))
       ;; alternatives
       ((eq symbol 'alt)
        (catch 'done
          (dolist (rule rules)
            (when (setq pair (aiern-parser
                              string rule grammar greedy syntax))
              (throw 'done pair)))))
       ;; sequence
       (t
        (setq func (or func 'list))
        (let ((last (car-safe (last rules)))
              current results rule)
          (catch 'done
            (while rules
              (setq rule (pop rules)
                    current (aiern-parser string rule grammar
                                         (when greedy
                                           (null rules))
                                         syntax))
              (cond
               ((null current)
                (setq results nil)
                (throw 'done nil))
               (t
                (setq result (car-safe current)
                      tail (cdr-safe current))
                (unless (memq (car-safe rule) '(& !))
                  (if (and syntax
                           (or (null result)
                               (and (listp result)
                                    (listp rule)
                                    ;; splice in single-element
                                    ;; (\? ...) expressions
                                    (not (and (eq (car-safe rule) '\?)
                                              (eq (length rule) 2))))))
                      (setq results (append results result))
                    (setq results (append results (list result)))))
                (setq string (or tail ""))))))
          (when results
            (setq pair (cons results tail))))))
      ;; semantic action
      (when (and pair func (not syntax))
        (setq result (car pair))
        (cond
         ((null func)
          (setq result nil))
         ;; lambda function
         ((eq (car-safe func) 'lambda)
          (if (memq symbol '(+ seq))
              (setq result `(funcall ,func ,@result))
            (setq result `(funcall ,func ,result))))
         ;; string replacement
         ((or (stringp func) (stringp (car-safe func)))
          (let* ((symbol (or (car-safe (cdr-safe func))
                             (and (boundp 'context) context)
                             (car-safe (car-safe grammar))))
                 (string (if (stringp func) func (car-safe func))))
            (setq result (car-safe (aiern-parser string symbol grammar
                                                greedy syntax)))))
         ;; dollar expression
         ((aiern-parser--dexp func)
          (setq result (aiern-parser--dval func result)))
         ;; function call
         ((listp func)
          (setq result (aiern-parser--dval func result)))
         ;; symbol
         (t
          (if (memq symbol '(+ seq))
              (setq result `(,func ,@result))
            (setq result `(,func ,result)))))
        (setcar pair result))))
    ;; weed out incomplete matches
    (when pair
      (if (not greedy) pair
        (if (null (cdr pair)) pair
          ;; ignore trailing whitespace
          (when (save-match-data (string-match "^[ \f\t\n\r\v]*$" (cdr pair)))
            (unless syntax (setcdr pair nil))
            pair))))))

(provide 'aiern-ex)

;;; aiern-ex.el ends here
