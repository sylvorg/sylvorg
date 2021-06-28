;;; doom-aiern-modeline-env.el --- A environment parser for doom-aiern-modeline -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Justin Barclay, Vincent Zhang

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
;;; Commentary:
;;
;; Parse programming environment.
;;

;;; Code:

(require 'subr-x)
(require 'doom-aiern-modeline-core)


;; Externals
(defvar python-shell-interpreter)


;; Customizations

(defgroup doom-aiern-modeline-env nil
  "The environment parser for doom-aiern-modeline."
  :group 'doom-aiern-modeline
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/doom-aiern-modeline"))

(defcustom doom-aiern-modeline-env-load-string "..."
  "What to display as the version while a new one is being loaded."
  :type 'string
  :group 'doom-aiern-modeline-env)

(defcustom doom-aiern-modeline-before-update-env-hook nil
  "Hooks that run before the modeline version string is updated."
  :type 'hook
  :group 'doom-aiern-modeline-env)

(defcustom doom-aiern-modeline-after-update-env-hook nil
  "Hooks that run after the modeline version string is updated."
  :type 'hook
  :group 'doom-aiern-modeline-env)


;; Variables

;; Show version string for multi-version managers like rvm, rbenv, pyenv, etc.
(defvar-local doom-aiern-modeline-env--version nil
  "The version to display with major-mode in mode-line.
Example: \"2.6.0\"")

(defvar-local doom-aiern-modeline-env--command nil
  "A program that we're looking to extract version information from.
Example: \"ruby\"")

(defvar-local doom-aiern-modeline-env--command-args nil
  "A list of arguments for the command to extract the version from.
Example: '(\"--version\") ")

(defvar-local doom-aiern-modeline-env--parser nil
  "A function that returns version number from a command --version (or similar).
Example: 'doom-aiern-modeline-env--ruby")


;; Functions & Macros

(defun doom-aiern-modeline-update-env ()
  "Update environment info on mode-line."
  (when (and doom-aiern-modeline-env-version
             doom-aiern-modeline-env--command
             (executable-find doom-aiern-modeline-env--command)
             doom-aiern-modeline-env--command-args
             doom-aiern-modeline-env--parser)
    (let ((default-directory (doom-aiern-modeline-project-root))
          (buffer (current-buffer)))
      (run-hooks 'doom-aiern-modeline-before-update-env-hook)
      (setq doom-aiern-modeline-env--version doom-aiern-modeline-env-load-string)
      (doom-aiern-modeline-env--get
       doom-aiern-modeline-env--command
       doom-aiern-modeline-env--command-args
       (lambda (prog-version)
         (with-current-buffer buffer
           (setq doom-aiern-modeline-env--version
                 (funcall doom-aiern-modeline-env--parser prog-version))
           (run-hooks 'doom-aiern-modeline-after-update-env-hook)))))))

(add-hook 'find-file-hook #'doom-aiern-modeline-update-env)
(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (add-function
       :after after-focus-change-function
       (lambda ()
         (if (frame-focus-state)
             (doom-aiern-modeline-update-env))))
    (add-hook 'focus-in-hook #'doom-aiern-modeline-update-env)))

(defun doom-aiern-modeline-env--get (prog args callback)
  "Start a sub process using PROG and apply the ARGS to the sub process.
Once it receives information from STDOUT, it closes off the subprocess and
passes on the information into the CALLBACK.
Example:
  (doom-aiern-modeline-env--get
     \"ruby\"
     '(\"--version\")
     (lambda (line)
        (message (doom-aiern-modeline-parser--ruby line)))"
  (let ((proc (apply 'start-process
                     ;; Flaten process-args into a single list so we can handle
                     ;; variadic length args
                     (append
                      (list "doom-aiern-modeline-env" nil prog)
                      args)))
        (parser callback))
    (set-process-filter proc
                        (lambda (_proc line)
                          (ignore-errors
                            (funcall parser line))))))

(cl-defmacro doom-aiern-modeline-def-env (name &key hooks command parser)
  "Defines a handler for updating & displaying a version string for a language.

NAME is an unquoted symbol representing the handler's unique ID.
HOOKS is a list of hook symbols where this handler should be triggered.
COMMAND should be a function that returns a shell command and its arguments (as
  a list). It is run on HOOKS. It takes no arguments.
PARSER should be a function for parsing COMMAND's output line-by-line, to
  extract the version string."
  (declare (indent defun))
  (unless (and hooks command parser)
    (error "'%s' env is missing either :hooks, :command or :parser" name))
  (let ((parse-fn  (intern (format "doom-aiern-modeline-env--%s-parse" name)))
        (action-fn (intern (format "doom-aiern-modeline-env--%s-args"  name)))
        (setup-fn  (intern (format "doom-aiern-modeline-env-setup-%s"  name)))
        (update-fn (intern (format "doom-aiern-modeline-env-update-%s" name)))
        (enable-var  (intern (format "doom-aiern-modeline-env-enable-%s" name)))
        (command-var (intern (format "doom-aiern-modeline-env-%s-command" name)))
        (parser-var  (intern (format "doom-aiern-modeline-env-%s-parser-fn" name)))
        (exe-var     (intern (format "doom-aiern-modeline-env-%s-executable" name))))
    (macroexp-progn
     `((defcustom ,enable-var t
         ,(format "Whether to display the version string for %s buffers." name)
         :type 'boolean
         :group 'doom-aiern-modeline-env)
       (defvar ,command-var ',action-fn
         ,(concat "A function that returns the shell command and arguments (as a list) to\n"
                  "produce a version string."))
       (defvar ,parser-var ',parse-fn
         ,(format "The function to parse each line of `%s'\'s output." command-var))
       (defcustom ,exe-var nil
         ,(format (concat "What executable to use for the version indicator in %s buffers.\n\n"
                          "If nil, the default binary for this language is used.")
                  name)
         :type 'string
         :group 'doom-aiern-modeline-env)
       (defalias ',parse-fn ,parser
         (format "The line parser for %s buffers.\n\nUsed by `%s'."
                 ',name ',update-fn))
       (defalias ',action-fn ,command
         (format "The command resolver for %s buffers.\n\nUsed by `%s'."
                 ',name ',update-fn))
       (defalias ',setup-fn
         (lambda ()
           (if enable-local-variables
               (add-hook 'hack-local-variables-hook #',update-fn nil t)
             (,update-fn)))
         (format "Prepares the modeline to later display the %s version string."
                 ',name))
       (defalias ',update-fn
         (lambda ()
           (when ,enable-var
             (when-let* ((command-list (funcall ,command-var))
                         (exe (executable-find (car command-list))))
               (setq doom-aiern-modeline-env--command exe
                     doom-aiern-modeline-env--command-args (cdr command-list)
                     doom-aiern-modeline-env--parser ,parser-var)
               (doom-aiern-modeline-update-env))))
         (format "Updates the %s version string in the modeline." ',name))
       (let ((hooks ',(eval hooks)))
         (dolist (hook (if (listp hooks) hooks (list hooks)))
           (add-hook hook #',setup-fn)))))))


;; Bootstrap
;; Versions, support Python, Ruby, Perl and Golang, etc.

;;;###autoload (autoload 'doom-aiern-modeline-env-setup-python "doom-aiern-modeline-env")
(doom-aiern-modeline-def-env python
  :hooks   'python-mode-hook
  :command (lambda () (cond ((and (fboundp 'pipenv-project-p)
                             (pipenv-project-p))
                        (list "pipenv" "run"
                              (or doom-aiern-modeline-env-python-executable
                                  python-shell-interpreter
                                  "python")
                              "--version"))
                       ((executable-find "pyenv") (list "pyenv" "version-name"))
                       ((list (or doom-aiern-modeline-env-python-executable
                                  python-shell-interpreter
                                  "python")
                              "--version"))))
  :parser  (lambda (line) (let ((version (split-string line)))
                       (if (>= (length version) 2)
                           (cadr version)
                         (car version)))))

;;;###autoload (autoload 'doom-aiern-modeline-env-setup-ruby "doom-aiern-modeline-env")
(doom-aiern-modeline-def-env ruby
  :hooks   '(ruby-mode-hook enh-ruby-mode-hook)
  :command (lambda () (list (or doom-aiern-modeline-env-ruby-executable "ruby") "--version"))
  :parser  (lambda (line)
             (car (split-string
                   (cadr
                    (split-string line))
                   "p"))))

;;;###autoload (autoload 'doom-aiern-modeline-env-setup-perl "doom-aiern-modeline-env")
(doom-aiern-modeline-def-env perl
  :hooks   'perl-mode-hook
  :command (lambda () (list (or doom-aiern-modeline-env-perl-executable "perl") "--version"))
  :parser  (lambda (line)
             (cadr
              (split-string
               (car
                (split-string
                 (cadr
                  (split-string line "("))
                 ")"))
               "v"))))

;;;###autoload (autoload 'doom-aiern-modeline-env-setup-go "doom-aiern-modeline-env")
(doom-aiern-modeline-def-env go
  :hooks   'go-mode-hook
  :command (lambda () (list (or doom-aiern-modeline-env-go-executable "go") "version"))
  :parser  (lambda (line)
             (cadr
              (split-string
               (cadr
                (cdr
                 (split-string line)))
               "go"))))

;;;###autoload (autoload 'doom-aiern-modeline-env-setup-elixir "doom-aiern-modeline-env")
(doom-aiern-modeline-def-env elixir
  :hooks   'elixir-mode-hook
  :command (lambda () (list (or doom-aiern-modeline-env-elixir-executable "iex") "--version"))
  :parser  (lambda (line) (cadr (split-string line))))

;;;###autoload (autoload 'doom-aiern-modeline-env-setup-rust "doom-aiern-modeline-env")
(doom-aiern-modeline-def-env rust
  :hooks   'rust-mode-hook
  :command (lambda () (list (or doom-aiern-modeline-env-rust-executable "rustc") "--version"))
  :parser  (lambda (line)
             (car
              (split-string
               (cadr
                (split-string line))
               "-"))))

(provide 'doom-aiern-modeline-env)

;;; doom-aiern-modeline-env.el ends here
