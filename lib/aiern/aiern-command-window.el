;;; aiern-command-window.el --- aiern command line window implementation -*- lexical-binding: t -*-
;; Author: Emanuel Evans <emanuel.evans at gmail.com>
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

;; This provides an implementation of the vim command line window for
;; editing and repeating past ex commands and searches.

;;; Code:

(require 'aiern-vars)
(require 'aiern-common)
(require 'aiern-search)
(require 'aiern-ex)

(defvar aiern-search-module)

(define-derived-mode aiern-command-window-mode fundamental-mode "aiern-cmd"
  "Major mode for the aiern command line window."
  (auto-fill-mode 0)
  (setq-local after-change-functions (cons 'aiern-command-window-draw-prefix
                                           after-change-functions)))

(defun aiern-command-window (hist cmd-key execute-fn)
  "Open a command line window for HIST with CMD-KEY and EXECUTE-FN.
HIST should be a list of commands.  CMD-KEY should be the string of
the key whose history is being shown (one of \":\", \"/\", or
\"?\").  EXECUTE-FN should be a function of one argument to
execute on the result that the user selects."
  (when (eq major-mode 'aiern-command-window-mode)
    (user-error "Cannot recursively open command line window"))
  (dolist (win (window-list))
    (when (equal (buffer-name (window-buffer win))
                 "*Command Line*")
      (kill-buffer (window-buffer win))
      (delete-window win)))
  (split-window nil
                (unless (zerop aiern-command-window-height)
                  aiern-command-window-height)
                'above)
  (setq aiern-command-window-current-buffer (current-buffer))
  (ignore-errors (kill-buffer "*Command Line*"))
  (switch-to-buffer "*Command Line*")
  (setq-local aiern-command-window-execute-fn execute-fn)
  (setq-local aiern-command-window-cmd-key cmd-key)
  (aiern-command-window-mode)
  (aiern-command-window-insert-commands hist))

(defun aiern-command-window-ex (&optional current-command execute-fn)
  "Open a command line window for editing and executing ex commands.
If CURRENT-COMMAND is present, it will be inserted under the
cursor as the current command to be edited. If EXECUTE-FN is given,
it will be used as the function to execute instead of
`aiern-command-window-ex-execute', the default."
  (interactive)
  (aiern-command-window (cons (or current-command "") aiern-ex-history)
                       ":"
                       (or execute-fn 'aiern-command-window-ex-execute)))

(defun aiern-ex-command-window ()
  "Start command window with ex history and current minibuffer content."
  (interactive)
  (let ((current (minibuffer-contents))
        (config (current-window-configuration)))
    (aiern-ex-teardown)
    (select-window (minibuffer-selected-window) t)
    (aiern-command-window-ex current (apply-partially 'aiern-ex-command-window-execute config))))

(defun aiern-ex-search-command-window ()
  "Start command window with search history and current minibuffer content."
  (interactive)
  (let ((current (minibuffer-contents))
        (config (current-window-configuration)))
    (select-window (minibuffer-selected-window) t)
    (aiern-command-window (cons current aiern-ex-search-history)
                         (aiern-search-prompt (eq aiern-ex-search-direction 'forward))
                         (apply-partially 'aiern-ex-command-window-execute config))))

(defun aiern-command-window-execute ()
  "Execute the command under the cursor in the appropriate buffer.
The local var `aiern-command-window-execute-fn' determines which
function to execute."
  (interactive)
  (let ((result (buffer-substring (line-beginning-position)
                                  (line-end-position)))
        (execute-fn aiern-command-window-execute-fn)
        (command-window (get-buffer-window)))
    (select-window (previous-window))
    (unless (equal aiern-command-window-current-buffer (current-buffer))
      (user-error "Originating buffer is no longer active"))
    (kill-buffer "*Command Line*")
    (delete-window command-window)
    (funcall execute-fn result)
    (setq aiern-command-window-current-buffer nil)))

(defun aiern-command-window-ex-execute (result)
  "Execute RESULT as an ex command in the appropriate buffer."
  (unless (string-match-p "^ *$" result)
    (unless (equal result (car aiern-ex-history))
      (setq aiern-ex-history (cons result aiern-ex-history)))
    (let ((aiern-ex-current-buffer aiern-command-window-current-buffer))
      (aiern-ex-execute result))))

(defun aiern-command-window-search-forward ()
  "Open a command line window for forward searches."
  (interactive)
  (aiern-command-window (cons ""
                             (if (eq aiern-search-module 'aiern-search)
                                 aiern-ex-search-history
                               aiern-search-forward-history))
                       "/"
                       (lambda (result)
                         (aiern-command-window-search-execute result t))))

(defun aiern-command-window-search-backward ()
  "Open a command line window for backward searches."
  (interactive)
  (aiern-command-window (cons ""
                             (if (eq aiern-search-module 'aiern-search)
                                 aiern-ex-search-history
                               aiern-search-backward-history))
                       "?"
                       (lambda (result)
                         (aiern-command-window-search-execute result nil))))

(defun aiern-command-window-search-execute (result forward)
  "Search for RESULT using FORWARD to determine direction."
  (unless (zerop (length result))

    (if (eq aiern-search-module 'aiern-search)
        (progn
          (setq aiern-ex-search-pattern (aiern-ex-make-search-pattern result)
                aiern-ex-search-direction (if forward 'forward 'backward))
          (unless (equal result (car-safe aiern-ex-search-history))
            (push result aiern-ex-search-history))
          (aiern-ex-search))
      (if forward
          (unless (equal result (car-safe aiern-search-forward-history))
            (push result aiern-search-forward-history))
        (unless (equal result (car-safe aiern-search-backward-history))
          (push result aiern-search-backward-history)))
      (aiern-search result forward aiern-regexp-search))))

(defun aiern-command-window-draw-prefix (&rest ignored)
  "Display `aiern-command-window-cmd-key' as a prefix to the current line.
Parameters passed in through IGNORED are ignored."
  (let ((prefix (propertize aiern-command-window-cmd-key
                            'font-lock-face 'minibuffer-prompt)))
    (set-text-properties (line-beginning-position) (line-beginning-position 2)
                         (list 'line-prefix prefix))))

(defun aiern-command-window-insert-commands (hist)
  "Insert the commands in HIST."
  (let ((inhibit-modification-hooks t))
    (mapc #'(lambda (cmd) (insert cmd) (newline)) hist)
    (reverse-region (point-min) (point-max)))
  (let ((prefix (propertize aiern-command-window-cmd-key
                            'font-lock-face 'minibuffer-prompt)))
    (set-text-properties (point-min) (point-max) (list 'line-prefix prefix)))
  (goto-char (point-max))
  (when (and (bolp) (not (bobp))) (backward-char))
  (aiern-adjust-cursor))

(provide 'aiern-command-window)

;;; aiern-command-window.el ends here
