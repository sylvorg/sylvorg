;;; aiern-god-state.el --- use god-mode keybindings in aiern-mode

;; Copyright (C) 2021 by Jeet Ray
;; Author: Jeet Ray
;; URL: https://github.com/shadowrylander/aiern-god-state
;; Filename: aiern-god-state.el
;; Description: use god-mode keybindings in aiern-mode
;; Version: 0.1
;; Keywords: aiern leader god-mode
;; Package-Requires: ((aiern "1.0.8") (god-mode "2.12.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This is an aiern-mode state for using god-mode.

;; It provides a command `aiern-execute-in-god-state' that switches to
;; `god-local-mode' for the next command. I bind it to ","
;;
;;     (aiern-define-key 'normal global-map "," 'aiern-execute-in-god-state)
;;
;; for an automatically-configured leader key.
;;
;; Since `aiern-god-state' includes an indicator in the mode-line, you may want
;; to use `diminish' to keep your mode-line uncluttered, e.g.
;;
;;     (add-hook 'aiern-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
;;     (add-hook 'aiern-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))

;; It's handy to be able to abort a `aiern-god-state' command.  The following
;; will make the <ESC> key unconditionally exit aiern-god-state.
;;     (aiern-define-key 'god global-map [escape] 'aiern-god-state-bail)


;;; Code:
(require 'aiern)
(require 'god-mode)

(aiern-define-state god
  "Aiern God state."
  :tag " AG "
  :message "-- AIERN GOD MODE --"
  :entry-hook (aiern-god-start-hook)
  :exit-hook (aiern-god-stop-hook)
  :input-method t
  :intercept-esc nil)

(defun aiern-god-start-hook ()
  "Run before entering `aiern-god-state'."
  (god-local-mode 1))

(defun aiern-god-stop-hook ()
  "Run before exiting `aiern-god-state'."
  (god-local-mode -1))

(defvar aiern-execute-in-god-state-buffer nil)

(defvar aiern-god-last-command nil)

(defun aiern-god-fix-last-command ()
  "Change `last-command' to be the command before `aiern-execute-in-god-state'."
  (setq last-command aiern-god-last-command))

(defun aiern-stop-execute-in-god-state ()
  "Switch back to previous aiern state."
  (unless (or (eq this-command #'aiern-execute-in-god-state)
              (eq this-command #'universal-argument)
              (eq this-command #'universal-argument-minus)
              (eq this-command #'universal-argument-more)
              (eq this-command #'universal-argument-other-key)
              (eq this-command #'digit-argument)
              (eq this-command #'negative-argument)
              (minibufferp))
    (remove-hook 'pre-command-hook 'aiern-god-fix-last-command)
    (remove-hook 'post-command-hook 'aiern-stop-execute-in-god-state)
    (when (buffer-live-p aiern-execute-in-god-state-buffer)
      (with-current-buffer aiern-execute-in-god-state-buffer
        (if (and (eq aiern-previous-state 'visual)
                 (not (use-region-p)))
            (progn
              (aiern-change-to-previous-state)
              (aiern-exit-visual-state))
          (aiern-change-to-previous-state))))
    (setq aiern-execute-in-god-state-buffer nil)))

;;;###autoload
(defun aiern-execute-in-god-state ()
  "Execute the next command in God state."
  (interactive)
  (add-hook 'pre-command-hook #'aiern-god-fix-last-command t)
  (add-hook 'post-command-hook #'aiern-stop-execute-in-god-state t)
  (setq aiern-execute-in-god-state-buffer (current-buffer))
  (setq aiern-god-last-command last-command)
  (cond
   ((aiern-visual-state-p)
    (let ((mrk (mark))
          (pnt (point)))
      (aiern-god-state)
      (set-mark mrk)
      (goto-char pnt)))
   (t
    (aiern-god-state)))
  (aiern-echo "Switched to God state for the next command ..."))

;;; Unconditionally exit aiern-God state.
(defun aiern-god-state-bail ()
  "Stop current God command and exit God state."
  (interactive)
  (aiern-stop-execute-in-god-state)
  (aiern-god-stop-hook)
  (aiern-normal-state))

(provide 'aiern-god-state)
;;; aiern-god-state.el ends here
