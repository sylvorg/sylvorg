;;; doom-aiern-modeline.el --- A minimal and modern mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-aiern-modeline
;; Version: 3.2.0
;; Package-Requires: ((emacs "25.1") (all-the-icons "2.2.0") (shrink-path "0.2.0") (dash "2.11.0"))
;; Keywords: faces mode-line

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
;; This package offers a fancy and fast mode-line inspired by minimalism design.
;;
;; It's integrated into Doom Emacs (https://github.com/hlissner/doom-emacs) and
;; Centaur Emacs (https://github.com/seagle0128/.emacs.d).
;;
;; The doom-aiern-modeline offers:
;; - A match count panel (for anzu, iedit, multiple-cursors, symbol-overlay,
;;   evil-search and evil-substitute)
;; - An indicator for recording a macro
;; - Current environment version (e.g. python, ruby, go, etc.) in the major-mode
;; - A customizable mode-line height (see doom-aiern-modeline-height)
;; - A minor modes segment which is compatible with minions
;; - An error/warning count segment for flymake/flycheck
;; - A workspace number segment for eyebrowse
;; - A perspective name segment for persp-mode
;; - A window number segment for winum and window-numbering
;; - An indicator for modal editing state, including evil, overwrite, god, ryo
;;   and xah-fly-keys, etc.
;; - An indicator for battery status
;; - An indicator for current input method
;; - An indicator for debug state
;; - An indicator for remote host
;; - An indicator for LSP state with lsp-mode or eglot
;; - An indicator for github notifications
;; - An indicator for unread emails with mu4e-alert
;; - An indicator for unread emails with gnus (basically builtin)
;; - An indicator for irc notifications with circe, rcirc or erc.
;; - An indicator for buffer position which is compatible with nyan-mode or poke-line
;; - An indicator for party parrot
;; - An indicator for PDF page number with pdf-tools
;; - An indicator for markdown/org previews with grip
;; - Truncated file name, file icon, buffer state and project name in buffer
;;   information segment, which is compatible with project, find-file-in-project
;;   and projectile
;; - New mode-line for Info-mode buffers
;; - New package mode-line for paradox
;; - New mode-line for helm buffers
;; - New mode-line for git-timemachine buffers
;;
;; Installation:
;; From melpa, `M-x package-install RET doom-aiern-modeline RET`.
;; In `init.el`,
;; (require 'doom-aiern-modeline)
;; (doom-aiern-modeline-mode 1)
;; or
;; (use-package doom-aiern-modeline
;;   :ensure t
;;   :hook (after-init . doom-aiern-modeline-mode))
;;

;;; Code:

(require 'doom-aiern-modeline-core)
(require 'doom-aiern-modeline-segments)


;;
;; Mode lines
;;

(doom-aiern-modeline-def-modeline 'main
  '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
  '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))

(doom-aiern-modeline-def-modeline 'minimal
  '(bar matches buffer-info-simple)
  '(media-info major-mode))

(doom-aiern-modeline-def-modeline 'special
  '(bar window-number modals matches buffer-info buffer-position word-count parrot selection-info)
  '(objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process))

(doom-aiern-modeline-def-modeline 'project
  '(bar window-number buffer-default-directory)
  '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process))

(doom-aiern-modeline-def-modeline 'dashboard
  '(bar window-number buffer-default-directory-simple)
  '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process))

(doom-aiern-modeline-def-modeline 'vcs
  '(bar window-number modals matches buffer-info buffer-position parrot selection-info)
  '(misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process))

(doom-aiern-modeline-def-modeline 'package
  '(bar window-number package)
  '(misc-info major-mode process))

(doom-aiern-modeline-def-modeline 'info
  '(bar window-number buffer-info info-nodes buffer-position parrot selection-info)
  '(misc-info buffer-encoding major-mode))

(doom-aiern-modeline-def-modeline 'media
  '(bar window-number buffer-size buffer-info)
  '(misc-info media-info major-mode process vcs))

(doom-aiern-modeline-def-modeline 'message
  '(bar window-number modals matches buffer-info-simple buffer-position word-count parrot selection-info)
  '(objed-state misc-info battery debug minor-modes input-method indent-info buffer-encoding major-mode))

(doom-aiern-modeline-def-modeline 'pdf
  '(bar window-number matches buffer-info pdf-pages)
  '(misc-info major-mode process vcs))

(doom-aiern-modeline-def-modeline 'org-src
  '(bar window-number modals matches buffer-info-simple buffer-position word-count parrot selection-info)
  '(objed-state misc-info debug lsp minor-modes input-method indent-info buffer-encoding major-mode process checker))

(doom-aiern-modeline-def-modeline 'helm
  '(bar helm-buffer-id helm-number helm-follow helm-prefix-argument)
  '(helm-help))

(doom-aiern-modeline-def-modeline 'timemachine
  '(bar window-number matches git-timemachine buffer-position word-count parrot selection-info)
  '(misc-info minor-modes indent-info buffer-encoding major-mode))


;;
;; Interfaces
;;

;;;###autoload
(defun doom-aiern-modeline-init ()
  "Initialize doom mode-line."
  (doom-aiern-modeline-mode 1))
(make-obsolete 'doom-aiern-modeline-init 'doom-aiern-modeline-mode "1.6.0")

;;;###autoload
(defun doom-aiern-modeline-set-main-modeline (&optional default)
  "Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (doom-aiern-modeline-set-modeline 'main default))

;;;###autoload
(defun doom-aiern-modeline-set-minimal-modeline ()
  "Set minimal mode-line."
  (doom-aiern-modeline-set-modeline 'minimal))

;;;###autoload
(defun doom-aiern-modeline-set-special-modeline ()
  "Set special mode-line."
  (doom-aiern-modeline-set-modeline 'special))

;;;###autoload
(defun doom-aiern-modeline-set-project-modeline ()
  "Set project mode-line."
  (doom-aiern-modeline-set-modeline 'project))

;;;###autoload
(defun doom-aiern-modeline-set-dashboard-modeline ()
  "Set dashboard mode-line."
  (doom-aiern-modeline-set-modeline 'dashboard))

;;;###autoload
(defun doom-aiern-modeline-set-vcs-modeline ()
  "Set vcs mode-line."
  (doom-aiern-modeline-set-modeline 'vcs))

;;;###autoload
(defun doom-aiern-modeline-set-info-modeline ()
  "Set Info mode-line."
  (doom-aiern-modeline-set-modeline 'info))

;;;###autoload
(defun doom-aiern-modeline-set-package-modeline ()
  "Set package mode-line."
  (doom-aiern-modeline-set-modeline 'package))

;;;###autoload
(defun doom-aiern-modeline-set-media-modeline ()
  "Set media mode-line."
  (doom-aiern-modeline-set-modeline 'media))

;;;###autoload
(defun doom-aiern-modeline-set-message-modeline ()
  "Set message mode-line."
  (doom-aiern-modeline-set-modeline 'message))

;;;###autoload
(defun doom-aiern-modeline-set-pdf-modeline ()
  "Set pdf mode-line."
  (doom-aiern-modeline-set-modeline 'pdf))

;;;###autoload
(defun doom-aiern-modeline-set-org-src-modeline ()
  "Set org-src mode-line."
  (doom-aiern-modeline-set-modeline 'org-src))

;;;###autoload
(defun doom-aiern-modeline-set-helm-modeline (&rest _) ; To advice helm
  "Set helm mode-line."
  (doom-aiern-modeline-set-modeline 'helm))

;;;###autoload
(defun doom-aiern-modeline-set-timemachine-modeline ()
  "Set timemachine mode-line."
  (doom-aiern-modeline-set-modeline 'timemachine))


;;
;; Minor mode
;;

(defvar doom-aiern-modeline-mode-map (make-sparse-keymap))

;; Suppress warnings
(defvar 2C-mode-line-format)
(declare-function helm-display-mode-line 'helm)

;;;###autoload
(define-minor-mode doom-aiern-modeline-mode
  "Toggle doom-aiern-modeline on or off."
  :group 'doom-aiern-modeline
  :global t
  :lighter nil
  :keymap doom-aiern-modeline-mode-map
  (if doom-aiern-modeline-mode
      (progn
        (doom-aiern-modeline-refresh-bars)        ; Create bars
        (doom-aiern-modeline-set-main-modeline t) ; Set default mode-line

        ;; Apply to all existing buffers.
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (doom-aiern-modeline-set-main-modeline)))

        ;; For two-column editing
        (setq 2C-mode-line-format (doom-aiern-modeline 'special))

        ;; Add hooks
        (add-hook 'Info-mode-hook #'doom-aiern-modeline-set-info-modeline)
        (add-hook 'dired-mode-hook #'doom-aiern-modeline-set-project-modeline)
        (add-hook 'dashboard-mode-hook #'doom-aiern-modeline-set-dashboard-modeline)
        (add-hook 'image-mode-hook #'doom-aiern-modeline-set-media-modeline)
        (add-hook 'message-mode-hook #'doom-aiern-modeline-set-message-modeline)
        (add-hook 'git-commit-mode-hook #'doom-aiern-modeline-set-message-modeline)
        (add-hook 'magit-mode-hook #'doom-aiern-modeline-set-vcs-modeline)
        (add-hook 'circe-mode-hook #'doom-aiern-modeline-set-special-modeline)
        (add-hook 'erc-mode-hook #'doom-aiern-modeline-set-special-modeline)
        (add-hook 'rcirc-mode-hook #'doom-aiern-modeline-set-special-modeline)
        (add-hook 'pdf-view-mode-hook #'doom-aiern-modeline-set-pdf-modeline)
        (add-hook 'org-src-mode-hook #'doom-aiern-modeline-set-org-src-modeline)
        (add-hook 'git-timemachine-mode-hook #'doom-aiern-modeline-set-timemachine-modeline)
        (add-hook 'paradox-menu-mode-hook #'doom-aiern-modeline-set-package-modeline)
        (add-hook 'xwidget-webkit-mode-hook #'doom-aiern-modeline-set-minimal-modeline)

        ;; Add advices
        (advice-add #'helm-display-mode-line :after #'doom-aiern-modeline-set-helm-modeline))
    (progn
      ;; Restore mode-line
      (let ((original-format (doom-aiern-modeline--original-value 'mode-line-format)))
        (setq-default mode-line-format original-format)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq mode-line-format original-format))))

      ;; For two-column editing
      (setq 2C-mode-line-format (doom-aiern-modeline--original-value '2C-mode-line-format))

      ;; Remove hooks
      (remove-hook 'Info-mode-hook #'doom-aiern-modeline-set-info-modeline)
      (remove-hook 'dired-mode-hook #'doom-aiern-modeline-set-project-modeline)
      (remove-hook 'dashboard-mode-hook #'doom-aiern-modeline-set-dashboard-modeline)
      (remove-hook 'image-mode-hook #'doom-aiern-modeline-set-media-modeline)
      (remove-hook 'message-mode-hook #'doom-aiern-modeline-set-message-modeline)
      (remove-hook 'git-commit-mode-hook #'doom-aiern-modeline-set-message-modeline)
      (remove-hook 'magit-mode-hook #'doom-aiern-modeline-set-vcs-modeline)
      (remove-hook 'circe-mode-hook #'doom-aiern-modeline-set-special-modeline)
      (remove-hook 'erc-mode-hook #'doom-aiern-modeline-set-special-modeline)
      (remove-hook 'rcirc-mode-hook #'doom-aiern-modeline-set-special-modeline)
      (remove-hook 'pdf-view-mode-hook #'doom-aiern-modeline-set-pdf-modeline)
      (remove-hook 'org-src-mode-hook #'doom-aiern-modeline-set-org-src-modeline)
      (remove-hook 'git-timemachine-mode-hook #'doom-aiern-modeline-set-timemachine-modeline)
      (remove-hook 'paradox-menu-mode-hook #'doom-aiern-modeline-set-package-modeline)
      (remove-hook 'xwidget-webkit-mode-hook #'doom-aiern-modeline-set-minimal-modeline)

      ;; Remove advices
      (advice-remove #'helm-display-mode-line #'doom-aiern-modeline-set-helm-modeline))))

(provide 'doom-aiern-modeline)

;;; doom-aiern-modeline.el ends here
