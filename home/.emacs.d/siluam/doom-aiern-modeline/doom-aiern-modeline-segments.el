;;; doom-aiern-modeline-segments.el --- The segments for doom-aiern-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Vincent Zhang

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
;; The segments for doom-aiern-modeline.
;; Use `doom-aiern-modeline-def-segment' to create a new segment.
;;

;;; Code:

(require 'all-the-icons)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'doom-aiern-modeline-core)
(require 'doom-aiern-modeline-env)


;;
;; Externals
;;

(defvar Info-current-file)
(defvar Info-current-node)
(defvar Info-mode-line-node-keymap)
(defvar anzu--cached-count)
(defvar anzu--current-position)
(defvar anzu--overflow-p)
(defvar anzu--state)
(defvar anzu--total-matched)
(defvar anzu-cons-mode-line-p)
(defvar aw-keys)
(defvar battery-echo-area-format)
(defvar battery-load-critical)
(defvar battery-mode-line-format)
(defvar battery-mode-line-limit)
(defvar battery-status-function)
(defvar boon-command-state)
(defvar boon-insert-state)
(defvar boon-off-state)
(defvar boon-special-state)
(defvar edebug-execution-mode)
(defvar eglot--managed-mode)
(defvar erc-modified-channels-alist)
(defvar evil-ex-active-highlights-alist)
(defvar evil-ex-argument)
(defvar evil-ex-range)
(defvar evil-mc-frozen)
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-visual-selection)
(defvar aiern-ex-active-highlights-alist)
(defvar aiern-ex-argument)
(defvar aiern-ex-range)
(defvar aiern-mc-frozen)
(defvar aiern-state)
(defvar aiern-visual-beginning)
(defvar aiern-visual-end)
(defvar aiern-visual-selection)
(defvar flycheck-current-errors)
(defvar flycheck-mode-menu-map)
(defvar flymake--backend-state)
(defvar flymake--mode-line-format)
(defvar flymake-menu)
(defvar gnus-newsrc-alist)
(defvar gnus-newsrc-hashtb)
(defvar grip--process)
(defvar helm--mode-line-display-prefarg)
(defvar iedit-occurrences-overlays)
(defvar meow--indicator)
(defvar minions-direct)
(defvar minions-mode-line-minor-modes-map)
(defvar mlscroll-minimum-current-width)
(defvar mlscroll-right-align)
(defvar mu4e-alert-mode-line)
(defvar mu4e-alert-modeline-formatter)
(defvar nyan-minimum-window-width)
(defvar objed--obj-state)
(defvar objed--object)
(defvar objed-modeline-setup-func)
(defvar persp-nil-name)
(defvar phi-replace--mode-line-format)
(defvar phi-search--selection)
(defvar phi-search-mode-line-format)
(defvar poke-line-minimum-window-width)
(defvar rcirc-activity)
(defvar symbol-overlay-keywords-alist)
(defvar symbol-overlay-temp-symbol)
(defvar text-scale-mode-amount)
(defvar tracking-buffers)
(defvar winum-auto-setup-mode-line)
(defvar xah-fly-insert-state-q)

(declare-function anzu--reset-status 'anzu)
(declare-function anzu--where-is-here 'anzu)
(declare-function async-inject-variables 'async)
(declare-function async-start 'async)
(declare-function avy-traverse 'avy)
(declare-function avy-tree 'avy)
(declare-function aw-update 'ace-window)
(declare-function aw-window-list 'ace-window)
(declare-function battery-format 'battery)
(declare-function battery-update 'battery)
(declare-function boon-modeline-string 'boon)
(declare-function boon-state-string 'boon)
(declare-function cider--connection-info 'cider)
(declare-function cider-connected-p 'cider)
(declare-function cider-current-repl 'cider)
(declare-function cider-jack-in 'cider)
(declare-function cider-quit 'cider)
(declare-function dap--cur-session 'dap-mode)
(declare-function dap--debug-session-name 'dap-mode)
(declare-function dap--debug-session-state 'dap-mode)
(declare-function dap--session-running 'dap-mode)
(declare-function dap-debug-recent 'dap-mode)
(declare-function dap-disconnect 'dap-mode)
(declare-function dap-hydra 'dap-hydra)
(declare-function dap-deino 'dap-deino)
(declare-function edebug-help 'edebug)
(declare-function edebug-next-mode 'edebug)
(declare-function edebug-stop 'edebug)
(declare-function eglot 'eglot)
(declare-function eglot--major-mode 'eglot)
(declare-function eglot--project-nickname 'eglot)
(declare-function eglot--spinner 'eglot)
(declare-function eglot-clear-status 'eglot)
(declare-function eglot-current-server 'eglot)
(declare-function eglot-events-buffer 'eglot)
(declare-function eglot-forget-pending-continuations 'eglot)
(declare-function eglot-reconnect 'eglot)
(declare-function eglot-shutdown 'eglot)
(declare-function eglot-stderr-buffer 'eglot)
(declare-function erc-switch-to-buffer 'erc)
(declare-function erc-track-switch-buffer 'erc-track)
(declare-function evil-delimited-arguments 'evil-common)
(declare-function evil-emacs-state-p 'evil-states)
(declare-function evil-force-normal-state 'evil-commands)
(declare-function evil-insert-state-p 'evil-states)
(declare-function evil-motion-state-p 'evil-states)
(declare-function evil-normal-state-p 'evil-states)
(declare-function evil-god-state-p 'evil-states)
(declare-function evil-operator-state-p 'evil-states)
(declare-function evil-replace-state-p 'evil-states)
(declare-function evil-state-property 'evil-common)
(declare-function evil-visual-state-p 'evil-states)
(declare-function aiern-delimited-arguments 'aiern-common)
(declare-function aiern-emacs-state-p 'aiern-states)
(declare-function aiern-force-normal-state 'aiern-commands)
(declare-function aiern-insert-state-p 'aiern-states)
(declare-function aiern-motion-state-p 'aiern-states)
(declare-function aiern-normal-state-p 'aiern-states)
(declare-function aiern-god-state-p 'aiern-states)
(declare-function aiern-operator-state-p 'aiern-states)
(declare-function aiern-replace-state-p 'aiern-states)
(declare-function aiern-state-property 'aiern-common)
(declare-function aiern-visual-state-p 'aiern-states)
(declare-function eyebrowse--get 'eyebrowse)
(declare-function face-remap-remove-relative 'face-remap)
(declare-function fancy-narrow-active-p 'fancy-narrow)
(declare-function flycheck-buffer 'flycheck)
(declare-function flycheck-count-errors 'flycheck)
(declare-function flycheck-error-level-compilation-level 'flycheck)
(declare-function flycheck-list-errors 'flycheck)
(declare-function flycheck-next-error 'flycheck)
(declare-function flycheck-previous-error 'flycheck)
(declare-function flymake--backend-state-diags 'flymake)
(declare-function flymake--diag-type 'flymake)
(declare-function flymake--handle-report 'flymake)
(declare-function flymake--lookup-type-property 'flymake)
(declare-function flymake-disabled-backends 'flymake)
(declare-function flymake-goto-next-error 'flymake)
(declare-function flymake-goto-prev-error 'flymake)
(declare-function flymake-reporting-backends 'flymake)
(declare-function flymake-running-backends 'flymake)
(declare-function flymake-show-diagnostics-buffer 'flymake)
(declare-function flymake-start 'flymake)
(declare-function gnus-demon-add-handler 'gnus-demon)
(declare-function grip--preview-url 'grip-mode)
(declare-function grip-browse-preview 'grip-mode)
(declare-function grip-restart-preview 'grip-mode)
(declare-function grip-stop-preview 'grip-mode)
(declare-function helm-candidate-number-at-point 'helm)
(declare-function helm-get-candidate-number 'helm)
(declare-function iedit-find-current-occurrence-overlay 'iedit-lib)
(declare-function iedit-prev-occurrence 'iedit-lib)
(declare-function image-get-display-property 'image-mode)
(declare-function jsonrpc--request-continuations 'jsonrpc)
(declare-function jsonrpc-last-error 'jsonrpc)
(declare-function lsp--workspace-print 'lsp-mode)
(declare-function lsp-describe-session 'lsp-mode)
(declare-function lsp-workspace-folders-open 'lsp-mode)
(declare-function lsp-workspace-restart 'lsp-mode)
(declare-function lsp-workspace-shutdown 'lsp-mode)
(declare-function lsp-workspaces 'lsp-mode)
(declare-function lv-message 'lv)
(declare-function mc/num-cursors 'multiple-cursors-core)
(declare-function mlscroll-mode-line 'mlscroll)
(declare-function mu4e-alert-default-mode-line-formatter 'mu4e-alert)
(declare-function mu4e-alert-enable-mode-line-display 'mu4e-alert)
(declare-function nyan-create 'nyan-mode)
(declare-function org-edit-src-save 'org-src)
(declare-function parrot-create 'parrot)
(declare-function pdf-cache-number-of-pages 'pdf-cache)
(declare-function persp-add-buffer 'persp-mode)
(declare-function persp-contain-buffer-p 'persp-mode)
(declare-function persp-switch 'persp-mode)
(declare-function phi-search--initialize 'phi-search)
(declare-function poke-line-create 'poke-line)
(declare-function popup-create 'popup)
(declare-function popup-delete 'popup)
(declare-function rcirc-next-active-buffer 'rcirc)
(declare-function rcirc-short-buffer-name 'rcirc)
(declare-function rcirc-switch-to-server-buffer 'rcirc)
(declare-function rcirc-window-configuration-change 'rcirc)
(declare-function rime--should-enable-p 'rime)
(declare-function rime--should-inline-ascii-p 'rime)
(declare-function symbol-overlay-assoc 'symbol-overlay)
(declare-function symbol-overlay-get-list 'symbol-overlay)
(declare-function symbol-overlay-get-symbol 'symbol-overlay)
(declare-function symbol-overlay-rename 'symbol-overlay)
(declare-function tab-bar--current-tab 'tab-bar)
(declare-function tab-bar--current-tab-index 'tab-bar)
(declare-function tracking-next-buffer 'tracking)
(declare-function tracking-previous-buffer 'tracking)
(declare-function tracking-shorten 'tracking)
(declare-function undo-tree-redo-1 'undo-tree)
(declare-function undo-tree-undo-1 'undo-tree)
(declare-function warning-numeric-level 'warnings)
(declare-function window-numbering-clear-mode-line 'window-numbering)
(declare-function window-numbering-get-number-string 'window-numbering)
(declare-function window-numbering-install-mode-line 'window-numbering)
(declare-function winum--clear-mode-line 'winum)
(declare-function winum--install-mode-line 'winum)
(declare-function winum-get-number-string 'winum)



;;
;; Buffer information
;;

(defvar-local doom-aiern-modeline--buffer-file-icon nil)
(defun doom-aiern-modeline-update-buffer-file-icon (&rest _)
  "Update file icon in mode-line."
  (setq doom-aiern-modeline--buffer-file-icon
        (when (and doom-aiern-modeline-icon doom-aiern-modeline-major-mode-icon)
          (let ((icon (all-the-icons-icon-for-buffer)))
            (propertize (if (or (null icon) (symbolp icon))
                            (doom-aiern-modeline-icon 'faicon "file-o" nil nil
                                                :face 'all-the-icons-dsilver
                                                :height 0.9
                                                :v-adjust 0.0)
                          icon)
                        'help-echo (format "Major-mode: %s" (format-mode-line mode-name))
                        'display '(raise -0.135))))))
(add-hook 'find-file-hook #'doom-aiern-modeline-update-buffer-file-icon)
(add-hook 'after-change-major-mode-hook #'doom-aiern-modeline-update-buffer-file-icon)
(add-hook 'clone-indirect-buffer-hook #'doom-aiern-modeline-update-buffer-file-icon)

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-aiern-modeline-update-buffer-file-icon))))))

(defun doom-aiern-modeline-buffer-file-state-icon (icon unicode text face)
  "Displays an ICON of buffer state with FACE.
UNICODE and TEXT are the alternatives if it is not applicable.
Uses `all-the-icons-material' to fetch the icon."
  (doom-aiern-modeline-icon 'material icon unicode text
                      :face face
                      :height  1.1
                      :v-adjust -0.225))

(defvar-local doom-aiern-modeline--buffer-file-state-icon nil)
(defun doom-aiern-modeline-update-buffer-file-state-icon (&rest _)
  "Update the buffer or file state in mode-line."
  (setq doom-aiern-modeline--buffer-file-state-icon
        (when doom-aiern-modeline-buffer-state-icon
          (ignore-errors
            (concat
             (cond (buffer-read-only
                    (doom-aiern-modeline-buffer-file-state-icon
                     "lock" "🔒" "%1*" `(:inherit doom-aiern-modeline-warning
                                         :weight ,(if doom-aiern-modeline-icon
                                                      'normal
                                                    'bold))))
                   ((and buffer-file-name (buffer-modified-p)
                         doom-aiern-modeline-buffer-modification-icon)
                    (doom-aiern-modeline-buffer-file-state-icon
                     "save" "💾" "%1*" `(:inherit doom-aiern-modeline-buffer-modified
                                         :weight ,(if doom-aiern-modeline-icon
                                                      'normal
                                                    'bold))))
                   ((and buffer-file-name
                         (not (file-remote-p buffer-file-name)) ; Avoid freezing while connection is lost
                         (not (file-exists-p buffer-file-name)))
                    (doom-aiern-modeline-buffer-file-state-icon
                     "do_not_disturb_alt" "🚫" "!" 'doom-aiern-modeline-urgent))
                   (t ""))
             (when (or (buffer-narrowed-p)
                       (and (bound-and-true-p fancy-narrow-mode)
                            (fancy-narrow-active-p))
                       (bound-and-true-p dired-narrow-mode))
               (doom-aiern-modeline-buffer-file-state-icon
                "vertical_align_center" "↕" "><" 'doom-aiern-modeline-warning)))))))

(defvar-local doom-aiern-modeline--buffer-file-name nil)
(defun doom-aiern-modeline-update-buffer-file-name (&rest _)
  "Update buffer file name in mode-line."
  (setq doom-aiern-modeline--buffer-file-name
        (ignore-errors
          (save-match-data
            (if buffer-file-name
                (doom-aiern-modeline-buffer-file-name)
              (propertize "%b"
                          'face 'doom-aiern-modeline-buffer-file
                          'mouse-face 'mode-line-highlight
                          'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                          'local-map mode-line-buffer-identification-keymap))))))
(add-hook 'find-file-hook #'doom-aiern-modeline-update-buffer-file-name)
(add-hook 'after-save-hook #'doom-aiern-modeline-update-buffer-file-name)
(add-hook 'clone-indirect-buffer-hook #'doom-aiern-modeline-update-buffer-file-name)
(add-hook 'evil-insert-state-exit-hook #'doom-aiern-modeline-update-buffer-file-name)
(add-hook 'aiern-insert-state-exit-hook #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'not-modified :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'rename-buffer :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'set-visited-file-name :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'pop-to-buffer :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'undo :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'undo-tree-undo-1 :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'undo-tree-redo-1 :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'fill-paragraph :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'popup-create :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'popup-delete :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'org-edit-src-save :after #'doom-aiern-modeline-update-buffer-file-name)
(advice-add #'symbol-overlay-rename :after #'doom-aiern-modeline-update-buffer-file-name)

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-buffer-file-name-style
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-buffer-file-name-style val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when buffer-file-name
           (doom-aiern-modeline-update-buffer-file-name)))))))

(defsubst doom-aiern-modeline--buffer-mode-icon ()
  "The icon of the current major mode."
  (when (and doom-aiern-modeline-icon doom-aiern-modeline-major-mode-icon)
    (when-let ((icon (or doom-aiern-modeline--buffer-file-icon
                         (doom-aiern-modeline-update-buffer-file-icon))))
      (concat
       (let ((active (doom-aiern-modeline--active)))
         (if (and active doom-aiern-modeline-major-mode-color-icon)
             icon
           (doom-aiern-modeline-propertize-icon icon (if active
                                                   'mode-line
                                                 'mode-line-inactive))))
       (doom-aiern-modeline-vspc)))))

(defsubst doom-aiern-modeline--buffer-state-icon ()
  "The icon of the current buffer state."
  (when doom-aiern-modeline-buffer-state-icon
    (when-let ((icon (doom-aiern-modeline-update-buffer-file-state-icon)))
      (concat
       (if (doom-aiern-modeline--active)
           icon
         (doom-aiern-modeline-propertize-icon icon 'mode-line-inactive))
       (doom-aiern-modeline-vspc)))))

(defsubst doom-aiern-modeline--buffer-name ()
  "The current buffer name."
  ;; Only display the buffer name if the window is small, but doesn't need to
  ;; respect file-name style.
  (if (and (not (eq doom-aiern-modeline-buffer-file-name-style 'file-name))
           doom-aiern-modeline--limited-width-p)
      (propertize "%b"
                  'face (cond ((and buffer-file-name (buffer-modified-p))
                               'doom-aiern-modeline-buffer-modified)
                              ((doom-aiern-modeline--active) 'doom-aiern-modeline-buffer-file)
                              (t 'mode-line-inactive))
                  'mouse-face 'mode-line-highlight
                  'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                  'local-map mode-line-buffer-identification-keymap)
    (when-let ((name (or doom-aiern-modeline--buffer-file-name
                         (doom-aiern-modeline-update-buffer-file-name))))
      (if (doom-aiern-modeline--active)
          ;; Check if the buffer is modified
          (if (and buffer-file-name (buffer-modified-p))
              (propertize name 'face 'doom-aiern-modeline-buffer-modified)
            name)
        (propertize name 'face 'mode-line-inactive)))))

(doom-aiern-modeline-def-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat
   (doom-aiern-modeline-spc)
   (doom-aiern-modeline--buffer-mode-icon)
   (doom-aiern-modeline--buffer-state-icon)
   (doom-aiern-modeline--buffer-name)))

(doom-aiern-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (concat
   (doom-aiern-modeline-spc)
   (doom-aiern-modeline--buffer-mode-icon)
   (doom-aiern-modeline--buffer-state-icon)
   (propertize "%b"
               'face (cond ((and buffer-file-name (buffer-modified-p))
                            'doom-aiern-modeline-buffer-modified)
                           ((doom-aiern-modeline--active) 'doom-aiern-modeline-buffer-file)
                           (t 'mode-line-inactive))
               'mouse-face 'mode-line-highlight
               'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
               'local-map mode-line-buffer-identification-keymap)))

(doom-aiern-modeline-def-segment buffer-default-directory
  "Displays `default-directory' with the icon and state . This is for special buffers
like the scratch buffer where knowing the current project directory is important."
  (let ((face (cond ((buffer-modified-p)
                     'doom-aiern-modeline-buffer-modified)
                    ((doom-aiern-modeline--active) 'doom-aiern-modeline-buffer-path)
                    (t 'mode-line-inactive))))
    (concat (doom-aiern-modeline-spc)
            (doom-aiern-modeline--buffer-state-icon)
            (and doom-aiern-modeline-major-mode-icon
                 (concat (doom-aiern-modeline-icon
                          'octicon "file-directory" "🖿" ""
                          :face face :v-adjust -0.05 :height 1.25)
                         (doom-aiern-modeline-vspc)))
            (propertize (abbreviate-file-name default-directory) 'face face))))

(doom-aiern-modeline-def-segment buffer-default-directory-simple
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (doom-aiern-modeline--active) 'doom-aiern-modeline-buffer-path 'mode-line-inactive)))
    (concat (doom-aiern-modeline-spc)
            (and doom-aiern-modeline-major-mode-icon
                 (concat (doom-aiern-modeline-icon
                          'octicon "file-directory" "🖿" ""
                          :face face :v-adjust -0.05 :height 1.25)
                         (doom-aiern-modeline-vspc)))
            (propertize (abbreviate-file-name default-directory) 'face face))))


;;
;; Encoding
;;

(doom-aiern-modeline-def-segment buffer-encoding
  "Displays the eol and the encoding style of the buffer the same way Atom does."
  (when doom-aiern-modeline-buffer-encoding
    (let ((face (if (doom-aiern-modeline--active) 'mode-line 'mode-line-inactive))
          (mouse-face 'mode-line-highlight))
      (concat
       (doom-aiern-modeline-spc)

       ;; eol type
       (let ((eol (coding-system-eol-type buffer-file-coding-system)))
         (when (or (eq doom-aiern-modeline-buffer-encoding t)
                   (and (eq doom-aiern-modeline-buffer-encoding 'nondefault)
                        (not (equal eol doom-aiern-modeline-default-eol-type))))
           (propertize
            (pcase eol
              (0 "LF ")
              (1 "CRLF ")
              (2 "CR ")
              (_ ""))
            'face face
            'mouse-face mouse-face
            'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
                               (pcase eol
                                 (0 "Unix-style LF")
                                 (1 "DOS-style CRLF")
                                 (2 "Mac-style CR")
                                 (_ "Undecided")))
            'local-map (let ((map (make-sparse-keymap)))
                         (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                         map))))

       ;; coding system
       (let* ((sys (coding-system-plist buffer-file-coding-system))
              (cat (plist-get sys :category))
              (sym (if (memq cat
                             '(coding-category-undecided coding-category-utf-8))
                       'utf-8
                     (plist-get sys :name))))
         (when (or (eq doom-aiern-modeline-buffer-encoding t)
                   (and (eq doom-aiern-modeline-buffer-encoding 'nondefault)
                        (not (eq cat 'coding-category-undecided))
                        (not (eq sym doom-aiern-modeline-default-coding-system))))
           (propertize
            (upcase (symbol-name sym))
            'face face
            'mouse-face mouse-face
            'help-echo 'mode-line-mule-info-help-echo
            'local-map mode-line-coding-system-map)))

       (doom-aiern-modeline-spc)))))


;;
;; Indentation
;;

(doom-aiern-modeline-def-segment indent-info
  "Displays the indentation information."
  (when doom-aiern-modeline-indent-info
    (let ((do-propertize
           (lambda (mode size)
             (propertize
              (format " %s %d " mode size)
              'face (if (doom-aiern-modeline--active) 'mode-line 'mode-line-inactive)))))
      (if indent-tabs-mode
          (funcall do-propertize "TAB" tab-width)
        (let ((lookup-var
               (seq-find (lambda (var)
                           (and var (boundp var) (symbol-value var)))
                         (cdr (assoc major-mode doom-aiern-modeline-indent-alist)) nil)))
          (funcall do-propertize "SPC"
                   (if lookup-var
                       (symbol-value lookup-var)
                     tab-width)))))))

;;
;; Remote host
;;

(doom-aiern-modeline-def-segment remote-host
  "Hostname for remote buffers."
  (when default-directory
    (when-let ((host (file-remote-p default-directory 'host)))
      (propertize
       (concat "@" host)
       'face (if (doom-aiern-modeline--active) 'doom-aiern-modeline-host 'mode-line-inactive)))))


;;
;; Major mode
;;

(doom-aiern-modeline-def-segment major-mode
  "The major mode, including environment and text-scale info."
  (propertize
   (concat
    (doom-aiern-modeline-spc)
    (propertize (format-mode-line
                 (or (and (boundp 'delighted-modes)
                          (cadr (assq major-mode delighted-modes)))
                     mode-name))
                'help-echo "Major mode\n\
  mouse-1: Display major mode menu\n\
  mouse-2: Show help for major mode\n\
  mouse-3: Toggle minor modes"
                'mouse-face 'mode-line-highlight
                'local-map mode-line-major-mode-keymap)
    (when (and doom-aiern-modeline-env-version doom-aiern-modeline-env--version)
      (format " %s" doom-aiern-modeline-env--version))
    (and (boundp 'text-scale-mode-amount)
         (/= text-scale-mode-amount 0)
         (format
          (if (> text-scale-mode-amount 0)
              " (%+d)"
            " (%-d)")
          text-scale-mode-amount))
    (doom-aiern-modeline-spc))
   'face (if (doom-aiern-modeline--active)
             'doom-aiern-modeline-buffer-major-mode
           'mode-line-inactive)))


;;
;; Process
;;

(doom-aiern-modeline-def-segment process
  "The process info."
  (if (doom-aiern-modeline--active)
      mode-line-process
    (propertize (format-mode-line mode-line-process)
                'face 'mode-line-inactive)))


;;
;; Minor modes
;;

(doom-aiern-modeline-def-segment minor-modes
  (when doom-aiern-modeline-minor-modes
    (let ((face (if (doom-aiern-modeline--active)
                    'doom-aiern-modeline-buffer-minor-mode
                  'mode-line-inactive))
          (mouse-face 'mode-line-highlight)
          (help-echo "Minor mode
  mouse-1: Display minor mode menu
  mouse-2: Show help for minor mode
  mouse-3: Toggle minor modes"))
      (if (bound-and-true-p minions-mode)
          `((:propertize ("" ,(--filter (memq (car it) minions-direct)
                                        minor-mode-alist))
             face ,face
		     mouse-face ,mouse-face
		     help-echo ,help-echo
		     local-map ,mode-line-minor-mode-keymap)
            ,(doom-aiern-modeline-spc)
            (:propertize ("" ,(doom-aiern-modeline-icon 'octicon "gear" "⚙" ";-"
  :face face :v-adjust -0.05))
             mouse-face ,mouse-face
             help-echo "Minions
mouse-1: Display minor modes menu"
             local-map ,minions-mode-line-minor-modes-map)
            ,(doom-aiern-modeline-spc))
        `((:propertize ("" minor-mode-alist)
           face ,face
           mouse-face ,mouse-face
           help-echo ,help-echo
           local-map ,mode-line-minor-mode-keymap)
          ,(doom-aiern-modeline-spc))))))


;;
;; VCS
;;

(defun doom-aiern-modeline-vcs-icon (icon &optional unicode text face voffset)
  "Displays the vcs ICON with FACE and VOFFSET.

UNICODE and TEXT are fallbacks.
Uses `all-the-icons-octicon' to fetch the icon."
  (doom-aiern-modeline-icon 'octicon icon unicode text
                      :face face :v-adjust (or voffset -0.1)))

(defvar-local doom-aiern-modeline--vcs-icon nil)
(defun doom-aiern-modeline-update-vcs-icon (&rest _)
  "Update icon of vcs state in mode-line."
  (setq doom-aiern-modeline--vcs-icon
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (cond ((memq state '(edited added))
                   (doom-aiern-modeline-vcs-icon "git-compare" "⇆" "*" 'doom-aiern-modeline-info -0.05))
                  ((eq state 'needs-merge)
                   (doom-aiern-modeline-vcs-icon "git-merge" "⛙" "?" 'doom-aiern-modeline-info))
                  ((eq state 'needs-update)
                   (doom-aiern-modeline-vcs-icon "arrow-down" "↓" "!" 'doom-aiern-modeline-warning))
                  ((memq state '(removed conflict unregistered))
                   (doom-aiern-modeline-vcs-icon "alert" "⚠" "!" 'doom-aiern-modeline-urgent))
                  (t
                   (doom-aiern-modeline-vcs-icon "git-branch" "" "@" 'doom-aiern-modeline-info -0.05)))))))
(add-hook 'find-file-hook #'doom-aiern-modeline-update-vcs-icon)
(add-hook 'after-save-hook #'doom-aiern-modeline-update-vcs-icon)
(advice-add #'vc-refresh-state :after #'doom-aiern-modeline-update-vcs-icon)

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-aiern-modeline-update-vcs-icon))))))

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-aiern-modeline-update-vcs-icon))))))

(defvar-local doom-aiern-modeline--vcs-text nil)
(defun doom-aiern-modeline-update-vcs-text (&rest _)
  "Update text of vcs state in mode-line."
  (setq doom-aiern-modeline--vcs-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state buffer-file-name backend))
                 (str (if vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        "")))
            (propertize (if (> (length str) doom-aiern-modeline-vcs-max-length)
                            (concat
                             (substring str 0 (- doom-aiern-modeline-vcs-max-length 3))
                             "...")
                          str)
                        'mouse-face 'mode-line-highlight
                        'face (cond ((eq state 'needs-update)
                                     'doom-aiern-modeline-warning)
                                    ((memq state '(removed conflict unregistered))
                                     'doom-aiern-modeline-urgent)
                                    (t 'doom-aiern-modeline-info)))))))
(add-hook 'find-file-hook #'doom-aiern-modeline-update-vcs-text)
(add-hook 'after-save-hook #'doom-aiern-modeline-update-vcs-text)
(advice-add #'vc-refresh-state :after #'doom-aiern-modeline-update-vcs-text)

(doom-aiern-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (let ((active (doom-aiern-modeline--active)))
    (when-let ((icon doom-aiern-modeline--vcs-icon)
               (text doom-aiern-modeline--vcs-text))
      (concat
       (doom-aiern-modeline-spc)
       (propertize
        (concat
         (if active
             icon
           (doom-aiern-modeline-propertize-icon icon 'mode-line-inactive))
         (doom-aiern-modeline-vspc))
        'mouse-face 'mode-line-highlight
        'help-echo (get-text-property 1 'help-echo vc-mode)
        'local-map (get-text-property 1 'local-map vc-mode))
       (if active
           text
         (propertize text 'face 'mode-line-inactive))
       (doom-aiern-modeline-spc)))))


;;
;; Checker
;;

(defun doom-aiern-modeline-checker-icon (icon unicode text face)
  "Displays the checker ICON with FACE.

UNICODE and TEXT are fallbacks.
Uses `all-the-icons-material' to fetch the icon."
  (doom-aiern-modeline-icon 'material icon unicode text
                      :face face :height 1.1 :v-adjust -0.225))

(defun doom-aiern-modeline-checker-text (text &optional face)
  "Displays TEXT with FACE."
  (propertize text 'face (or face 'mode-line)))

;; Flycheck

(defun doom-aiern-modeline--flycheck-count-errors ()
  "Count the number of ERRORS, grouped by level.

Return an alist, where each ITEM is a cons cell whose `car' is an
error level, and whose `cdr' is the number of errors of that
level."
  (let ((info 0) (warning 0) (error 0))
    (mapc
     (lambda (item)
       (let ((count (cdr item)))
         (pcase (flycheck-error-level-compilation-level (car item))
           (0 (cl-incf info count))
           (1 (cl-incf warning count))
           (2 (cl-incf error count)))))
     (flycheck-count-errors flycheck-current-errors))
    `((info . ,info) (warning . ,warning) (error . ,error))))

(defvar-local doom-aiern-modeline--flycheck-icon nil)
(defun doom-aiern-modeline-update-flycheck-icon (&optional status)
  "Update flycheck icon via STATUS."
  (setq doom-aiern-modeline--flycheck-icon
        (when-let
            ((icon
              (pcase status
                ('finished  (if flycheck-current-errors
                                (let-alist (doom-aiern-modeline--flycheck-count-errors)
                                  (doom-aiern-modeline-checker-icon
                                   "block" "🚫" "!"
                                   (cond ((> .error 0) 'doom-aiern-modeline-urgent)
                                         ((> .warning 0) 'doom-aiern-modeline-warning)
                                         (t 'doom-aiern-modeline-info))))
                              (doom-aiern-modeline-checker-icon "check" "✓" "-" 'doom-aiern-modeline-info)))
                ('running     (doom-aiern-modeline-checker-icon "access_time" "⏱" "*" 'doom-aiern-modeline-debug))
                ('no-checker  (doom-aiern-modeline-checker-icon "sim_card_alert" "⚠" "-" 'doom-aiern-modeline-debug))
                ('errored     (doom-aiern-modeline-checker-icon "sim_card_alert" "⚠" "-" 'doom-aiern-modeline-urgent))
                ('interrupted (doom-aiern-modeline-checker-icon "pause" "⏸" "=" 'doom-aiern-modeline-debug))
                ('suspicious  (doom-aiern-modeline-checker-icon "priority_high" "❗" "!" 'doom-aiern-modeline-urgent))
                (_ nil))))
          (propertize icon
                      'help-echo (concat "Flycheck\n"
                                         (pcase status
                                           ('finished "mouse-1: Display minor mode menu
mouse-2: Show help for minor mode")
                                           ('running "Running...")
                                           ('no-checker "No Checker")
                                           ('errored "Error")
                                           ('interrupted "Interrupted")
                                           ('suspicious "Suspicious")))
                      'mouse-face 'mode-line-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line down-mouse-1]
                                     flycheck-mode-menu-map)
                                   (define-key map [mode-line mouse-2]
                                     (lambda ()
                                       (interactive)
                                       (describe-function 'flycheck-mode)))
                                   map)))))
(add-hook 'flycheck-status-changed-functions #'doom-aiern-modeline-update-flycheck-icon)
(add-hook 'flycheck-mode-hook #'doom-aiern-modeline-update-flycheck-icon)

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flycheck-mode)
           (flycheck-buffer)))))))

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flycheck-mode)
           (flycheck-buffer)))))))

(defvar-local doom-aiern-modeline--flycheck-text nil)
(defun doom-aiern-modeline-update-flycheck-text (&optional status)
  "Update flycheck text via STATUS."
  (setq doom-aiern-modeline--flycheck-text
        (when-let
            ((text
              (pcase status
                ('finished  (when flycheck-current-errors
                              (let-alist (doom-aiern-modeline--flycheck-count-errors)
                                (if doom-aiern-modeline-checker-simple-format
                                    (doom-aiern-modeline-checker-text
                                     (number-to-string (+ .error .warning .info))
                                     (cond ((> .error 0) 'doom-aiern-modeline-urgent)
                                           ((> .warning 0) 'doom-aiern-modeline-warning)
                                           (t 'doom-aiern-modeline-info)))
                                  (format "%s/%s/%s"
                                          (doom-aiern-modeline-checker-text (number-to-string .error)
                                                                      'doom-aiern-modeline-urgent)
                                          (doom-aiern-modeline-checker-text (number-to-string .warning)
                                                                      'doom-aiern-modeline-warning)
                                          (doom-aiern-modeline-checker-text (number-to-string .info)
                                                                      'doom-aiern-modeline-info))))))
                ('running     nil)
                ('no-checker  nil)
                ('errored     (doom-aiern-modeline-checker-text "Error" 'doom-aiern-modeline-urgent))
                ('interrupted (doom-aiern-modeline-checker-text "Interrupted" 'doom-aiern-modeline-debug))
                ('suspicious  (doom-aiern-modeline-checker-text "Suspicious" 'doom-aiern-modeline-urgent))
                (_ nil))))
          (propertize
           text
           'help-echo (pcase status
                        ('finished
                         (concat
                          (when flycheck-current-errors
                            (let-alist (doom-aiern-modeline--flycheck-count-errors)
                              (format "error: %d, warning: %d, info: %d\n" .error .warning .info)))
                          "mouse-1: Show all errors
mouse-3: Next error"
                          (if (featurep 'mwheel)
                              "\nwheel-up/wheel-down: Previous/next error")))
                        ('running "Running...")
                        ('no-checker "No Checker")
                        ('errored "Error")
                        ('interrupted "Interrupted")
                        ('suspicious "Suspicious"))
           'mouse-face 'mode-line-highlight
           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line mouse-1]
                          #'flycheck-list-errors)
                        (define-key map [mode-line mouse-3]
                          #'flycheck-next-error)
                        (when (featurep 'mwheel)
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-down-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-previous-error 1))))
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-up-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-next-error 1))))
                          map))))))
(add-hook 'flycheck-status-changed-functions #'doom-aiern-modeline-update-flycheck-text)
(add-hook 'flycheck-mode-hook #'doom-aiern-modeline-update-flycheck-text)

;; Flymake

(defvar-local doom-aiern-modeline--flymake-icon nil)
(defun doom-aiern-modeline-update-flymake-icon (&rest _)
  "Update flymake icon."
  (setq flymake--mode-line-format nil) ; remove the lighter of minor mode
  (setq doom-aiern-modeline--flymake-icon
        (let* ((known (hash-table-keys flymake--backend-state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported)))
          (when-let
              ((icon
                (cond
                 (some-waiting (doom-aiern-modeline-checker-icon "access_time" "⏰" "*" 'doom-aiern-modeline-debug))
                 ((null known) (doom-aiern-modeline-checker-icon "sim_card_alert" "❓" "?" 'doom-aiern-modeline-debug))
                 (all-disabled (doom-aiern-modeline-checker-icon "sim_card_alert" "❗" "!" 'doom-aiern-modeline-urgent))
                 (t (let ((.error 0)
                          (.warning 0)
                          (.note 0))
                      (progn
                        (cl-loop
                         with warning-level = (warning-numeric-level :warning)
                         with note-level = (warning-numeric-level :debug)
                         for state being the hash-values of flymake--backend-state
                         do (cl-loop
                             with diags = (flymake--backend-state-diags state)
                             for diag in diags do
                             (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                            (warning-numeric-level :error))))
                               (cond ((> severity warning-level) (cl-incf .error))
                                     ((> severity note-level)    (cl-incf .warning))
                                     (t                          (cl-incf .note))))))
                        (if (> (+ .error .warning .note) 0)
                            (doom-aiern-modeline-checker-icon "do_not_disturb_alt" "🚫" "!"
                                                        (cond ((> .error 0) 'doom-aiern-modeline-urgent)
                                                              ((> .warning 0) 'doom-aiern-modeline-warning)
                                                              (t 'doom-aiern-modeline-info)))
                          (doom-aiern-modeline-checker-icon "check" "✔" "-" 'doom-aiern-modeline-info))))))))
            (propertize
             icon
             'help-echo (concat "Flymake\n"
                                (cond
                                 (some-waiting "Running...")
                                 ((null known) "No Checker")
                                 (all-disabled "All Checkers Disabled")
                                 (t (format "%d/%d backends running
mouse-1: Display minor mode menu
mouse-2: Show help for minor mode"
                                            (length running) (length known)))))
             'mouse-face 'mode-line-highlight
             'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line down-mouse-1]
                            flymake-menu)
                          (define-key map [mode-line mouse-2]
                            (lambda ()
                              (interactive)
                              (describe-function 'flymake-mode)))
                          map))))))
(advice-add #'flymake--handle-report :after #'doom-aiern-modeline-update-flymake-icon)

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (flymake-start)))))))

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (flymake-start)))))))

(defvar-local doom-aiern-modeline--flymake-text nil)
(defun doom-aiern-modeline-update-flymake-text (&rest _)
  "Update flymake text."
  (setq flymake--mode-line-format nil) ; remove the lighter of minor mode
  (setq doom-aiern-modeline--flymake-text
        (let* ((known (hash-table-keys flymake--backend-state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported))
               (warning-level (warning-numeric-level :warning))
               (note-level (warning-numeric-level :debug))
               (.error 0)
               (.warning 0)
               (.note 0))
          (maphash (lambda (_b state)
                     (cl-loop
                      with diags = (flymake--backend-state-diags state)
                      for diag in diags do
                      (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                     (warning-numeric-level :error))))
                        (cond ((> severity warning-level) (cl-incf .error))
                              ((> severity note-level) (cl-incf .warning))
                              (t (cl-incf .note))))))
                   flymake--backend-state)
          (when-let
              ((text
                (cond
                 (some-waiting doom-aiern-modeline--flymake-text)
                 ((null known) nil)
                 (all-disabled nil)
                 (t (let ((num (+ .error .warning .note)))
                      (when (> num 0)
                        (if doom-aiern-modeline-checker-simple-format
                            (doom-aiern-modeline-checker-text (number-to-string num)
                                                        (cond ((> .error 0) 'doom-aiern-modeline-urgent)
                                                              ((> .warning 0) 'doom-aiern-modeline-warning)
                                                              (t 'doom-aiern-modeline-info)))
                          (format "%s/%s/%s"
                                  (doom-aiern-modeline-checker-text (number-to-string .error)
                                                              'doom-aiern-modeline-urgent)
                                  (doom-aiern-modeline-checker-text (number-to-string .warning)
                                                              'doom-aiern-modeline-warning)
                                  (doom-aiern-modeline-checker-text (number-to-string .note)
                                                              'doom-aiern-modeline-info)))))))))
            (propertize
             text
             'help-echo (cond
                         (some-waiting "Running...")
                         ((null known) "No Checker")
                         (all-disabled "All Checkers Disabled")
                         (t (format "error: %d, warning: %d, note: %d
mouse-1: List all problems%s"
                                    .error .warning .note
                                    (if (featurep 'mwheel)
                                        "\nwheel-up/wheel-down: Previous/next problem"))))
             'mouse-face 'mode-line-highlight
             'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line mouse-1]
                            #'flymake-show-diagnostics-buffer)
                          (when (featurep 'mwheel)
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-down-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-prev-error 1 nil t))))
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-up-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-next-error 1 nil t))))
                            map)))))))
(advice-add #'flymake--handle-report :after #'doom-aiern-modeline-update-flymake-text)

(doom-aiern-modeline-def-segment checker
  "Displays color-coded error status in the current buffer with pretty icons."
  (let ((active (doom-aiern-modeline--active))
        (seg (cond ((and (bound-and-true-p flymake-mode)
                         (bound-and-true-p flymake--backend-state)) ; only support 26+
                    `(,doom-aiern-modeline--flymake-icon . ,doom-aiern-modeline--flymake-text))
                   ((bound-and-true-p flycheck-mode)
                    `(,doom-aiern-modeline--flycheck-icon . ,doom-aiern-modeline--flycheck-text)))))
    (let ((icon (car seg))
          (text (cdr seg)))
      (concat
       (when icon
         (concat
          (doom-aiern-modeline-spc)
          (if active
              icon
            (doom-aiern-modeline-propertize-icon icon 'mode-line-inactive))))
       (when text
         (concat
          (if icon (doom-aiern-modeline-vspc) (doom-aiern-modeline-spc))
          (if active
              text
            (propertize text 'face 'mode-line-inactive))))
       (doom-aiern-modeline-spc)))))


;;
;; Word Count
;;

(doom-aiern-modeline-def-segment word-count
  "The buffer word count.
Displayed when in a major mode in `doom-aiern-modeline-continuous-word-count-modes'.
Respects `doom-aiern-modeline-enable-word-count'."
  (when (and doom-aiern-modeline-enable-word-count
             (member major-mode doom-aiern-modeline-continuous-word-count-modes))
    (propertize (format " %dW" (count-words (point-min) (point-max)))
                'face (if (doom-aiern-modeline--active)
                          'mode-line
                        'mode-line-inactive))))


;;
;; Selection
;;

(defsubst doom-aiern-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(doom-aiern-modeline-def-segment selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (or mark-active (or (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual))
                                (and (bound-and-true-p aiern-local-mode)
                                  (eq aiern-state 'visual))))
             (doom-aiern-modeline--active))
    (cl-destructuring-bind (beg . end)
      (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
          (cons evil-visual-beginning evil-visual-end)
        (cons (region-beginning) (region-end)))
      (if (and (bound-and-true-p aiern-local-mode) (eq aiern-state 'visual))
          (cons aiern-visual-beginning aiern-visual-end)
        (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (doom-aiern-modeline-spc)
                 (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (or (and (bound-and-true-p evil-visual-selection)
                                 (eq 'block evil-visual-selection))
                                (and (bound-and-true-p aiern-visual-selection)
                                 (eq 'block aiern-visual-selection))))
                        (let ((cols (abs (- (doom-aiern-modeline-column end)
                                            (doom-aiern-modeline-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((or (and (bound-and-true-p evil-visual-selection)
                             (eq evil-visual-selection 'line))
                            (and (bound-and-true-p aiern-visual-selection)
                             (eq aiern-visual-selection 'line)))
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       ((format "%dC" (- end beg))))
                 (when doom-aiern-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))
                 (doom-aiern-modeline-spc)))
       'face 'doom-aiern-modeline-highlight))))


;;
;; Matches (macro, anzu, evil-substitute, iedit, symbol-overlay and multi-cursors)
;;

(defsubst doom-aiern-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (doom-aiern-modeline--active)
             (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-aiern-modeline-panel ))
          (vsep (propertize " " 'face
                            '(:inherit (doom-aiern-modeline-panel variable-pitch)))))
      (concat
       sep
       (doom-aiern-modeline-icon 'material "fiber_manual_record" "●"
                           (cond ((bound-and-true-p evil-this-macro)
                               (char-to-string evil-this-macro))
                                ((bound-and-true-p aiern-this-macro)
                               (char-to-string aiern-this-macro))
                             (t "Macro"))
                           :face 'doom-aiern-modeline-panel
                           :v-adjust -0.225)
       vsep
       (doom-aiern-modeline-icon 'octicon "triangle-right" "▶" ">"
                           :face 'doom-aiern-modeline-panel
                           :v-adjust -0.05)
       sep))))

;; `anzu' and `evil-anzu' expose current/total state that can be displayed in the
;; mode-line.
(defun doom-aiern-modeline-fix-anzu-count (positions here)
  "Calulate anzu count via POSITIONS and HERE."
  (cl-loop for (start . end) in positions
           collect t into before
           when (and (>= here start) (<= here end))
           return (length before)
           finally return 0))

(advice-add #'anzu--where-is-here :override #'doom-aiern-modeline-fix-anzu-count)

(setq anzu-cons-mode-line-p nil) ; manage modeline segment ourselves
;; Ensure anzu state is cleared when searches & iedit are done
(with-eval-after-load 'anzu
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
  (advice-add #'evil-force-normal-state :after #'anzu--reset-status)
  (advice-add #'aiern-force-normal-state :after #'anzu--reset-status)
  ;; Fix matches segment mirroring across all buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched
          anzu--current-position anzu--state anzu--cached-count
          anzu--cached-positions anzu--last-command
          anzu--last-isearch-string anzu--overflow-p)))

(defsubst doom-aiern-modeline--anzu ()
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (when (and (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " anzu--cached-count))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (doom-aiern-modeline--active) 'doom-aiern-modeline-panel 'mode-line-inactive))))

(defsubst doom-aiern-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and (bound-and-true-p evil-local-mode)
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (doom-aiern-modeline--active) 'doom-aiern-modeline-panel 'mode-line-inactive))))

(defsubst doom-aiern-modeline--aiern-substitute ()
  "Show number of matches for aiern-ex substitutions and highlights in real time."
  (when (and (bound-and-true-p aiern-local-mode)
             (or (assq 'aiern-ex-substitute aiern-ex-active-highlights-alist)
                 (assq 'aiern-ex-global-match aiern-ex-active-highlights-alist)
                 (assq 'aiern-ex-buffer-match aiern-ex-active-highlights-alist)))
    (propertize
     (let ((range (if aiern-ex-range
                      (cons (car aiern-ex-range) (cadr aiern-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (aiern-delimited-arguments aiern-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (doom-aiern-modeline--active) 'doom-aiern-modeline-panel 'mode-line-inactive))))

(defun doom-aiern-modeline-themes--overlay-sort (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defsubst doom-aiern-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (save-excursion (iedit-prev-occurrence)
                                        (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'doom-aiern-modeline-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (doom-aiern-modeline--active) 'doom-aiern-modeline-panel 'mode-line-inactive))))

(defsubst doom-aiern-modeline--symbol-overlay ()
  "Show the number of matches for symbol overlay."
  (when-let ((active (doom-aiern-modeline--active)))
    (when (and (bound-and-true-p symbol-overlay-keywords-alist)
               (not (bound-and-true-p symbol-overlay-temp-symbol))
               (not (bound-and-true-p iedit-mode)))
      (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
             (symbol (car keyword))
             (before (symbol-overlay-get-list -1 symbol))
             (after (symbol-overlay-get-list 1 symbol))
             (count (length before)))
        (if (symbol-overlay-assoc symbol)
            (propertize
             (format (concat  " %d/%d " (and (cadr keyword) "in scope "))
                     (+ count 1)
                     (+ count (length after)))
             'face (if active 'doom-aiern-modeline-panel 'mode-line-inactive)))))))

(defsubst doom-aiern-modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (cl-destructuring-bind (count . face)
    (cond ((bound-and-true-p multiple-cursors-mode)
           (cons (mc/num-cursors)
                 (if (doom-aiern-modeline--active)
                     'doom-aiern-modeline-panel
                   'mode-line-inactive)))
          ((bound-and-true-p evil-mc-cursor-list)
           (cons (length evil-mc-cursor-list)
                 (cond ((not (doom-aiern-modeline--active)) 'mode-line-inactive)
                       (evil-mc-frozen 'doom-aiern-modeline-bar)
                       ('doom-aiern-modeline-panel))))
          ((bound-and-true-p aiern-mc-cursor-list)
           (cons (length aiern-mc-cursor-list)
                 (cond ((not (doom-aiern-modeline--active)) 'mode-line-inactive)
                       (aiern-mc-frozen 'doom-aiern-modeline-bar)
                       ('doom-aiern-modeline-panel))))
          ((cons nil nil)))
    (when count
      (concat (propertize " " 'face face)
              (or (doom-aiern-modeline-icon 'faicon "i-cursor" nil nil
                                      :face face :v-adjust -0.0575)
                  (propertize "I"
                              'face `(:inherit ,face :height 1.4 :weight normal)
                              'display '(raise -0.1)))
              (propertize (doom-aiern-modeline-vspc)
                          'face `(:inherit (variable-pitch ,face)))
              (propertize (format "%d " count)
                          'face face)))))

(defsubst doom-aiern-modeline--phi-search ()
  "Show the number of matches for `phi-search' and `phi-replace'."
  (when-let ((active (doom-aiern-modeline--active)))
    (when (bound-and-true-p phi-search--overlays)
      (let ((total (length phi-search--overlays))
            (selection phi-search--selection))
        (when selection
          (propertize
           (format " %d/%d " (1+ selection) total)
           'face (if active 'doom-aiern-modeline-panel 'mode-line-inactive)))))))

(defun doom-aiern-modeline--override-phi-search-mode-line (orig-fun &rest args)
  "Override the mode-line of `phi-search' and `phi-replace'."
  (if (bound-and-true-p doom-aiern-modeline-mode)
      (apply orig-fun mode-line-format (cdr args))
    (apply orig-fun args)))
(advice-add #'phi-search--initialize :around #'doom-aiern-modeline--override-phi-search-mode-line)

(defsubst doom-aiern-modeline--buffer-size ()
  "Show buffer size."
  (when size-indication-mode
    (concat (doom-aiern-modeline-spc)
            (propertize "%I"
                        'face (if (doom-aiern-modeline--active) 'mode-line 'mode-line-inactive)
                        'help-echo "Buffer size
mouse-1: Display Line and Column Mode Menu"
                        'mouse-face 'mode-line-highlight
                        'local-map mode-line-column-line-number-mode-map)
            (doom-aiern-modeline-spc))))

(doom-aiern-modeline-def-segment matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with `anzu'), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions,
5. The current/total for the highlight term (with `symbol-overlay'), 6. The number
of active `multiple-cursors'."
  (let ((meta (concat (doom-aiern-modeline--macro-recording)
                      (doom-aiern-modeline--anzu)
                      (doom-aiern-modeline--phi-search)
                      (doom-aiern-modeline--evil-substitute)
                      (doom-aiern-modeline--aiern-substitute)
                      (doom-aiern-modeline--iedit)
                      (doom-aiern-modeline--symbol-overlay)
                      (doom-aiern-modeline--multiple-cursors))))
    (or (and (not (equal meta "")) meta)
        (doom-aiern-modeline--buffer-size)))
  )

(doom-aiern-modeline-def-segment buffer-size
  "Display buffer size"
  (doom-aiern-modeline--buffer-size))

;;
;; Media
;;

(doom-aiern-modeline-def-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  ;; TODO Include other information
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
           (when (fboundp 'image-size)
             (image-size (image-get-display-property) :pixels))
           (propertize
            (format "  %dx%d  " width height)
            'face (if (doom-aiern-modeline--active) 'mode-line 'mode-line-inactive))))))


;;
;; Bars
;;

(defvar doom-aiern-modeline--bar-active nil)
(defvar doom-aiern-modeline--bar-inactive nil)

(defsubst doom-aiern-modeline--bar ()
  "The default bar regulates the height of the mode-line in GUI."
  (unless (and doom-aiern-modeline--bar-active doom-aiern-modeline--bar-inactive)
    (let ((width doom-aiern-modeline-bar-width)
          (height (max doom-aiern-modeline-height
                       (doom-aiern-modeline--font-height))))
      (when (and (numberp width) (numberp height))
        (setq doom-aiern-modeline--bar-active
              (doom-aiern-modeline--create-bar-image 'doom-aiern-modeline-bar width height)
              doom-aiern-modeline--bar-inactive
              (doom-aiern-modeline--create-bar-image
               'doom-aiern-modeline-bar-inactive width height)))))
  (if (doom-aiern-modeline--active)
      doom-aiern-modeline--bar-active
    doom-aiern-modeline--bar-inactive))

(defun doom-aiern-modeline-refresh-bars ()
  "Refresh mode-line bars on next redraw."
  (setq doom-aiern-modeline--bar-active nil
        doom-aiern-modeline--bar-inactive nil))

(cl-defstruct doom-aiern-modeline--hud-cache active inactive top-margin bottom-margin)

(defsubst doom-aiern-modeline--hud ()
  "Powerline's hud segment reimplemented in the style of Doom's bar segment."
  (let* ((ws (window-start))
         (we (window-end))
         (bs (buffer-size))
         (height (max doom-aiern-modeline-height
                      (doom-aiern-modeline--font-height)))
         (top-margin (if (zerop bs)
                         0
                       (/ (* height (1- ws)) bs)))
         (bottom-margin (if (zerop bs)
                            0
                          (max 0 (/ (* height (- bs we 1)) bs))))
         (cache (or (window-parameter nil 'doom-aiern-modeline--hud-cache)
                    (set-window-parameter nil 'doom-aiern-modeline--hud-cache
                                          (make-doom-aiern-modeline--hud-cache)))))
    (unless (and (doom-aiern-modeline--hud-cache-active cache)
                 (doom-aiern-modeline--hud-cache-inactive cache)
                 (= top-margin (doom-aiern-modeline--hud-cache-top-margin cache))
                 (= bottom-margin
                    (doom-aiern-modeline--hud-cache-bottom-margin cache)))
      (setf (doom-aiern-modeline--hud-cache-active cache)
            (doom-aiern-modeline--create-hud-image
             'doom-aiern-modeline-bar 'default doom-aiern-modeline-bar-width
             height top-margin bottom-margin)
            (doom-aiern-modeline--hud-cache-inactive cache)
            (doom-aiern-modeline--create-hud-image
             'doom-aiern-modeline-bar-inactive 'default doom-aiern-modeline-bar-width
             height top-margin bottom-margin)
            (doom-aiern-modeline--hud-cache-top-margin cache) top-margin
            (doom-aiern-modeline--hud-cache-bottom-margin cache) bottom-margin))
    (if (doom-aiern-modeline--active)
        (doom-aiern-modeline--hud-cache-active cache)
      (doom-aiern-modeline--hud-cache-inactive cache))))

(defun doom-aiern-modeline-invalidate-huds ()
  "Invalidate all cached hud images."
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (set-window-parameter window 'doom-aiern-modeline--hud-cache nil))))

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-height
 (lambda (_sym val op _where)
   (when (and (eq op 'set) (integerp val))
     (doom-aiern-modeline-refresh-bars)
     (doom-aiern-modeline-invalidate-huds))))

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-bar-width
 (lambda (_sym val op _where)
   (when (and (eq op 'set) (integerp val))
     (doom-aiern-modeline-refresh-bars)
     (doom-aiern-modeline-invalidate-huds))))

(add-hook 'after-setting-font-hook #'doom-aiern-modeline-refresh-bars)
(add-hook 'after-setting-font-hook #'doom-aiern-modeline-invalidate-huds)

(doom-aiern-modeline-def-segment bar
  "The bar regulates the height of the mode-line in GUI."
  (if doom-aiern-modeline-hud
      (doom-aiern-modeline--hud)
    (doom-aiern-modeline--bar)))

(doom-aiern-modeline-def-segment hud
  "Powerline's hud segment reimplemented in the style of Doom's bar segment."
  (doom-aiern-modeline--hud))


;;
;; Window number
;;

;; HACK: `ace-window-display-mode' should respect the ignore buffers.
(defun doom-aiern-modeline-aw-update ()
  "Update ace-window-path window parameter for all windows.
Ensure all windows are labeled so the user can select a specific
one. The ignored buffers are excluded unless `aw-ignore-on' is nil."
  (let ((ignore-window-parameters t))
    (avy-traverse
     (avy-tree (aw-window-list) aw-keys)
     (lambda (path leaf)
       (set-window-parameter
        leaf 'ace-window-path
        (propertize
         (apply #'string (reverse path))
         'face 'aw-mode-line-face))))))
(advice-add #'aw-update :override #'doom-aiern-modeline-aw-update)

;; Remove original window number of `ace-window-display-mode'.
(add-hook 'ace-window-display-mode-hook
          (lambda ()
            (setq-default mode-line-format
                          (assq-delete-all 'ace-window-display-mode
                                           (default-value 'mode-line-format)))))

(advice-add #'window-numbering-install-mode-line :override #'ignore)
(advice-add #'window-numbering-clear-mode-line :override #'ignore)
(advice-add #'winum--install-mode-line :override #'ignore)
(advice-add #'winum--clear-mode-line :override #'ignore)

(doom-aiern-modeline-def-segment window-number
  "The current window number."
  (let ((num (cond
              ((bound-and-true-p ace-window-display-mode)
               (aw-update)
               (window-parameter (selected-window) 'ace-window-path))
              ((bound-and-true-p winum-mode)
               (setq winum-auto-setup-mode-line nil)
               (winum-get-number-string))
              ((bound-and-true-p window-numbering-mode)
               (window-numbering-get-number-string))
              (t ""))))
    (if (and (< 0 (length num))
             (< 1 (length (cl-mapcan
                           (lambda (frame)
                             ;; Exclude minibuffer and child frames
                             (unless (and (fboundp 'frame-parent)
                                          (frame-parent frame))
                               (window-list frame 'never)))
                           (visible-frame-list)))))
        (propertize (format " %s " num)
                    'face (if (doom-aiern-modeline--active)
                              'doom-aiern-modeline-buffer-major-mode
                            'mode-line-inactive)))))


;;
;; Workspace
;;

(doom-aiern-modeline-def-segment workspace-name
  "The current workspace name or number.
Requires `eyebrowse-mode' to be enabled or `tab-bar-mode' tabs to be created."
  (when doom-aiern-modeline-workspace-name
    (when-let
        ((name (cond
                ((and (bound-and-true-p eyebrowse-mode)
                      (< 1 (length (eyebrowse--get 'window-configs))))
                 (assq-delete-all 'eyebrowse-mode mode-line-misc-info)
                 (when-let*
                     ((num (eyebrowse--get 'current-slot))
                      (tag (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                   (if (< 0 (length tag)) tag (int-to-string num))))
                ((and (fboundp 'tab-bar-mode)
                      (< 1 (length (frame-parameter nil 'tabs))))
                 (let* ((current-tab (tab-bar--current-tab))
                        (tab-index (tab-bar--current-tab-index))
                        (explicit-name (alist-get 'explicit-name current-tab))
                        (tab-name (alist-get 'name current-tab)))
                   (if explicit-name tab-name (+ 1 tab-index)))))))
      (propertize (format " %s " name) 'face
                  (if (doom-aiern-modeline--active)
                      'doom-aiern-modeline-buffer-major-mode
                    'mode-line-inactive)))))


;;
;; Perspective
;;

(defvar-local doom-aiern-modeline--persp-name nil)
(defun doom-aiern-modeline-update-persp-name (&rest _)
  "Update perspective name in mode-line."
  (setq doom-aiern-modeline--persp-name
        ;; Support `persp-mode', while not support `perspective'
        (when (and doom-aiern-modeline-persp-name
                   (bound-and-true-p persp-mode)
                   (fboundp 'safe-persp-name)
                   (fboundp 'get-current-persp))
          (let* ((persp (get-current-persp))
                 (name (safe-persp-name persp))
                 (face (if (and persp
                                (not (persp-contain-buffer-p (current-buffer) persp)))
                           'doom-aiern-modeline-persp-buffer-not-in-persp
                         'doom-aiern-modeline-persp-name))
                 (icon (doom-aiern-modeline-icon 'material "folder" "🖿" "#"
                                           :face `(:inherit ,face :slant normal)
                                           :height 1.1
                                           :v-adjust -0.225)))
            (when (or doom-aiern-modeline-display-default-persp-name
                      (not (string-equal persp-nil-name name)))
              (concat (doom-aiern-modeline-spc)
                      (propertize (concat (and doom-aiern-modeline-persp-icon
                                               (concat icon (doom-aiern-modeline-vspc)))
                                          (propertize name 'face face))
                                  'help-echo "mouse-1: Switch perspective
mouse-2: Show help for minor mode"
                                  'mouse-face 'mode-line-highlight
                                  'local-map (let ((map (make-sparse-keymap)))
                                               (define-key map [mode-line mouse-1]
                                                 #'persp-switch)
                                               (define-key map [mode-line mouse-2]
                                                 (lambda ()
                                                   (interactive)
                                                   (describe-function 'persp-mode)))
                                               map))
                      (doom-aiern-modeline-spc)))))))

(add-hook 'buffer-list-update-hook #'doom-aiern-modeline-update-persp-name)
(add-hook 'find-file-hook #'doom-aiern-modeline-update-persp-name)
(add-hook 'persp-activated-functions #'doom-aiern-modeline-update-persp-name)
(add-hook 'persp-renamed-functions #'doom-aiern-modeline-update-persp-name)
(advice-add #'lv-message :after #'doom-aiern-modeline-update-persp-name)

(doom-aiern-modeline-def-segment persp-name
  "The current perspective name."
  (when (and (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p))
    doom-aiern-modeline--persp-name))


;;
;; Misc info
;;

(doom-aiern-modeline-def-segment misc-info
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  (when (and (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p))
    '("" mode-line-misc-info)))


;;
;; Position
;;

;; Be compatible with Emacs 25.
(defvar doom-aiern-modeline-column-zero-based
  (if (boundp 'column-number-indicator-zero-based)
      column-number-indicator-zero-based
    t)
  "When non-nil, mode line displays column numbers zero-based.
See `column-number-indicator-zero-based'.")

(defvar doom-aiern-modeline-percent-position
  (if (boundp 'mode-line-percent-position)
      mode-line-percent-position
    '(-3 "%p"))
  "Specification of \"percentage offset\" of window through buffer.
See `mode-line-percent-position'.")

(doom-aiern-modeline-add-variable-watcher
 'column-number-indicator-zero-based
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-column-zero-based val))))

(doom-aiern-modeline-add-variable-watcher
 'mode-line-percent-position
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-percent-position val))))

(doom-aiern-modeline-def-segment buffer-position
  "The buffer position information."
  (let* ((active (doom-aiern-modeline--active))
         (lc '(line-number-mode
               (column-number-mode
                (doom-aiern-modeline-column-zero-based "%l:%c" "%l:%C")
                "%l")
               (column-number-mode (doom-aiern-modeline-column-zero-based ":%c" ":%C"))))
         (face (if active 'mode-line 'mode-line-inactive))
         (mouse-face 'mode-line-highlight)
         (local-map mode-line-column-line-number-mode-map))
    (concat
     (doom-aiern-modeline-spc)
     (doom-aiern-modeline-spc)

     (propertize (format-mode-line lc)
                 'face face
                 'help-echo "Buffer position\n\
mouse-1: Display Line and Column Mode Menu"
                 'mouse-face mouse-face
                 'local-map local-map)

     (cond ((and active
                 (bound-and-true-p nyan-mode)
                 (not doom-aiern-modeline--limited-width-p)
                 (>= (window-width) nyan-minimum-window-width))
            (concat
             (doom-aiern-modeline-spc)
             (doom-aiern-modeline-spc)
             (propertize (nyan-create) 'mouse-face mouse-face)))
           ((and active
                 (bound-and-true-p poke-line-mode)
                 (not doom-aiern-modeline--limited-width-p)
                 (>= (window-width) poke-line-minimum-window-width))
            (concat
             (doom-aiern-modeline-spc)
             (doom-aiern-modeline-spc)
             (propertize (poke-line-create) 'mouse-face mouse-face)))
           ((and active
                 (bound-and-true-p mlscroll-mode)
                 (not doom-aiern-modeline--limited-width-p)
                 (>= (window-width) mlscroll-minimum-current-width))
            (concat
             (doom-aiern-modeline-spc)
             (doom-aiern-modeline-spc)
             (let ((mlscroll-right-align nil))
               (format-mode-line (mlscroll-mode-line)))))
           (t
            (when doom-aiern-modeline-percent-position
              (concat
               (doom-aiern-modeline-spc)
               (propertize (format-mode-line '("" doom-aiern-modeline-percent-position "%%"))
                           'face face
                           'help-echo "Buffer percentage\n\
mouse-1: Display Line and Column Mode Menu"
                           'mouse-face mouse-face
                           'local-map local-map)))))
     (when (or line-number-mode column-number-mode doom-aiern-modeline-percent-position)
       (doom-aiern-modeline-spc)))))

;;
;; Party parrot
;;
(doom-aiern-modeline-def-segment parrot
  "The party parrot animated icon. Requires `parrot-mode' to be enabled."
  (when (and (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p)
             (bound-and-true-p parrot-mode))
    (concat (doom-aiern-modeline-spc)
            (doom-aiern-modeline-spc)
            (parrot-create)
            (doom-aiern-modeline-spc))))

;;
;; Modals (evil, overwrite, god, ryo and xah-fly-keys, etc.)
;;

(defun doom-aiern-modeline--modal-icon (text face help-echo)
  "Display the model icon with FACE and HELP-ECHO.
TEXT is alternative if icon is not available."
  (propertize (doom-aiern-modeline-icon
               'material
               (when doom-aiern-modeline-modal-icon "fiber_manual_record")
               "●"
               text
               :face (if (doom-aiern-modeline--active) face 'mode-line-inactive)
               :v-adjust -0.225)
              'help-echo help-echo))

(defsubst doom-aiern-modeline--evil ()
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (doom-aiern-modeline--modal-icon
     (let ((tag (replace-regexp-in-string
          "> "
          ""
          (replace-regexp-in-string
            " <"
            ""
            (evil-state-property evil-state :tag t)))))
       (if (stringp tag) tag (funcall tag)))
     (cond
      ((evil-normal-state-p) 'doom-aiern-modeline-evil-normal-state)
      ((evil-god-state-p) 'doom-aiern-modeline-evil-god-state)
      ((evil-emacs-state-p) 'doom-aiern-modeline-evil-emacs-state)
      ((evil-insert-state-p) 'doom-aiern-modeline-evil-insert-state)
      ((evil-motion-state-p) 'doom-aiern-modeline-evil-motion-state)
      ((evil-visual-state-p) 'doom-aiern-modeline-evil-visual-state)
      ((evil-operator-state-p) 'doom-aiern-modeline-evil-operator-state)
      ((evil-replace-state-p) 'doom-aiern-modeline-evil-replace-state)
      (t 'doom-aiern-modeline-evil-normal-state))
     (evil-state-property evil-state :name t))))

(defsubst doom-aiern-modeline--aiern ()
  "The current aiern state. Requires `aiern-mode' to be enabled."
  (when (bound-and-true-p aiern-local-mode)
    (doom-aiern-modeline--modal-icon
     (let ((tag (aiern-state-property aiern-state :tag t)))
       (if (stringp tag) tag (funcall tag)))
     (cond
      ((aiern-normal-state-p) 'doom-aiern-modeline-aiern-normal-state)
      ((aiern-god-state-p) 'doom-aiern-modeline-aiern-god-state)
      ((aiern-emacs-state-p) 'doom-aiern-modeline-aiern-emacs-state)
      ((aiern-insert-state-p) 'doom-aiern-modeline-aiern-insert-state)
      ((aiern-motion-state-p) 'doom-aiern-modeline-aiern-motion-state)
      ((aiern-visual-state-p) 'doom-aiern-modeline-aiern-visual-state)
      ((aiern-operator-state-p) 'doom-aiern-modeline-aiern-operator-state)
      ((aiern-replace-state-p) 'doom-aiern-modeline-aiern-replace-state)
      (t 'doom-aiern-modeline-aiern-normal-state))
     (aiern-state-property aiern-state :name t))))

(defsubst doom-aiern-modeline--overwrite ()
  "The current overwrite state which is enabled by command `overwrite-mode'."
  (when (and (bound-and-true-p overwrite-mode)
             (not (or (bound-and-true-p evil-local-mode)
                      (bound-and-true-p aiern-local-mode))))
    (doom-aiern-modeline--modal-icon " O " 'doom-aiern-modeline-urgent "Overwrite mode")))

(defsubst doom-aiern-modeline--god ()
  "The current god state which is enabled by the command `god-mode'."
  (when (bound-and-true-p god-local-mode)
    (doom-aiern-modeline--modal-icon " G " 'doom-aiern-modeline-evil-normal-state "God mode")))

(defsubst doom-aiern-modeline--ryo ()
  "The current ryo-modal state which is enabled by the command `ryo-modal-mode'."
  (when (bound-and-true-p ryo-modal-mode)
    (doom-aiern-modeline--modal-icon " R " 'doom-aiern-modeline-evil-normal-state "Ryo modal mode")))

(defsubst doom-aiern-modeline--sorrow ()
  "The current sorrow state which is enabled by the command `sorrow-mode'."
  (when (bound-and-true-p sorrow-mode)
    (doom-aiern-modeline--modal-icon " S " 'doom-aiern-modeline-evil-normal-state "Sorrow mode")))

(defsubst doom-aiern-modeline--modalka ()
  "The current modalka state which is enabled by the command `modalka-mode'."
  (when (bound-and-true-p modalka-mode)
    (doom-aiern-modeline--modal-icon " M " 'doom-aiern-modeline-evil-normal-state "Modalka mode")))

(defsubst doom-aiern-modeline--hydra ()
  "The current hydra state."
  (when hydra-curr-map
    (doom-aiern-modeline--modal-icon " H " 'doom-aiern-modeline-evil-normal-state "Hydra")))

(defsubst doom-aiern-modeline--deino ()
  "The current deino state."
  (when deino-curr-map
    (doom-aiern-modeline--modal-icon " D " 'doom-aiern-modeline-evil-normal-state "Deino")))

(defvar meq/var/titan-fell-mode nil)
(defsubst doom-aiern-modeline--titan-fell ()
  "The current titan-fell state."
  (when meq/var/titan-fell-mode
    (doom-aiern-modeline--modal-icon " TF " 'doom-aiern-modeline-evil-normal-state "Titan Fell")))

(defvar meq/var/titan-doc-mode nil)
(defsubst doom-aiern-modeline--titan-doc ()
  "The current titan-doc state."
  (when meq/var/titan-doc-mode
    (doom-aiern-modeline--modal-icon " TD " 'doom-aiern-modeline-evil-normal-state "Titan Doc")))

(defsubst doom-aiern-modeline--xah-fly-keys ()
  "The current `xah-fly-keys' state."
  (when (bound-and-true-p xah-fly-keys)
    (if xah-fly-insert-state-q
        (doom-aiern-modeline--modal-icon " I "
                                   'doom-aiern-modeline-evil-insert-state
                                   (format "Xah-fly insert mode"))
      (doom-aiern-modeline--modal-icon " C "
                                 'doom-aiern-modeline-evil-normal-state
                                 (format "Xah-fly command mode")))))

(defsubst doom-aiern-modeline--boon ()
  "The current Boon state. Requires `boon-mode' to be enabled."
  (when (bound-and-true-p boon-local-mode)
    (doom-aiern-modeline--modal-icon
     (boon-state-string)
     (cond
      (boon-command-state 'doom-aiern-modeline-evil-normal-state)
      (boon-insert-state 'doom-aiern-modeline-evil-insert-state)
      (boon-special-state 'doom-aiern-modeline-evil-emacs-state)
      (boon-off-state 'doom-aiern-modeline-evil-operator-state)
      (t 'doom-aiern-modeline-evil-operator-state))
     (boon-modeline-string))))

(defsubst doom-aiern-modeline--meow ()
  "The current Meow state. Requires `meow-mode' to be enabled."
  (when (bound-and-true-p meow-mode)
    meow--indicator))

(doom-aiern-modeline-def-segment modals
  "Displays modal editing states, including `evil', `overwrite', `god', `ryo'
and `xah-fly-kyes', etc."
  (let* ((evil (doom-aiern-modeline--evil))
         (aiern (doom-aiern-modeline--aiern))
         (ow (doom-aiern-modeline--overwrite))
         (god (doom-aiern-modeline--god))
         (ryo (doom-aiern-modeline--ryo))
         (sorrow (doom-aiern-modeline--sorrow))
         (modalka (doom-aiern-modeline--modalka))
         (hydra (doom-aiern-modeline--hydra))
         (deino (doom-aiern-modeline--deino))
         (titan-fell (doom-aiern-modeline--titan-fell))
         (titan-doc (doom-aiern-modeline--titan-doc))
         (xf (doom-aiern-modeline--xah-fly-keys))
         (boon (doom-aiern-modeline--boon))
         (nospc (doom-aiern-modeline-nospc))
         (meow (doom-aiern-modeline--meow))
         (sep (and (or evil aiern ow god ryo sorrow modalka hydra deino titan-fell titan-doc xf boon) (doom-aiern-modeline-spc))))
    (concat "<"
            (and evil (concat evil (and (or aiern ow god ryo sorrow modalka hydra deino titan-fell titan-doc xf boon meow) nospc)))
            (and aiern (concat aiern (and (or ow god ryo sorrow modalka hydra deino titan-fell titan-doc xf boon meow) nospc)))
            (and ow (concat ow (and (or god ryo sorrow modalka hydra deino titan-fell titan-doc xf boon meow) nospc)))
            (and god (concat god (and (or ryo sorrow modalka hydra deino titan-fell titan-doc xf boon meow) nospc)))
            (and ryo (concat ryo (and (or sorrow modalka hydra deino titan-fell titan-doc xf boon meow) nospc)))
            (and sorrow (concat sorrow (and (or modalka hydra deino titan-fell titan-doc xf boon meow) nospc)))
            (and modalka (concat modalka (and (or hydra deino titan-fell titan-doc xf boon meow) nospc)))
            (and hydra (concat hydra (and (or deino titan-fell titan-doc xf boon meow) nospc)))
            (and deino (concat deino (and (or titan-fell titan-doc xf boon meow) nospc)))
            (and titan-fell (concat titan-fell (and (or titan-doc xf boon meow) nospc)))
            (and titan-doc (concat titan-doc (and (or xf boon meow) nospc)))
            (and xf (concat xf (and (or boon meow) nospc)))
            (and boon (concat boon (and meow nospc)))
            meow ">")))

;;
;; Objed state
;;

(defvar doom-aiern-modeline--objed-active nil)

(defun doom-aiern-modeline-update-objed (_ &optional reset)
  "Update `objed' status, inactive when RESET is true."
  (setq doom-aiern-modeline--objed-active (not reset)))

(setq objed-modeline-setup-func #'doom-aiern-modeline-update-objed)

(doom-aiern-modeline-def-segment objed-state ()
  "The current objed state."
  (when (and doom-aiern-modeline--objed-active
             (doom-aiern-modeline--active))
    (propertize (format " %s(%s) "
                        (symbol-name objed--object)
                        (char-to-string (aref (symbol-name objed--obj-state) 0)))
                'face 'doom-aiern-modeline-evil-emacs-state
                'help-echo (format "Objed object: %s (%s)"
                                   (symbol-name objed--object)
                                   (symbol-name objed--obj-state)))))


;;
;; Input method
;;

(doom-aiern-modeline-def-segment input-method
  "The current input method."
  (propertize (cond (current-input-method
                     (concat (doom-aiern-modeline-spc)
                             current-input-method-title
                             (doom-aiern-modeline-spc)))
                    ((or (and (bound-and-true-p evil-local-mode)
                          (bound-and-true-p evil-input-method))
                        (and (bound-and-true-p aiern-local-mode)
                          (bound-and-true-p aiern-input-method)))
                     (concat
                      (doom-aiern-modeline-spc)
                      (nth 3 (assoc default-input-method input-method-alist))
                      (doom-aiern-modeline-spc)))
                    (t ""))
              'face (if (doom-aiern-modeline--active)
                        (if (and (bound-and-true-p rime-mode)
                                 (equal current-input-method "rime"))
                            (if (and (rime--should-enable-p)
                                     (not (rime--should-inline-ascii-p)))
                                'doom-aiern-modeline-input-method
                              'doom-aiern-modeline-input-method-alt)
                          'doom-aiern-modeline-input-method)
                      'mode-line-inactive)
              'help-echo (concat
                          "Current input method: "
                          current-input-method
                          "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
              'mouse-face 'mode-line-highlight
              'local-map mode-line-input-method-map))


;;
;; Info
;;

(doom-aiern-modeline-def-segment info-nodes
  "The topic and nodes in the Info buffer."
  (let ((active (doom-aiern-modeline--active)))
    (concat
     (propertize " (" 'face (if active 'mode-line 'mode-line-inactive))
     ;; topic
     (propertize (if (stringp Info-current-file)
                     (replace-regexp-in-string
                      "%" "%%"
                      (file-name-sans-extension
                       (file-name-nondirectory Info-current-file)))
                   (format "*%S*" Info-current-file))
                 'face (if active 'doom-aiern-modeline-info 'mode-line-inactive))
     (propertize ") " 'face (if active 'mode-line 'mode-line-inactive))
     ;; node
     (when Info-current-node
       (propertize (replace-regexp-in-string
                    "%" "%%" Info-current-node)
                   'face (if active 'doom-aiern-modeline-buffer-path 'mode-line-inactive)
                   'help-echo
                   "mouse-1: scroll forward, mouse-3: scroll back"
                   'mouse-face 'mode-line-highlight
                   'local-map Info-mode-line-node-keymap)))))


;;
;; REPL
;;

(defun doom-aiern-modeline-repl-icon (text face)
  "Display REPL icon (or TEXT in terminal) with FACE."
  (doom-aiern-modeline-icon 'faicon "terminal" "$" text
                      :face face :height 1.0 :v-adjust -0.0575))

(defvar doom-aiern-modeline--cider nil)

(defun doom-aiern-modeline-update-cider ()
  "Update cider repl state."
  (setq doom-aiern-modeline--cider
        (let* ((connected (cider-connected-p))
               (face (if connected 'doom-aiern-modeline-repl-success 'doom-aiern-modeline-repl-warning))
               (repl-buffer (cider-current-repl nil nil))
               (cider-info (when repl-buffer
                             (cider--connection-info repl-buffer t)))
               (icon (doom-aiern-modeline-repl-icon "REPL" face)))
          (propertize icon
                      'help-echo
                      (if connected
                          (format "CIDER Connected %s\nmouse-2: CIDER quit" cider-info)
                        "CIDER Disconnected\nmouse-1: CIDER jack-in")
                      'mouse-face 'mode-line-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (if connected
                                       (define-key map [mode-line mouse-2]
                                         #'cider-quit)
                                     (define-key map [mode-line mouse-1]
                                       #'cider-jack-in))
                                   map)))))

(add-hook 'cider-connected-hook #'doom-aiern-modeline-update-cider)
(add-hook 'cider-disconnected-hook #'doom-aiern-modeline-update-cider)
(add-hook 'cider-mode-hook #'doom-aiern-modeline-update-cider)

(doom-aiern-modeline-def-segment repl
  "The REPL state."
  (when doom-aiern-modeline-repl
    (when-let (icon (when (bound-and-true-p cider-mode)
                      doom-aiern-modeline--cider))
      (concat
       (doom-aiern-modeline-spc)
       (if (doom-aiern-modeline--active)
           icon
         (doom-aiern-modeline-propertize-icon icon 'mode-line-inactive))
       (doom-aiern-modeline-spc)))))


;;
;; LSP
;;

(defun doom-aiern-modeline-lsp-icon (text face)
  "Display LSP icon (or TEXT in terminal) with FACE."
  (doom-aiern-modeline-icon 'faicon "rocket" "🚀" text
                      :face face :height 1.0 :v-adjust -0.0575))

(defvar-local doom-aiern-modeline--lsp nil)
(defun doom-aiern-modeline-update-lsp (&rest _)
  "Update `lsp-mode' state."
  (setq doom-aiern-modeline--lsp
        (let* ((workspaces (lsp-workspaces))
               (face (if workspaces 'doom-aiern-modeline-lsp-success 'doom-aiern-modeline-lsp-warning))
               (icon (doom-aiern-modeline-lsp-icon "LSP" face)))
          (propertize icon
                      'help-echo
                      (if workspaces
                          (concat "LSP Connected "
                                  (string-join
                                   (mapcar (lambda (w)
                                             (format "[%s]\n" (lsp--workspace-print w)))
                                           workspaces))
                                  "C-mouse-1: Switch to another workspace folder
mouse-1: Describe current session
mouse-2: Quit server
mouse-3: Reconnect to server")
                        "LSP Disconnected
mouse-1: Reload to start server")
                      'mouse-face 'mode-line-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (if workspaces
                                       (progn
                                         (define-key map [mode-line C-mouse-1]
                                           #'lsp-workspace-folders-open)
                                         (define-key map [mode-line mouse-1]
                                           #'lsp-describe-session)
                                         (define-key map [mode-line mouse-2]
                                           #'lsp-workspace-shutdown)
                                         (define-key map [mode-line mouse-3]
                                           #'lsp-workspace-restart))
                                     (progn
                                       (define-key map [mode-line mouse-1]
                                         (lambda ()
                                           (interactive)
                                           (ignore-errors (revert-buffer t t))))))
                                   map)))))
(add-hook 'lsp-before-initialize-hook #'doom-aiern-modeline-update-lsp)
(add-hook 'lsp-after-initialize-hook #'doom-aiern-modeline-update-lsp)
(add-hook 'lsp-after-uninitialized-functions #'doom-aiern-modeline-update-lsp)
(add-hook 'lsp-before-open-hook #'doom-aiern-modeline-update-lsp)
(add-hook 'lsp-after-open-hook #'doom-aiern-modeline-update-lsp)

(defvar-local doom-aiern-modeline--eglot nil)
(defun doom-aiern-modeline-update-eglot ()
  "Update `eglot' state."
  (setq doom-aiern-modeline--eglot
        (pcase-let* ((server (eglot-current-server))
                     (nick (and server (eglot--project-nickname server)))
                     (pending (and server (hash-table-count
                                           (jsonrpc--request-continuations server))))
                     (`(,_id ,doing ,done-p ,detail) (and server (eglot--spinner server)))
                     (last-error (and server (jsonrpc-last-error server)))
                     (face (cond (last-error 'doom-aiern-modeline-lsp-error)
                                 ((and doing (not done-p)) 'doom-aiern-modeline-lsp-running)
                                 ((and pending (cl-plusp pending)) 'doom-aiern-modeline-lsp-warning)
                                 (nick 'doom-aiern-modeline-lsp-success)
                                 (t 'doom-aiern-modeline-lsp-warning)))
                     (icon (doom-aiern-modeline-lsp-icon "EGLOT" face)))
          (propertize icon
                      'help-echo (cond
                                  (last-error
                                   (format "EGLOT\nAn error occured: %s
mouse-3: Clear this status" (plist-get last-error :message)))
                                  ((and doing (not done-p))
                                   (format "EGLOT\n%s%s" doing
                                           (if detail (format "%s" detail) "")))
                                  ((and pending (cl-plusp pending))
                                   (format "EGLOT\n%d outstanding requests" pending))
                                  (nick (format "EGLOT Connected (%s/%s)
C-mouse-1: Go to server errors
mouse-1: Go to server events
mouse-2: Quit server
mouse-3: Reconnect to server" nick (eglot--major-mode server)))
                                  (t "EGLOT Disconnected
mouse-1: Start server"))
                      'mouse-face 'mode-line-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (cond (last-error
                                          (define-key map [mode-line mouse-3]
                                            #'eglot-clear-status))
                                         ((and pending (cl-plusp pending))
                                          (define-key map [mode-line mouse-3]
                                            #'eglot-forget-pending-continuations))
                                         (nick
                                          (define-key map [mode-line C-mouse-1]
                                            #'eglot-stderr-buffer)
                                          (define-key map [mode-line mouse-1]
                                            #'eglot-events-buffer)
                                          (define-key map [mode-line mouse-2]
                                            #'eglot-shutdown)
                                          (define-key map [mode-line mouse-3]
                                            #'eglot-reconnect))
                                         (t (define-key map [mode-line mouse-1]
                                              #'eglot)))
                                   map)))))
(add-hook 'eglot--managed-mode-hook #'doom-aiern-modeline-update-eglot)

(doom-aiern-modeline-def-segment lsp
  "The LSP server state."
  (when (and doom-aiern-modeline-lsp
             (not doom-aiern-modeline--limited-width-p))
    (let ((active (doom-aiern-modeline--active))
          (icon (cond ((bound-and-true-p lsp-mode)
                       doom-aiern-modeline--lsp)
                      ((bound-and-true-p eglot--managed-mode)
                       doom-aiern-modeline--eglot))))
      (when icon
        (concat
         (doom-aiern-modeline-spc)
         (if active
             icon
           (doom-aiern-modeline-propertize-icon icon 'mode-line-inactive))
         (doom-aiern-modeline-spc))))))

(defun doom-aiern-modeline-override-eglot-modeline ()
  "Override `eglot' mode-line."
  (if (bound-and-true-p doom-aiern-modeline-mode)
      (setq mode-line-misc-info
            (delq (assq 'eglot--managed-mode mode-line-misc-info) mode-line-misc-info))
    (add-to-list 'mode-line-misc-info
                 `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))))
(add-hook 'eglot--managed-mode-hook #'doom-aiern-modeline-override-eglot-modeline)
(add-hook 'doom-aiern-modeline-mode-hook #'doom-aiern-modeline-override-eglot-modeline)


;;
;; GitHub
;;

(defvar doom-aiern-modeline--github-notification-number 0)
(defvar doom-aiern-modeline-before-github-fetch-notification-hook nil
  "Hooks before fetching GitHub notifications.
Example:
  (add-hook 'doom-aiern-modeline-before-github-fetch-notification-hook
            #'auth-source-pass-enable)")
(defun doom-aiern-modeline--github-fetch-notifications ()
  "Fetch GitHub notifications."
  (when (and doom-aiern-modeline-github
             (require 'async nil t))
    (async-start
     `(lambda ()
        ,(async-inject-variables
          "\\`\\(load-path\\|auth-sources\\|doom-aiern-modeline-before-github-fetch-notification-hook\\)\\'")
        (run-hooks 'doom-aiern-modeline-before-github-fetch-notification-hook)
        (when (require 'ghub nil t)
          (with-timeout (10)
            (ignore-errors
              (when-let* ((username (ghub--username ghub-default-host))
                          (token (ghub--token ghub-default-host username 'ghub t)))
                (ghub-get "/notifications" nil
                          :query '((notifications . "true"))
                          :username username
                          :auth token
                          :noerror t))))))
     (lambda (result)
       (message "")                     ; suppress message
       (setq doom-aiern-modeline--github-notification-number (length result))))))

(defvar doom-aiern-modeline--github-timer nil)
(defun doom-aiern-modeline-github-timer ()
  "Start/Stop the timer for GitHub fetching."
  (if (timerp doom-aiern-modeline--github-timer)
      (cancel-timer doom-aiern-modeline--github-timer))
  (setq doom-aiern-modeline--github-timer
        (and doom-aiern-modeline-github
             (run-with-idle-timer 30
                                  doom-aiern-modeline-github-interval
                                  #'doom-aiern-modeline--github-fetch-notifications))))

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-github
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-github val)
     (doom-aiern-modeline-github-timer))))

(doom-aiern-modeline-github-timer)

(doom-aiern-modeline-def-segment github
  "The GitHub notifications."
  (when (and doom-aiern-modeline-github
             (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p)
             (numberp doom-aiern-modeline--github-notification-number)
             (> doom-aiern-modeline--github-notification-number 0))
    (concat
     (doom-aiern-modeline-spc)
     (propertize
      (concat
       (doom-aiern-modeline-icon 'faicon "github" "🔔" "#"
                           :face 'doom-aiern-modeline-notification
                           :v-adjust -0.0575)
       (doom-aiern-modeline-vspc)
       ;; GitHub API is paged, and the limit is 50
       (propertize
        (if (>= doom-aiern-modeline--github-notification-number 50)
            "50+"
          (number-to-string doom-aiern-modeline--github-notification-number))
        'face '(:inherit
                (doom-aiern-modeline-unread-number doom-aiern-modeline-notification))))
      'help-echo "Github Notifications
mouse-1: Show notifications
mouse-3: Fetch notifications"
      'mouse-face 'mode-line-highlight
      'local-map (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line mouse-1]
                     (lambda ()
                       "Open GitHub notifications page."
                       (interactive)
                       (run-with-idle-timer 300 nil #'doom-aiern-modeline--github-fetch-notifications)
                       (browse-url "https://github.com/notifications")))
                   (define-key map [mode-line mouse-3]
                     (lambda ()
                       "Fetching GitHub notifications."
                       (interactive)
                       (message "Fetching GitHub notifications...")
                       (doom-aiern-modeline--github-fetch-notifications)))
                   map))
     (doom-aiern-modeline-spc))))


;;
;; Debug states
;;

;; Highlight the mode-line while debugging.
(defvar-local doom-aiern-modeline--debug-cookie nil)
(defun doom-aiern-modeline--debug-visual (&rest _)
  "Update the face of mode-line for debugging."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (setq doom-aiern-modeline--debug-cookie
                  (face-remap-add-relative 'mode-line 'doom-aiern-modeline-debug-visual))
            (force-mode-line-update)))
        (buffer-list)))

(defun doom-aiern-modeline--normal-visual (&rest _)
  "Restore the face of mode-line."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (when doom-aiern-modeline--debug-cookie
              (face-remap-remove-relative doom-aiern-modeline--debug-cookie)
              (force-mode-line-update))))
        (buffer-list)))

(add-hook 'dap-session-created-hook #'doom-aiern-modeline--debug-visual)
(add-hook 'dap-terminated-hook #'doom-aiern-modeline--normal-visual)

(defun doom-aiern-modeline-debug-icon (face &rest args)
  "Display debug icon with FACE and ARGS."
  (doom-aiern-modeline-icon 'faicon "bug" "🐛" "!" :face face :v-adjust -0.0575 args))

(defun doom-aiern-modeline--debug-dap ()
  "The current `dap-mode' state."
  (when (and (bound-and-true-p dap-mode)
             (bound-and-true-p lsp-mode))
    (when-let ((session (dap--cur-session)))
      (when (dap--session-running session)
        (propertize (doom-aiern-modeline-debug-icon 'doom-aiern-modeline-info)
                    'help-echo (format "DAP (%s - %s)
mouse-1: Display debug hydra
mouse-2: Display recent configurations
mouse-3: Disconnect session"
                                       (dap--debug-session-name session)
                                       (dap--debug-session-state session))
                    'mouse-face 'mode-line-highlight
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [mode-line mouse-1]
                                   #'dap-hydra)
                                 (define-key map [mode-line mouse-2]
                                   #'dap-debug-recent)
                                 (define-key map [mode-line mouse-3]
                                   #'dap-disconnect)
                                 map))))))

(defvar-local doom-aiern-modeline--debug-dap nil)
(defun doom-aiern-modeline-update-debug-dap (&rest _)
  "Update dap debug state."
  (setq doom-aiern-modeline--debug-dap (doom-aiern-modeline--debug-dap)))

(add-hook 'dap-session-created-hook #'doom-aiern-modeline-update-debug-dap)
(add-hook 'dap-session-changed-hook #'doom-aiern-modeline-update-debug-dap)
(add-hook 'dap-terminated-hook #'doom-aiern-modeline-update-debug-dap)

(defsubst doom-aiern-modeline--debug-edebug ()
  "The current `edebug' state."
  (when (bound-and-true-p edebug-mode)
    (propertize (doom-aiern-modeline-debug-icon 'doom-aiern-modeline-info)
                'help-echo (format "EDebug (%s)
mouse-1: Show help
mouse-2: Next
mouse-3: Stop debugging"
                                   edebug-execution-mode)
                'mouse-face 'mode-line-highlight
                'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-1]
                               #'edebug-help)
                             (define-key map [mode-line mouse-2]
                               #'edebug-next-mode)
                             (define-key map [mode-line mouse-3]
                               #'edebug-stop)
                             map))))

(defsubst doom-aiern-modeline--debug-on-error ()
  "The current `debug-on-error' state."
  (when debug-on-error
    (propertize (doom-aiern-modeline-debug-icon 'doom-aiern-modeline-urgent)
                'help-echo "Debug on Error
mouse-1: Toggle Debug on Error"
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-error))))

(defsubst doom-aiern-modeline--debug-on-quit ()
  "The current `debug-on-quit' state."
  (when debug-on-quit
    (propertize (doom-aiern-modeline-debug-icon 'doom-aiern-modeline-warning)
                'help-echo "Debug on Quit
mouse-1: Toggle Debug on Quit"
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-quit))))

(doom-aiern-modeline-def-segment debug
  "The current debug state."
  (when (and (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p))
    (let* ((dap doom-aiern-modeline--debug-dap)
           (edebug (doom-aiern-modeline--debug-edebug))
           (on-error (doom-aiern-modeline--debug-on-error))
           (on-quit (doom-aiern-modeline--debug-on-quit))
           (vsep (doom-aiern-modeline-vspc))
           (sep (and (or dap edebug on-error on-quit) (doom-aiern-modeline-spc))))
      (concat sep
              (and dap (concat dap (and (or edebug on-error on-quit) vsep)))
              (and edebug (concat edebug (and (or on-error on-quit) vsep)))
              (and on-error (concat on-error (and on-quit vsep)))
              on-quit
              sep))))


;;
;; PDF pages
;;

(defvar-local doom-aiern-modeline--pdf-pages nil)
(defun doom-aiern-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq doom-aiern-modeline--pdf-pages
        (format "  P%d/%d "
                (eval `(pdf-view-current-page))
                (pdf-cache-number-of-pages))))
(add-hook 'pdf-view-change-page-hook #'doom-aiern-modeline-update-pdf-pages)

(doom-aiern-modeline-def-segment pdf-pages
  "Display PDF pages."
  (propertize doom-aiern-modeline--pdf-pages
              'face (if (doom-aiern-modeline--active) 'mode-line 'mode-line-inactive)))


;;
;; `mu4e-alert' notifications
;;

(doom-aiern-modeline-def-segment mu4e
  "Show notifications of any unread emails in `mu4e'."
  (when (and doom-aiern-modeline-mu4e
             (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p)
             (bound-and-true-p mu4e-alert-mode-line)
             (numberp mu4e-alert-mode-line)
             ;; don't display if the unread mails count is zero
             (> mu4e-alert-mode-line 0))
    (concat
     (doom-aiern-modeline-spc)
     (propertize
      (concat
       (doom-aiern-modeline-icon 'material "email" "📧" "#"
                           :face 'doom-aiern-modeline-notification
                           :height 1.1 :v-adjust -0.2)
       (doom-aiern-modeline-vspc)
       (propertize
        (if (> mu4e-alert-mode-line doom-aiern-modeline-number-limit)
            (format "%d+" doom-aiern-modeline-number-limit)
          (number-to-string mu4e-alert-mode-line))
        'face '(:inherit
                (doom-aiern-modeline-unread-number doom-aiern-modeline-notification))))
      'mouse-face 'mode-line-highlight
      'keymap '(mode-line keymap
                          (mouse-1 . mu4e-alert-view-unread-mails)
                          (mouse-2 . mu4e-alert-view-unread-mails)
                          (mouse-3 . mu4e-alert-view-unread-mails))
      'help-echo (concat (if (= mu4e-alert-mode-line 1)
                             "You have an unread email"
                           (format "You have %s unread emails" mu4e-alert-mode-line))
                         "\nClick here to view "
                         (if (= mu4e-alert-mode-line 1) "it" "them")))
     (doom-aiern-modeline-spc))))

(defun doom-aiern-modeline-override-mu4e-alert-modeline (&rest _)
  "Delete `mu4e-alert-mode-line' from global modeline string."
  (when (featurep 'mu4e-alert)
    (if (and doom-aiern-modeline-mu4e
             (bound-and-true-p doom-aiern-modeline-mode))
        ;; Delete original modeline
        (progn
          (setq global-mode-string
                (delete '(:eval mu4e-alert-mode-line) global-mode-string))
          (setq mu4e-alert-modeline-formatter #'identity))
      ;; Recover default settings
      (setq mu4e-alert-modeline-formatter #'mu4e-alert-default-mode-line-formatter))))
(advice-add #'mu4e-alert-enable-mode-line-display
            :after #'doom-aiern-modeline-override-mu4e-alert-modeline)
(add-hook 'doom-aiern-modeline-mode-hook #'doom-aiern-modeline-override-mu4e-alert-modeline)


;;
;; `gnus' notifications
;;

(defvar doom-aiern-modeline--gnus-unread-mail 0)
(defvar doom-aiern-modeline--gnus-started nil
  "Used to determine if gnus has started.")
(defun doom-aiern-modeline-update-gnus-status (&rest _)
  "Get the total number of unread news of gnus group."
  (setq doom-aiern-modeline--gnus-unread-mail
        (when (and doom-aiern-modeline-gnus
                   doom-aiern-modeline--gnus-started)
          (let ((total-unread-news-number 0))
            (mapc (lambda (g)
                    (let* ((group (car g))
                           (unread (eval `(gnus-group-unread ,group))))
                      (when (and (not (seq-contains-p doom-aiern-modeline-gnus-excluded-groups group))
                                 (numberp unread)
                                 (> unread 0))
                        (setq total-unread-news-number (+ total-unread-news-number unread)))))
                  gnus-newsrc-alist)
            total-unread-news-number))))

;; Update the modeline after changes have been made
(add-hook 'gnus-group-update-hook #'doom-aiern-modeline-update-gnus-status)
(add-hook 'gnus-summary-update-hook #'doom-aiern-modeline-update-gnus-status)
(add-hook 'gnus-group-update-group-hook #'doom-aiern-modeline-update-gnus-status)
(add-hook 'gnus-after-getting-new-news-hook #'doom-aiern-modeline-update-gnus-status)

;; Only start to listen to gnus when gnus is actually running
(defun doom-aiern-modeline-start-gnus-listener ()
  "Start GNUS listener."
  (when (and doom-aiern-modeline-gnus
             (not doom-aiern-modeline--gnus-started))
    (setq doom-aiern-modeline--gnus-started t)
    ;; Scan gnus in the background if the timer is higher than 0
    (doom-aiern-modeline-update-gnus-status)
    (if (> doom-aiern-modeline-gnus-timer 0)
        (gnus-demon-add-handler 'gnus-demon-scan-news doom-aiern-modeline-gnus-timer nil))))
(add-hook 'gnus-started-hook #'doom-aiern-modeline-start-gnus-listener)

;; Stop the listener if gnus isn't running
(defun doom-aiern-modeline-stop-gnus-listener ()
  "Stop GNUS listener."
  (setq doom-aiern-modeline--gnus-started nil))
(add-hook 'gnus-exit-gnus-hook #'doom-aiern-modeline-stop-gnus-listener)

(doom-aiern-modeline-def-segment gnus
  "Show notifications of any unread emails in `gnus'."
  (when (and (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p)
             doom-aiern-modeline-gnus
             doom-aiern-modeline--gnus-started
             ;; Don't display if the unread mails count is zero
             (numberp doom-aiern-modeline--gnus-unread-mail)
             (> doom-aiern-modeline--gnus-unread-mail 0))
    (concat
     (doom-aiern-modeline-spc)
     (propertize
      (concat
       (doom-aiern-modeline-icon 'material "email" "📧" "#"
                           :face 'doom-aiern-modeline-notification
                           :height 1.1 :v-adjust -0.2)
       (doom-aiern-modeline-vspc)
       (propertize
        (if (> doom-aiern-modeline--gnus-unread-mail doom-aiern-modeline-number-limit)
            (format "%d+" doom-aiern-modeline-number-limit)
          (number-to-string doom-aiern-modeline--gnus-unread-mail))
        'face '(:inherit
                (doom-aiern-modeline-unread-number doom-aiern-modeline-notification))))
      'mouse-face 'mode-line-highlight
      'help-echo (if (= doom-aiern-modeline--gnus-unread-mail 1)
                     "You have an unread email"
                   (format "You have %s unread emails" doom-aiern-modeline--gnus-unread-mail)))
     (doom-aiern-modeline-spc))))


;;
;; IRC notifications
;;

(defun doom-aiern-modeline--shorten-irc (name)
  "Wrapper for `tracking-shorten' and `erc-track-shorten-function' with NAME.

One key difference is that when `tracking-shorten' and
`erc-track-shorten-function' returns nil we will instead return the original
value of name. This is necessary in cases where the user has stylized the name
to be an icon and we don't want to remove that so we just return the original."
  (or (and (boundp 'tracking-shorten)
           (car (tracking-shorten (list name))))
      (and (boundp 'erc-track-shorten-function)
           (functionp erc-track-shorten-function)
	       (car (funcall erc-track-shorten-function (list name))))
      (and (boundp 'rcirc-short-buffer-name)
           (rcirc-short-buffer-name name))
      name))

(defun doom-aiern-modeline--tracking-buffers (buffers)
  "Logic to convert some irc BUFFERS to their font-awesome icon."
  (mapconcat
   (lambda (b)
     (propertize
      (doom-aiern-modeline--shorten-irc (funcall doom-aiern-modeline-irc-stylize b))
      'face '(:inherit (doom-aiern-modeline-unread-number doom-aiern-modeline-notification))
      'help-echo (format "IRC Notification: %s\nmouse-1: Switch to buffer" b)
      'mouse-face 'mode-line-highlight
      'local-map (make-mode-line-mouse-map 'mouse-1
                                           (lambda ()
                                             (interactive)
                                             (when (buffer-live-p (get-buffer b))
                                               (switch-to-buffer b))))))
   buffers
   (doom-aiern-modeline-vspc)))

(defun doom-aiern-modeline--circe-p ()
  "Check if `circe' is in use."
  (boundp 'tracking-mode-line-buffers))

(defun doom-aiern-modeline--erc-p ()
  "Check if `erc' is in use."
  (boundp 'erc-modified-channels-alist))

(defun doom-aiern-modeline--rcirc-p ()
  "Check if `rcirc' is in use."
  (bound-and-true-p rcirc-track-minor-mode))

(defun doom-aiern-modeline--get-buffers ()
  "Gets the buffers that have activity."
  (cond
   ((doom-aiern-modeline--circe-p)
    tracking-buffers)
   ((doom-aiern-modeline--erc-p)
    (mapcar (lambda (l)
              (buffer-name (car l)))
            erc-modified-channels-alist))
   ((doom-aiern-modeline--rcirc-p)
    (mapcar (lambda (b)
              (buffer-name b))
            rcirc-activity))))

;; Create a modeline segment that contains all the irc tracked buffers
(doom-aiern-modeline-def-segment irc-buffers
  "The list of shortened, unread irc buffers."
  (when (and doom-aiern-modeline-irc
             (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p))
    (let* ((buffers (doom-aiern-modeline--get-buffers))
           (number (length buffers)))
      (when (> number 0)
        (concat
         (doom-aiern-modeline-spc)
         (doom-aiern-modeline--tracking-buffers buffers)
         (doom-aiern-modeline-spc))))))

(doom-aiern-modeline-def-segment irc
  "A notification icon for any unread irc buffer."
  (when (and doom-aiern-modeline-irc
             (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p))
    (let* ((buffers (doom-aiern-modeline--get-buffers))
           (number (length buffers)))
      (when (> number 0)
        (concat
         (doom-aiern-modeline-spc)

         (propertize (concat
                      (doom-aiern-modeline-icon 'material "message" "🗊" "#"
                                          :face 'doom-aiern-modeline-notification
                                          :height 1.0 :v-adjust -0.225)
                      (doom-aiern-modeline-vspc)
                      ;; Display the number of unread buffers
                      (propertize (number-to-string number)
                                  'face '(:inherit
                                          (doom-aiern-modeline-unread-number
                                           doom-aiern-modeline-notification))))
                     'help-echo (format "IRC Notifications: %s\n%s"
                                        (mapconcat
                                         (lambda (b) (funcall doom-aiern-modeline-irc-stylize b))
                                         buffers
                                         ", ")
                                        (cond
                                         ((doom-aiern-modeline--circe-p)
                                          "mouse-1: Switch to previous unread buffer
mouse-3: Switch to next unread buffer")
                                         ((doom-aiern-modeline--erc-p)
                                          "mouse-1: Switch to buffer
mouse-3: Switch to next unread buffer")
                                         ((doom-aiern-modeline--rcirc-p)
                                          "mouse-1: Switch to server buffer
mouse-3: Switch to next unread buffer")))
                     'mouse-face 'mode-line-highlight
                     'local-map (let ((map (make-sparse-keymap)))
                                  (cond
                                   ((doom-aiern-modeline--circe-p)
                                    (define-key map [mode-line mouse-1]
                                      #'tracking-previous-buffer)
                                    (define-key map [mode-line mouse-3]
                                      #'tracking-next-buffer))
                                   ((doom-aiern-modeline--erc-p)
                                    (define-key map [mode-line mouse-1]
                                      #'erc-switch-to-buffer)
                                    (define-key map [mode-line mouse-3]
                                      #'erc-track-switch-buffer))
                                   ((doom-aiern-modeline--rcirc-p)
                                    (define-key map [mode-line mouse-1]
                                      #'rcirc-switch-to-server-buffer)
                                    (define-key map [mode-line mouse-3]
                                      #'rcirc-next-active-buffer)))
                                  map))

         ;; Display the unread irc buffers as well
         (when doom-aiern-modeline-irc-buffers
           (concat (doom-aiern-modeline-spc)
                   (doom-aiern-modeline--tracking-buffers buffers)))

         (doom-aiern-modeline-spc))))))

(defun doom-aiern-modeline-override-rcirc-modeline ()
  "Override default `rcirc' mode-line."
  (if (bound-and-true-p doom-aiern-modeline-mode)
      (setq global-mode-string
		    (delq 'rcirc-activity-string global-mode-string))
    (when (and rcirc-track-minor-mode
               (not (memq 'rcirc-activity-string global-mode-string)))
	  (setq global-mode-string
		    (append global-mode-string '(rcirc-activity-string))))))
(add-hook 'rcirc-track-minor-mode-hook #'doom-aiern-modeline-override-rcirc-modeline)
(add-hook 'doom-aiern-modeline-mode-hook #'doom-aiern-modeline-override-rcirc-modeline)


;;
;; Battery status
;;

(defvar doom-aiern-modeline--battery-status nil)
(defun doom-aiern-modeline-update-battery-status ()
  "Update battery status."
  (setq doom-aiern-modeline--battery-status
        (when (bound-and-true-p display-battery-mode)
          (let* ((data (and battery-status-function
                            (functionp battery-status-function)
                            (funcall battery-status-function)))
                 (charging? (string-equal "AC" (cdr (assoc ?L data))))
                 (percentage (car (read-from-string (or (cdr (assq ?p data)) "ERR"))))
                 (valid-percentage? (and (numberp percentage)
                                         (>= percentage 0)
                                         (<= percentage battery-mode-line-limit)))
                 (face (if valid-percentage?
                           (cond (charging? 'doom-aiern-modeline-battery-charging)
                                 ((< percentage battery-load-critical) 'doom-aiern-modeline-battery-critical)
                                 ((< percentage 25) 'doom-aiern-modeline-battery-warning)
                                 ((< percentage 95) 'doom-aiern-modeline-battery-normal)
                                 (t 'doom-aiern-modeline-battery-full))
                         'doom-aiern-modeline-battery-error))
                 (icon (if valid-percentage?
                           (cond (charging?
                                  (doom-aiern-modeline-icon 'alltheicon "battery-charging" "🔋" "+"
                                                      :face face :height 1.4 :v-adjust -0.1))
                                 ((> percentage 95)
                                  (doom-aiern-modeline-icon 'faicon "battery-full" "🔋" "-"
                                                      :face face :v-adjust -0.0575))
                                 ((> percentage 70)
                                  (doom-aiern-modeline-icon 'faicon "battery-three-quarters" "🔋" "-"
                                                      :face face :v-adjust -0.0575))
                                 ((> percentage 40)
                                  (doom-aiern-modeline-icon 'faicon "battery-half" "🔋" "-"
                                                      :face face :v-adjust -0.0575))
                                 ((> percentage battery-load-critical)
                                  (doom-aiern-modeline-icon 'faicon "battery-quarter" "🔋" "-"
                                                      :face face :v-adjust -0.0575))
                                 (t (doom-aiern-modeline-icon 'faicon "battery-empty" "🔋" "!"
                                                        :face face :v-adjust -0.0575)))
                         (doom-aiern-modeline-icon 'faicon "battery-empty" "⚠" "N/A"
                                             :face face :v-adjust -0.0575)))
                 (text (if valid-percentage? (format "%d%%%%" percentage) ""))
                 (help-echo (if (and battery-echo-area-format data valid-percentage?)
                                (battery-format battery-echo-area-format data)
                              "Battery status not available")))
            (cons (propertize icon 'help-echo help-echo)
                  (propertize text 'face face 'help-echo help-echo))))))

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-aiern-modeline-update-battery-status))))))

(doom-aiern-modeline-add-variable-watcher
 'doom-aiern-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-aiern-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-aiern-modeline-update-battery-status))))))

(doom-aiern-modeline-def-segment battery
  "Display battery status."
  (when (and (doom-aiern-modeline--active)
             (not doom-aiern-modeline--limited-width-p)
             (bound-and-true-p display-battery-mode))
    (concat (doom-aiern-modeline-spc)
            (concat
             (car doom-aiern-modeline--battery-status)
             (doom-aiern-modeline-vspc)
             (cdr doom-aiern-modeline--battery-status))
            (doom-aiern-modeline-spc))))

(defun doom-aiern-modeline-override-battery-modeline ()
  "Override default battery mode-line."
  (if (bound-and-true-p doom-aiern-modeline-mode)
      (progn
        (advice-add #'battery-update :override #'doom-aiern-modeline-update-battery-status)
        (setq global-mode-string
		      (delq 'battery-mode-line-string global-mode-string))
        (and (bound-and-true-p display-battery-mode) (battery-update)))
    (progn
      (advice-remove #'battery-update #'doom-aiern-modeline-update-battery-status)
      (when (and display-battery-mode battery-status-function battery-mode-line-format
                 (not (memq 'battery-mode-line-string global-mode-string)))
        (setq global-mode-string
		      (append global-mode-string '(battery-mode-line-string)))))))
(add-hook 'display-battery-mode-hook #'doom-aiern-modeline-override-battery-modeline)
(add-hook 'doom-aiern-modeline-mode-hook #'doom-aiern-modeline-override-battery-modeline)


;;
;; Package information
;;

(doom-aiern-modeline-def-segment package
  "Show package information via `paradox'."
  (let ((active (doom-aiern-modeline--active)))
    (concat
     (let ((front (format-mode-line 'mode-line-front-space)))
       (if active
           front
         (propertize front 'face 'mode-line-inactive)))

     (when (and doom-aiern-modeline-icon doom-aiern-modeline-major-mode-icon)
       (concat
        (doom-aiern-modeline-spc)
        (doom-aiern-modeline-icon 'faicon "archive" nil nil
                            :face (if active
                                      (if doom-aiern-modeline-major-mode-color-icon
                                          'all-the-icons-silver
                                        'mode-line)
                                    'mode-line-inactive)
                            :height 1.0
                            :v-adjust -0.0575)))
     (let ((info (format-mode-line 'mode-line-buffer-identification)))
       (if active
           info
         (propertize info 'face 'mode-line-inactive))))))


;;
;; Helm
;;

(defvar doom-aiern-modeline--helm-buffer-ids
  '(("*helm*" . "HELM")
    ("*helm M-x*" . "HELM M-x")
    ("*swiper*" . "SWIPER")
    ("*Projectile Perspectives*" . "HELM Projectile Perspectives")
    ("*Projectile Layouts*" . "HELM Projectile Layouts")
    ("*helm-ag*" . (lambda ()
                     (format "HELM Ag: Using %s"
                             (car (split-string helm-ag-base-command))))))
  "Alist of custom helm buffer names to use.
The cdr can also be a function that returns a name to use.")

(doom-aiern-modeline-def-segment helm-buffer-id
  "Helm session identifier."
  (when (bound-and-true-p helm-alive-p)
    (let ((active (doom-aiern-modeline--active)))
      (concat
       (doom-aiern-modeline-spc)
       (when doom-aiern-modeline-icon
         (concat
          (doom-aiern-modeline-icon 'fileicon "elisp" nil nil
                              :face (if active
                                        (if doom-aiern-modeline-major-mode-color-icon
                                            'all-the-icons-blue
                                          'mode-line)
                                      'mode-line-inactive)
                              :height 1.0
                              :v-adjust -0.15)
          (doom-aiern-modeline-spc)))
       (propertize
        (let ((custom (cdr (assoc (buffer-name) doom-aiern-modeline--helm-buffer-ids)))
              (case-fold-search t)
              (name (replace-regexp-in-string "-" " " (buffer-name))))
          (cond ((stringp custom) custom)
                ((functionp custom) (funcall custom))
                (t
                 (string-match "\\*helm:? \\(mode \\)?\\([^\\*]+\\)\\*" name)
                 (concat "HELM " (capitalize (match-string 2 name))))))
        'face (if active' doom-aiern-modeline-buffer-file 'mode-line-inactive))
       (doom-aiern-modeline-spc)))))

(doom-aiern-modeline-def-segment helm-number
  "Number of helm candidates."
  (when (bound-and-true-p helm-alive-p)
    (let ((active (doom-aiern-modeline--active)))
      (concat
       (propertize (format " %d/%d"
                           (helm-candidate-number-at-point)
                           (helm-get-candidate-number t))
                   'face (if active 'doom-aiern-modeline-buffer-path 'mode-line-inactive))
       (propertize (format " (%d total) " (helm-get-candidate-number))
                   'face (if active 'doom-aiern-modeline-info 'mode-line-inactive))))))

(doom-aiern-modeline-def-segment helm-help
  "Helm keybindings help."
  (when (bound-and-true-p helm-alive-p)
    (let ((active (doom-aiern-modeline--active)))
      (-interleave
       (mapcar (lambda (s)
                 (propertize (substitute-command-keys s)
                             'face (if active
                                       'doom-aiern-modeline-buffer-file
                                     'mode-line-inactive)))
               '("\\<helm-map>\\[helm-help]"
                 "\\<helm-map>\\[helm-select-action]"
                 "\\<helm-map>\\[helm-maybe-exit-minibuffer]/F1/F2..."))
       (mapcar (lambda (s)
                 (propertize s 'face (if active 'mode-line 'mode-line-inactive)))
               '("(help) " "(actions) " "(action) "))))))

(doom-aiern-modeline-def-segment helm-prefix-argument
  "Helm prefix argument."
  (when (and (bound-and-true-p helm-alive-p)
             helm--mode-line-display-prefarg)
    (let ((arg (prefix-numeric-value (or prefix-arg current-prefix-arg))))
      (unless (= arg 1)
        (propertize (format "C-u %s" arg)
                    'face (if (doom-aiern-modeline--active)
                              'doom-aiern-modeline-info
                            'mode-line-inactive))))))

(defvar doom-aiern-modeline--helm-current-source nil
  "The currently active helm source.")
(doom-aiern-modeline-def-segment helm-follow
  "Helm follow indicator."
  (when (and (bound-and-true-p helm-alive-p)
             doom-aiern-modeline--helm-current-source
             (eq 1 (cdr (assq 'follow doom-aiern-modeline--helm-current-source))))
    (propertize "HF" 'face (if (doom-aiern-modeline--active)
                               'mode-line
                             'mode-line-inactive))))

;;
;; Git timemachine
;;

(doom-aiern-modeline-def-segment git-timemachine
  (let ((active (doom-aiern-modeline--active)))
    (concat
     (doom-aiern-modeline-spc)
     (doom-aiern-modeline--buffer-mode-icon)
     ;; Snapshot icon
     (doom-aiern-modeline-icon 'material "camera_alt" "📷" "%1*"
                         :face (if active
                                   '(:inherit doom-aiern-modeline-warning :weight normal)
                                 'mode-line-inactive)
                         :height 1.1 :v-adjust -0.25)
     (and doom-aiern-modeline-icon (doom-aiern-modeline-vspc))
     ;; Buffer name
     (propertize "*%b*" 'face (if active
                                  'doom-aiern-modeline-buffer-timemachine
                                'mode-line-inactive)))))

;;
;; Markdown/Org preview
;;

(doom-aiern-modeline-def-segment grip
  (when (bound-and-true-p grip-mode)
    (concat
     (doom-aiern-modeline-spc)
     (let ((face (if (doom-aiern-modeline--active)
                     (if grip--process
                         (pcase (process-status grip--process)
                           ('run 'doom-aiern-modeline-buffer-path)
                           ('exit 'doom-aiern-modeline-warning)
                           (_ 'doom-aiern-modeline-urgent))
                       'doom-aiern-modeline-urgent)
                   'mode-line-inactive)))
       (propertize (doom-aiern-modeline-icon 'material "pageview" "🗐" "@"
                                       :face (if doom-aiern-modeline-icon
                                                 `(:inherit ,face :weight normal)
                                               face)
                                       :height 1.2 :v-adjust -0.2)
                   'help-echo (format "Preview on %s
mouse-1: Preview in browser
mouse-2: Stop preview
mouse-3: Restart preview"
                                      (grip--preview-url))
                   'mouse-face 'mode-line-highlight
                   'local-map (let ((map (make-sparse-keymap)))
                                (define-key map [mode-line mouse-1]
                                  #'grip-browse-preview)
                                (define-key map [mode-line mouse-2]
                                  #'grip-stop-preview)
                                (define-key map [mode-line mouse-3]
                                  #'grip-restart-preview)
                                map)))
     (doom-aiern-modeline-spc))))

(provide 'doom-aiern-modeline-segments)

;;; doom-aiern-modeline-segments.el ends here
