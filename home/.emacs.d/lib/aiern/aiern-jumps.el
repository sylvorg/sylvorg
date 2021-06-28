;;; aiern-jumps.el --- Jump list implementation -*- lexical-binding: t -*-

;; Author: Bailey Ling <bling at live.ca>

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

(require 'cl-lib)
(require 'aiern-core)
(require 'aiern-states)

;;; Code:

(defgroup aiern-jumps nil
  "aiern jump list configuration options."
  :prefix "aiern-jumps"
  :group 'aiern)

(defcustom aiern-jumps-cross-buffers t
  "When non-nil, the jump commands can cross borders between buffers, otherwise the jump commands act only within the current buffer."
  :type 'boolean
  :group 'aiern-jumps)

(defcustom aiern-jumps-max-length 100
  "The maximum number of jumps to keep track of."
  :type 'integer
  :group 'aiern-jumps)

(defcustom aiern-jumps-pre-jump-hook nil
  "Hooks to run just before jumping to a location in the jump list."
  :type 'hook
  :group 'aiern-jumps)

(defcustom aiern-jumps-post-jump-hook nil
  "Hooks to run just after jumping to a location in the jump list."
  :type 'hook
  :group 'aiern-jumps)

(defcustom aiern-jumps-ignored-file-patterns '("COMMIT_EDITMSG$" "TAGS$")
  "A list of pattern regexps to match on the file path to exclude from being included in the jump list."
  :type '(repeat string)
  :group 'aiern-jumps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar savehist-additional-variables)

(defvar aiern--jumps-jumping nil)

(defvar aiern--jumps-jumping-backward nil
  "Set by `aiern--jump-backward', used and cleared in the
`post-command-hook' by `aiern--jump-handle-buffer-crossing'")

(eval-when-compile (defvar aiern--jumps-debug nil))

(defvar aiern--jumps-buffer-targets "\\*\\(new\\|scratch\\)\\*"
  "Regexp to match against `buffer-name' to determine whether it's a valid jump target.")

(defvar aiern--jumps-window-jumps (make-hash-table)
  "Hashtable which stores all jumps on a per window basis.")

(defvar aiern-jumps-history nil
  "History of `aiern-mode' jumps that are persisted with `savehist'.")

(cl-defstruct aiern-jumps-struct
  ring
  (idx -1)
  previous-pos)

;; Is inlining this really worth it?
(defsubst aiern--jumps-message (format &rest args)
  (when (eval-when-compile aiern--jumps-debug)
    (with-current-buffer (get-buffer-create "*aiern-jumps*")
       (goto-char (point-max))
       (insert (apply #'format format args) "\n"))))

(defun aiern--jumps-get-current (&optional window)
  (unless window
    (setq window (frame-selected-window)))
  (let* ((jump-struct (gethash window aiern--jumps-window-jumps)))
    (unless jump-struct
      (setq jump-struct (make-aiern-jumps-struct))
      (puthash window jump-struct aiern--jumps-window-jumps))
    jump-struct))

(defun aiern--jumps-get-jumps (struct)
  (let ((ring (aiern-jumps-struct-ring struct)))
    (unless ring
      (setq ring (make-ring aiern-jumps-max-length))
      (setf (aiern-jumps-struct-ring struct) ring))
    ring))

(defun aiern--jumps-get-window-jump-list ()
  (let ((struct (aiern--jumps-get-current)))
    (aiern--jumps-get-jumps struct)))

(defun aiern--jumps-savehist-load ()
  (add-to-list 'savehist-additional-variables 'aiern-jumps-history)
  (let ((ring (make-ring aiern-jumps-max-length)))
    (cl-loop for jump in (reverse aiern-jumps-history)
             do (ring-insert ring jump))
    (setf (aiern-jumps-struct-ring (aiern--jumps-get-current)) ring))
  (add-hook 'savehist-save-hook #'aiern--jumps-savehist-sync)
  (remove-hook 'savehist-mode-hook #'aiern--jumps-savehist-load))

(defun aiern--jumps-savehist-sync ()
  "Updates the printable value of window jumps for `savehist'."
  (setq aiern-jumps-history
        (delq nil (mapcar #'(lambda (jump)
                              (let* ((mark (car jump))
                                     (pos (if (markerp mark)
                                              (marker-position mark)
                                            mark))
                                     (file-name (cadr jump)))
                                (when (and (not (file-remote-p file-name))
                                           (file-exists-p file-name)
                                           pos)
                                  (list pos file-name))))
                          (ring-elements (aiern--jumps-get-window-jump-list))))))

(defun aiern--jumps-jump (idx shift)
  (let ((target-list (aiern--jumps-get-window-jump-list)))
    (aiern--jumps-message "jumping from %s by %s" idx shift)
    (aiern--jumps-message "target list = %s" target-list)
    (setq idx (+ idx shift))
    (let* ((current-file-name (or (buffer-file-name) (buffer-name)))
           (size (ring-length target-list)))
      (unless aiern-jumps-cross-buffers
        ;; skip jump marks pointing to other buffers
        (while (and (< idx size) (>= idx 0)
                    (not (string= current-file-name (cadr (ring-ref target-list idx)))))
          (setq idx (+ idx shift))))
      (when (and (< idx size) (>= idx 0))
        ;; actual jump
        (run-hooks 'aiern-jumps-pre-jump-hook)
        (let* ((place (ring-ref target-list idx))
               (pos (car place))
               (file-name (cadr place)))
          (setq aiern--jumps-jumping t)
          (if (string-match-p aiern--jumps-buffer-targets file-name)
              (switch-to-buffer file-name)
            (find-file file-name))
          (setq aiern--jumps-jumping nil)
          (goto-char pos)
          (setf (aiern-jumps-struct-idx (aiern--jumps-get-current)) idx)
          (run-hooks 'aiern-jumps-post-jump-hook))))))

(defun aiern--jumps-push ()
  "Pushes the current cursor/file position to the jump list."
  (let ((target-list (aiern--jumps-get-window-jump-list)))
    (let ((file-name (buffer-file-name))
          (buffer-name (buffer-name))
          (current-pos (point-marker))
          (first-pos nil)
          (first-file-name nil)
          (excluded nil))
      (when (and (not file-name)
                 (string-match-p aiern--jumps-buffer-targets buffer-name))
        (setq file-name buffer-name))
      (when file-name
        (dolist (pattern aiern-jumps-ignored-file-patterns)
          (when (string-match-p pattern file-name)
            (setq excluded t)))
        (unless excluded
          (unless (ring-empty-p target-list)
            (setq first-pos (car (ring-ref target-list 0)))
            (setq first-file-name (car (cdr (ring-ref target-list 0)))))
          (unless (and (equal first-pos current-pos)
                       (equal first-file-name file-name))
            (aiern--jumps-message "pushing %s on %s" current-pos file-name)
            (ring-insert target-list `(,current-pos ,file-name))))))
    (aiern--jumps-message "%s %s"
                         (selected-window)
                         (and (not (ring-empty-p target-list))
                              (ring-ref target-list 0)))))

(aiern-define-command aiern-show-jumps ()
  "Display the contents of the jump list."
  :repeat nil
  (aiern-with-view-list
    :name "aiern-jumps"
    :mode "aiern Jump List"
    :format [("Jump" 5 nil)
             ("Marker" 8 nil)
             ("File/text" 1000 t)]
    :entries (let* ((jumps (aiern--jumps-savehist-sync))
                    (count 0))
               (cl-loop for jump in jumps
                        collect `(nil [,(number-to-string (cl-incf count))
                                       ,(number-to-string (car jump))
                                       (,(cadr jump))])))
    :select-action #'aiern--show-jumps-select-action))

(defun aiern--show-jumps-select-action (jump)
  (let ((position (string-to-number (elt jump 1)))
        (file (car (elt jump 2))))
    (kill-buffer)
    (switch-to-buffer (find-file file))
    (goto-char position)))

(defun aiern-set-jump (&optional pos)
  "Set jump point at POS.
POS defaults to point."
  (save-excursion
    (when (markerp pos)
      (set-buffer (marker-buffer pos)))

    (unless (or (region-active-p) (aiern-visual-state-p))
      (push-mark pos t))

    (unless aiern--jumps-jumping
      ;; clear out intermediary jumps when a new one is set
      (let* ((struct (aiern--jumps-get-current))
             (target-list (aiern--jumps-get-jumps struct))
             (idx (aiern-jumps-struct-idx struct)))
        (cl-loop repeat idx
                 do (ring-remove target-list))
        (setf (aiern-jumps-struct-idx struct) -1))
      (when pos
        (goto-char pos))
      (aiern--jumps-push))))

(defun aiern--jump-backward (count)
  (setq aiern--jumps-jumping-backward t)
  (let ((count (or count 1)))
    (aiern-motion-loop (nil count)
      (let* ((struct (aiern--jumps-get-current))
             (idx (aiern-jumps-struct-idx struct)))
        (aiern--jumps-message "jumping back %s" idx)
        (when (= idx -1)
          (setq idx 0)
          (setf (aiern-jumps-struct-idx struct) 0)
          (aiern--jumps-push))
        (aiern--jumps-jump idx 1)))))

(defun aiern--jump-forward (count)
  (let ((count (or count 1)))
    (aiern-motion-loop (nil count)
      (let* ((struct (aiern--jumps-get-current))
             (idx (aiern-jumps-struct-idx struct)))
        (when (= idx -1)
          (setq idx 0)
          (setf (aiern-jumps-struct-idx struct) 0)
          (aiern--jumps-push))
        (aiern--jumps-jump idx -1)))))

(defun aiern--jumps-window-configuration-hook (&rest _args)
  (let* ((window-list (window-list-1 nil nil t))
         (existing-window (selected-window))
         (new-window (previous-window)))
    (when (and (not (eq existing-window new-window))
               (> (length window-list) 1))
      (let* ((target-jump-struct (aiern--jumps-get-current new-window)))
        (if (not (ring-empty-p (aiern--jumps-get-jumps target-jump-struct)))
            (aiern--jumps-message "target window %s already has %s jumps" new-window
                                 (ring-length (aiern--jumps-get-jumps target-jump-struct)))
          (aiern--jumps-message "new target window detected; copying %s to %s" existing-window new-window)
          (let* ((source-jump-struct (aiern--jumps-get-current existing-window))
                 (source-list (aiern--jumps-get-jumps source-jump-struct)))
            (when (= (ring-length (aiern--jumps-get-jumps target-jump-struct)) 0)
              (setf (aiern-jumps-struct-previous-pos target-jump-struct) (aiern-jumps-struct-previous-pos source-jump-struct))
              (setf (aiern-jumps-struct-idx target-jump-struct) (aiern-jumps-struct-idx source-jump-struct))
              (setf (aiern-jumps-struct-ring target-jump-struct) (ring-copy source-list)))))))
    ;; delete obsolete windows
    (maphash (lambda (key _val)
               (unless (member key window-list)
                 (aiern--jumps-message "removing %s" key)
                 (remhash key aiern--jumps-window-jumps)))
             aiern--jumps-window-jumps)))

(defun aiern--jump-hook (&optional command)
  "`pre-command-hook' for aiern-jumps.
Set jump point if COMMAND has a non-nil `:jump' property. Otherwise,
save the current position in case the command being executed will
change the current buffer."
  (setq command (or command this-command))
  (if (aiern-get-command-property command :jump)
      (aiern-set-jump)
    (setf (aiern-jumps-struct-previous-pos (aiern--jumps-get-current))
          (point-marker))))

(defun aiern--jump-handle-buffer-crossing ()
  (let ((jumping-backward aiern--jumps-jumping-backward))
    (setq aiern--jumps-jumping-backward nil)
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (let* ((struct (aiern--jumps-get-current window))
               (previous-pos (aiern-jumps-struct-previous-pos struct)))
          (when previous-pos
            (setf (aiern-jumps-struct-previous-pos struct) nil)
            (if (and
                 ;; `aiern-jump-backward' (and other backward jumping
                 ;; commands) needs to be handled specially. When
                 ;; jumping backward multiple times, calling
                 ;; `aiern-set-jump' is always wrong: If you jump back
                 ;; twice and we call `aiern-set-jump' after the second
                 ;; time, we clear the forward jump list and
                 ;; `aiern--jump-forward' won't work.

                 ;; The first time you jump backward, setting a jump
                 ;; point is sometimes correct. But we don't do it
                 ;; here because this function is called after
                 ;; `aiern--jump-backward' has updated our position in
                 ;; the jump list so, again, `aiern-set-jump' would
                 ;; break `aiern--jump-forward'.
                 (not jumping-backward)
                 (let ((previous-buffer (marker-buffer previous-pos)))
                   (and previous-buffer
                        (not (eq previous-buffer (window-buffer window))))))
                (aiern-set-jump previous-pos)
              (set-marker previous-pos nil))))))))

(if (bound-and-true-p savehist-loaded)
    (aiern--jumps-savehist-load)
  (add-hook 'savehist-mode-hook #'aiern--jumps-savehist-load))

(defun aiern--jumps-install-or-uninstall ()
  (if aiern-local-mode
      (progn
        (add-hook 'pre-command-hook #'aiern--jump-hook nil t)
        (add-hook 'post-command-hook #'aiern--jump-handle-buffer-crossing nil t)
        (add-hook 'next-error-hook #'aiern-set-jump nil t)
        (add-hook 'window-configuration-change-hook #'aiern--jumps-window-configuration-hook nil t))
    (remove-hook 'pre-command-hook #'aiern--jump-hook t)
    (remove-hook 'post-command-hook #'aiern--jump-handle-buffer-crossing t)
    (remove-hook 'next-error-hook #'aiern-set-jump t)
    (remove-hook 'window-configuration-change-hook #'aiern--jumps-window-configuration-hook t)
    (aiern--jump-handle-buffer-crossing)))

(add-hook 'aiern-local-mode-hook #'aiern--jumps-install-or-uninstall)

(provide 'aiern-jumps)

;;; aiern-jumps.el ends here
