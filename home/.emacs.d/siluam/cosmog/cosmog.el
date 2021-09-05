;;; cosmog.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jeet Ray

;; Author: Jeet Ray <aiern@protonmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

(require 'meq)
(require 'org)
(require 'dash)
(require 'f)
(require 'deino)
(require 'alloy)

(let* ((dir (meq/ued ".cosmog.")))
    (unless (file-directory-p dir) (mkdir dir)))

(defvar meq/var/cosmog-nibs-dir (meq/ued ".cosmog."))
(defvar meq/var/cosmog-nibs (meq/remove-dot-dirs (directory-files meq/var/cosmog-nibs-dir)))
(defvar meq/var/cosmog-update-timer nil)
(defvar meq/var/cosmog-update-time "0 min 5 sec")
(defvar meq/var/cosmog-idle-timer nil)
(defvar meq/var/cosmog-idle-time 5)

;;;###autoload
(defun meq/add-to-cosmog-nibs (nib &optional no-find-file)
    (unless no-find-file (find-file-noselect nib))
    (unless (or (string= nib ".") (string= nib "..")) (push nib meq/var/cosmog-nibs)))

;;;###autoload
(defun meq/create-cosmog-nib (&optional nib*) (interactive)
    (let* ((nib (or nib* (concat meq/var/cosmog-nibs-dir "/.cosmog." (meq/timestamp) "."))))
        (find-file nib) (org-mode) (meq/add-to-cosmog-nibs nib t)))

;;;###autoload
(defun meq/cosmog-nib-p nil (interactive) (meq/substring ".cosmog." (buffer-file-name)))

;;;###autoload
(defmacro meq/when-cosmog-nib (&rest args)
    `(if (not (meq/cosmog-nib-p))
        (error "Buffer '%s' is not a .cosmog. nib!" (buffer-file-name))
        ,@args))

;;;###autoload
(defun meq/delete-cosmog-nib nil (interactive)
    (meq/when-cosmog-nib
        (delete (buffer-file-name) meq/var/cosmog-nibs)
        (meq/delete-current-buffer-file)))

;;;###autoload
(defun meq/rename-cosmog-nib nil (interactive)
    (meq/when-cosmog-nib
        (delete (buffer-file-name) meq/var/cosmog-nibs)
        (meq/rename-current-buffer-file)))

;;;###autoload
(defun meq/update-cosmog-nibs (&optional nibs*) (interactive)
    (let* ((nibs (meq/remove-dot-dirs (or nibs* meq/var/cosmog-nibs))))
        (mapc #'(lambda (nib) (interactive)
            (let* ((nib-buffer (get-file-buffer nib))) (when (and
                                                                (buffer-modified-p nib-buffer)
                                                                (meq/cosmog-nib-p))
                (save-buffer nib-buffer)))) nibs)))

;;;###autoload
(defun meq/cancel-cosmog-update-timer nil (interactive) (cancel-timer meq/var/cosmog-update-timer))
;;;###autoload
(defun meq/set-cosmog-update-timer nil (interactive)
    (meq/update-cosmog-nibs)
    (if (current-idle-time)
        (setq meq/var/cosmog-update-timer
            `(run-at-time ,meq/var/cosmog-update-time t #'meq/update-cosmog-nibs))
        (meq/cancel-cosmog-update-timer)))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/10775/31428
;; User: https://emacs.stackexchange.com/users/454/phils
;;;###autoload
(run-with-idle-timer meq/var/cosmog-idle-time t #'meq/set-cosmog-update-timer)

;;;###autoload
(defun meq/load-nibs (&optional nibs*) (interactive)
    (let* ((nibs (meq/remove-dot-dirs (or nibs* meq/var/cosmog-nibs))))
        ;; Adapted From:
        ;; Asnwer: https://emacs.stackexchange.com/a/9590/31428
        ;; User: https://emacs.stackexchange.com/users/4085/colin-fraizer
        (mapc #'meq/add-to-cosmog-nibs nibs)))

;;;###autoload
(add-hook 'emacs-startup-hook #'meq/load-nibs)

(defdeino deino-cosmog (:color blue) "; c c"
    ("`" nil "cancel")
    ("c" meq/create-cosmog-nib "create")
    ("d" meq/delete-cosmog-nib "delete" :color red)
    ("r" meq/rename-cosmog-nib "rename")
    ("t" deino-cosmog/timer/body "timers"))
(defdeino deino-cosmog/timer (:color blue) "; c t"
    ("`" nil "quit")
    ("c" meq/cancel-cosmog-update-timer "cancel")
    ("s" meq/set-cosmog-update-timer "set"))
(defdeinor+ "; c" "cosmog")

(provide 'cosmog)
;;; cosmog.el ends here
