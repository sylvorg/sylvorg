;;; janus.el --- a simple package                     -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'meq)

(defcustom meq/var/current-theme nil "The default theme.")
(defcustom meq/var/current-theme-mode nil "The default theme mode.")
(defvar meq/var/aliases '(:orange (orange flamingo-pink)))
(defvar meq/var/modes '(:light nil :dark nil))
(defvar meq/var/rainbow-identifiers-saturation-light 50)
(defvar meq/var/rainbow-identifiers-saturation-dark 30)
(setq rainbow-identifiers-cie-l*a*b*-color-count most-positive-fixnum)
(setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)

(mapc #'(lambda (color) (interactive)
    (mapc #'(lambda (alias) (interactive)
        (push alias (cl-getf meq/var/modes :light))) (cl-getf meq/var/aliases color)))
    '(:orange))

(mapc #'(lambda (color) (interactive)
    (mapc #'(lambda (alias) (interactive)
        (push alias (cl-getf meq/var/modes :dark))) (cl-getf meq/var/aliases color)))
    '())

(defvar meq/var/faces `(
    ;; Adapted From: http://ergoemacs.org/emacs/elisp_define_face.html
    (flamingo-pink . (
                    ;; :alternate ((((class color) (background light))
                    ;;                 :foreground "#ab5dee" :bold t)
                    ;;                 (((class color) (background dark))
                    ;;                 :foreground "#fca78e" :bold t))
                    :alternate ((t (:foreground "#ab5dee" :bold t)))
                    :original ((t (:foreground "#fca78e" :bold t)))
                    :aliases ,(cl-getf meq/var/aliases :orange)))
    (orange . (
                    ;; :alternate ((((class color) (background light))
                    ;;                 :foreground "#ab5dee" :bold t)
                    ;;                 (((class color) (background dark))
                    ;;                 :foreground "#ffb86c" :bold t))
                    :alternate ((t (:foreground "#ab5dee" :bold t)))
                    :original ((t (:foreground "#ffb86c" :bold t)))
                    :aliases ,(cl-getf meq/var/aliases :orange)))))

;;;###autoload
(defmacro meq/set-alternate-color (color) (interactive)
    (face-spec-set
        (meq/inconcat "meq/" (symbol-name color))
        (cl-getf (cdr (assq color meq/var/faces)) :alternate)
        'face-defface-spec))

;;;###autoload
(defmacro meq/set-original-color (color) (interactive)
    (face-spec-set
        (meq/inconcat "meq/" (symbol-name color))
        (cl-getf (cdr (assq color meq/var/faces)) :original)
        'face-defface-spec))

;;;###autoload
(defun meq/same-color-switch (name mode) (interactive)
    (mapc #'(lambda (color) (interactive)
        (let* ((contains-list (mapcar #'(lambda (alias) (interactive)
            (and
                (s-contains? (symbol-name alias) name)
                (member alias (cl-getf
                    meq/var/modes
                    (meq/inconcat ":" mode))))) (cl-getf (cdr color) :aliases))))
        (if (--any? (and it t) contains-list)
            (eval `(meq/set-alternate-color ,(car color)))
            (eval `(meq/set-original-color ,(car color)))))) meq/var/faces))

;; (mapc #'(lambda (color) (interactive)
;;     (eval `(defface
;;         ,(meq/inconcat "meq/" (symbol-name (car color)))
;;         ',(cl-getf (cdr color)  :original)
;;         ,(symbol-name (car color))))) meq/var/faces)

(mapc #'(lambda (color) (interactive)
    `(face-spec-set
        ,(meq/inconcat "meq/" (symbol-name (car color)))
        ',(cl-getf (cdr color) :original)
        'face-defface-spec)) meq/var/faces)

;;;###autoload
(defun meq/first-char-numeric-p (hex)
    (s-with hex
        (s-chop-prefix "#")
        (s-left 1)
        (s-numeric?)))

(defmacro meq/rainbow-conditions (a b)
    (and
        (string= meq/var/current-theme-mode (symbol-name a))
        (eval `(=
                rainbow-identifiers-cie-l*a*b*-saturation
                ,(meq/inconcat "meq/var/rainbow-identifiers-" (symbol-name b))))))

;;;###autoload
(defun meq/load-theme (theme) (interactive)
    (let* ((name (symbol-name theme))
            (mode (car (last (split-string name "-"))))
            (rmode (meq/inconcat "meq/var/rainbow-identifiers-saturation-" mode)))

        ;; Adapted From:
        ;; Answer: https://stackoverflow.com/a/18552615/10827766
        ;; User: https://stackoverflow.com/users/729907/drew
        (save-excursion (setq meq/var/current-theme theme)
        (customize-save-variable 'meq/var/current-theme theme)
        (setq meq/var/current-theme-mode mode)
        (customize-save-variable 'meq/var/current-theme-mode mode))

        (meq/same-color-switch name mode)
        (load-theme theme)        
        (with-eval-after-load 'rainbow-identifiers
            (rainbow-identifiers-mode 0)
            (if (string= meq/var/current-theme-mode "dark")
                
                ;; Adapted From:
                ;; Answer: https://emacs.stackexchange.com/a/29184/31428
                ;; User: https://emacs.stackexchange.com/users/897/lindydancer
                (if (meq/first-char-numeric-p (face-attribute 'default :background))
                
                    (eval `(setq rainbow-identifiers-cie-l*a*b*-saturation ,rmode))
                    (setq rainbow-identifiers-cie-l*a*b*-saturation 275))
                (eval `(setq rainbow-identifiers-cie-l*a*b*-saturation ,rmode)))
            (rainbow-identifiers-mode 1))))

;;;###autoload
(defun meq/which-theme nil (interactive)
    (meq/when-item-in-cla "--theme"
        (let* ((name (meq/get-next-in-cla "--theme")))
            (meq/load-theme (intern (concat
                name
                (if (meq/item-in-cla "--light") "-light" "-dark"))))
            (delete "--dark" command-line-args))))

;;;###autoload
(defun meq/switch-theme-mode nil (interactive)
    (meq/load-theme (intern (concat
        (replace-regexp-in-string "-dark" "" (replace-regexp-in-string "-light" "" (symbol-name meq/var/current-theme)))
        "-"
        (if (string= meq/var/current-theme-mode "light") "dark" "light")))))

(provide 'janus)
;;; janus.el ends here
