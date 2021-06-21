;;; aiern-development.el --- Useful features for aiern developers -*- lexical-binding: t -*-

;; Author: Justin Burkett <justin at burkett dot cc>

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

;;; Code:

;;; Teach imenu about aiern macros

(with-eval-after-load 'lisp-mode
  (when (boundp 'lisp-imenu-generic-expression)
    (dolist (macro '("interactive-code"
                     "type"
                     "text-object"
                     "motion"
                     "command"
                     "operator"))
      (let ((macro-name (format "aiern-%s" macro)))
        (unless (assoc macro-name lisp-imenu-generic-expression)
          (push (list
                 macro-name
                 (format "^\\s-*(aiern-define-%s\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
                         macro)
                 1)
                lisp-imenu-generic-expression))))))

(provide 'aiern-development)

;;; aiern-development.el ends here
