;;; lode.el --- a simple package                     -*- lexical-binding: t; -*-

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

(require 'deino)
(require 'alloy)
(require 'dash)
(require 'meq)
(require 'cl-lib)

;;;###autoload
(defmacro lode* (parent alloy ryo-key key* func &rest keychain)
    (let* ((last-head (= (-count 'keywordp keychain) 1))
            (open-keychain (-partition-before-pred #'keywordp keychain))
            (current-keychain (car open-keychain))
            (next-keychain (unless last-head (cadr open-keychain)))
            (current-name (concat
                (if parent (concat parent "/") "")
                (deino--replace-key (meq/keyword-to-symbol-name (car current-keychain)))))
            (deino-name (concat "lodestar/" current-name))
            (deino-plus (fboundp (meq/inconcat deino-name "/body")))
            (key (if (stringp key*) key* (symbol-name key*)))
            (next-head (if last-head
                `(,key ,func ,(symbol-name func))
                `(,(meq/keyword-to-symbol-name (car next-keychain))

                    ;; Another lode* call
                    ,(eval `(lode* ,current-name nil nil ,key ,func ,@(-flatten-n 1 (cdr open-keychain))))

                    :color blue)))
            (head-list (cdr current-keychain))
            (default-settings nil)
            (settings-list (let* ((fhh (caar head-list)))
                            (if (or (keywordp fhh) (keymapp fhh))
                                (pop head-list)
                                default-settings))))

        (push next-head head-list)

        (when alloy (eval `(alloy-def ,@alloy 'lodestar/a/body)))

        ;; Adapted From: https://github.com/abo-abo/deino/issues/164#issuecomment-136650511
        `(,(meq/inconcat "defdeino" (when deino-plus "+"))
            ,(intern deino-name)
            ,settings-list
            ,@(if deino-plus (list nil) '((:color blue) nil ("`" nil "cancel")))
            ,@head-list)))

;;;###autoload
(defmacro lodestar (key func &rest keychain)
    `(lode* nil nil ,key ,func ,@keychain))
;;;###autoload
(defmacro lodemaps (alloy key func &rest keychain)
    `(lode* nil ,alloy ,key ,func ,@keychain))
;;;###autoload
(defmacro lodemon (alloy key func &rest keychain)
    `(lode*
        nil
        (:keymaps demon-run ,@alloy)
        ,key
        ,func
        ,@keychain))

;; Adapted From: https://github.com/noctuid/general.el/blob/master/general.el#L2708
;;;###autoload
(defun use-package-handler/:lodestar (name _keyword arglists rest state)
"Use-package handler for :lodestar."
(use-package-concat
    (use-package-process-keywords name rest state)
    `(,@(mapcar (lambda (arglist) arglist `(lodestar ,@arglist)) arglists))))

;;;###autoload
(defalias 'use-package-autoloads/:lodestar #'use-package-autoloads/:ghook)
;;;###autoload
(defalias 'use-package-normalize/:lodestar #'use-package-normalize/:ghook)

;; Adapted From: https://github.com/noctuid/general.el/blob/master/general.el#L2708
;;;###autoload
(defun use-package-handler/:lodemaps (name _keyword arglists rest state)
"Use-package handler for :lodemaps."
(use-package-concat
    (use-package-process-keywords name rest state)
    `(,@(mapcar (lambda (arglist) arglist `(lodemaps ,@arglist)) arglists))))

;;;###autoload
(defalias 'use-package-autoloads/:lodemaps #'use-package-autoloads/:ghook)
;;;###autoload
(defalias 'use-package-normalize/:lodemaps #'use-package-normalize/:ghook)

;; Adapted From: https://github.com/noctuid/general.el/blob/master/general.el#L2708
;;;###autoload
(defun use-package-handler/:lodemon (name _keyword arglists rest state)
"Use-package handler for :lodemon."
(use-package-concat
    (use-package-process-keywords name rest state)
    `(,@(mapcar (lambda (arglist) arglist `(lodemon ,@arglist)) arglists))))

;;;###autoload
(defalias 'use-package-autoloads/:lodemon #'use-package-autoloads/:ghook)
;;;###autoload
(defalias 'use-package-normalize/:lodemon #'use-package-normalize/:ghook)

;; Adapted From: https://github.com/noctuid/general.el/blob/master/general.el#L2554
(setq use-package-keywords
    ;; should go in the same location as :bind
    ;; adding to end may not cause problems, but see issue #22
    (cl-loop for item in use-package-keywords
                if (eq item :bind-keymap*)
                collect :bind-keymap* and
                collect :lodestar and
                collect :lodemaps and
                collect :lodemon
                else
                ;; don't add duplicates
                unless (memq item '(:lodestar :lodemaps :lodemon))
                collect item))

(provide 'lode)
;;; lode.el ends here
