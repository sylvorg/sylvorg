;;; prime.el --- a simple package                     -*- lexical-binding: t; -*-

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

(require 'alloy)
(require 'deino)
(require 's)

(defvar primus-key ",")
(eval `(defdeino primus (:color blue) "j" ("`" nil "cancel")))
(eval `(alloy-def :keymaps demon-run (alloy-chord ,(s-repeat 2 primus-key)) 'primus/body))

(defun prime--replace-spaces (str) (s-replace " " "/" str))
(defun prime--construct-name (str) (prime--replace-spaces (concat "prime/" str)))

(defmacro prime* (parent first-call key func &optional name* &rest args)
    (let* ((ds (deino--create-dataset
                (if (stringp name*) name* (if (symbolp func) (symbol-name func) nil))
                key
                parent
                func
                #'prime--construct-name))

            (next-key (string-join (cdr (d--g ds :keys)) " "))
            (next-deino-body (if (d--g ds :two-key) func (meq/inconcat (d--g ds :next-name) "/body")))
            (next-deino-settings (when (d--g ds :two-key) args)))
        (when first-call (eval `(defdeino+ primus nil (,(d--g ds :carkeys)
                                                        ,(d--g ds :current-body)
                                                        ,(d--g ds :current-name)))))
        (unless (d--g ds :one-key)
            (eval `(prime* ,(d--g ds :current-parent) nil ,next-key ,func ,name* ,@next-deino-settings))
            `(,(meq/inconcat "defdeino" (if (d--g ds :current-body-plus) "+" ""))
                ,(intern (d--g ds :current-name))
                ,@(if (d--g ds :current-body-plus) (list nil) '((:color blue) nil ("`" nil "cancel")))
                (,(d--g ds :spare-keys) ,next-deino-body ,(d--g ds :next-name) ,@next-deino-settings)))))

(defun prime--construct-name+ (keys) (deino--construct-name+ keys prime--construct-name))

;;;###autoload
(defmacro prime (key func &optional name &rest args) `(prime* nil t ,key ,func ,name ,@args))

;;;###autoload
(defun prime+ (&rest args) (eval `(defdeino+ primus ,@args)))

;;;###autoload
(defun primer+ (key &rest args)
    (eval `(defdeino+ primus nil (,key
                                    ,(meq/inconcat (prime--construct-name key) "/body")
                                    ,@args))))

;;;###autoload
(defun primer++ (key &rest args) (deino--nested-rename key #'prime--construct-name+ args))

;; Primarily adapted from https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el

;;;###autoload
(defalias 'use-package-normalize/:prime 'use-package-normalize-forms)

;;;###autoload
(defun use-package-handler/:prime (name keyword args rest state)
  "Generate prime with NAME for `:prime' KEYWORD.
ARGS, REST, and STATE are prepared by `use-package-normalize/:prime'."
  (use-package-concat
   (mapcar #'(lambda (def) `(prime ,@def)) args)
   (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :prime t)

;;;###autoload
(defalias 'use-package-normalize/:prime+ 'use-package-normalize-forms)

;;;###autoload
(defun use-package-handler/:prime+ (name keyword args rest state)
  "Generate prime+ with NAME for `:prime+' KEYWORD.
ARGS, REST, and STATE are prepared by `use-package-normalize/:prime+'."
  (use-package-concat
   (mapcar #'(lambda (def) `(prime+ ,@def)) args)
   (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :prime+ t)

;;;###autoload
(defalias 'use-package-normalize/:primer+ 'use-package-normalize-forms)

;;;###autoload
(defun use-package-handler/:primer+ (name keyword args rest state)
  "Generate primer+ with NAME for `:primer+' KEYWORD.
ARGS, REST, and STATE are prepared by `use-package-normalize/:primer+'."
  (use-package-concat
   (mapcar #'(lambda (def) `(primer+ ,@def)) args)
   (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :primer+ t)

;;;###autoload
(defalias 'use-package-normalize/:primer++ 'use-package-normalize-forms)

;;;###autoload
(defun use-package-handler/:primer++ (name keyword args rest state)
  "Generate primer++ with NAME for `:primer++' KEYWORD.
ARGS, REST, and STATE are prepared by `use-package-normalize/:primer++'."
  (use-package-concat
   (mapcar #'(lambda (def) `(primer++ ,@def)) args)
   (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :primer++ t)

(provide 'prime)
;;; prime.el ends here
