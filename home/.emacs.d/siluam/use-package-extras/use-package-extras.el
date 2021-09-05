;;; use-package-extras.el --- a simple package                     -*- lexical-binding: t; -*-

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

;; A bunch of `use-package' keywords that SEEM to work for me.

;;; Code:


;; Adapted From: https://github.com/jwiegley/use-package#use-package-chords
;; Important: https://github.com/noctuid/general.el/issues/53#issuecomment-307262154
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'use-package-core)

(defvar meq/var/profiled nil)

;;;###autoload
(defun meq/load-emacs-file (path) (interactive)
    (load (f-full (f-join
            (if meq/var/profiled pre-user-emacs-directory user-emacs-directory)
            "lib" path))))

;;;###autoload
(defun meq/load-siluam-file (path) (interactive)
    (load (f-full (f-join
            (if meq/var/profiled pre-user-emacs-directory user-emacs-directory)
            "siluam" path))))

;;;###autoload
(defmacro meq/up (&rest args) (interactive)
    `(use-package ,@args :demand ,(cl-getf args :demand t)))

;;;###autoload
(defmacro meq/upns (&rest args) (interactive)
    `(use-package ,@args ,@(with-eval-after-load 'straight `(:straight ,(cl-getf args :straight nil)))))

;;;###autoload
(defmacro meq/upnsd (&rest args) (interactive)
    `(use-package
        ,@args
        :demand ,(cl-getf args :demand t)
        ,@(with-eval-after-load 'straight `(:straight ,(cl-getf args :straight nil)))))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:leaf 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:leaf (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(leaf ,@def)) args)
    (use-package-process-keywords name rest state)))

;; TODO: Convert these to an elisp or noweb function

;; Adapted From: https://github.com/noctuid/general.el/blob/master/general.el#L2553
(setq use-package-keywords
    (cl-loop for item in use-package-keywords
        if (eq item :hook)
        collect :hook and collect :leaf
        else
        unless (eq item :leaf)
        collect item))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:init/defun 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:init/defun (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(defun ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:init/defun* 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:init/defun* (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(cl-defun ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:load-emacs-file-preconfig 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:load-emacs-file-preconfig (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/load-emacs-file ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:load-siluam-file-preconfig 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:load-siluam-file-preconfig (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/load-siluam-file ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:use-package-preconfig 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:use-package-preconfig (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/up ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:upnsd-preconfig 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:upnsd-preconfig (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/upnsd ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/noctuid/general.el/blob/master/general.el#L2620
(setq use-package-keywords
    (cl-loop for item in use-package-keywords
        if (eq item :commands)
        collect :commands and
        collect :init/defun and
        collect :init/defun* and
        collect :load-emacs-file-preconfig and
        collect :load-siluam-file-preconfig and
        collect :use-package-preconfig and
        collect :upnsd-preconfig
        else
        unless (memq item '(:init/defun
                            :init/defun*
                            :load-emacs-file-preconfig
                            :load-siluam-file-preconfig
                            :use-package-preconfig
                            :upnsd-preconfig))
        collect item))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:config/defun 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:config/defun (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(defun ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:config/defun* 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:config/defun* (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(cl-defun ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:load-emacs-file-postconfig 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:load-emacs-file-postconfig (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/load-emacs-file ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:use-package-postconfig 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:use-package-postconfig (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/up ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:upnsd-postconfig 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:upnsd-postconfig (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/upnsd ,@def)) args)
    (use-package-process-keywords name rest state)))

(setq use-package-keywords
    (cl-loop for item in use-package-keywords
        if (eq item :load)
        collect :load and
        collect :config/defun and
        collect :config/defun* and
        collect :load-emacs-file-postconfig and
        collect :use-package-postconfig and
        collect :upnsd-postconfig
        else
        unless (memq item '(:config/defun
                            :config/defun*
                            :load-emacs-file-postconfig
                            :use-package-postconfig
                            :upnsd-postconfig))
        collect item))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:gsetq 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:gsetq (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(alloy-setq ,@def)) args)
    (use-package-process-keywords name rest state)))

(setq use-package-keywords
    (cl-loop for item in use-package-keywords
        if (eq item :init)
        collect :init and collect :gsetq
        else
        unless (eq item :gsetq)
        collect item))
;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:gadvice 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:gadvice (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(alloy-add-advice ,@def)) args)
    (use-package-process-keywords name rest state)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:gradvice 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:gradvice (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(alloy-remove-advice ,@def)) args)
    (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :gadvice t)
(add-to-list 'use-package-keywords :gradvice t)

(with-eval-after-load 'alloy
    ;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
    ;;;###autoload
    (defalias 'use-package-normalize/:grook 'use-package-normalize-forms)
    
    ;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
    ;;;###autoload
    (defun use-package-handler/:grook (name keyword args rest state)
        (use-package-concat (mapcar #'(lambda (def) `(alloy-remove-hook ,@def)) args)
        (use-package-process-keywords name rest state)))

    (setq use-package-keywords
        (cl-loop for item in use-package-keywords
            if (eq item :gfhook)
            collect :gfhook and
            collect :grook
            else
            unless (eq item :grook)
            collect item)))

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:evil-ex 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:evil-ex (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(evil-ex-define-cmd ,@def)) args)
    (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :evil-ex t)

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:aiern-ex 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:aiern-ex (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(aiern-ex-define-cmd ,@def)) args)
    (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :aiern-ex t)

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:both-ex 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:both-ex (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/both-ex-define-cmd ,@def)) args)
    (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :both-ex t)

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:which-key-change 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:which-key-change (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/which-key-change ,@def)) args)
    (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :which-key-change t)

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:which-key-change-ryo 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:which-key-change-ryo (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/which-key-change-ryo ,@def)) args)
    (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :which-key-change-ryo t)

;; Adapted From: https://github.com/jwiegley/use-package/blob/master/use-package-core.el#L1153
;;;###autoload
(defalias 'use-package-normalize/:which-key-change-sorrow 'use-package-normalize-forms)

;; Adapted From: https://gitlab.com/to1ne/use-package-hydra/-/blob/master/use-package-hydra.el#L79
;;;###autoload
(defun use-package-handler/:which-key-change-sorrow (name keyword args rest state)
    (use-package-concat (mapcar #'(lambda (def) `(meq/which-key-change-sorrow ,@def)) args)
    (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :which-key-change-sorrow t)

(provide 'use-package-extras)
;;; use-package-extras.el ends here
