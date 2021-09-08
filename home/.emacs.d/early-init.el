;; [[file:meta.aiern.org::20210604182053300746900][20210604182053300746900]]
;;; $EMACSDIR/early-init.el -*- lexical-binding: t; -*-

(defvar pre-user-emacs-directory (file-name-directory load-file-name))

;; To get the latest version of `org-mode', require the load-file before
;; byte-compilation of `lib'
(add-to-list 'load-path (concat pre-user-emacs-directory "lib/org/lisp"))
(require 'org-loaddefs)

;; Adapted From: https://github.com/emacscollective/borg/blob/master/borg.el#L912
(defun meq/call (program buffer-name &rest args)
  (let ((process-connection-type nil)
        (buffer (generate-new-buffer buffer-name)))
    (if (eq (apply #'call-process program nil buffer nil args) 0)
        (kill-buffer buffer)
      (with-current-buffer buffer
        (special-mode))
      (pop-to-buffer buffer)
      (error "%s failed" program))))

;; Adapted From: https://code.orgmode.org/bzg/org-mode/src/master/lisp/org.el#L222
(defun meq/org-babel-load-file-advice (file &optional compile)
  "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded."
  (interactive "fFile to load: \nP")
  (let ((tangled-file (concat (file-name-sans-extension file) ".el")))
    ;; Tangle only if the Org file is newer than the Elisp file.
    (unless (org-file-newer-than-p
                tangled-file
                (file-attribute-modification-time
                    (file-attributes (file-truename file))))
        (meq/call (concat pre-user-emacs-directory "org-tangle.sh") "*literally-configuring*" file))
    (if compile
        (progn
            (byte-compile-file tangled-file)
            (load tangled-file)
            (message "Compiled and loaded %s" tangled-file))
        (load-file tangled-file)
        (message "Loaded %s" tangled-file))))

(advice-add #'org-babel-load-file :override #'meq/org-babel-load-file-advice)

(defun meq/reload-early-init nil (interactive)
    (org-babel-load-file (concat pre-user-emacs-directory "README.org") t))
(meq/reload-early-init)
;; 20210604182053300746900 ends here
