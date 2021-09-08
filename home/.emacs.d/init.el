;; [[file:meta.aiern.org::20210804184605617540800][20210804184605617540800]]
;;; $EMACSDIR/init.el -*- lexical-binding: t; -*-
(when (version< emacs-version "27") (load (concat
                                            (file-name-directory load-file-name)
                                            "early-init.el")))

(defun meq/reload-first-init nil (interactive) (meq/cl (meq/ued* "FEEDME.org")))
(meq/reload-first-init)
;; 20210804184605617540800 ends here
