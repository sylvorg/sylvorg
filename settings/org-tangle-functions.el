(let* ((file (cond
                ((file-exists-p "~/.emacs.d/settings/README.org") "~/.emacs.d/settings/README.org")
                ((file-exists-p "home/.emacs.d/settings/README.org") "home/.emacs.d/settings/README.org")
                ((file-exists-p "settings/README.org") "settings/README.org"))))
    (when file (org-babel-lob-ingest file)))

(defun meq/get-header nil (interactive)
    (nth 4 (org-heading-components)))
(defun meq/tangle-path nil (interactive)
    (string-remove-prefix "/" (concat
        (org-format-outline-path (org-get-outline-path)) "/"
            (meq/get-header))))
(defun meq/get-theme-from-header nil (interactive)
    (string-remove-suffix "-theme.el" (meq/get-header)))
