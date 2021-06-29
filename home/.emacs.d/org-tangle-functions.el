(when (file-exists-p "~/shadowrylander/README.org")
    (org-babel-lob-ingest "~/shadowrylander/README.org"))
(when (file-exists-p "~/shadowrylander/strange.aiern.org")
    (org-babel-lob-ingest "~/shadowrylander/strange.aiern.org"))

(defun meq/get-header nil (interactive)
    (nth 4 (org-heading-components)))
(defun meq/tangle-path nil (interactive)
    (string-remove-prefix "/" (concat
        (org-format-outline-path (org-get-outline-path)) "/"
            (meq/get-header))))
(defun meq/get-theme-from-header nil (interactive)
    (string-remove-suffix "-theme.el" (meq/get-header)))
