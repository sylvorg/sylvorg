(defun aiern/get-header nil (interactive)
    (nth 4 (org-heading-components)))
(defun aiern/tangle-path nil (interactive)
    (org-babel-lob-ingest "./README.org")
    (string-remove-prefix "/" (concat
        (org-format-outline-path (org-get-outline-path)) "/"
            (aiern/get-header))))
(defun aiern/tangle-oreo nil (interactive)
    (org-babel-lob-ingest "./strange.aiern.org")
    (aiern/tangle-path))
(defun aiern/get-theme-from-header nil (interactive)
    (string-remove-suffix "-theme.el" (aiern/get-header)))
