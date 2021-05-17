(defun jr/get-header nil (interactive)
    (nth 4 (org-heading-components)))
(defun jr/tangle-path nil (interactive)
    (string-remove-prefix "/" (concat
        (org-format-outline-path (org-get-outline-path)) "/"
            (jr/get-header))))
(defun jr/tangle-oreo nil (interactive)
    (org-babel-lob-ingest "./strange.aiern.org")
    (jr/tangle-path))
(defun jr/get-theme-from-header nil (interactive)
    (string-remove-suffix "-theme.el" (jr/get-header)))
