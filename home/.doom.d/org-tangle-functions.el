(defun jr/tangle-path nil (interactive)
    (string-remove-prefix "/" (concat
        (org-format-outline-path (org-get-outline-path)) "/"
            (nth 4 (org-heading-components)))))
(defun jr/tangle-oreo nil (interactive)
    (org-babel-lob-ingest "./strange.aiern.org")
    (jr/tangle-path))
