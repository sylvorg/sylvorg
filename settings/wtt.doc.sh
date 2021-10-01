#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(load (concat (getenv "HOME") "/.emacs.d/early-init.el"))
(meq/up markdown-mode :mode ("\\.md\\'")
    :use-package-postconfig (yasnippet)
    :upnsd-preconfig (titan :custom (meq/var/titan-snippets-dir (meq/ued-lib "titan" "snippets"))))
(meq/upnsd doc
    :custom (meq/var/doc-snippets-dir (meq/ued-lib "doc" "snippets"))
    :mode ("\\.doc\\.md\\'" . doc-markdown-mode))
(find-file (concat (meq/timestamp) ".doc.md"))
(meq/insert-snippet "markdown titan template")
(save-buffer)
