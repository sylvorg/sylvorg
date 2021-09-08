#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(load (concat (getenv "HOME") "/.emacs.d/early-init.el"))
(meq/up markdown-mode :mode ("\\.md\\'")
    :use-package-postconfig (yasnippet)
    :upnsd-preconfig (titan :custom (meq/var/titan-snippets-dir (meq/ued-siluam "titan" "snippets"))))
(meq/upnsd fell
    :custom (meq/var/fell-snippets-dir (meq/ued-siluam "fell" "snippets"))
    :mode ("\\.fell\\.md\\'" . fell-markdown-mode))
(find-file (concat (meq/timestamp) ".fell.md"))
(meq/insert-snippet "markdown titan template")
(save-buffer)
