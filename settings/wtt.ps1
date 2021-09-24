#!/usr/bin/env pwsh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(princ (format-time-string "%Y%m%d%H%M%S%N")) (terpri)
