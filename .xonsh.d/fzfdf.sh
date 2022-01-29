#!/usr/bin/env zsh
if [ -z "$1" ]; then echo $(fd | fzf-tmux) || echo $(fd . "$1" | fzf-tmux); fi
