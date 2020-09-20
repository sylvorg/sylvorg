starship init fish | source

alias c="clear"
alias n="exit"
alias sf="source ~/.config/fish/config.fish"
alias djn='docker run -p 8888:8888 -v "$PWD":/home/jovyan/work --rm jupyter/datascience-notebook'
alias elvis="rm -rf /tmp/elvish-1000/sock && elvish"