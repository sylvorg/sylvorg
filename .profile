. $HOME/.nix-profile/etc/profile.d/nix.sh
. $HOME/.asdf/asdf.sh
. $HOME/resources/functions
eval "$(fasd --init auto)"
alias -="pushd"
alias ..="cd .."
alias .="exa -la"
alias c="clear"
alias emd="systemctl --user start emacs.service"
alias git="hub"
alias kemd="systemctl --user stop emacs.service"
alias md="mkdir -p"
alias mosh="mosh --experimental-remote-ip=remote"
alias n="exit"
alias remd="systemctl --user restart emacs.service"
alias semd="systemctl status emacs"
alias ssh="assh wrapper ssh --"
alias vi="emacsclient -c"
alias vim="emacsclient -c"
alias mdg='mdg'

# fasd
alias o="fasd -ae xdg-open"
alias e="fasd -fe 'emacsclient -t'"
mdg () { mkdir -p "$@"; cd "$1"; }
# export EDITOR='emacsclient -c'
# export VISUAL='emacsclient -c'
export LESSOPEN='| /usr/share/source-highlight/src-hilite-lesspipe.sh %s'
export LESS=' -R '
alias s="source $HOME/.profile"
