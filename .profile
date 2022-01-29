. $HOME/.nix-profile/etc/profile.d/nix.sh
. $HOME/.asdf/asdf.sh
. $(fasd --init auto)
. $HOME/resources/functions
cd $HOME
alias c="clear"
alias n="exit"
alias ssh="assh wrapper ssh --"
alias vi='ec'
alias vim='ec'
alias e='ec'
alias f='ec -t'
alias r='systemctl --user restart emacs && ec'
alias s='systemctl --user restart emacs && ec -t'
alias p='emactl'
alias ep='emacs-push'
alias phy='PYTHONPATH="$HOME/.local/syvl/python/hy:$HOME/.local/syvl/python/bakery:$PYTHONPATH" ~/.local/syvl/python/hy/bin/hy'
ec() { { [ -z "$1" ] || [ "$1" != -* ]; } && emacsclient -s damascus -c $@ || emacsclient -s damascus $@; }
emactl() { systemctl --user $1 emacs.service; }
emacs-push() { make -f ~/.emacs.d/lib/$1/settings/makefile; }
export EDITOR='ec'
export VISUAL='ec'
alias s="source $HOME/.profile"
