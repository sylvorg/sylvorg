. $HOME/.nix-profile/etc/profile.d/nix.sh
. $HOME/.asdf/asdf.sh
. $(fasd --init auto)
. $HOME/resources/functions
ec() { { [ -z "$1" ] || [ "$1" != -* ]; } && emacsclient -s damascus -c $@ || emacsclient -s damascus $@; }
emactl() { systemctl --user $1 emacs.service; }
emacs-push() { make -f ~/.emacs.d/lib/$1/settings/makefile; }
export EDITOR='ec'
export VISUAL='ec'
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
PATH="$PATH:$HOME/.local/bin:$HOME/.nimble/bin:/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$HOME/.nix-profile/bin:$HOME/.guix-profile/bin:$HOME/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/local/go/bin:/usr/lib/node_modules"
alias s="source ~/.bashrc"
eval "$(direnv hook bash)"
eval "$(starship init bash)"
