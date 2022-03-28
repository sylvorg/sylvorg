# Adapted From:
# Comment: https://stackoverflow.com/questions/7110119/bash-history-without-line-numbers#comment8517296_7110197
# User: https://stackoverflow.com/users/827263/keith-thompson
rc () { eval $(history | sed 's/^ *[0-9]* *//' | fzf-tmux); }

source $HOME/.nix-profile/etc/profile.d/nix.sh
eval "$(fasd --init auto)"
cdf () { cd $(fasd -ld | fzf-tmux); }
cdi () { cd $(getFzfdfOutput "$@" "-t" "d"); }
cdr () { cd $(zoxide query -l | fzf-tmux); }
direnvAllow () {
    if [ -z "$1" ]; then
        direnv allow
    else
        for d in "$@"; do
            direnv allow "$d"
        done
    fi
}
getFzfdfOutput () {
    if [ -z "$1" ]; then
        echo $(fd | fzf-tmux)
    else
        if [ -d "$1" ]; then
            echo $(fd "." "$@" | fzf-tmux)
        else
            echo $(fd "$@" | fzf-tmux)
        fi
    fi
}
mdg () { mkdir -p "$@" && cd "$1"; }

Run () { curl --create-dirs -fsSLo "$2" "$1" && shift && run "$@"; }
run () { chmod +x "$1" && "$@"; }
# export EDITOR='emacsclient -c'
# export VISUAL='emacsclient -c'
export HISTCONTROL='ignoreboth:erasedups'
export LESSOPEN='| /usr/share/source-highlight/src-hilite-lesspipe.sh %s'
export LESS=' -R '
# Adapted From:
# Answer: https://askubuntu.com/a/146034/1058868
# User: https://askubuntu.com/users/1059/gilles-so-stop-being-evil
alias -- -="pushd"

alias ..="cd .."
alias .="exa -la --octal-permissions"
alias c="clear"
alias cdf='cdf'
alias cdi='cdi'
alias cdr='cdr'
alias da='direnvAllow'
alias emd="systemctl --user start emacs.service"
alias git="hub"
alias kemd="systemctl --user stop emacs.service"
alias la='exa -la --octal-permissions'
alias md="mkdir -p"
alias mdg='mdg'
alias mosh="mosh --experimental-remote-ip=remote"
alias n="exit"
alias rc="rc"
alias remd="systemctl --user restart emacs.service"
alias Run="Run"
alias run="run"
alias semd="systemctl status emacs"
alias ssh="assh wrapper ssh --"
alias vi="emacsclient -c"
alias vim="emacsclient -c"

# fasd
alias o="fasd -ae xdg-open"

# quick opening files with emacs
alias e="fasd -fe 'emacsclient -t'"
export PATH="$PATH:$HOME/.local/bin:$HOME/.nimble/bin:/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$HOME/.nix-profile/bin:$HOME/.guix-profile/bin:$HOME/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/local/go/bin:/usr/lib/node_modules"
alias s="source ~/.bashrc"
eval "$(direnv hook bash)"
eval "$(zoxide init bash)"
eval "$(starship init bash)"
set -o vi
