source ~/resources/nix-env.fish/conf.d/nix-env.fish
source ~/.asdf/asdf.fish
cd ~/resources/bass
make install &>/dev/null
cd -

set -gx PATH $PATH$HOME/.local/bin $HOME/.nimble/bin /home/linuxbrew/.linuxbrew/bin /home/linuxbrew/.linuxbrew/sbin $HOME/.nix-profile/bin $HOME/.guix-profile/bin $HOME/go/bin /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/games /usr/local/games /snap/bin /usr/local/go/bin /usr/lib/node_modules
alias s=source ~/.config/fish/config.fish
direnv hook fish | source
starship init fish | source
