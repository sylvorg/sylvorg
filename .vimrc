set runtimepath+=~/vimrc

source ~/.e/vim/vimrc/vimrcs/basic.vim
source ~/.e/vim/vimrc/vimrcs/extended.vim
source ~/.e/vim/vimrc/vimrcs/filetypes.vim
source ~/.e/vim/vimrc/vimrcs/plugins_config.vim

execute pathogen#infect()
syntax on
filetype plugin indent on
