set runtimepath+=~/vimrc

source ~/vimrc/vimrcs/basic.vim
source ~/vimrc/vimrcs/extended.vim
source ~/vimrc/vimrcs/filetypes.vim
source ~/vimrc/vimrcs/plugins_config.vim

execute pathogen#infect()
syntax on
filetype plugin indent on
