set runtimepath+=~/vimrc

source ~/.e/vim/vimrc/vimrcs/basic.vim
source ~/.e/vim/vimrc/vimrcs/extended.vim
source ~/.e/vim/vimrc/vimrcs/filetypes.vim
source ~/.e/vim/vimrc/vimrcs/plugins_config.vim

execute pathogen#infect()
syntax on
filetype plugin indent on

" enable 24bit true color
if (has("termguicolors"))
  set termguicolors
endif

" enable the theme
syntax enable
colorscheme true

" let g:true_airline = 1
" let g:airline_theme='true'

" for kitty
let &t_ut=''
