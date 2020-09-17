set runtimepath+=~/vimrc

source ~/.s/vim/vimrc/vimrcs/basic.vim
source ~/.s/vim/vimrc/vimrcs/extended.vim
source ~/.s/vim/vimrc/vimrcs/filetypes.vim
source ~/.s/vim/vimrc/vimrcs/plugins_config.vim

execute pathogen#infect()
syntax on
filetype plugin indent on

" enable 24bit true color
if (has("termguicolors"))
  set termguicolors
endif

" enable the theme
syntax enable
colorscheme mountaineer

" let g:true_airline = 1
" let g:airline_theme='true'

" for kitty
let &t_ut=''

set foldcolumn=0
