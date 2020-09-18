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

" Spaces & Tabs
set tabstop=4       " number of visual spaces per TAB
set softtabstop=4   " number of spaces in tab when editing
set shiftwidth=4    " number of spaces to use for autoindent
set expandtab       " tabs are space
set autoindent
set copyindent      " copy indent from the previous line

set number

" Insert Toggle
imap kk <ESC>
map kk i <BACKSPACE>

" Change two spaces to four
map jj :set ts=2 sts=2 noet <bar> :retab! <bar> :set ts=4 sts=4 et <bar> :retab <CR>
imap jj <ESC> :set ts=2 sts=2 noet <bar> :retab! <bar> :set ts=4 sts=4 et <bar> :retab <CR>

" Set Paste
command SMP :set paste <CR>

" Set NoPaste
command SNP :set nopaste <CR>

" Tab to insert
map <TAB> i <TAB>
