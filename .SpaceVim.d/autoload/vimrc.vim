function! vimrc#before() abort
endfunction

function! vimrc#after() abort
    syntax on
    filetype plugin indent on

    " enable 24bit true color
    if (has("termguicolors"))
    set termguicolors
    endif

    " enable the theme
    syntax enable
    colorscheme fairyfloss

    let g:true_airline = 1
    let g:airline_theme='true'

    " for kitty
    let &t_ut=""

    " disables border on left side
    set foldcolumn=0

    " Spaces & Tabs
    set tabstop=4       " number of visual spaces per TAB
    set softtabstop=4   " number of spaces in tab when editing
    set shiftwidth=4    " number of spaces to use for autoindent
    set expandtab       " tabs are space
    set autoindent
    set copyindent      " copy indent from the previous line

    " set number relativenumber

    " Insert Toggle
    imap ;; <ESC>
    map ;; i <BACKSPACE>

    " Change two spaces to four
    map \\ :set ts=2 sts=2 noet <bar> :retab! <bar> :set ts=4 sts=4 et <bar> :retab <CR>
    imap \\ <ESC> :set ts=2 sts=2 noet <bar> :retab! <bar> :set ts=4 sts=4 et <bar> :retab <CR>

    " Change movement keys to <space>wasd
    noremap <Space-a> h
    noremap <Space-s> j
    noremap <Space-w> k
    noremap <Space-d> l

    " Set Paste
    command SMP :set paste <CR>

    " Set NoPaste
    command SNP :set nopaste <CR>

    " Tab to insert
    map <TAB> i <TAB>

    " Vim Signit
    let g:signit_initials = "JR"
    let g:signit_name = "Jeet Ray"
    " let g:signit_extra_1
    " let g:signit_extra_2
    " let g:signit_position
    let g:signit_ascii_font = "isometric1.flf"
    " let g:signit_ascii_spacing

    if has('nvim')
        call dein#add('iron-e/nvim-libmodal')
        call dein#add('shougo/deol.nvim')
        call dein#add('shougo/deoppet.nvim')
        call dein#add('shougo/deorise.nvim')
    else
        call dein#add('iron-e/vim-libmodal')
        call dein#add('roxma/nvim-yarp')
        call dein#add('roxma/vim-hug-neovim-rpc')
    endif

    let g:deoplete#enable_at_startup = 1
endfunction
