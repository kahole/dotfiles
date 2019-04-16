
"Plugins {{{

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'flazz/vim-colorschemes'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'Raimondi/delimitMate'
Plug 'mhinz/vim-startify'
" Plug 'kshenoy/vim-signature'

call plug#end()

"}}}

"Basics {{{

set nocompatible

" disable beeping and blinking
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

set backspace=indent,eol,start

filetype plugin indent on

" autoreload file (changed somewhere else)
set autoread

" :find  search path
set path +=,**

" open splits to the right and below
set splitbelow
set splitright

autocmd FileType vim setlocal foldmethod=marker

set ignorecase
set incsearch "jump to search hit while typing
"set hlsearch "highlight search results

" share clipboard with system clipboard
set clipboard=unnamed

"tabs to spaces
set expandtab
set tabstop=4
set shiftwidth=4
"set softtabstop=4
set smarttab

"}}}

"Key remaps {{{

map <C-n> :NERDTreeToggle<CR>

" make underscore behave exactly like carrot to move to first non-whitespace character
map _ ^

" scroll with ALT-J and ALT-K
nmap √ <C-E>
nmap ª <C-Y>
" accelerate scrolling
noremap <C-E> 3<C-E>
noremap <C-Y> 3<C-Y>

"}}}

"UI {{{

" set termguicolors
syntax on

if has("gui_running")
     set guifont=Menlo\ Regular:h15
     set guioptions=
endif

" Always keep show 5 lines below the cursor
set scrolloff=5
set number
set noshowcmd

"set ruler " show cursor location bottom right

" highlight cursor guifg=white guibg=black
set cursorline " highlight current line
" hi CursorLine   cterm=NONE ctermbg=grey ctermfg=white guibg=darkred guifg=white

set wildmenu " visual autocomplete for command menu

"}}}

"Colors {{{

" colorscheme slate
colorscheme xoria256

"}}}


" let g:SignatureMarkLineHL = "CursorLine"
