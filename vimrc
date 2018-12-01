
"Plugins {{{

" Specify a directory for plugins
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'flazz/vim-colorschemes'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'Raimondi/delimitMate'
" Plug 'ervandew/supertab'

" Initialize plugin system
call plug#end()

" NERDTree
map <C-n> :NERDTreeToggle<CR>

"}}}

"Basics {{{

set nocompatible

" english, for nvim
language en_US.UTF-8

" disable beeping and blinking
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

set ignorecase
filetype plugin indent on

" autoreload file
set autoread

" :find  search path
set path +=,**

" open splits to the right and below
set splitbelow
set splitright

autocmd FileType vim setlocal foldmethod=marker

set incsearch " jump to word while searching /
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

" SET leader key
" let mapleader = "\<Space>"

" terminal (nvim only)
"
" exit terminal mode, enter normal mode from terminal
"tnoremap <Esc> <C-\><C-n>
" map <Leader>i :10split term://zsh<CR>
" map <Leader>q :q<CR>

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

set number " show line numbers
" set relativenumber " relative line numbers
" skru av relative line number i insert mode, eller når buffer mister fokus
" augroup numbertoggle
"   autocmd!
"   autocmd bufenter,focusgained,insertleave * set relativenumber
"   autocmd bufleave,focuslost,insertenter   * set norelativenumber
" augroup end

set noshowcmd

"set ruler " show cursor location bottom right

"highlight cursor guifg=white guibg=black
" set cursorline " highlight current line
"hi CursorLine   cterm=NONE ctermbg=grey ctermfg=white guibg=darkred guifg=white

set wildmenu " visual autocomplete for command menu

"}}}

"Colors {{{
" colorscheme from plugin-path
"colorscheme slate
colorscheme xoria256
"colorscheme wombat256
"}}}

