set nocompatible
filetype off

" PLUGINS {{{1
call plug#begin()

" Plug 'Shougo/vimproc.vim', {'do': 'make'}
" Plug 'Shougo/unite.vim'
" Plug 'Shougo/neomru.vim'
" Plug 'Shougo/unite-outline'
" Plug 'tsukkee/unite-tag'
" Plug 'rstacruz/vim-fastunite'
" Plug 'Shougo/neoyank.vim'
" Plug 'justinmk/vim-sneak'
" Plug 'chrisbra/Colorizer'

Plug 'tpope/vim-dispatch'
Plug 'wincent/ferret'
Plug 'tomtom/tcomment_vim'
Plug 'coderifous/textobj-word-column.vim'
Plug 'edsono/vim-matchit'
Plug 'tpope/vim-abolish'
Plug 'ivyl/vim-bling'
Plug 'scrooloose/syntastic'
Plug 'itchyny/lightline.vim'
Plug 'mizuchi/vim-ranger'
Plug 'junegunn/fzf', {'dir': '$XDG_CACHE_HOME/fzf', 'do': './install --all'}
Plug 'Raimondi/delimitMate'
Plug 'atimholt/spiffy_foldtext'

call plug#end()

" scroll when within 8 lines of edge
set scrolloff=8

" KEYS {{{1
let mapleader = " "
nnoremap <C-p> :FZF<CR>

" reload things
" reload vim
nnoremap <leader>rv :source $XDG_CONFIG_HOME/nvim/init.vim<CR>
" reload plugins
nnoremap <leader>rp :PlugUpdate<CR>

" clear all the things
nnoremap <leader>c :noh<CR>

inoremap kj <ESC>
vnoremap kj <ESC>

nnoremap <C-n> O<Esc>j
nnoremap <C-m> o<Esc>k
nnoremap s <C-w>
map Q <Nop>

" automatically recenter
nnoremap n nzz
nnoremap N Nzz
nnoremap Y y$

set foldmethod=marker

" COLOR {{{1
syntax on
colorscheme delek

highlight UnwantedWhitespace ctermbg=1
match UnwantedWhitespace /\s\+$\|\n\%$/

highlight Search ctermbg=8
highlight Folded ctermbg=0 ctermfg=1

" don't automatically continue comments on newline
autocmd BufNewFile,BufRead * setlocal formatoptions-=cro
