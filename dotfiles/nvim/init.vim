filetype off
set showcmd

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

"--------"
" Colors "
"--------"
Plug 'atelierbram/vim-colors_duotones'
Plug 'lilydjwg/colorizer'

"-----------"
" Languages "
"-----------"
Plug 'sheerun/vim-polyglot'

Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-surround'
Plug 'wincent/ferret'
Plug 'tomtom/tcomment_vim'
Plug 'coderifous/textobj-word-column.vim'
Plug 'edsono/vim-matchit'
Plug 'tpope/vim-abolish'
Plug 'ivyl/vim-bling'
Plug 'scrooloose/syntastic'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf', {'dir': '$XDG_CACHE_HOME/fzf', 'do': './install --all'}
Plug 'Raimondi/delimitMate'
Plug 'atimholt/spiffy_foldtext'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'tpope/vim-vinegar'

call plug#end()

" scroll when within 8 lines of edge
set scrolloff=8

" FUNCTIONS {{{1

" AUTOCOMMANDS {{{1
augroup reload_vimrc
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

" WHITESPACE {{{1
set expandtab
set softtabstop=2
set shiftwidth=2
set nowrap

" KEYS {{{1
let mapleader = " "
nnoremap <C-t> :FZF<CR>
" nnoremap gx :silent !xdg-open &>/dev/null <C-R>=escape("<C-R><C-F>","#?&;\|%")<CR><CR>:<C-c>
vnoremap <leader>w :'<,'>:w !xargs chromium &>/dev/null<CR>

" reload things
" reload vim
nnoremap <leader>rv :source $XDG_CONFIG_HOME/nvim/init.vim<CR>
" reload plugins
nnoremap <leader>rp :PlugUpdate<CR>

" clear all the things
nnoremap <leader>c :noh<CR>

" exit things
inoremap kj <ESC>
vnoremap kj <ESC>
cnoremap kj <C-e><C-u><C-c>

nnoremap <C-n> O<Esc>j
nnoremap <C-m> o<Esc>k
nnoremap s <C-w>
map Q <Nop>

" automatically recenter
nnoremap n nzz
nnoremap N Nzz
nnoremap Y y$

" FOLDS {{{1
set foldmethod=marker

" SYNTASTIC {{{1
"" JavaScript {{{2
let g:syntastic_javascript_checkers = ['eslint']
let g:eslint_conf = "-c $XDG_CONFIG_HOME/eslint/eslint.json"
let g:eslint_cache = "--cache --cache-location $XDG_CACHE_HOME/eslint/"
let g:syntastic_javascript_eslint_args = eslint_conf . ' ' . eslint_cache


" COLORS {{{1
syntax on
set background=dark
color duotone-darkmeadow

highlight UnwantedWhitespace ctermbg=1
match UnwantedWhitespace /\s\+$\|\n\%$/

" overrides
highlight Search ctermbg=8
highlight Folded ctermbg=0 ctermfg=1
highlight Normal ctermbg=0

" don't automatically continue comments on newline
autocmd BufNewFile,BufRead * setlocal formatoptions-=cro
