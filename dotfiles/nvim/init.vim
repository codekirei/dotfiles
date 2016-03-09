filetype off
set showcmd

" scroll when within 8 lines of edge
set scrolloff=8

"-------------------------------------------------------------------------------
" PLUGINS
"-------------------------------------------------------------------------------
call plug#begin()

" Plug 'Shougo/vimproc.vim', {'do': 'make'}
" Plug 'Shougo/vimshell.vim'
" Plug 'Shougo/unite.vim'
" Plug 'Shougo/neomru.vim'
" Plug 'Shougo/unite-outline'
" Plug 'tsukkee/unite-tag'
" Plug 'rstacruz/vim-fastunite'
" Plug 'Shougo/neoyank.vim'
" Plug 'justinmk/vim-sneak'
" Plug 'chrisbra/Colorizer'
" Plug 'tpope/vim-endwise' " doesn't work with non-standard syntax highlighting =/

" COLORS
"-------------------------------------------------------------------------------
Plug 'atelierbram/vim-colors_duotones'
" Plug 'lilydjwg/colorizer' -- super cool but causes weird flickering in insert mode =/

" LANGUAGES
"-------------------------------------------------------------------------------
Plug 'sheerun/vim-polyglot'
Plug 'tbastos/vim-lua'
Plug 'mattn/emmet-vim'
Plug 'larsbs/vim-xmll'

" OTHER
"-------------------------------------------------------------------------------
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-surround'
Plug 'wincent/ferret'
Plug 'tomtom/tcomment_vim'
Plug 'coderifous/textobj-word-column.vim'
Plug 'edsono/vim-matchit'
Plug 'tpope/vim-abolish'
Plug 'ivyl/vim-bling'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf', {'dir': '$XDG_CACHE_HOME/fzf', 'do': './install --all'}
Plug 'Raimondi/delimitMate'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-rsi'
Plug 'junegunn/vim-easy-align'
Plug 'benekastah/neomake'

" END PLUGINS
"-------------------------------------------------------------------------------
call plug#end()

"-------------------------------------------------------------------------------
" SPACING
"-------------------------------------------------------------------------------
set expandtab
set softtabstop=2
set shiftwidth=2
set nowrap
set nofoldenable

"-------------------------------------------------------------------------------
" KEYS
"-------------------------------------------------------------------------------
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
nnoremap <leader>c :noh<CR>:<C-c>

" exit things
inoremap kj <ESC>
cnoremap kj <C-e><C-u><C-c>

" automatically recenter
nnoremap n nzz
nnoremap N Nzz

" adjust shiftwidth
function! Adjust_shiftwidth(dir)
  if a:dir == 'up'
    set shiftwidth +=1
  elseif a:dir == 'down'
    set shiftwidth -=1
  endif
  echo 'shiftwidth =' &shiftwidth
endfunction
nnoremap <leader>> :call Adjust_shiftwidth('up')<CR>
nnoremap <leader>< :call Adjust_shiftwidth('down')<CR>

" other
nnoremap <C-n> O<Esc>j
nnoremap <C-m> o<Esc>k
nnoremap s <C-w>
map Q <Nop>
nnoremap Y y$

" PLUGINS
"-------------------------------------------------------------------------------
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

"-------------------------------------------------------------------------------
" NEOMAKE
"-------------------------------------------------------------------------------
" Global
" let g:neomake_verbose = 3 " debug flag
autocmd! BufWritePost * Neomake
let g:neomake_error_sign = { 'text': '!>', 'texthl': 'Error' }
let g:neomake_warning_sign = { 'text': '?>', 'texthl': 'Error' }

" JavaScript
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_jsx_enabled_makers = ['eslint']
let g:my_neomake_eslint = {
  \ 'args': [
    \ '--format', 'compact',
    \ '--config', '$XDG_CONFIG_HOME/eslint/eslint.json',
    \ '--cache',
    \ '--cache-location', '$XDG_CACHE_HOME/eslint/'
    \ ],
  \ 'errorformat': '%f: line %l\, col %c\, %m'
  \ }
let g:neomake_javascript_eslint_maker = my_neomake_eslint
let g:neomake_jsx_eslint_maker = my_neomake_eslint

" CSS
let g:neomake_css_enabled_makers = ['stylelint']
let g:neomake_scss_enabled_makers = ['stylelint']
let g:my_neomake_stylelint = {
  \ 'args': [
    \ '--config', '$XDG_CONFIG_HOME/stylelint/stylelint.config.js'
  \ ],
  \ 'errorformat': '%+P%f, %W%l:%c%*\s%m, %-Q'
  \ }
let g:neomake_css_stylelint_maker = my_neomake_stylelint
let g:neomake_scss_stylelint_maker = my_neomake_stylelint

"-------------------------------------------------------------------------------
" SEARCH
"-------------------------------------------------------------------------------
set ignorecase
set smartcase

"-------------------------------------------------------------------------------
" COLORS
"-------------------------------------------------------------------------------
syntax on
set background=dark
color duotone-darkmeadow

set list
set listchars=tab:>-,trail:_

au BufRead,BufNewFile *.css set ft=scss

highlight SpecialKey ctermfg=red guifg=red
highlight Normal ctermbg=0

" don't automatically continue comments on newline
autocmd BufNewFile,BufRead * setlocal formatoptions-=cro
