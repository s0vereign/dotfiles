execute pathogen#infect()
syntax on
filetype plugin indent on
let g:Powerline_symbols = 'fancy'
colorscheme molokai
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
set number
set autoread 
