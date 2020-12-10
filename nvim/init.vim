" Plugins {{{
" Auto install plugins apparently {{{
if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
else
	autocmd VimEnter *
				\  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
				\|   PlugInstall --sync | q
				\| endif
endif
" }}}

call plug#begin('~/.vim/plugged')
Plug 'rakr/vim-one'
Plug 'vim-airline/vim-airline'
Plug 'doums/darcula'
Plug 'vim-airline/vim-airline-themes'
Plug 'machakann/vim-highlightedyank'
Plug 'psliwka/vim-smoothie'
Plug 'unblevable/quick-scope'
Plug 'ap/vim-css-color'
Plug 'glacambre/firenvim'
Plug 'tpope/vim-surround'
Plug 'wellle/targets.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'justinmk/vim-sneak'
Plug 'kien/rainbow_parentheses.vim'
Plug 'sheerun/vim-polyglot'
call plug#end()
" }}}

colorscheme darcula
let g:airline_powerline_fonts = 1
let g:airline_theme='deus'

set foldmethod=marker
set number

" Remaps {{{

" }}}
