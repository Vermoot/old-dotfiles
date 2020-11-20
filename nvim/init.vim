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
call plug#end()
" }}}

colorscheme one
set background=dark " for the dark version
" set background=light " for the light version

set foldmethod=marker
set number

" Remaps {{{
" Training wheels {{{
inoremap <Down> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>
inoremap <Up> <Nop>
nnoremap <Down> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
nnoremap <Up> <Nop>
vnoremap <Down> <Nop>
vnoremap <Left> <Nop>
vnoremap <Right> <Nop>
vnoremap <Up> <Nop>

inoremap <C-Left> <Nop>
inoremap <C-Right> <Nop>
nnoremap <C-Left> <Nop>
nnoremap <C-Right> <Nop>
vnoremap <C-Left> <Nop>
vnoremap <C-Right> <Nop>


nnoremap h <Nop>
nnoremap l <Nop>
vnoremap h <Nop>
vnoremap l <Nop>
" }}}
" }}}
