"derived from: //gist.github.com/simonista/8703723
"echo "VIMRC LOADED SUCCESSFULLY"

"""vundle instructions POSIX
" https://github.com/VundleVim/Vundle.vim
" $ git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

"""vundle instructions WINDOWS
" https://github.com/VundleVim/Vundle.vim/wiki/Vundle-for-Windows
" C:\> choco install -y git
" C:\> choco install -y curl

"""VUNDLE PLUGINS"""

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'   " package manager
Plugin 'dracula/vim'            " color scheme
Plugin 'scrooloose/syntastic'   " syntax checker
" Plugin 'scrooloose/nerdtree'    " file tree
"filetype plugin on
Plugin 'tpope/vim-fugitive'     " git wrapper
Plugin 'junegunn/goyo.vim'      " zen mode
Plugin 'bling/vim-airline'      " status bar
Plugin 'kien/ctrlp.vim'         " fuzzy search
Plugin 'mbbill/undotree'        " go through undos
Plugin 'junegunn/gv.vim'        " git repo explorer
" Plugin 'edkolev/tmuxline.vim' " tmux themer
Plugin 'tpope/vim-dispatch'     " async stuff
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-obsession'
Plugin 'jreybert/vimagit'
Plugin 'tpope/vim-vinegar'
" Plugin 'scrooloose/nerdcommenter'

call vundle#end()


"""""""


"""MAPPINGS"""

" TODO: Pick a leader key
let mapleader = " "
" Move up/down editor lines
nnoremap j gj
nnoremap k gk

map <leader><space> :let @/=''<cr> " clear search

map <C-n> :NERDTreeToggle<CR>
nnoremap <leader><C-u> :UndotreeToggle<CR>

" Visualize tabs and newlines
set listchars=tab:▸\ ,eol:¬,trail:•
"set listchars=tab:▸\ ,trail:•,extends:»,precedes:« " Unprintable chars mapping
" set list " To enable by default
" Or use your leader key + l to toggle on/off
map <leader>l :set list!<CR> " Toggle tabs and EOL

"http://vimcasts.org/transcripts/16/en/
"nmap <leader>w :set wrap!<CR>
"command! -nargs=* Wrap set wrap linebreak nolist
set wrap nolist linebreak

"https://stackoverflow.com/questions/6832364/gvim-switching-tabs-with-keyboard
map <A-Left> <Esc>:tabprev<CR>
map <A-Right> <Esc>:tabnext<CR>

" https://stackoverflow.com/questions/16082991/vim-switching-between-files-rapidly-using-vanilla-vim-no-plugins
nnoremap <leader>l :ls<CR>:b<space>

"https://www.reddit.com/r/vim/comments/6kfyae/vimfugitive_workflow/
map <leader>gs :Gstatus<CR>gg<C-n>
noremap <leader>gc :Gcommit<CR>
map <leader>gp :Gpush<CR>


" https://stackoverflow.com/questions/23292917/vim-key-mapping-compile-and-run-for-java-and-c-code
" autocmd FileType python nnoremap <buffer> <F9> :exec '!python' shellescape(%)<cr>
" autocmd FileType java nnoremap <buffer> <F9> :exec '!javac' shellescape(%) && '!java' shellescape(%:r)<cr>
" autocmd FileType c,cpp nnoremap <buffer> <F9> :exec '!gcc' shellescape(%) && './a.out'<cr>
" autocmd FileType c,cpp nnoremap <buffer> <C-b> :exec '!gcc' shellescape(expand('%'), 1) '&& ./a.out' shellescape(expand('%:r'), 1)<cr>
nnoremap <leader>cc :w <bar> exec '!g++ -std=c++0x '.shellescape('%').' -o '.shellescape('%:r').'.out;echo;echo;echo Press ENTER to continue; read line;exit'<CR><CR>
nnoremap <leader>B :w <bar> exec '!'.shellescape('%:p:r').'.out;echo;echo;echo Press ENTER to continue; read line;exit'<CR><CR>

nnoremap <F7> :w <bar> exec '! /usr/bin/x-terminal-emulator -e bash -c "'.shellescape('%:p:r').';echo;echo;echo Press ENTER to continue; read line;exit; exec bash"'<CR><CR>

" autocmd filetype py nnoremap <F8> :w <bar> exec '!g++ '.shellescape('%').' -o '.shellescape('%:r').''<CR><CR>
nnoremap <F8> :w <bar> exec '!/usr/bin/x-terminal-emulator -e bash -c "python3 '.shellescape('%:p').';echo;echo;echo Press ENTER to continue; read line;exit; exec bash"'<CR><CR>

nnoremap <leader>h :set paste<CR>i/*********************************************************************<CR>** Author: liam beckman<CR>** Date: <Esc>:put =strftime('%d %B %Y')<CR>I<bs><Esc>A<CR>** Description:<CR>*********************************************************************/<Esc>:set nopaste<CR>kA<Space>

nnoremap <leader>d :set paste<CR>i/*********************************************************************<CR>** Description:<CR>*********************************************************************/<Esc>:set nopaste<CR>kA<Space>

" http://vim.wikia.com/wiki/Insert_newline_without_entering_insert_mode
nmap <leader><CR> O<Esc>j

"""COLORS"""

" Color scheme (terminal)<CR>set t_Co=256
set background=dark
let g:solarized_termcolors=256
let g:solarized_termtrans=1
" put https://raw.github.com/altercation/vim-colors-solarized/master/colors/solarized.vim
" in ~/.vim/colors/ and uncomment:
" colorscheme solarized
"
" Color scheme (dracula)
syntax on
color dracula

"""""""


"""SETTINGS"""

" Don't try to be vi compatible
set nocompatible

" Helps force plugins to load correctly when it is turned back on below
filetype off

" Turn on syntax highlighting
syntax on

" For plugins to load correctly
" https://vi.stackexchange.com/questions/10124/what-is-the-difference-between-filetype-plugin-indent-on-and-filetype-indent
filetype plugin indent on

" Security
set modelines=0

" Show line numbers
set number

" Show file stats
set ruler

" Blink cursor on error instead of beeping (grr)
set visualbell

" Encoding
set encoding=utf-8

" Whitespace
"set wrap
"set textwidth=79
set formatoptions="" "this is the old value -> tcqrn1
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set noshiftround

" Cursor motion
set scrolloff=3
set backspace=indent,eol,start
set matchpairs+=<:> " use % to jump between pairs
runtime! macros/matchit.vim

" Allow hidden buffers
set hidden

" Rendering
set ttyfast

" Status bar
set laststatus=2

" Last line
"set showmode
"set showcmd

" searching
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch
set noshowmatch " Halts annoying parentheses jumping highlighting!

" https://stackoverflow.com/questions/923737/detect-file-change-offer-to-reload-file
au FileChangedShell * echo "Warning: File changed on disk"

" syntastic syntax checker https://vimawesome.com/plugin/syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" and because i'm a heathen...
set mouse=a

" airline takes care of showing the command pretty well
set noshowcmd

" http://vim.wikia.com/wiki/Disable_automatic_comment_insertion
" au FileType c,cpp setlocal comments-=:// comments+=f://

" https://stackoverflow.com/questions/6076592/vim-set-formatoptions-being-lost
" autocmd BufNewFile,BufRead * setlocal formatoptions+=cqn

set autoindent

"""""""


"""FUNCTIONS"""

" https://stackoverflow.com/questions/5700389/using-vims-persistent-undo
" Put plugins and dictionaries in this dir (also on Windows)
let vimDir = '$HOME/.vim'
let &runtimepath.=','.vimDir

" Keep undo history across sessions by storing it in a file
if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undodir')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif

"""""""

