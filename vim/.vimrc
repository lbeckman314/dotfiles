" A vim/neovim config cobbled together from the far corners of the world.
" Feel free to copy and paste, fork, clone, or anything you like.

" adapted from: //gist.github.com/simonista/8703723


" ---------------------------- "
" INITIALIZE VIM-PLUG
" ---------------------------- "

" automatically install vim plug
" https://github.com/junegunn/vim-plug/wiki/tips#automatic-installation
 if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
 endif

" initialize vim-plug
call plug#begin('~/.vim/plugged')


" ---------------------------- "
" PLUGINS
" ---------------------------- "

Plug 'https://github.com/w0rp/ale'               " asynchronous syntax checker
Plug 'https://github.com/mattn/emmet-vim'        " web-development toolkit
Plug 'https://github.com/junegunn/goyo.vim'      " zen mode
Plug 'https://github.com/Yggdroot/indentLine'    " python indent visualizer
Plug 'https://github.com/luochen1990/rainbow'    " rainbow parens
Plug 'https://github.com/majutsushi/tagbar'      " Tag bar
Plug 'https://github.com/mbbill/undotree'        " go through undos
Plug 'https://github.com/osyo-manga/vim-over'
Plug 'https://github.com/lbeckman314/vim'        " forked color scheme (dracula)
Plug 'https://github.com/bling/vim-airline'      " status bar
"Plug 'https://github.com/ludovicchabant/vim-gutentags' " Tag generator
Plug 'https://github.com/lervag/vimtex'
Plug 'https://github.com/justinmk/vim-sneak'
Plug 'https://github.com/chrisbra/Colorizer'
Plug 'https://github.com/jiangmiao/auto-pairs'

if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
let g:deoplete#enable_at_startup = 1
call plug#end()

let g:vimtex_quickfix_mode = 0
let g:vimtex_view_method = 'zathura'

" https://vi.stackexchange.com/questions/7258/how-do-i-prevent-vim-from-hiding-symbols-in-markdown-and-json
let g:indentLine_setConceal = 0

let g:AutoPairsMultilineClose = 0
let g:AutoPairs = {'(':')', '[':']', '{':'}'}
"Plug 'https://github.com/edkolev/tmuxline.vim'


" ---------------------------- "
" MAPPINGS
" ---------------------------- "

" Pick a leader key
let mapleader = " "

nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
" By default timeoutlen is 1000 ms
set timeoutlen=500

"http://vimcasts.org/transcripts/16/en/
"nmap <leader>w :set wrap!<CR>
"command! -nargs=* Wrap set wrap linebreak nolist
set wrap nolist linebreak

" https://kevinjalbert.com/vim-substitution-feedback-using-vim-over/
nnoremap <leader>s :OverCommandLine<CR> %s/

function! VisualFindAndReplace()
        :OverCommandLine %s/
        :noh
endfunction
nnoremap <Leader>v :call VisualFindAndReplace()<CR>

function! VisualFindAndReplaceWithSelection() range
        :'<,'>OverCommandLine s/
        :noh
        endfunction
        xnoremap <Leader>v :call VisualFindAndReplaceWithSelection()<CR>

" https://stackoverflow.com/questions/16082991/vim-switching-between-files-rapidly-using-vanilla-vim-no-plugins
nnoremap <leader>l :ls<CR>:b<space>

" https://stackoverflow.com/questions/23292917/vim-key-mapping-compile-and-run-for-java-and-c-code
" compile
nnoremap <leader>cc :w <bar> exec '!gcc --std=c99 '.shellescape('%').' -o '.shellescape('%:r').'.out;echo;echo;echo Press ENTER to continue; read line;exit'<CR><CR>

" compile and run
nnoremap <leader>bb :w <bar> exec '!gcc --std=c99 '.shellescape('%').' -o '.shellescape('%:r').'.out && '.shellescape('%:p:r').'.out;echo;echo;echo Press ENTER to continue; read line;exit'<CR><CR>

" run
nnoremap <leader>B :w <bar> exec '!'.shellescape('%:p:r').'.out;echo;echo;echo Press ENTER to continue; read line;exit'<CR><CR>


" ---------------------------- "
" COLORS
" ---------------------------- "

" Color scheme (dracula)
" https://stackoverflow.com/questions/5698284/in-my-vimrc-how-can-i-check-for-the-existence-of-a-color-scheme
silent! colorscheme dracula


" ---------------------------- "
" SETTINGS
" ---------------------------- "

" https://old.reddit.com/r/vim/comments/bgumn8/til_about_diffoptiwhite/
set diffopt+=hiddenoff

" https://github.com/gopasspw/gopass/blob/master/docs/setup.md
au BufNewFile,BufRead /dev/shm/gopass.* setlocal noswapfile nobackup noundofile

" don't hide characters
set conceallevel=0

" don't add additional comments
autocmd FileType * set formatoptions-=ro

" Don't try to be vi compatible
set nocompatible

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

"set formatoptions="cql" "this is the old value -> tcqrn1
" https://vi.stackexchange.com/questions/1983/how-can-i-get-vim-to-stop-putting-comments-in-front-of-new-lines
au FileType cpp setlocal fo-=c fo-=r fo-=o

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

" and because i'm a heathen...
set mouse=a

" airline takes care of showing the command pretty well
set noshowcmd

" http://vim.wikia.com/wiki/Disable_automatic_comment_insertion
au FileType c,cpp setlocal comments-=:// comments+=f://

" https://stackoverflow.com/questions/6076592/vim-set-formatoptions-being-lost
" autocmd BufNewFile,BufRead * setlocal formatoptions+=cqn

set autoindent

autocmd FileType markdown set nonumber

" https://github.com/vim-airline/vim-airline/issues/124
set ttimeoutlen=50

set shell=zsh

" ---------------------------- "
" FUNCTIONS
" ---------------------------- "

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

" Simple re-format for minified Javascript
command! UnMinify call UnMinify()
function! UnMinify()
    %s/{\ze[^\r\n]/{\r/g
    %s/){/) {/g
    %s/};\?\ze[^\r\n]/\0\r/g
    %s/;\ze[^\r\n]/;\r/g
    %s/[^\s]\zs[=&|]\+\ze[^\s]/ \0 /g
    normal ggVG=
endfunction

command! MyFormat call MyFormat()
function! MyFormat()
    let currBuff=bufnr("%") | let save_pos = getpos(".") | silent bufdo %s/\s\+$//e | silent retab | update | execute 'buffer ' . currBuff | call setpos('.', save_pos) | noh
endfunction
