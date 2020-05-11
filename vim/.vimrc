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

" ---------- "
" Emacs-like search/replace preview
" ---------- "
Plug 'https://github.com/osyo-manga/vim-over'

" ---------- "
" Undo tree
" ---------- "
Plug 'https://github.com/mbbill/undotree'        " go through undos

" ---------- "
" Status bar
" ---------- "
Plug 'https://github.com/bling/vim-airline'      " status bar

" ---------- "
" Navigation
" ---------- "
Plug 'https://github.com/justinmk/vim-sneak'

" ---------- "
" Colors/visualizers
" ---------- "
Plug 'https://github.com/lbeckman314/vim'        " forked color scheme (dracula)
Plug 'https://github.com/chrisbra/Colorizer'
Plug 'https://github.com/luochen1990/rainbow'    " rainbow parens
Plug 'https://github.com/Yggdroot/indentLine'    " python indent visualizer
" https://vi.stackexchange.com/questions/7258/how-do-i-prevent-vim-from-hiding-symbols-in-markdown-and-json
let g:indentLine_setConceal = 0
let g:indentLine_color_term = 239

" ---------- "
" Latex
" ---------- "
Plug 'https://github.com/lervag/vimtex'
let g:vimtex_quickfix_mode = 0
let g:vimtex_view_method = 'zathura'

" ---------- "
" Comments
" ---------- "
Plug 'https://github.com/scrooloose/nerdcommenter'

" ---------- "
" Sessions
" ---------- "
Plug 'https://github.com/tpope/vim-obsession'
Plug 'https://github.com/dhruvasagar/vim-prosession'

" ---------- "
" Language Server Protocol
" ---------- "
Plug 'https://github.com/prabirshrestha/vim-lsp'
Plug 'https://github.com/prabirshrestha/async.vim'

" ---------- "
" Startup
" ---------- "
Plug 'https://github.com/mhinz/vim-startify'
Plug 'https://github.com/iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
Plug 'https://github.com/rhysd/vim-clang-format'
Plug 'https://github.com/kana/vim-operator-user'
Plug 'https://github.com/alvan/vim-closetag'
Plug 'https://github.com/dhruvasagar/vim-table-mode'
Plug 'https://github.com/jremmen/vim-ripgrep'

" ---------- "
" Tags
" ---------- "
Plug 'https://github.com/majutsushi/tagbar'      " Tag bar
"Plug 'https://github.com/ludovicchabant/vim-gutentags' " Tag generator

" ---------- "
" Autocomplete
" ---------- "
Plug 'https://github.com/alvan/vim-closetag'
let g:closetag_filenames = '*.html,*.xhtml,*.xml'

Plug 'https://github.com/jiangmiao/auto-pairs'
let g:AutoPairsMultilineClose = 0
let g:AutoPairs = {'(':')', '[':']', '{':'}'}

if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
let g:deoplete#enable_at_startup = 0

" ---------- "
" Linting
" ---------- "
Plug 'https://github.com/w0rp/ale'               " asynchronous syntax checker
let g:ale_linters = {'rust': ['rls']}
let g:ale_rust_rls_toolchain = 'stable' " this is needed, otherwise rls uses nightly toolchain
let g:rustfmt_command = "rustfmt"
let g:rustfmt_emit_files = 1

" ---------- "
" C family formatter
" ---------- "
Plug 'https://github.com/rhysd/vim-clang-format'

" ---------- "
" Rust
" ---------- "
Plug 'https://github.com/rust-lang/rust.vim'
Plug 'https://github.com/rhysd/rust-doc.vim'

" ---------- "
" Markdown
" ---------- "
Plug 'https://github.com/iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
let g:mkdp_preview_options = {
    \ 'disable_sync_scroll': 1,
    \ }

Plug 'https://github.com/dhruvasagar/vim-table-mode'

call plug#end()

" ---------------------------- "
" MAPPINGS
" ---------------------------- "

" Pick a leader key
let mapleader = " "

" https://stackoverflow.com/questions/16082991/vim-switching-between-files-rapidly-using-vanilla-vim-no-plugins
nnoremap <leader>l :ls<CR>:b<space>

" nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
" By default timeoutlen is 1000 ms
set timeoutlen=500

" http://vimcasts.org/transcripts/16/en/
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

" https://hellricer.github.io/2019/05/21/ctrl-arrows-in-terminal.html
" ctrl+left/right
nmap <ESC>[1;5D <C-Left>
nmap <ESC>[1;5C <C-Right>
cmap <ESC>[1;5D <C-Left>
cmap <ESC>[1;5C <C-Right>
imap <ESC>[1;5D <C-o><C-Left>
imap <ESC>[1;5C <C-o><C-Right>
" ctrl+backspace
nmap <C-h> <C-w>
cmap <C-h> <C-w>
imap <C-h> <C-w>

" ---------------------------- "
" COLORS
" ---------------------------- "

" Color scheme (dracula)
" https://stackoverflow.com/questions/5698284/in-my-vimrc-how-can-i-check-for-the-existence-of-a-color-scheme
silent! colorscheme dracula

" ---------------------------- "
" SETTINGS
" ---------------------------- "

filetype plugin indent on
let g:is_posix = 1

" https://github.com/SpaceVim/SpaceVim/issues/1714
let g:omni_sql_no_default_maps = 1

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

set autoindent

" autocmd FileType markdown set nonumber

" https://github.com/vim-airline/vim-airline/issues/124
set ttimeoutlen=50

set shell=zsh

set number

" add yaml stuffs
" https://lornajane.net/posts/2018/vim-settings-for-working-with-yaml
au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

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

