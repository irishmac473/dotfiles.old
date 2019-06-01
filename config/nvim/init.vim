" #######################################################################
" ### Author : Caleb McCaffery <irishmac473@gmail.com>                ###
" #######################################################################
" ### Neovim Configuration focused on Web development and HEAVILY     ###
" ### inspired by github.com/martinsvk                                ###
" ### Neovimmer since : Wed Nov 1 2017                                ###
" ### Vimmer since    : sometime in 2015                              ###
" #######################################################################

" ======================================================================
" 1.0 Plugin manager (Plug) settings
" ======================================================================
"{{{

" Autoinstall vim plug {{{

if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -flo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  augroup plug_install
    autocmd VimEnter * PlugInstall
  augroup END
endif
"}}}

call plug#begin('~/.config/nvim/plugged')
" ======================================================================
" 1.1 Plugin list
" ======================================================================

" ======================================================================
" Language agnostic plugins {{{
" ======================================================================

" Autocomplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Automatically closing pair stuff
Plug 'cohama/lexima.vim'
" Commenting support
Plug 'tpope/vim-commentary'
"}}}

" ======================================================================
" Other languages {{{
" ======================================================================

" Markdown syntax
Plug 'tpope/vim-markdown'
" Git syntax
Plug 'tpope/vim-git'
" Tmux syntax
Plug 'keith/tmux.vim'
" VimL syntax
Plug 'Shougo/neco-vim'
"}}}

" ======================================================================
" External tools intergration plugins {{{
" ======================================================================

" Fuzzy searching/replacing/etc
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
" Git swiss-army knife
Plug 'tpope/vim-fugitive'
" Git changes showed on line numbers
Plug 'airblade/vim-gitgutter'
" Color picker
Plug 'KabbAmine/vCoolor.vim', { 'on': ['VCoolor', 'VCase'] }
"}}}

" ======================================================================
" Colorschemes {{{
" =====================================================================

" Badwolf
Plug 'sjl/badwolf'
" Molokai
Plug 'tomasr/molokai'
" Tender
Plug 'jacoborus/tender.vim'
" Hybrid
Plug 'w0ng/vim-hybrid'
"}}}

" ======================================================================
" Other {{{
" ======================================================================

" Provide a simpler way to use motions (<leader><leader>motion)
Plug 'easymotion/vim-easymotion'

"}}}

" ======================================================================
" 1.2 End of plugin declaration
" ======================================================================
call plug#end()
"}}}

" ======================================================================
" 2.0 Basic settings
" ======================================================================
"{{{
"{{{

set encoding=utf-8                              " The encoding displayed.
set fileencoding=utf-8                          " The encoding written to file.
scriptencoding utf-8                            " Set utf-8 as default script encoding

set shell=/usr/bin/zsh                          " Setting shell to zsh
set number                                      " Line numbers on
set relativenumber                              " Relative line numbers on
set showmode                                    " Always show mode
set showcmd                                     " Show commands as you type them
set textwidth=72                                " Text width is 72 characters
set colorcolumn=72                              " Visual line at column 72
set cmdheight=1                                 " Command line height
set pumheight=10                                " Completion window max size
set noswapfile                                  " New buffers will be loaded without creating a swapfile
set hidden                                      " enables to switch between unsaved buffers and keep undo history
set clipboard+=unnamed                          " Allow to use system clipboard
set lazyredraw                                  " Don't redraw while executing macros (better performance)
set showmatch                                   " Show matching brackets when text indicator is over them
set matchtime=2                                 " How many tenths of a second to blink when matching brackets
set nostartofline                               " Prevent cursor from moving to beginning of line when switching buffers
set virtualedit=block                           " To be able to select past EOL in visual block mode
set nojoinspaces                                " No extra space when joining a line which ends with . ? !
set scrolloff=5                                 " Scroll when closing to top or bottom of the screen
set updatetime=1000                             " Update time used to create swap file or other things
set suffixesadd+=.js,.rb                        " Add js and ruby files to suffixes
set synmaxcol=220                               " Don't try to syntax highlight minified files
"}}}

" ======================================================================
" 2.1 Indention settings (2 space tabs) {{{
" ======================================================================
set autoindent
set expandtab
set shiftwidth=2
set softtabstop=2
"}}}

" ======================================================================
" 2.2 Split settings (more natural) {{{
" ======================================================================
set splitbelow                                  " Splitting a window will put the new window below the current
set splitright                                  " Splitting a window will put the new window right of the current
"}}}

" ======================================================================
" 2.3 Timeout Settings {{{
" ======================================================================
" Time out on key codes but not mappings. Basically this makes terminal Vim work sanely. (by Steve Losh)
set notimeout
set ttimeout
set ttimeoutlen=10
"}}}

" ======================================================================
" 2.4 Spelling settings {{{
" ======================================================================
set spellfile=~/.config/nvim/dictionary.utf-8.add
set spelllang=en_us                             " Set languague to US English
set nospell                                     " Disable checking by default
"}}}

" ======================================================================
" 2.5 Search settings {{{
" ======================================================================
set ignorecase                                  " Ignore case by default
set smartcase                                   " Make search case sensitive only if it contains uppercase letters
set wrapscan                                    " Search again from top when reached the bottome
set nohlsearch                                  " Don't highlight after search
"}}}

" ======================================================================
" 2.6 Persistent undo settings {{{
" ======================================================================
if has('persistent_undo')
  set undofile
  set undodir=~/.config/nvim/tmp/undo//
endif
"}}}

" ======================================================================
" 2.7 White characters settings {{{
" ======================================================================
set list                                        " Show listchars by default
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮,trail:·,nbsp:·
set showbreak=↪
"}}}

" ======================================================================
" 2.8 Filetype settings {{{
" ======================================================================
filetype plugin on
filetype indent on
"}}}

" ======================================================================
" 2.9 Folding settings {{{
" ======================================================================
set foldmethod=marker                           " Markers are used to specify folds.
set foldlevel=2                                 " Start folding automatically from level 2
set fillchars="fold: "                          " Characters to fill the statusline and vertical seperators
"}}}

" ======================================================================
" 2.10 Omni completion settings {{{
" ======================================================================
set completeopt-=preview                        " Don't show preview scratch buffers
set wildignore=*.o,*.obj,*~
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=tmp/**
"}}}

" ======================================================================
" 2.11 Neovim specific settings {{{
" ======================================================================
if has('nvim')
  let g:loaded_python_provider=1                " Disable python 2 interface
  let g:python_host_skip_check=1                " Skip python 2 host check
  let g:python3_host_prog='/usr/bin/python3'    " Set python 3 host program
  set inccommand=nosplit                        " Live preview of substitutes and other similar commands
endif
"}}}

" ======================================================================
" 2.12 Terminal settings {{{
" ======================================================================

"}}}

" ======================================================================
" 2.13 True colors settings {{{
" ======================================================================
"if has('termguicolors')
"  set termguicolors " turn on true color support
"endif
"}}}

" ======================================================================
" 2.14 Language settings {{{
" ======================================================================
let $LANG = 'en_US'
"}}}

"}}}

" ======================================================================
" 3.0 Mapping settings
" ======================================================================
"{{{

" ======================================================================
" 3.1 Setting leader {{{
" ======================================================================
let g:mapleader="\<space>"
"}}}

" ======================================================================
" 3.2 Disabling arrow keys, space key, exmode enter with Q key, help with F1, etc. {{{
" ======================================================================
nnoremap <up> <NOP>
nnoremap <down> <NOP>
nnoremap <left> <NOP>
nnoremap <right> <NOP>
nnoremap <bs> <NOP>
nnoremap <delete> <NOP>
inoremap <up> <NOP>
inoremap <down> <NOP>
inoremap <left> <NOP>
inoremap <right> <NOP>
nnoremap <Space> <NOP>
inoremap <F1> <NOP>
nnoremap <F1> <NOP>
nnoremap Q <NOP>
"}}}

" ======================================================================
" 3.3 Vim defaults overriding {{{
" ======================================================================

" Easier window switcing
nmap <silent> <C-w><C-w> :call utils#intelligentCycling()<CR>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Visual linewise up and down by default (and use gj gk to go quicker)
nnoremap j gj
nnoremap k gk
nnoremap gj 5j
nnoremap gk 5k
vnoremap j gj
vnoremap k gk
vnoremap gj 5j
vnoremap gk 5k

" When jump to next match also center screen
" Note: Use :norm! to make it count as one command.
nnoremap <silent> n :norm! nzz<CR>
nnoremap <silent> N :norm! Nzz<CR>
vnoremap <silent> n :norm! nzz<CR>
vnoremap <silent> N :norm! Nzz<CR>

" Same when moving up and down
nnoremap <C-u> <C-u>zz
nnoremap <C-d> <C-d>zz
nnoremap <C-f> <C-f>zz
nnoremap <C-b> <C-b>zz
vnoremap <C-u> <C-u>zz
vnoremap <C-d> <C-d>zz
vnoremap <C-f> <C-f>zz
vnoremap <C-b> <C-b>zz

" Remap H and L (top, bottome of screen to left and right end of line)
nnoremap H ^
nnoremap L $
vnoremap H ^
vnoremap L g_

" More logical Y (default was alias for yy)
nnoremap Y y$

" Quick replay 'q' macro
nnoremap Q @q

" Fix the cw at the end of line bug default vim has special treatment
nmap cw ce
nmap dw de

" Uppercase word in insert mode
inoremap <C-u> <ESC>mzgUiw`za

" Matching brackets with TAB (using matchit) (Breaks wth <C-i> jump))
map <TAB> %
silent! unmap [%
silent! unmap ]%

" Don't cancel visual select when shifting
xnoremap < <gv
xnoremap > >gv

" Stay down after creating fold
vnoremap zf mzzf`zzz

" Make . work with visually selected lines
xnoremap . :norm.<CR>

" f & F use easymotion to jump to characters
map  f <Plug>(easymotion-bd-f)
nmap f <Plug>(easymotion-overwin-f)
map  F <Plug>(easymotion-bd-f)
nmap F <Plug>(easymotion-overwin-f)
"}}}

" ======================================================================
" 3.4 Common tasks {{{
" ======================================================================

" exit insert mode with jk
inoremap jk <ESC>
tnoremap jk <C-\><C-n>

" Quick save and close buffer
nnoremap ,w :w<CR>
nnoremap <silent> ,c :Sayonara!<CR>
nnoremap <silent> ,q :Sayonara<CR>

" Yank and paste from clipboard
nnoremap ,y "+y
vnoremap ,y "+y
nnoremap ,yy "+yy
nnoremap ,p "+p

" Move visual block
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Reselect last-pasted text
nnoremap gp `[v`]

" Keep the cursor in place while joining lines
nnoremap J mzJ`z

" [S]plit line (sister to [J]oin lines) S is covered by cc.
nnoremap S mzi<CR><ESC>`z

" Easier fold toggling
nnoremap ,z za

" Start substitute on current word under the cursor
nnoremap ,s :%s/\<<C-r><C-w>\>//gc<Left><Left><Left>

" Start search on current word under the cursor
nnoremap ,/ /\<<C-r><C-w>\><CR>

" Start reverse search on current word under the cursor
nnoremap ,? ?\<<C-r><C-w>\><CR>

" Faster sort
vnoremap ,s :!sort<CR>

" Fix spelling error on the go
inoremap <C-l> <C-g>u<ESC>[s1z=`]a<C-g>u
"}}}

" ======================================================================
" 3.5 F-key actions {{{
" ======================================================================

" NERDTree wrapper
" nnoremap <silent> <F1> :call utils#nerdWrapper()<CR>

" Turn on relativenumber (use this until neovim updates so it no longer turns off rn when exiting fzf)
nnoremap <silent> <F2> :set relativenumber<CR>

" Free
" nnoremap <silent> <F3>

" Toggle spelling
nnoremap <silent> <F4> :set spell!<CR> :set spell?<CR>

" Source (reload configuration)
nnoremap <silent> <F5> :source /home/irishmac473/.config/nvim/init.vim<CR>

" Toggle search highlight
nnoremap <silent> <F6> :set nohlsearch!<CR> :set nohlsearch?<CR>

" Toggle white characters visibility
nnoremap <silent> <F7> :set list!<CR> :set list?<CR>

" New term buffer named
nnoremap <silent> <F8> :terminal zsh \#

" Free
" nnoremap <silent> <F9>

" Free
" nnoremap <silent> <F1>
"
" " Free (probably won't use due to system mapping)
" " nnoremap <silent> <F11>
"
" " Informative echo
" nnoremap <F12> :call utils#showToggles()<CR>

"}}}

" ======================================================================
" 3.7 Command abbreviations and mappings {{{
" ======================================================================

" Quitting and saving all
cnoremap ww wqall
cnoremap qq qall
"}}}

" ======================================================================
" 3.8 Custom commands and functions {{{
" ======================================================================
"}}}

"}}}

" ======================================================================
" 4.0 Plugins settings
" ======================================================================
"{{{

" ======================================================================
" 4.1 FZF {{{
" ======================================================================
let $FZF_DEFAULT_OPTS='--reverse'
let $FZF_DEFAULT_COMMAND='rg --files'
let g:fzf_layout = { 'down': '~50%' }
"}}}

" ======================================================================
" 4.3 Gitgutter settings {{{
" ======================================================================
let g:gitgutter_map_keys=0
let g:gitgutter_max_signs=9999
let g:gitgutter_sign_added='+'
let g:gitgutter_sign_modified='~'
let g:gitgutter_sign_removed='-'
let g:gitgutter_sign_modified_removed='~'
let g:gitgutter_sign_removed_first_line='-'
"}}}

" ======================================================================
" 4.7 Deoplete autocomplete settings {{{
" ======================================================================
let g:deoplete#enable_at_startup=1
let g:deoplete#enable_refresh_always=0
let g:deoplete#enable_smart_case=1
let g:deoplete#file#enable_buffer_path=1

let g:deoplete#sources={}
let g:deoplete#sources._    = ['around', 'buffer', 'file', 'ultisnips']
let g:deoplete#sources.ruby = ['around', 'buffer', 'member', 'file', 'ultisnips']
let g:deoplete#sources.vim  = ['around', 'buffer', 'member', 'file', 'ultisnips']
let g:deoplete#sources['javascript.jsx'] = ['around', 'buffer', 'file', 'ultisnips', 'ternjs']
let g:deoplete#sources.css  = ['around', 'buffer', 'member', 'file', 'omni', 'ultisnips']
let g:deoplete#sources.scss = ['around', 'buffer', 'member', 'file', 'omni', 'ultisnips']
let g:deoplete#sources.html = ['around', 'buffer', 'member', 'file', 'omni', 'ultisnips']
"}}}

" ======================================================================
" 4.8 Plug settings {{{
" ======================================================================
let g:plug_timeout=20
"}}}

" ======================================================================
" 4.9 Vim-markdown settings {{{
" ======================================================================
let g:markdown_fenced_languages=[
      \'bash=sh',
      \'git=gitconfig',
      \'javascript',
      \'lua',
      \'ruby',
      \'tmux',
      \'viml=vim',
      \'xdefaults',
      \'zsh']
"}}}

" ======================================================================
" 4.14 vCoolor settings {{{
" ======================================================================
let g:vcoolor_disable_mappings=1
let g:vcoolor_lowercase=1
"}}}

" ======================================================================
" 4.18 Easymotion settings {{{
" ======================================================================
let g:EasyMotion_smartcase = 1
let g:EasyMotion_use_smartsign_us = 1
"}}}

"}}}

" ======================================================================
" 5.0 Plugin mappings
" ======================================================================
"{{{

" ======================================================================
" 5.1 FZF {{{
" ======================================================================

" Search files starting in home directory ([O]pen file)
nnoremap <silent> <leader>o :Files~<CR>
" Search file recursively ([o]pen file)
nnoremap <silent> <leader>O :Files<CR>
" Search git status (edited) [f]iles
nnoremap <silent> <leader>f :GFiles?<CR>
" Search in local buffer [c]ommits
nnoremap <silent> <leader>c :BCommits<CR>
" Search in all the project [C]ommits
nnoremap <silent> <leader>C :Commits<CR>
" Search between open files - [b]uffers
nnoremap <silent> <leader>b :Buffers<CR>
" Search in [l]ines on current buffer
nnoremap <silent> <leader>l :BLines<CR>
" Search in all the opened buffers [L]ines
nnoremap <silent> <leader>L :Lines<CR>
" Search in ultisnips [s]nippets
nnoremap <silent> <leader>s :Snippets<CR>
" Search in [m]arks
nnoremap <silent> <leader>m :Marks<CR>
" Search in edited files [h]istory
nnoremap <silent> <leader>h :History<CR>
" Search in search [/] history
nnoremap <silent> <leader>/ :History/<CR>
"}}}

" ======================================================================
" 5.2 Gitgutter {{{
" ======================================================================
nnoremap [h :GitGutterPrevHunk<CR>
nnoremap ]h :GitGutterNextHunk<CR>
nnoremap ,hs :GitGutterStageHunk<CR>
nnoremap ,hr :GitGutterRevertHunk<CR>
"}}}

" ======================================================================
" 5.5 Deoplete autocomplete {{{
" ======================================================================
" Insert <TAB> or select next match
inoremap <silent> <expr> <Tab> utils#tabComplete()

" Manually trigger tag autocomplete
inoremap <silent> <expr> <C-]> utils#manualTagComplete()

" <C-h>, <BS>: close popup and delete backword char
inoremap <expr><C-h> deolete#mappings#smart_close_popup()."\<C-h>"
inoremap <expr><BS> deoplete#mappings#smart_close_popup()."\<C-h>"
"}}}

" ======================================================================
" 5.6 Vim-Plug {{{
" ======================================================================
nnoremap <leader>pi :PlugInstall<CR>
nnoremap <leader>pu :PlugUpdate<CR>
nnoremap <leader>pU :PlugUpgrade<CR>
nnoremap <leader>pc :PlugClean<CR>
"}}}

"}}}

" ======================================================================
" 6.0 Color and highlighting settings
" ======================================================================
"{{{
" Syntax highlighting {{{
syntax on
"}}}

" Color scheme {{{
colorscheme hybrid
set background=dark
"}}}

" Highlight VCS conflict markers {{{
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'
"}}}

" Highlight term cursor differently {{{
highlight TermCursor ctermfg=green guifg=green
"}}}

" Remove underline in folded lines {{{
hi! Folded term=NONE cterm=NONE gui=NONE ctermbg=NONE
"}}}

"}}}
"
"}}}
