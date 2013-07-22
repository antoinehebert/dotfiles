filetype off

" Get pathogen up and running
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

filetype plugin indent on
filetype plugin on

"set nocompatible

colorscheme molokai

syntax on

if has("gui_running")
  set lines=50 columns=100
endif

" naviguate where there's no text
set virtualedit=all

set tabstop=3
set shiftwidth=3
set softtabstop=3
set expandtab

" tabs are 2 chars for xml... 
" autocmd Filetype xml setlocal tabstop=2
" autocmd Filetype xml setlocal shiftwidth=2
" autocmd Filetype xml setlocal softtabstop=2

set encoding=utf-8
" When the page starts to scroll, keep the cursor 8 lines from the top and 8
" lines from the bottom
set scrolloff=8
set autoindent
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set visualbell
"set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start
set laststatus=2
"set relativenumber
"set undofile

"nnoremap / /\v
"vnoremap / /\
set ignorecase
set smartcase
"set gdefault
set incsearch
set showmatch
set hlsearch
"clear search highlight
nnoremap <leader><space> :noh<cr>

"set wrap
set textwidth=80
set formatoptions=qrn1
"set showbreak==>
"set colorcolumn=85

" Highlight over 80 char with dark red
"highlight OverLength ctermbg=red guibg=#582929
"match OverLength /\%81v.\+/

" decent Font under windows
if has("gui_running")
  if has("win32")
    set guifont=Consolas:h9
  elseif has("gui_gtk2")
    set guifont=Inconsolata\ Medium\ 10
  endif
endif

"-----------------------------------------------------------------------------
" NERD Tree Plugin Settings
"-----------------------------------------------------------------------------
" open/close nerdtree
nmap <leader>n :NERDTreeToggle<cr>
" open nerd tree to the current file
nmap <leader>m :NERDTreeFind<cr>
"Show hidden files
"let NERDTreeShowHidden=1

"-----------------------------------------------------------------------------
" FuzzyFinder plugin
"-----------------------------------------------------------------------------
" FuzzyFile search shortcut
nnoremap <leader>f :FufFile<cr>

" FuzzyBuffer search shortcut
nnoremap <leader>b :FufBuffer<cr>

" FuzzyTags search shortcut
nnoremap <leader>t :FufTag<cr>

"-----------------------------------------------------------------------------
" File Switcher plugin
"-----------------------------------------------------------------------------
" FileSwitch Here
nnoremap <leader><tab> :FSHere<cr>

" FileSwitch Left
"nnoremap <leader><tab>l :FSSplitLeft<cr>
