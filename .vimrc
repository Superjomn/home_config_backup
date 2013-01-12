set number
"输出命令显示
set showcmd
set ruler
set shiftwidth=4
set softtabstop =4
set tabstop=4
set expandtab
set smarttab
set ai
set si
"set to auto read when a file is changed from the outside
set autoread
"when vimrc is edited,reload it
":colo torte 
autocmd! bufwritepost vimrc source ~/.vimrc

"the commandbar height
"set cmdheight=2
"change buffer without saving
"set hid

set smartcase
set noswapfile

"make search act like search in mordern browsers
set incsearch
"set magic on, for regular expressions
set magic

"show matching bracets when text indicator is over them
set showmatch
"how many tenths of a second to blink
set mat=2

"turn backup off
set nobackup

"总是显示状态栏
set laststatus=2

"" set cursorline
"" highlight CursorLine guibg=black ctermbg=darkyellow

":map <C-v> :vsplit<cr><C-w>l
"分割窗口相关 
:map vs :vsplit<cr><C-w>l
:map vl <C-w>l
:map vh <C-w>h
:map vt :split<cr><C-w>k
:map vk <C-w>k
:map vj <C-w>j
"tabs相关" 
:map ten :tabnew <cr> 
:map tp :tabprevious <cr> 
:map tn :tabnext <cr> 
:map tc :tabclose<cr> 
:map tt :e!<cr>

:inoremap uu <c-n>

"for vimim" 
:inoremap bb <C-^>

"模板设置" 
"" :autocmd BufNewFile *.pyx Or $VIMHOME/templates/pyx.tpl
:autocmd BufNewFile * silent! 0r ~/.vim/templates/%:e.tpl
"" :autocmd BufNewFile *.pyx 0r ~/.vim/templates/pyx.tpl

"自动跳跃" 
nnoremap <c-j> /<+.\{-1,}+><cr>c/+>/e<cr>
inoremap <c-j> <ESC>/<+.\{-1,}+><cr>c/+>/e<cr>


filetype plugin on


"for vimim c-6 for single word 
"c-7 for long chinese paragraph
let g:vimim_cloud='sogou'
let g:vimim_punctuation=0


" ======= 引号 && 括号自动匹配 ======= "

:inoremap ( ()<ESC>i
:inoremap ) <c-r>=ClosePair(')')<CR>
:inoremap { {}<ESC>i
:inoremap } <c-r>=ClosePair('}')<CR>
:inoremap [ []<ESC>i
:inoremap ] <c-r>=ClosePair(']')<CR>

":inoremap < <><ESC>i
":inoremap > <c-r>=ClosePair('>')<CR>

"":inoremap " ""<ESC>i
"":inoremap ' ''<ESC>i
"":inoremap ` ``<ESC>i

function ClosePair(char)
    if getline('.')[col('.') - 1] == a:char
        return "\<Right>"
    else
        return a:char
    endif
endf


"-- vim latex-suite setting --
" REQUIRED. This makes vim invoke Latex-Suite when you open a tex file.
filetype plugin on

" IMPORTANT: win32 users will need to have 'shellslash' set so that latex
" can be called correctly.
set shellslash

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*

" OPTIONAL: This enables automatic indentation as you type.
filetype indent on

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'

:map cc <F5>
