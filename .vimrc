set number
"输出命令显示
set showcmd
""set autoindent 
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
set autoread
"when vimrc is edited,reload it
":colo torte 
autocmd! bufwritepost vimrc source ~/.vimrc

"the commandbar height
"set cmdheight=2
"change buffer without saving
"set hid

set smartcase

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

set noswapfile


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

:inoremap " ""<ESC>i
:inoremap ' ''<ESC>i
:inoremap ` ``<ESC>i

function ClosePair(char)
    if getline('.')[col('.') - 1] == a:char
        return "\<Right>"
    else
        return a:char
    endif
endf







