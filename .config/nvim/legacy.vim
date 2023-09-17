set nocompatible		" use VIM settings rather than Vi settings

set number			" show current line number
set relativenumber		" show relative line numbers

syntax on			" enable syntax highlighting

set updatetime=100		" set updatetime to 100 milliseconds (0.1 s)

set cindent			" enable automatic indentation

" set default indentation to 8 characters tabs
set tabstop=8
set shiftwidth=8

filetype plugin indent on	" enable filetype detection and plugins

" define indentation settings for specific file types
augroup custom_indent
  au!

  " C: set indent to 8 spaces
  au FileType c set tabstop=8 shiftwidth=8

  " Java, Python, R, Markdown and R Markdown: set indent to 4 spaces, convert tabs to spaces
  au FileType java,python,r,md,rmd set tabstop=4 shiftwidth=4 expandtab

  " HTML, CSS, JavaScript and Lua: set indent to 2 spaces, convert tabs to spaces
  au FileType html,css,javascript,lua set tabstop=2 shiftwidth=2 expandtab
augroup END

" autowrap line at 80 columns
set textwidth=80
set formatoptions+=t

set colorcolumn=80		" mark 80th column

set laststatus=2		" display status bar
set statusline=%<%f\ \|\ %{&enc}\[\%{&ff}\]\ %h%m%r%=%-8.(%l,%c%V%)\ %P

" highlight cursor line
set cursorline
hi CursorLineNr cterm=NONE ctermfg=white
hi clear CursorLine
hi CursorLine ctermbg=236

" enable thin cursor in insert mode
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"
set ttimeout
set ttimeoutlen=1
set ttyfast

" mark trailing spaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" enhancement for netrw
let g:netrw_banner=0		" disable banner
let g:netrw_liststyle=3		" enable tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" enhancement for termdebug
packadd termdebug		" enable termdebug
let g:termdebug_wide=1		" split vertically

" disable arrow keys insert mode
inoremap <Down> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>
inoremap <Up> <Nop>

" disable arrow keys in normal mode
nnoremap <Down> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
nnoremap <Up> <Nop>

" disable arrow keys in visual mode
vnoremap <Down> <Nop>
vnoremap <Left> <Nop>
vnoremap <Right> <Nop>
vnoremap <Up> <Nop>

" map global leader key to space
nnoremap <SPACE> <Nop>
let mapleader=" "

" terminal emulator
nnoremap <Leader>sh :terminal<CR>
nnoremap <Leader>shs :split<CR>:wincmd j<CR>:term<CR>
nnoremap <Leader>sht :tabnew<CR>:term<CR>

" GNU Debugger
nnoremap <Leader>gdb :Termdebug<CR>shell ls<CR>
nnoremap <Leader>asm :Termdebug<CR>shell ls<CR><C-w>l:Asm<CR><C-w>h

" format using preset indentation
nnoremap <Leader>fmt maggvG=`a

" wrap using preset textwidth
nnoremap <Leader>wrap maggvGgq`a

" reset highlight
nnoremap <Leader>nh :nohlsearch<CR>

" file explore
nnoremap <Leader>fe :Explore<CR>
nnoremap <Leader>vfe :Vexplore<CR>

" buffer switching
nnoremap <Leader>bn :bnext<CR>
nnoremap <Leader>bp :bprevious<CR>
