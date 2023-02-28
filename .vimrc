set number			" show current line number
set relativenumber		" show relative line numbers

syntax on			" enable syntax highlighting

set cindent			" enable automatic indentation
set shiftwidth=8		" set indentation to 8 spaces

" autowrap line at 80 columns
set textwidth=80
set formatoptions+=t

set colorcolumn=80		" mark 80th column

set laststatus=2		" display status bar
set ruler                       " display cursor position on status bar

" highlight cursor line
set cursorline
hi CursorLineNr cterm=NONE ctermfg=white
hi clear CursorLine
hi CursorLine ctermbg=236

" auto highlight current word when idle
" highlight the word under cursor (CursorMoved is inperformant)
highlight WordUnderCursor cterm=underline gui=underline
autocmd CursorHold * call HighlightCursorWord()
function! HighlightCursorWord()
	" disable overwrite when hlsearch is active
	let search = getreg('/')
	let cword = expand('<cword>')
	if match(cword, search) == -1
		exe printf('match WordUnderCursor /\V\<%s\>/', escape(cword, '/\'))
	endif
endfunction
" modify autosave delay, cursorhold trigger, default: 4000ms
setl updatetime=0

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
