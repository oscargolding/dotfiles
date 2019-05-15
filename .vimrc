" Options for syntax
syntax enable
set termguicolors
" Relative line numbers
set relativenumber
set nu
" Packages (if needed)
execute pathogen#infect()
" PostgreSQL syntax
let g:sql_type_default = 'pgsql'
set autoindent
" Don't go over a certain amount of columns
set textwidth=80
set colorcolumn=80
highlight ColorColumn ctermbg=52
" Don't want any trailing white space on save
autocmd BufWritePre * :%s/\s\+$//e
" Tab
set tabstop=4
" Options for when the vim gui is running
if has('gui')
	set guifont=Source\ Code\ Pro\ 11
	set guioptions-=m
	set guioptions-=T
	set guioptions-=r
	set guioptions-=L
	colorscheme solarized
endif
