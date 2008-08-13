" Vimchant - An Enchant-based spell-checker for Vim text editor
"
" Maintainer:	Teemu Likonen <tlikonen@iki.fi>
" License:	Public domain

" {{{1 The Beginning
if &compatible || v:version < 700 || !executable('enchant') || exists('g:loaded_vimchant')
	finish
endif
let s:save_cpo = &cpo
set cpo&vim

let s:spellcheck_prg = 'enchant -l'

let s:match_group = 'SpellBad'
if !hlexists(s:match_group)
	execute 'highlight '.s:match_group.' term=reverse ctermbg=Red gui=undercurl guisp=Red'
endif

function! s:SpellCheckSwitch(switch, ...) "{{{1
	if tolower(a:switch) == 'on'
		let switch = 1
	elseif tolower(a:switch) == 'off'
		let switch = 0
	elseif tolower(a:switch) == 'switch'
		if exists('#VimchantSpellCheck#CursorHold#<buffer>')
			let switch = 0
		else
			let switch = 1
		endif
	else
		return
	endif
	if exists('a:1') && a:1 == 1
		let silence = 1
	else
		let silence = 0
	endif

	if switch
		if !exists('b:vimchant_spellcheck_save_isk')
			let b:vimchant_spellcheck_save_isk = &l:isk
		endif
		setlocal isk&
		call s:CheckSpelling()
		augroup VimchantSpellCheck
			autocmd! * <buffer>
			autocmd CursorHold,CursorHoldI <buffer> call s:CheckSpelling()
			autocmd BufDelete <buffer> call s:RemoveAutoCmds(expand('<abuf>'))
			autocmd BufLeave,WinLeave,TabLeave <buffer> call clearmatches()
		augroup END
		if !silence | echo 'Spell-checking turned on' | endif
	else
		augroup VimchantSpellCheck
			autocmd! * <buffer>
		augroup END
		call clearmatches()
		if exists('b:vimchant_spellcheck_save_isk')
			let &l:isk = b:vimchant_spellcheck_save_isk
			unlet b:vimchant_spellcheck_save_isk
		endif
		if &term != 'builtin_gui'
			redraw!
		endif
		if !silence | echo 'Spell-checking turned off' | endif
	endif
endfunction

function! s:RemoveAutoCmds(buffer) "{{{1
	call clearmatches()
	execute 'autocmd! VimchantSpellCheck * <buffer='.a:buffer.'>'
endfunction

function! s:CheckSpelling() "{{{1
	call clearmatches()

	let line = line('w0')
	let last_line = line('w$')
	let content_string = ' '
	while line <= last_line
		let fold_end = foldclosedend(line)
		if fold_end > 0
			let line = fold_end
		else
			let content_string .= getline(line).' '
		endif
		let line += 1
	endwhile

	let content_string = substitute(content_string,'\v\s[[:punct:]]*''(\k)',' \1','g')
	let content_string = substitute(content_string,'\v(\k)''[[:punct:]]*\s','\1 ','g')
	let spelling_errors = split(system(s:spellcheck_prg,content_string))
	for word in spelling_errors
		call matchadd(s:match_group,'\V\C\<'.word.'\>')
	endfor
	if &term != 'builtin_gui'
		redraw!
	endif
endfunction

" {{{1 Commands and key mappings

if !exists(':VimchantSpellCheckOn') && !exists(':VimchantSpellCheckOff')
	command VimchantSpellCheckOn  call s:SpellCheckSwitch('On')
	command VimchantSpellCheckOff call s:SpellCheckSwitch('Off')
endif

nnoremap <silent> <Plug>VimchantSpellCheckSwitch :call <SID>SpellCheckSwitch('Switch')<CR>

if maparg('<Leader>ss') == '' && !hasmapto('<Plug>VimchantSpellCheckSwitch')
	nmap <Leader>ss <Plug>VimchantSpellCheckSwitch
endif

" {{{1 The End
let g:loaded_vimchant = 1
let &cpo = s:save_cpo
" vim600: fdm=marker
