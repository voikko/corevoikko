" Vimchant - An Enchant-based spell-checker for Vim text editor
"
" Version:	1.0 (2008-08-19)
" Maintainer:	Teemu Likonen <tlikonen@iki.fi>
" License:	Public domain
"
" GetLatestVimScripts: 2345 1 :AutoInstall: Vimchant

" {{{1 The Beginning
if &compatible || !executable('enchant') || exists('g:loaded_vimchant')
	finish
endif
if v:version < 701 || (v:version == 701 && !has('patch040'))
	echohl WarningMsg
	echo 'Vimchant spell-checker plugin requires Vim version 7.1.040 or later. Sorry.'
	echohl None
	finish
endif

let s:save_cpo = &cpo
set cpo&vim

let s:spellcheck_prg = 'enchant -l'

if !hlexists('SpellBad')
	highlight SpellBad term=reverse ctermbg=1 gui=undercurl guisp=Red
endif
if !hlexists('ErrorMsg')
	highlight ErrorMsg term=standout cterm=bold ctermfg=7 ctermbg=1 guifg=White guibg=Red
endif
if !hlexists('WarningMsg')
	highlight WarningMsg term=standout cterm=bold ctermfg=1 guifg=Red
endif
if !hlexists('Question')
	highlight Question term=standout cterm=bold ctermfg=2 gui=bold guifg=Green
endif

function! s:SpellCheckSwitch(switch, ...) "{{{1
	if a:switch ==? 'on'
		let switch = 1
	elseif a:switch ==? 'off'
		let switch = 0
	elseif a:switch ==? 'switch'
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
		setlocal isk=@,48-57,192-255
		if s:CheckSpelling()
			" There was an error so don't start autocmds.
			return
		endif
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
		if &term != 'builtin_gui' | redraw! | endif
		if !silence | echo 'Spell-checking turned off' | endif
	endif
endfunction

function! s:CheckSpelling() "{{{1
	if exists('b:vimchant_spellcheck_lang') && b:vimchant_spellcheck_lang != ''
		let lang = 'LANG='.split(b:vimchant_spellcheck_lang)[0].' '
	elseif exists('g:vimchant_spellcheck_lang') && g:vimchant_spellcheck_lang != ''
		let lang = 'LANG='.split(g:vimchant_spellcheck_lang)[0].' '
	else
		let lang = ''
	endif

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

	let content_string = tr(content_string,'_',' ')
	let content_string = substitute(content_string,'\v\s[[:punct:]]*''(\k)',' \1','g')
	let content_string = substitute(content_string,'\v(\k)''[[:punct:]]*\s','\1 ','g')
	let spelling_errors = system(lang.s:spellcheck_prg,content_string)
	if v:shell_error != 0
		if spelling_errors =~ '\cCouldn''t create a dictionary for'
			call s:SpellCheckSwitch('Off',1)
			echohl WarningMsg
			echo 'No dictionary available for language "'.substitute(lang,'\v\C^LANG\=(\S*)\s*$','\1','').
						\'". Spell-checking turned off.'
			echohl None
		else
			call s:SpellCheckSwitch('Off',1)
			echohl ErrorMsg
			echo 'Error in executing spell-checking program. Spell-checking turned off.'
			echohl None
		endif
		return 1
	endif
	for word in split(spelling_errors)
		call matchadd('SpellBad','\V\C\<'.word.'\>')
	endfor
	if &term != 'builtin_gui' | redraw! | endif
	return 0
endfunction

function! s:RemoveAutoCmds(buffer) "{{{1
	call clearmatches()
	execute 'autocmd! VimchantSpellCheck * <buffer='.a:buffer.'>'
endfunction

function! s:ChangeLanguage() "{{{1
	echohl Question
	let b:vimchant_spellcheck_lang = input('Language code: ',
				\(exists('b:vimchant_spellcheck_lang') && b:vimchant_spellcheck_lang != '') ?
				\split(b:vimchant_spellcheck_lang)[0] : '')
	echohl None
	if b:vimchant_spellcheck_lang != ''
		let b:vimchant_spellcheck_lang = split(b:vimchant_spellcheck_lang)[0]
	endif
endfunction

" {{{1 Commands and key mappings

if !exists(':VimchantSpellCheckOn') && !exists(':VimchantSpellCheckOff')
	command VimchantSpellCheckOn  call s:SpellCheckSwitch('On')
	command VimchantSpellCheckOff call s:SpellCheckSwitch('Off')
endif

nnoremap <silent> <Plug>VimchantSpellCheckSwitch :call <SID>SpellCheckSwitch('Switch')<CR>
nnoremap <silent> <Plug>VimchantChangeLanguage :call <SID>ChangeLanguage()<CR>

if maparg('<Leader>ss') == '' && !hasmapto('<Plug>VimchantSpellCheckSwitch')
	nmap <Leader>ss <Plug>VimchantSpellCheckSwitch
endif
if maparg('<Leader>sl') == '' && !hasmapto('<Plug>VimchantChangeLanguage')
	nmap <Leader>sl <Plug>VimchantChangeLanguage
endif

" {{{1 The End
let g:loaded_vimchant = 1
let &cpo = s:save_cpo
" vim600: fdm=marker
