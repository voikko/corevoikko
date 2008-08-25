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

" {{{1 Commands and key mappings

if !exists(':VimchantSpellCheckOn') && !exists(':VimchantSpellCheckOff')
	command VimchantSpellCheckOn  call vimchant#SpellCheckSwitch('On')
	command VimchantSpellCheckOff call vimchant#SpellCheckSwitch('Off')
endif

nnoremap <silent> <Plug>VimchantSpellCheckSwitch :call vimchant#SpellCheckSwitch('Switch')<CR>
nnoremap <silent> <Plug>VimchantChangeLanguage :call vimchant#ChangeLanguage()<CR>

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
