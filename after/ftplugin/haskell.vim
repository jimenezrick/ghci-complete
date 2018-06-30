if exists('b:did_ftplugin_ghci_complete') && b:did_ftplugin_ghci_complete
	finish
endif
let b:did_ftplugin_ghci_complete = 1

let g:ghci_complete_batch_size = 10

setlocal completeopt+=preview
setlocal omnifunc=ghci#omnifunc
autocmd TextChangedI * :pclose

" TODO: haskell syntax preview window: http://vimdoc.sourceforge.net/htmldoc/windows.html
