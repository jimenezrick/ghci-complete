if exists('b:did_ftplugin_ghci_complete')
	finish
endif
let b:did_ftplugin_ghci_complete = 1

let g:ghci_complete_batch_size = 10
let g:ghci_complete_timeout = 4000

setlocal completeopt+=preview
setlocal omnifunc=ghci#omnifunc

autocmd TextChangedI <buffer> :pclose
