if exists('b:did_ftplugin_ghci_complete')
	finish
endif
let b:did_ftplugin_ghci_complete = 1

let g:ghci_complete_batch_size = 4
let g:ghci_complete_timeout = 4000

setlocal completeopt+=preview
setlocal omnifunc=ghci#omnifunc

autocmd CompleteDone <buffer> :pclose

command! -buffer Ghcid terminal ghcid --command='cabal new-repl'
command! -buffer Ghci terminal cabal new-repl
command! -buffer -nargs=0 GhciTypeAt call ghci#typeat()
command! -buffer -nargs=0 GhciReload call ghci#load()
command! -buffer -nargs=0 GhciLoadModule call ghci#load(expand('%:p'))
