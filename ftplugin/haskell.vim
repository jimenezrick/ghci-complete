set completeopt+=preview
set omnifunc=ghci#omnifunc
autocmd TextChangedI * :pclose

let g:ghci_complete_batch_size = 10
