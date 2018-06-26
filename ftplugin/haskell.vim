" TODO: reconnect, script local? s:
let g:chan = ch_open('localhost:8000', {"timeout":2000}) " timeout -1?

set completeopt+=preview
set omnifunc=ghci#omnifunc
autocmd TextChangedI * :pclose
