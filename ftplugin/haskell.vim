echom "Haskell autocomplete"

function! CompleteHaskellGhci(findstart, base)
    let msg = {
      \ 'findstart': a:findstart,
      \ 'base': a:base,
      \ 'line': getline('.'),
      \ 'column': col('.') - 1,
      \ 'file': expand('%:p'),
      \ }

    let resp = ch_evalexpr(s:chan, msg)

    echom printf('findstart: %s', a:findstart)
    echom printf('channel response: %s', resp)
  if a:findstart
    return resp['start']
  else
    for r in resp['results']
      call complete_add(r)
    endfor
    return []
  endif
endfun

" TODO: reconnect
let s:chan = ch_open('localhost:8000', {"timeout":2000}) " timeout -1?

set completeopt+=preview
set omnifunc=CompleteHaskellGhci
autocmd TextChangedI * :pclose
