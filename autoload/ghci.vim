function! ghci#omnifunc(findstart, base) abort
    let msg = {
      \ 'findstart': a:findstart,
      \ 'base': a:base,
      \ 'line': getline('.'),
      \ 'column': col('.') - 1,
      \ 'file': expand('%:p'),
      \ }

    let resp = ch_evalexpr(g:chan, msg)

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
