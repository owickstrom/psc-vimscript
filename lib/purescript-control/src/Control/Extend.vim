function! s:arrayExtend(f, xs)
  let l:result = []
  for l:i in range(0, len(a:xs) - 1)
    let l:result = add(l:result, a:f(a:xs[(l:i):]))
  endfor
  return l:result
endfunction
let g:Control#Extend#arrayExtend = {f -> {xs -> s:arrayExtend(f, xs)}}
