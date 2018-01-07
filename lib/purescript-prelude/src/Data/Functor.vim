function! s:arrayMap(f, arr)
  let l:result = []
  for l:x in a:arr
    let l:result = add(l:result, a:f(l:x))
  endfor
  return l:result
endfunction

let g:Data#Functor#arrayMap = {f -> {arr -> s:arrayMap(f, arr)}}
