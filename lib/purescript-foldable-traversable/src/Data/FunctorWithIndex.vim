function! s:mapWithIndexArray(f, xs)
  let l:result = []
  for l:i in range(0, len(a:xs) - 1)
    let l:result = add(l:result, a:f(l:i)(a:xs[l:i]))
  endfor
  return l:result
endfunction
let g:Data#FunctorWithIndex#mapWithIndexArray = {f -> {xs -> s:mapWithIndexArray(f, xs)}}
