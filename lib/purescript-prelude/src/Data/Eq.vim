let g:Data#Eq#refEq = {r1 -> {r2 -> (r1 == r2)}}

function! s:eqArrayImpl(f, xs, ys)
  if (len(a:xs) != len(a:ys))
    return 0
  endif
  for l:i in range(0, len(a:xs) - 1)
    if !a:f(a:xs[l:i])(a:ys[l:i])
      return 0
    endif
  endfor
  return 1
endfunction

let g:Data#Eq#eqArrayImpl = {f -> {xs -> {ys -> s:eqArrayImpl(f, xs, ys)}}}
