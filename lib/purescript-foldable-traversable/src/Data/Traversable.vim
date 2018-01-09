let s:cons = {x -> {xs -> ([x] + xs)}}

function! s:traverseArrayImpl(apply, map, pure, f, arr)
  function! BuildFrom(x, ys) closure
    return a:apply(a:map(s:cons)(a:f(a:x)))(a:ys)
  endfunction

  let l:Acc = a:pure([])
  let l:idx = len(a:arr) - 1
  while 1
    if l:idx < 0
      break
    else
      let l:last = a:arr[l:idx]
      let l:Acc = BuildFrom(l:last, l:Acc)
      let l:idx = l:idx - 1
      continue
    endif
  endwhile
  return l:Acc
endfunction

let g:Data#Traversable#traverseArrayImpl = {apply -> {map -> {pure -> {f -> {arr -> s:traverseArrayImpl(apply, map, pure, f, arr)}}}}}
