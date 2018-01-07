let g:Data#Show#showIntImpl = {n -> (n . '')}

let g:Data#Show#showNumberImpl = {n -> (n . '')}

let g:Data#Show#showCharImpl = {c -> c}

let g:Data#Show#showStringImpl = {s -> "\'TODO\'"}

function! s:showArrayImpl(f, xs)
  let l:ss = []
  for l:x in a:xs
    let l:ss = add(l:ss, a:f(l:x))
  endfor
  return ('[' . join(l:ss, ',') . ']')
endfunction

let g:Data#Show#showArrayImpl = {f -> {xs -> s:showArrayImpl(f, xs)}}
