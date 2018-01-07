function! g:Data#Functor#arrayMap(f)
  function! Closure(arr) closure
    let l:result = []
    for l:x in a:arr
      let l:result = add(l:result, a:f(l:x))
    endfor
    return l:result
  endfunction
  return funcref('Closure')
endfunction
