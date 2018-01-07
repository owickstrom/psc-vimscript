function! g:Control#Apply#arrayApply(fs)
  function! Closure(xs) closure
    let l:result = []
    for l:F in a:fs
      for l:x in a:xs
        let l:result = add(l:result, l:F(l:x))
      endfor
    endfor
    return l:result
  endfunction
  return funcref('Closure')
endfunction
