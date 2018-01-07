function! s:arrayApply(fs, xs)
  let l:result = []
  for l:F in a:fs
    for l:x in a:xs
      let l:result = add(l:result, l:F(l:x))
    endfor
  endfor
  return l:result
endfunction
let g:Control#Apply#arrayApply = {fs -> {xs -> s:arrayApply(fs, xs)}}
