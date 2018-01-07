function! s:arrayBind(arr, f)
  let l:result = []
  for l:i in range(0, len(a:arr) - 1)
    let l:result = extend(l:result, a:f(a:arr[l:i]))
  endfor
  return l:result
endfunction

let g:Control#Bind#arrayBind = {arr -> {f -> {-> s:arrayBind(arr, f)}}}
