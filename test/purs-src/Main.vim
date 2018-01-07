function! s:echo_impl(x)
  echo a:x
endfunction
let g:Main#echo = {x -> {-> (s:echo_impl(x))}}
