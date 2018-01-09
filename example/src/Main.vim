function! s:echo_impl(x)
  echo a:x
endfunction

let g:Main#echo = {x -> {-> (s:echo_impl(x))}}

let g:Main#input = {s -> {-> input(s)}}

let g:Main#confirm = {s -> {-> confirm(s)}}
