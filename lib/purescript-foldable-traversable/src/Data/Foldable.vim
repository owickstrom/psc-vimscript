function s:foldrArray(f, Init, xs)
  let l:Acc = a:Init
  for l:x in reverse(a:xs)
    let l:Acc = a:f(l:x)(l:Acc)
  endfor
  return l:Acc
endfunction

let g:Data#Foldable#foldrArray = {f -> {Init -> {xs -> s:foldrArray(f, Init, xs)}}}

function s:foldlArray(f, Init, xs)
  let l:Acc = a:Init
  for l:x in a:xs
    let l:Acc = a:f(l:Acc)(l:x)
  endfor
  return l:Acc
endfunction

let g:Data#Foldable#foldlArray = {f -> {Init -> {xs -> s:foldlArray(f, Init, xs)}}}
