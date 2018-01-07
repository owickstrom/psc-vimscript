let g:Control#Monad#Eff#pureE = {a -> {-> a:a}}

let g:Control#Monad#Eff#bindE {a -> {f-> {-> f(a:a())()}}}

function! s:untilE(f)
  while (!a:f())
  endwhile
  return {}
endfunction

let g:Control#Monad#Eff#untilE = {f -> {-> s:untilE(f)}}

function! s:whileE(f, a)
  while (a:f())
    call a:a()
  endwhile
  return {}
endfunction

let g:Control#Monad#Eff#whileE = {f ->{a -> {-> s:whileE(a:f, a)}}}

function! s:forE(hi, lo, f)
  for l:i in range(a:hi, a:lo - 1)
    call a:f(l:i)
  endfor
  return {}
endfunction

let g:Control#Monad#Eff#forE = {lo -> {hi -> {f -> {-> s:forE(a:lo, hi, f)}}}}
