function! g:Main#echo(x)
  function! Echo() closure
    echo(a:x)
  endfunction
  return funcref('Echo')
endfunction
function! g:Main#addInt(x)
  return {y -> a:x + y}
endfunction
