function! s:unsafeCompareImpl(lt, eq, gt, x, y)
  if a:x < a:y
    return a:lt
  elseif a:x ==# a:y
    return a:eq
  else
    return a:gt
  endif
endfunction

let g:Data#Ord#Unsafe#unsafeCompareImpl = { lt -> { eq -> { gt -> { x -> { y -> unsafeCompareImpl(lt, eq, gt, x, y)}}}}}
