let s:cmd='set packpath+=' . expand('%:p:h') . '/vim-output'
exec s:cmd
packadd Main
