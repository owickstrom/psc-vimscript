# psc-vimscript (WORK IN PROGRESS)

*This is an experimental project, and a work in progress.*

**PLEASE, LOOK AWAY FOR NOW!**

## Build

    stack build
    make -C example compile

## Run

To run the example in Vim, there's a helper in the Makefile:

    make -C example run

It automatically invokes Vim and runs the `Main.main` function.

## Compiler Output

As a starting step, each module becomes a lone standing "pack", a Vim8/NeoVim
feature. Modules are "imported" using `packadd`, and all are placed in `opt`
directories to be loaded lazily, as the order is significant for top-level
non-function definitions.

## Porting Libraries

Currently, in lieu of a better solution, all ported PureScript packages are
vendored in `lib/`. One could use psc-package with a custom package set, but
the explosion of Git repositories would likely become problematic, especially
since the foreign definitions are coupled to the implementation of this backend
in these early days.

[There is currently no way of disabling JS codegen in
PureScript](https://github.com/purescript/purescript/issues/3196), and thus all
modules with foreign definitions need corresponding `.js` files. These can be
`undefined`:

    exports.myForeignVimSpecificImpl = undefined;

The foreign `.vim` files are injected into the output VimScript code.

### Top-Level Let Bindings

When providing a foreign function in a `.vim` file, it has to be exposed as a
global qualified name, and using a `let`, not a `function`. This is due to how
functions can be referred to as values in the generated code.

As functions are curried in PureScript, it often makes sense to create an
uncurried top-level function (especially if you need statements) and call that
from a curried definition:

    function! s:myMapImpl(f, xs)
      let l:result = []
      for l:x in a:xs
        let l:result = add(l:result, a:f(l:x))
      endfor
      return l:result
    endfunction

    let g:My#Cool#Module#myMapImpl = {f -> {xs -> s:myMapImpl(f, xs)}}

For simpler expressions, you can implement it directly in the lambda:

    let g:My#Cool#Module#myConsImpl = {x -> {xs -> add(x, xs)}}

## License

[Mozilla Public License 2.0](LICENSE)

Copyright 2018 Oskar Wickström
