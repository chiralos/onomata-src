# A Onomata interpreter in Haskell

August 2024

Made with GHC 9.6.1

See `Haskell/Makefile` (it's short) for building. Makes `ono`
executable that accepts source files on command line, or goes
into REPL if there are no args.

From GHCi you can invoke the repl by loading `Onomata.Interactive`
and invoking `onoi`.

## Host details

The system is dynamically typed and is properly tail-recursive. 
I'm not sure it properly bounds checks all string operations.

It supports a lot of [these](../README.md#language) 
but not have IO or byte buffers (so no `load` etc), so definitely
(even more of) a toy.

## Motivatiion

Because it was easy.
