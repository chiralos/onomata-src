# A heapless Onomata interpreter in C

The overall goal of this Onomata host was to make something like
the BASIC environments provided by 8-bit computers of the 1970s and
80s. But with first-class functions. So ideally:

  * compact. The Jupiter Ace had a Forth system, floating-point 
    library, and all hardware support including tape IO in an 8K ROM.

  * have a read-eval-print loop that allows definitions to be 
    added, removed, listed, inspected, saved, and loaded

  * show stack contents

  * uncrashable under ordinary usage

## Supported onomata

### Types

`Int` `Bool` `Str` `Tup` `Proc` `Buf`

`Int` is platform default C 'int' type.

Traits supported are `Ord` and `Eq` ( for `Int Bool Str`),
`Len` (for `Str Tup`), `ByteSrc` (for `Str Buf`).

### Names

| Group     | Names                                           |
|:----------|:------------------------------------------------|
| universal | nop pop dup                                     |
| stack     | swp swu swa dig bury                            |
| boolean   | true false and or not                           |
| control   | quo cat run dip loop ife                        |
| int math  | add sub mul div mod                             |
| ordering  | cmp eq lt lte gt gte neq                        |
| bitwise   | bit-and bit-or bit-not bit-xor bit-shift        |
| length    | len                                             |
| string    | str str-cat str-brk str-get str-set parse-int   |
| tuples    | pack unpack tup-get tup-set                     |
| buffers   | mem-slice mem-cpy peek-int poke-int             |
| memory    | alloc-static                                    |
| system    | exit sleep-milli wordsize avail                 |
| IO        | stdin stdout open close seek read-str write-str |
|           | seek-set seek-cur seek-end read-only write-only |
|           | read-write trunc creat nonblocking              |
| def       | def def-mem                                     |
| dynamic   | clear reset list show undef freeze load load-fd |
|           | save save-fd                                    |
| misc      | errstr parse write-stack write-line repl        |

## Generic Unix target

Should just need make and a C compiler.

    cd unix
    make ono-unix

## Zeal OS target

Cross-compiled from Unix. Has been known to build from MacOS
and WSL Ubuntu.

Requires SDCC (Small Device C Compiler), and Zeal-8-Bit-OS repo for
headers.

Look at `zos/Makefile`. Need to set up `ZOS_PATH` and `SDCC_BASE`.

Generates `ono-zos.bin` which is a standard ZOS binary. Has been
tested on the Zeal 8-bit emulator and (occasionally) hardware.

