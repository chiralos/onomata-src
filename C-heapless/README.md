# A heapless Onomata interpreter in C

September 2024

## Targets

### Zeal 8-Bit OS Target

The repo 'zos' directory should contain a compiled binary (it is
small and standard so I don't feel too bad about including it).

Cross-compiled from Unix. Has been known to build from MacOS and
WSL Ubuntu.

Requires Zeal 8-Bit-OS for headers: set ZOS_PATH to root repo
directory (that contains kernel_headers dir).

Requires SDCC (Small Device C Compiler): set SDCC_BASE to 
sdcc directory containing 'include', 'lib', 'scripts' dir.
The 'sdcc' apt package on Ubuntu 24.02 works, with SDCC_BASE
at /usr/share/sdcc.

(On OS X I had to install and use 'gobjcopy' instead of 'objcopy').

Should be able to build with:

    cd C-heapless/zos
    make onoi

Generates `onoi` which is a standard ZOS binary. Has been
tested on the Zeal 8-bit emulator and (occasionally) hardware.

There is currently (September 2024) a difficulty in running on Zeal
hardware, in that the binary is >16K, and the most practical way
of loading software is via a serial link and `uartrcv` command ...
which is limited to files <16K .

To get around this there is a program called `glue` in the zos
source folder which should be buildable with `make glue`. This is
<16K so it should be possible to (a) chop up onoi (or any other
large file) in to 16K chunks with e.g. `dd` on unix, (b) transferring
`glue`, (c) transferring the chunks, and (d) assembling them with
`glue`. I have not actually tried this due to my Zeal machine being
out of commission :-)

Host-specific words:

```

| Name         | Type              |  Notes
|--------------|-------------------|----------------------
| z80/in       | Int        -> Int | port -> byte
| z80/out      | Int Int    ->     | port byte ->
|              |                   | 
| to-physical  | Int Int Bs ->     | high8 low16 data ->

```

The target of `to-physical` must not overlap a 16 page boundary.

### Generic Unix target

Should just need make and a C compiler.

    cd C-heapless/unix
    make onoi

## Host details

The system is dynamically typed, checks array bounds and memory
overflow, and is properly tail-recursive. The heap size is set in
the main.c of each target.

See [here](../README.md#language) for list of words and types it supports.

## Motivatiion

The overall goal of this interpreter host was to make something like
the BASIC environments provided by 8-bit computers of the 1970s and
80s. But with first-class functions. So ideally:

  * Compact.  The Zeal 8-Bit OS Z80 binary (compiled with SDCC) weighs 
    in just under 24KB which I consider a failure. 
    
    The [Jupiter Ace](https://en.wikipedia.org/wiki/Jupiter_Ace) had a 
    Forth system, floating-point library, and all hardware support 
    including tape IO in an 8K ROM.

    I'm sure a hand-written Z80 assembly translation could get it down 
    under 12K. But if I was going to spend the time on a good Z80 host 
    I would start from something better than this.

  * Have a read-eval-print loop that allows definitions to be 
    loaded, saved, added, removed, listed, and inspected.

  * Show stack contents.

  * At least some error messages: syntax errors, type mismatches, 
    bounds violations, undefined symbols, out-of-memory.

  * Uncrashable under ordinary usage. Not quite achieved: there is
    C-stack recursion in the printing and optimisation code that is
    unchecked, so you can probably crash it with deep enough 
    procedure nesting.

## Architecture

How can you implement first-class functions and runtime-sized strings
in a memory-safe environment without a heap ? Memcpy. Lots of memcpy.

The system works in a single block of RAM, with an upward growing
dictionary and argument stack, and a downward growing return stack,
which is separate from the C stack.

At the base are static memory allocations and "frozen" optimised
bytecode dictionary definitions. These are fixed and once allocated
cannot be freed without resetting the system. Above that are
relocatable bytecode definitions which can be undefined. Above that
is the argument stack.

A lexical (return) stack grows down from the top of the the block.
This holds return addresses and copies of code that is being executed.

Code is stored as bytecode, which is essentially a tokenised representation
of the source.

When executing dynamically constructed code objects from the stack
some sort of copy is unavoidable (it's got to exist somewhere). But
a major flaw in the implementation is that _all_ calls to unfrozen
code make a copy to the lexical stack.  You _should_ be able to
just point to the definition bodies in the dictionary; but the
`static-alloc`, `undef`, and `freeze` words shuffle definitions
around, so you can't be running from there when that happens. Which
is particularly bad because nearly all code never causes shuffling.

You could get around that by tracking which code can never cause a
shuffle and avoiding the copy. Or by restricting shuffling operations
to special top-level directives. But I'd rather move on to a host
with a proper heap. It might be interesting to compare the performance 
and memory usage of this to that, both on a modern CPU and the
Z80.

### Optimisation

The `freeze` word takes all the currently unfrozen definitions,
compiles them a bit, and freezes them iin place. Frozen code is
still bytecode, but seems to run about three times faster.

It does a tiny bit of peephole optimisation, converting `ife` and
`loop` to branch instructions so that it doesn't have to push the
code it's about to run onto the stack. The `dip` word is converted
a sequence that more straightforwardly moves the item to be hidden
to the lexical stack, runs the stack top, and moves the item back
to the stack top (instead of implementing the definition 
`dip <-> swp quo cat run`, which does more copying).

It also links all the word calls so they point directly to their
entry points rather than them up by name.