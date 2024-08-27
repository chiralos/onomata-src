# Some Onomata hosts

## [Heapless C](C-heapless/README.md)

## Language

Onomata is set of standard words for a concatenative programming
language, along with a trivial syntax and semantics for programs
(expressions).

Onomata expressions are S-expressions. That is, literals, identifiers,
and balanced parentheses.

The semantics of the language is defined as follows:
* literal numbers and strings are pushed onto the stack
* parenthesised expressions are pushed onto the stack
* words are looked up in an environment and executed

What appears here is an early sketch of a langauge. The full language
would have an open-ended vocabulary, namespaces, types, type traits,
additional syntax for defining programs, modules, and types; and
an algebra defining valid program transformations.

### Types

| Type  | Notes                           |
|-------|---------------------------------|
| Bool  |                                 |
| Int   | signed; host-dependent width    |
| Str   | immutable byte array            |
| Proc  | procedures / programs (`->`)    |
| Buf   | reference to mutable byte array |
| Tup-N | family of N-tuple types         |

There is a subset of words that are considered 'statically typed' -
these consume and produce a fixed, finite number of stack items.

We have let in `pack unpack tup/get tup/set up` which are not
well typed. But they're pretty close if you restrict your usage 
to literal numeric args.

| Trait      | Types        | Notes           |
|------------|--------------|-----------------|
| Eq         | Bool Int Str | has `eq neq`    |
| Ord        | Bool Int Str | has `cmp` etc   |
| Len        | Str Buf Tup  | has `len`       |
| ByteSrc    | Str Buf      | has `overwrite` |
| Stringform | Str Buf Int  | has `str`       |

### Words

In the following all-caps types are type variables.  `(A)` means a
single arbitrary item. `A` means a finite number (including zero)
items.

When writing out a stack state, the convention is to put the top
on the right. This means that a sequence of pushes looks like the
resulting stack.

Generally, if there is a conventional ordering of arguments to
functions, the Onomata word will use the same sequence ordering
i.e. deepest stack item for leftmost argument, stack top for
rightmost. So `(X) (Y) sub` is `(X) - (Y)`.

When there is an operation than has a container and an index (get
and set words), the container goes on the bottom and the index on
top. Update operations put the value to be inserted in between.

```

| Name         | Type                               |  Notes
|--------------|------------------------------------|----------------------
| nop          |                    ->              | [1]
| pop          | (A)                ->              |
| dup          | (A)                -> (A) (A)      |
| swp          | (A) (B)            -> (B) (A)      |
| quo          | (A)                -> (-> (A))     |
| cat          | (A -> B) (B -> C)  -> (A -> C)     |
| run          | A (A -> B)         -> B            |
| swu          | (A) (B) (C)        -> (B) (A) (C)  | swap under
| swa          | (A) (B) (C)        -> (C) (B) (A)  | swap around
| dig          | (A) (B) (C)        -> (B) (C) (A)  |
| bry          | (A) (B) (C)        -> (C) (A) (B)  |
| up           | (A) ... (Int)      -> (A) ... (A)  | [3], rhymes with dup
| true         |                    -> (Bool)       |
| false        |                    -> (Bool)       |
| not          | (Bool)             -> (Bool)       |
| and          | (Bool) (Bool)      -> (Bool)       |
| or           | (Bool) (Bool)      -> (Bool)       |
| dip          | A (B) (A -> C)     -> C (B)        |
| loop         | A (A -> A Bool)    -> A            |
| ife          | [2]                                |
| add          | (Int) (Int)        -> (Int)        | [4]
| sub          | (Int) (Int)        -> (Int)        |
| mul          | (Int) (Int)        -> (Int)        |
| div          | (Int) (Int)        -> (Int)        |
| mod          | (Int) (Int)        -> (Int)        |
| bit/and      | (Int) (Int)        -> (Int)        | bitwise ops
| bit/or       | (Int) (Int)        -> (Int)        |
| bit/not      | (Int)              -> (Int)        |
| bit/xor      | (Int) (Int)        -> (Int)        |
| bit/shift    | (Int) (Int)        -> (Int)        | [5]
| cmp          | (O) (O)            -> (Int)        | (O Ord), -ve,0,+ve
| eq           | (O) (O)            -> (Bool)       | (O Ord) (E Eq)
| neq          | (O) (O)            -> (Bool)       | (O Ord) (E Eq)
| lt           | (O) (O)            -> (Bool)       | (O Ord)
| lte          | (O) (O)            -> (Bool)       | (O Ord)
| gt           | (O) (O)            -> (Bool)       | (O Ord)
| gte          | (O) (O)            -> (Bool)       | (O Ord)
| len          | (L)                -> (Int)        | (L Len)
| slc          | (BS) (Int) (Int)   -> (BS)         | (BS ByteSrc)
| brk          | (BS) (Int)         -> (BS)         | (BS ByteSrc)
| pack         | ... (Int)          -> (Tup)        | [3] [10]
| unpack       | (Tup)              -> ...          | [3]
| tup/get      | (Tup) (Int)        -> (A)          | [10]
| tup/set      | (Tup) (A) (Int)    -> (Tup)        | [10]
| str          | (S)                -> (Str)        | (S Stringform)
| str/cat      | (Str) (Str)        -> (Str)        |
| str/get      | (Str) (Int)        -> (Int)        | peek byte
| str/set      | (Str) (Int) (Int)  -> (Str)        | with updated byte
| parse-int    | (Str)              -> (Int) (Int)  | [6]
| overwrite    | (Buf) (BS)         ->              | dst src ->
| peek         | (Buf) (Int)        -> (Int)        | peek byte
| poke         | (Buf) (Int) (Int)  ->              | poke byte
| peek-int     | (Buf) (Int)        -> (Int)        |
| poke-int     | (Buf) (Int) (Int)  ->              |
| alloc-static | (Int)              -> (Buf)        | [7]
| sleep-milli  | (Int)              ->              |
| stdin        |                    -> (Int)        |
| stdout       |                    -> (Int)        |
| open         | (Str) (Int)        -> (Int)        | name flags -> err
| close        | (Int)              -> (Int)        | fd -> err
| seek         | (Int) (Int) (Int)  -> (Int)        | fd ofs whence -> pos
| read-str     | (Int) (Int)        -> (Str) (Int)  | fd len -> str err
| write-str    | (Int) (Str)        -> (Int)        | fd str -> err
| seek-set     |                    -> (Int)        |
| seek-cur     |                    -> (Int)        |
| seek-end     |                    -> (Int)        |
| read-only    |                    -> (Int)        |
| write-only   |                    -> (Int)        |
| read-write   |                    -> (Int)        |
| trunc        |                    -> (Int)        |
| creat        |                    -> (Int)        |
| nonblocking  |                    -> (Int)        |
| avail        |                    -> (Int)        |
| wordsize     |                    -> (Int)        | in bytes

| def          | (Str) (A -> B)     ->              | [9]
| def-mem      | (Str) (Int)        ->              | alloc-static quo def
| load         | (Str)              ->              | [11]
| load-once    | (Str)              ->              | [12]
| load-fd      | (Int)              ->              | [11]
| save         | (Str)              ->              | [13]
| save-fd      | (Int)              ->              |

| exit         |                                    | [8]
| clear        |                                    | clears stack
| reset        |                                    | resets system
| list         |                    ->              | lists defs
| show         | (Str)              ->              | writes def to stdout
| freeze       |                    ->              | optimise current defs
| undef        | (Str)              ->              |

```

[1] `nop pop dup swp quo cat run` are together Turing-complete.

[2] `ife` has type: `A (Bool) (A -> B) (A -> B) -> B`

[3] Words that consume or produce a dynamically determined number of
items are not considered well typed. If you want your program to
be compiled, restrict to arguments that easily statically resolve
to constants.

[4] Overflow or division by zero yields an undefined but valid number.

[5] First arg is bits in integer, second argument is number of bits
    to shift, positive is shift left, shift is logical (and result
    interpreted as signed int.

[6] Top result is number of bytes parsed; if zero second is undefined.

[7] Kind of precarious, since once the buf is dropped the memory
    leaks unless the buf reference is stored somwhere. See def-mem.

[8] Interactive environment commands. Not part of expression language proper.

[9] This `def` works with a single global scope; and bindings are
    permanent (ignore `undef`, which should be considered an
interactive environment feature and not a program-forming one).
Think of it as an alias for `global/def` or something.

This is one avenue for structuring programs. I'm not sure it's
    a good one. On the plus side it requires zero additional syntax,
is easy to understand, and allows compile-time programming. The down 
side is that it becomes undecidable whether a program can be fully 
compiled.

[10] `pack unpack tup/get tup/set` are standing in for a family of
     well-typed words. eg `pack-2` is well typed; `pack` isn't, but
`pack-2 <-> 2 pack` . If a compiler can handle that without working
too hard I don't feel too back about including 'pack'.

[11] Load and run a program from file or file descriptor. Actually
     streams program in and runs it incrementally.

[12] Poor man's module system. Won't hold up at scale.

[13] Provided as a historical re-creation. On a machine with
     limited memory, no text editor, and/or cassette tape storage 
you might actually have developed code with the interactive system
and saved it. Cannot save frozen code.
