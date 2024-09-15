# Some Onomata hosts

## [Haskell](Haskell/README.md)

## [Heapless C](C-heapless/README.md)

## Language

**NOTE: version 0.1 : everything here is subject to breaking change**

Onomata is set of standard words for a concatenative programming
language, along with a trivial syntax and semantics for programs
(expressions).

Onomata expressions are S-expressions. That is, literals, identifiers,
and balanced parentheses.

The semantics of the language is defined as follows:
* literal numbers and strings are pushed onto the stack
* parenthesised expressions push the denoted procedure onto the stack
* words are looked up in an environment and executed

**What appears here is an early sketch of a langauge.** 
The full language would have an open-ended vocabulary, namespaces,
types, type traits, additional syntax for defining programs, modules,
and types; and an algebra defining valid program transformations.

### Types

| Type  | Notes                           |
|-------|---------------------------------|
| Bool  |                                 |
| Int   | signed; host-dependent width    |
| Str   | immutable byte array            |
| ->    | procedures / programs           |
| Buf   | reference to mutable byte array |
| Tup   | tuple                           |
| (A)   | type variable (single item)     |
| A     | type variable N items           |

In the following all-caps types are type variables.  `(A)` means a
single arbitrary item. `A` means a finite number (including zero)
items.

Most words are 'statically typed' i.e. these consume and produce 
a fixed, finite number of stack items.

### Traits

We currently have some ad-hoc polymorphism. Where these trait names
appear in the table below, it stands for a single type variable
constrained to be a member of the trait. So e.g. 
`Ord Ord -> Bool` means `(A) (A) -> Bool where (A) is Ord` .

| Trait           | Types        | Notes           |
|-----------------|--------------|-----------------|
| Eq              | Bool Int Str | has `eq neq`    |
| Ord             | Bool Int Str | has `cmp` etc   |
| Len             | Str Buf Tup  | has `len`       |
| ByteSrc (Bs)    | Str Buf      | has `cpy` etc   |
| Stringform (Sf) | Str Buf Int  | has `str`       |

### Words

When writing out a stack state, the convention is to put the top
on the right. This means that a sequence of pushes looks like the
resulting stack.

Generally, if there is a conventional ordering of arguments to
functions, the Onomata word will use the same sequence ordering
i.e. deepest stack item for leftmost argument, stack top for
rightmost. So `x y sub` is `x - y`.

When there is an operation than has a container and an index (get
and set words), the container goes on the bottom and the index on
top. Update operations put the value to be inserted in between.

```

| Name         | Type                                    |  Notes
|--------------|-----------------------------------------|---------------------
| nop          |                          ->             | 
| pop          | (A)                      ->             | [1]
| dup          | (A)                      -> (A) (A)     |
| swp          | (A) (B)                  -> (B) (A)     |
| quo          | (A)                      -> (-> (A))    |
| cat          | (A -> B) (B -> C)        -> (A -> C)    |
| run          | A (A -> B)               -> B           |
| swu          | (A) (B) (C)              -> (B) (A) (C) | swap under
| swa          | (A) (B) (C)              -> (C) (B) (A) | swap around
| dig          | (A) (B) (C)              -> (B) (C) (A) |
| bry          | (A) (B) (C)              -> (C) (A) (B) |
| up           | (A) ... Int              -> (A) ... (A) | [2], rhymes dup
| true         |                          -> Bool        |
| false        |                          -> Bool        |
| not          | Bool                     -> Bool        |
| and          | Bool Bool                -> Bool        |
| or           | Bool Bool                -> Bool        |
| choose       | (A) (A) Bool             -> (A)         |
| ife          | A Bool (A -> B) (A -> B) -> B           |
| dip          | A (B) (A -> C)           -> C (B)       |
| loop         | A (A -> A Bool)          -> A           |
| add          | Int Int                  -> Int         | [3]
| sub          | Int Int                  -> Int         | [3]
| mul          | Int Int                  -> Int         | [3]
| div          | Int Int                  -> Int         | [4]
| mod          | Int Int                  -> Int         | [4]
| divmod       | Int Int                  -> Int Int     | [4]
| abs          | Int                      -> Int         |
| neg          | Int                      -> Int         |
| bit/and      | Int Int                  -> Int         |
| bit/or       | Int Int                  -> Int         |
| bit/not      | Int                      -> Int         |
| bit/xor      | Int Int                  -> Int         |
| bit/shift    | Int Int                  -> Int         | [5]
| cmp          | Ord Ord                  -> Int         | -ve,0,+ve
| eq           | Ord Ord                  -> Bool        |
| neq          | Ord Ord                  -> Bool        |
| lt           | Ord Ord                  -> Bool        |
| lte          | Ord Ord                  -> Bool        |
| gt           | Ord Ord                  -> Bool        |
| gte          | Ord Ord                  -> Bool        |

| len          | Len                      -> Int         |
| slc          | Bs Int Int               -> Bs          | bytes ofs end
| sbs          | Bs Int Int               -> Bs          | bytes ofs len
| brk          | Bs Int                   -> Bs Bs       | 

| pack         | ... Int                  -> Tup         | [2]
| unpack       | Tup                      -> ...         | [2]
| tup/get      | Tup Int                  -> (A)         | [2]
| tup/set      | Tup (A) Int              -> Tup         | [2]

| str          | Sf                       -> Str         |
| chr          | Int                      -> Str         |
| str/cat      | Str Str                  -> Str         |
| str/get      | Str Int                  -> Int         | peek byte
| str/set      | Str Int Int              -> Str         | with updated byte
| parse-int    | Str                      -> Int Int     | [6]

| cpy          | Buf Bs                   ->             | dst src ->
| fill         | Buf Bs                   ->             | dst src ->
| peek         | Buf Int                  -> Int         | peek byte
| poke         | Buf Int Int              ->             | poke byte
| peek-int     | Buf Int                  -> Int         |
| poke-int     | Buf Int Int              ->             |

| sleep-milli  | Int                      ->             |

| alloc-static | Int                      -> Buf         | [7]

| stdin        |                          -> Int         |
| stdout       |                          -> Int         |
| open         | Str Int                  -> Int         | name flags -> err
| close        | Int                      -> Int         | fd -> err
| seek         | Int Int Int              -> Int         | fd ofs whence -> pos
| read-str     | Int Int                  -> Str Int     | fd len -> str err/len
| read         | Int Buf                  -> Int         | fd buf -> err/len
| write        | Int Bs                   -> Int         | fd str -> err/len
| seek-set     |                          -> Int         |
| seek-cur     |                          -> Int         |
| seek-end     |                          -> Int         |
| read-only    |                          -> Int         |
| write-only   |                          -> Int         |
| read-write   |                          -> Int         |
| trunc        |                          -> Int         |
| creat        |                          -> Int         |
| nonblocking  |                          -> Int         |

| term/cls             | Int               ->            |
| term/cursor-to       | Int Int Int       ->            | fd x y ->
| term/raw             | Int               -> Int        | fd -> err
| term/raw-nonblocking | Int               -> Int        | fd -> err
| term/reset           | Int               -> Int        | fd -> err

| wordsize     |                           -> Int        | in bytes
| avail        |                           -> Int        |

| def          | Str (A -> B)              ->            | [8]
| def-mem      | Str Int                   ->            | alloc-static quo def
| load         | Str                       ->            | [9]
| load-once    | Str                       ->            | [9,10]
| load-fd      | Int                       ->            | [9]
| save         | Str                       ->            | [11]
| save-fd      | Int                       ->            | [11]

| exit         |                                         | [12]
| clear        |                                         | [12] clears stack
| reset        |                                         | [12] resets everything
| list         |                           ->            | [12] lists defs
| show         | Str                       ->            | [12] writes def to stdout
| undef        | Str                       ->            | [12]
| freeze       |                           ->            | [12] optimise current defs

(internal : errstr parse-part write-stack repl)

```

[1] `pop dup swp quo cat run` are together Turing-complete.

[2] Words that consume or produce a dynamically determined number of
items are not considered well typed. If you want your program to
be compiled, restrict to arguments that easily statically resolve
to constants. If you're writing a compiler make a little effort
to resolve these.

`pack unpack tup/get tup/set` are standing in for a family of
well-typed words. eg `pack-2` is well typed; `pack` isn't, but
`pack-2 <-> 2 pack` .

[3] Overflow or out-of-range result yields an undefined but valid 
    number. 

[4] Divide by zero is undefined and should panic.

[5] First arg is bits in integer, second argument is number of bits
    to shift, positive is shift left, shift is arithmetic on signed.

[6] Top result is number of bytes parsed; if zero second is undefined.

[7] Kind of precarious, since once the buf is dropped the memory
    leaks unless the buf reference is stored somwhere. See def-mem.

[8] This `def` works with a single global scope; and bindings are
    permanent (ignore `undef`, which should be considered an
interactive environment feature and not a program-forming one).
Think of it as an alias for `global/def` or something.

This is a freeform way of structuring onomata programs that is
probably not a good idea. On the plus side it requires zero additional
syntax, is easy to understand, and allows compile-time programming.
The down side is that compilation becomes undecidable, and it is
difficult to define a standard for what a compiler 'should' be able
to accept.

[9] Load and run a program from file or file descriptor. Actually
     streams program in and runs it incrementally.

[10] Poor man's module system. Won't hold up at scale.

[11] Provided as a historical re-creation. On a machine with
     limited memory, no text editor, and/or cassette tape storage 
you might actually have developed code with the interactive system
and saved it. Cannot save frozen code.

[12] Interactive environment commands. Not part of expression
language proper.

