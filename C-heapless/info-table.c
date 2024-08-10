#include "bytecode.h"

// b bool
// n number
// s string
// r runnable
// t tuple (struct)
// p pointer (buf)
// m byte source (str or buf)
// o ordered
// l length (str, buf, struct)
// a all

// THIS MUST BE KEPT IN Opcode ORDER

const InstructionInfo opInfoTable[N_BASIC_OPS] = {
  { "nop",          ""    }, // OP_NOP

  { "pop",          "a"   }, // OP_POP
  { "dup",          "a"   }, // OP_DUP
  { "swp",          "aa"  }, // OP_SWP
  { "swu",          "aaa" }, // OP_SWU
  { "swa",          "aaa" }, // OP_SWA
  { "dig",          "aaa" }, // OP_DIG
  { "bry",          "aaa" }, // OP_BRY

  { "quo",          "a"   }, // OP_QUO
  { "cat",          "rr"  }, // OP_CAT
  { "run",          "r"   }, // OP_RUN

  { "true",         ""    }, // OP_TRUE
  { "false",        ""    }, // OP_FALSE
  { "and",          "bb"  }, // OP_AND
  { "or",           "bb"  }, // OP_OR
  { "not",          "b"   }, // OP_NOT

  { "ife",          "rrb" }, // OP_IFE

  { "add",          "nn"  }, // OP_ADD
  { "sub",          "nn"  }, // OP_SUB
  { "mul",          "nn"  }, // OP_SUB
  { "div",          "nn"  }, // OP_DIV
  { "mod",          "nn"  }, // OP_MOD

  { "bit-and",      "nn"  }, // OP_BITAND
  { "bit-or",       "nn"  }, // OP_BITOR
  { "bit-not",      "n"   }, // OP_BITNOT
  { "bit-xor",      "nn"  }, // OP_BITXOR
  { "bit-shift",    "nn"  }, // OP_BITSHIFT

  { "cmp",          "oo"  }, // OP_CMP
  { "eq",           "oo"  }, // OP_EQ
  { "neq",          "oo"  }, // OP_NEQ
  { "lt",           "oo"  }, // OP_LT
  { "lte",          "oo"  }, // OP_LTE
  { "gt",           "oo"  }, // OP_GT
  { "gte",          "oo"  }, // OP_GTE

  { "len",          "l"   }, // OP_LEN

  { "pack",         "n"   }, // OP_PACK not well typed, takes additional a
  { "unpack",       "t"   }, // OP_UNPACK
  { "tup-get",      "nt"  }, // OP_TUPGET
  { "tup-set",      "nat" }, // OP_TUPSET

  { "str",          "a"   }, // OP_STR see [1]
  { "str-cat",      "ss"  }, // OP_STRCAT
  { "str-brk",      "ns"  }, // OP_STRBRK
  { "str-get",      "ns"  }, // OP_STRGET
  { "str-set",      "nns" }, // OP_STRSET
  { "parse-int",    "s"   }, // OP_PARSEINT

  { "mem-slice",    "nnp" }, // OP_MEMSLICE
  { "mem-cpy",      "mp"  }, // OP_MEMCPY
  { "peek-int",     "np"  }, // OP_PEEKINT
  { "poke-int",     "nnp" }, // OP_POKEINT

  { "alloc-static", "n"   }, // OP_ALLOC_STATIC

  { "sleep-milli",  "n"   }, // OP_SLEEPMILLI

  { "stdin",        ""    }, // OP_STDIN
  { "stdout",       ""    }, // OP_STDOUT
  { "open",         "ns"  }, // OP_OPEN
  { "close",        "n"   }, // OP_CLOSE
  { "seek",         "nnn" }, // OP_SEEK
  { "read-str",     "nn"  }, // OP_READSTR
  { "write-str",    "sn"  }, // OP_WRITESTR
  { "seek-set",     ""    }, // OP_SEEKSET
  { "seek-cur",     ""    }, // OP_SEEKCUR
  { "seek-end",     ""    }, // OP_SEEKEND
  { "read-only",    ""    }, // OP_RDONLY
  { "write-only",   ""    }, // OP_WRONLY
  { "read-write",   ""    }, // OP_RDWR
  { "trunc",        ""    }, // OP_TRUNC
  { "creat",        ""    }, // OP_CREAT
  { "nonblocking",  ""    }, // OP_NONBLOCKING

  { "save-fd",      "n"   }, // OP_SAVEFD
  { "errstr",       "n"   }, // OP_ERRSTR
  { "avail",        ""    }, // OP_AVAIL
  { "wordsize",     ""    }, // OP_WORDSIZE
  { "parse",        "snr" }, // OP_PARSE
  { "write-stack",  ""    }, // OP_WRITESTACK
  { "undef",        "s"   }, // OP_UNDEF
  { "list",         ""    }, // OP_LIST
  { "show",         "s"   }, // OP_SHOW
  { "clear",        ""    }, // OP_CLEAR
  { "reset",        ""    }, // OP_RESET
#ifdef AUTOQUOTE
  { "def",          "as"  }, // OP_DEF
#else
  { "def",          "rs"  }, // OP_DEF
#endif
  { "freeze",       ""    }, // OP_FREEZE
  { "exit",         ""    }, // OP_EXIT

  { "dip",          "ra"  }, // OP_DIP
  { "loop",         "r"   }, // OP_LOOP
  { "write-line",   "s"   }, // OP_WRITELINE
  { "load",         "s"   }, // OP_LOAD
  { "load-fd",      "t"   }, // OP_LOADFD
  { "load-str",     "s"   }, // OP_LOADSTR
  { "save",         "s"   }, // OP_SAVE
  { "repl",          ""   }, // OP_REPL
  { "def-mem",      "ns"  }, // OP_DEFMEM

  { "_sto",          "a"  }, // OP_STO
  { "_rcl",          ""   }, // OP_RCL
};

////////
// Notes
/*
[1] Actually only bool, int, string are printable, but there's
    only one thing (str) that uses it so don't bother adding it
to the generic types check.
*/