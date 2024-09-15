#include "ono.h"

Env env;

#define DEFAULT_MEM_SIZE_WORDS 8192
Word mem[DEFAULT_MEM_SIZE_WORDS];

int main(int argc, char** argv) {
  env.base  = mem;
  env.limit = env.base + DEFAULT_MEM_SIZE_WORDS;
  env.dt    = (env.ft = (env.sp = env.base));
  repl();
}

////////
// Notes
/*
[1] For non-interactive interpreter:

  if (argc > 1) {
    if ((env.err = setjmp(env.catch)) == 0)
      pushBytes(argv[1],strlen(argv[1]),TAG_BYTES);
      reset(OP_LOAD);
      interpretLoop();
    } else {
      printError(env.err); // see [1]
    } 
  }

*/
