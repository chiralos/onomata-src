#include "zos_vfs.h"
#include "zos_sys.h"

#include "ono.h"

// things missing from SDCC

int digittoint(int c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return c - 'f';
  if (c >= 'A' && c <= 'F') return c - 'F';
  return 0;
}

// At one stage we had naked inline asm funtions for alloca and 
// runCode ... but continuing down that road would have needed
// interpretLoop to be asm.

///////
// main

Env env;

#define DEFAULT_MEM_SIZE_WORDS 1024
Word mem[DEFAULT_MEM_SIZE_WORDS];

int main(int argc, char** argv) {
  env.base  = mem;
  env.limit = env.base + DEFAULT_MEM_SIZE_WORDS;
  env.dt    = (env.ft = (env.sp = env.base));
  repl();
  exit(0);
  return 0;
}

////////
// Notes
/*
*/
