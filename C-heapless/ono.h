#ifndef _ONOMATA_H
#define _ONOMATA_H

#include "bytecode.h"
#include <stdbool.h>
#include <setjmp.h>

////////
// types

typedef struct Buf {
  Word  len;
  Byte* data;
} Buf;

typedef struct Seg {
  Byte* cursor;
  Byte* end;
} Seg;

typedef struct Search {
  Buf name;
  Word* dp;
  int n;
} Search;

typedef enum Err {
  ERR_OK = 0,
  ERR_EXIT,
  ERR_INTERNAL,

  // parse
  ERR_UNEXPECTED_CHARACTER,
  ERR_INCOMPLETE,
  ERR_TOKEN_TOO_LONG,

  // parse and runtime
  ERR_OUT_OF_MEMORY,

  // runtime
  ERR_BAD_ARGUMENT,
  ERR_STACK_UNDERFLOW,
  ERR_TYPE_MISMATCH,
  ERR_ARRAY_BOUNDS,
  ERR_UNKNOWN_SYMBOL,
  ERR_IO_ERROR,

  ERR_UNKNOWN
} Err;

#define ERR_WORD_BUFSIZE 9

typedef struct Env {
  Word*   base;   // bottom
  Word*   limit;  // above last word

  // dictionary and arg stack grow upwards
  Word*   sp;  // argument stack pointer
  Word*   dt;  // dictionary top, ie tag of dict name entry
  Word*   ft;  // fixed top: below this cannot be moved or deallocd

  // lexical (return) stack grows downwards
  Word*   lsp; // lexical stack pointer
  Word*   fp;  // frame ptr

  Code    pc;  // program counter
  Code    pe;  // procedure end
  bool    pcAdjusted; // signal that prim already adjusted pc

  jmp_buf catch; 
  Err     err;
  char    errWord[ERR_WORD_BUFSIZE]; // null terminated
} Env;

#define AVAILABLE_WORDS (env.lsp - env.sp - 1)
#define AVAILABLE_BYTES (AVAILABLE_WORDS * WORDSIZE)
#define THROW(e) longjmp(env.catch,e)
#define SAVE_CATCH(V) memcpy(&V,&env.catch,sizeof(jmp_buf))
#define RESTORE_CATCH(V) memcpy(&env.catch,&V,sizeof(jmp_buf))

////////////
// interface

typedef enum Tag {
  TAG_FALSE = 0,
  TAG_TRUE,
  TAG_INT,
  TAG_BYTES,
  TAG_CODE,
  TAG_STRUCT,
  TAG_BUF, // in the dictionary this is a mem buffer
} Tag;

extern Env env;

Int  popInt(void);
void pushInt(Int x);
void pushBytes(void *buf, int len, Tag tag);
void pushBuf(void *buf, Word len);

void popCode(void);

void emitByte(Seg* out, Byte x);
Word alignedSize(Word bytes);
Word stackItemSize(Word *sp);
void overflowCheck(Word sz);
void reset(Opcode);

Word* next(Word* sp);
Word* stack2(void);
Word* itemBase(Word *p);

void repl(void);

#endif

////////
// Notes
