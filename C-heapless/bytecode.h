#ifndef _ONOMATA_BYTECODE_H
#define _ONOMATA_BYTECODE_H

#include <stdint.h>
#include <stdbool.h>

typedef enum Opcode {
  OP_NOP  = 0,

  OP_POP,
  OP_DUP,
  OP_SWP,
  OP_SWU,
  OP_SWA,
  OP_DIG,
  OP_BRY,
  OP_UP,

  OP_QUO,
  OP_CAT,
  OP_RUN,

  OP_TRUE,
  OP_FALSE,
  OP_AND,
  OP_OR,
  OP_NOT,

  OP_CHOOSE,
  OP_IFE,
  OP_DIP,

  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_MOD,

  OP_BITAND,
  OP_BITOR,
  OP_BITNOT,
  OP_BITXOR,
  OP_BITSHIFT,

  OP_CMP,
  OP_EQ,
  OP_NEQ,
  OP_LT,
  OP_LTE,
  OP_GT,
  OP_GTE,

  OP_LEN,
  OP_SLC,
  OP_SBS,
  OP_BRK,

  OP_PACK,
  OP_UNPACK,
  OP_TUPGET,
  OP_TUPSET,

  OP_STR,
  OP_CHR,
  OP_STRCAT,
  OP_STRGET,
  OP_STRSET,
  OP_PARSEINT,

  OP_OVERWRITE,
  OP_PEEK,
  OP_POKE,
  OP_PEEKINT,
  OP_POKEINT,

  OP_ALLOCSTATIC,

  OP_SLEEPMILLI,

  OP_STDIN,
  OP_STDOUT,
  OP_OPEN,
  OP_CLOSE,
  OP_SEEK,
  OP_READSTR,
  OP_WRITESTR,
  OP_SEEKSET,
  OP_SEEKCUR,
  OP_SEEKEND,
  OP_RDONLY,
  OP_WRONLY,
  OP_RDWR,
  OP_TRUNC,
  OP_CREAT,
  OP_NONBLOCK,

  OP_TERMCLS,
  OP_TERMCURSORTO,
  OP_TERMRAW,
  OP_TERMRAWNONBLOCKING,
  OP_TERMRESET,

  OP_AVAIL,
  OP_WORDSIZE,

  OP_SAVEFD,
  OP_ERRSTR,
  OP_PARSE_PART,
  OP_WRITESTACK,
  OP_ISDEF,
  OP_UNDEF,
  OP_LIST,
  OP_SHOW,
  OP_CLEAR,
  OP_RESET,

  OP_DEF,
  OP_FREEZE,
  OP_EXIT,

  OP_LOOP,
  OP_WRITELINE,
  OP_LOADONCE,
  OP_LOAD,
  OP_LOADFD,
  OP_LOADSTR,
  OP_SAVE,
  OP_REPL,
  OP_DEFMEM,

  OP_STO,
  OP_RCL,

  OP_PUSH_INT = 128,
  OP_PUSH_BYTES,
  OP_PUSH_CODE,
  OP_PUSH_ITEM,
  OP_PUSH_BUF,
  OP_CALL_NAME,
  OP_CALL_STATIC,

  OP_BR, // see [1]
  OP_BRF,
  OP_BRT,
} Opcode;

#ifdef WORDTYPE
typedef INTTYPE Int;
typedef WORDTYPE Word;
#else
typedef intptr_t      Int;
typedef uintptr_t     Word;
#endif
typedef unsigned char Byte;
typedef Byte*         Code;

#define WORDSIZE sizeof(Word)

#define FIRST_BYTECODE_OP OP_LOOP
#define LAST_BYTECODE_OP OP_DEFMEM
#define N_BYTECODE_OPS (LAST_BYTECODE_OP-FIRST_BYTECODE_OP+1)
#define LAST_PARSEABLE_OP OP_DEFMEM
#define LAST_BASIC_OP OP_RCL
#define N_BASIC_OPS (LAST_BASIC_OP+1)
#define FIRST_IMMEDIATE_OP OP_PUSH_INT
#define LAST_IMMEDIATE_OP OP_BRT
#define N_IMMEDIATE_OPS (LAST_IMMEDIATE_OP-FIRST_IMMEDIATE_OP+1)

typedef struct InstructionInfo {
  char* name;
  char* type;
} InstructionInfo;

typedef struct BytecodeRef {
  unsigned char* start;
  unsigned char* end;
} BytecodeRef;

extern const InstructionInfo opInfoTable[N_BASIC_OPS];
const extern BytecodeRef bytecodePrimTable[N_BYTECODE_OPS];
extern void (*basicOpJumpTable[N_BASIC_OPS])(void);
extern void (*immediateOpJumpTable[N_IMMEDIATE_OPS])(void);

#define MAX_INSTRUCTION_PAYLOAD (((Word)1) << ((WORDSIZE*8)-1))

bool   isImmediatePrim(Byte opByte);
Byte   encodedWordLogSize(Word x);
Opcode opcodeWithoutLogSize(Byte opByte);
Byte   opcodeWithLogSize(Opcode op, Byte lgSz);
Opcode decodeOpcode(Byte opByte);
Byte   sizeFromLogSize(Byte lgSz);
Byte   logSizeFromSize(Byte sz);
Byte   logSizeFromOpcode(Byte opByte);
void   encodeWord(Byte* pc, Word x, Byte n);

Opcode decodeInstruction(Byte* pc, Word *x, Word *iLen);
Byte   decodePrefix(Byte* pc, Word *x, Word* n);

Word   encodePushIntInstruction(Byte*pc, Int x);
Int    decodeInt(Byte *pc,Byte lgSz);

#endif

// Note
/*
[1] At the moment unconditional and branch-if-false offsets are 
    positive, for ife (and ife) instruction optimisation; 
branch-if-true offsets are negative for loop instruction.

[2] What we do is make sure all static call instructions are
    created with n = WORDSIZE .
*/
