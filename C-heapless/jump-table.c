#include "bytecode.h"

extern void nopCode        (void);
extern void popCode        (void);
extern void dupCode        (void);
extern void swpCode        (void);
extern void quoCode        (void);
extern void catCode        (void);
extern void runCode        (void);
extern void trueCode       (void);
extern void falseCode      (void);
extern void andCode        (void);
extern void orCode         (void);
extern void notCode        (void);
extern void ifeCode        (void);
extern void addCode        (void);
extern void subCode        (void);
extern void mulCode        (void);
extern void divCode        (void);
extern void modCode        (void);
extern void bitandCode     (void);
extern void bitorCode      (void);
extern void bitnotCode     (void);
extern void bitxorCode     (void);
extern void bitshiftCode   (void);
extern void cmpCode        (void);
extern void eqCode         (void);
extern void neqCode        (void);
extern void ltCode         (void);
extern void lteCode        (void);
extern void gtCode         (void);
extern void gteCode        (void);
extern void lenCode        (void);
extern void packCode       (void);
extern void unpackCode     (void);
extern void tupgetCode     (void);
extern void tupsetCode     (void);
extern void strCode        (void);
extern void strbrkCode     (void);
extern void strgetCode     (void);
extern void strsetCode     (void);
extern void parseintCode   (void);
extern void sleepmilliCode (void);
extern void stdinCode      (void);
extern void stdoutCode     (void);
extern void openCode       (void);
extern void closeCode      (void);
extern void seekCode       (void);
extern void readstrCode    (void);
extern void writestrCode   (void);
extern void seeksetCode    (void);
extern void seekcurCode    (void);
extern void seekendCode    (void);
extern void rdonlyCode     (void);
extern void wronlyCode     (void);
extern void rdwrCode       (void);
extern void truncCode      (void);
extern void creatCode      (void);
extern void nonblockCode   (void);
extern void memsliceCode   (void);
extern void memcpyCode     (void);
extern void peekpokeintCode(void);
extern void savefdCode     (void);
extern void errstrCode     (void);
extern void availCode      (void);
extern void wordsizeCode   (void);
extern void parseCode      (void);
extern void writestackCode (void);
extern void undefCode      (void);
extern void listCode       (void);
extern void showCode       (void);
extern void clearCode      (void);
extern void resetCode      (void);
extern void defCode        (void);
extern void allocstaticCode(void);
extern void freezeCode     (void);
extern void exitCode       (void);
extern void bytecodeCode   (void);
extern void pushintCode    (void);
extern void pushbytesCode  (void);
extern void pushbufCode    (void);
extern void callnameCode   (void);
extern void staticcallCode (void);
extern void branchCode     (void);
extern void stoCode        (void);
extern void rclCode        (void);

// THIS MUST BE KEPT IN Opcode ORDER

void (*basicOpJumpTable[N_BASIC_OPS])(void) = {
  nopCode,        // OP_NOP

  popCode,        // OP_POP
  dupCode,        // OP_DUP
  swpCode,        // OP_SWP
  swpCode,        // OP_SWU
  swpCode,        // OP_SWA
  swpCode,        // OP_DIG
  swpCode,        // OP_BRY

  quoCode,        // OP_QUO
  catCode,        // OP_CAT
  runCode,        // OP_RUN

  trueCode,       // OP_TRUE
  falseCode,      // OP_FALSE
  andCode,        // OP_AND
  orCode,         // OP_OR
  notCode,        // OP_NOT

  ifeCode,        // OP_IFE

  addCode,        // OP_ADD
  subCode,        // OP_SUB
  mulCode,        // OP_SUB
  divCode,        // OP_DIV
  modCode,        // OP_MOD

  bitandCode,     // OP_BITAND
  bitorCode,      // OP_BITOR
  bitnotCode,     // OP_BITNOT
  bitxorCode,     // OP_BITXOR
  bitshiftCode,   // OP_BITSHIFT

  cmpCode,        // OP_CMP
  eqCode,         // OP_EQ
  neqCode,        // OP_NEQ
  ltCode,         // OP_LT
  lteCode,        // OP_LTE
  gtCode,         // OP_GT
  gteCode,        // OP_GTE

  lenCode,        // OP_LEN

  packCode,       // OP_PACK
  unpackCode,     // OP_UNPACK
  tupgetCode,     // OP_TUPGET
  tupsetCode,     // OP_TUPSET

  strCode,        // OP_STR
  catCode,        // OP_STRCAT
  strbrkCode,     // OP_STRBRK
  strgetCode,     // OP_STRGET
  strsetCode,     // OP_STRSET
  parseintCode,   // OP_PARSEINT

  memsliceCode,   // OP_MEMRAW
  memcpyCode,     // OP_MEMCPY
  peekpokeintCode,// OP_PEEKINT
  peekpokeintCode,// OP_POKEINT

  allocstaticCode,// OP_DEFMEM

  sleepmilliCode, // OP_SLEEPMILLI

  stdinCode,      // OP_STDIN
  stdoutCode,     // OP_STDOUT
  openCode,       // OP_OPEN
  closeCode,      // OP_CLOSE
  seekCode,       // OP_SEEK
  readstrCode,    // OP_READSTR
  writestrCode,   // OP_WRITESTR
  seeksetCode,    // OP_SEEKSET
  seekcurCode,    // OP_SEEKCUR
  seekendCode,    // OP_SEEKEND
  rdonlyCode,     // OP_RDONLY
  wronlyCode,     // OP_WRONLY
  rdwrCode,       // OP_RDWR
  truncCode,      // OP_TRUNC
  creatCode,      // OP_CREAT
  nonblockCode,   // OP_NONBLOCK

  savefdCode,     // OP_SAVEFD
  errstrCode,     // OP_ERRSTR
  availCode,      // OP_AVAIL
  wordsizeCode,   // OP_WORDSIZE
  parseCode,      // OP_PARSE
  writestackCode, // OP_WRITESTACK
  undefCode,      // OP_UNDEF
  listCode,       // OP_LIST
  showCode,       // OP_SHOW
  clearCode,      // OP_CLEAR
  resetCode,      // OP_RESET
#ifdef AUTOQUOTE
  defCode,        // OP_DEF
#else
  defCode,        // OP_DEF
#endif
  freezeCode,     // OP_FREEZE
  exitCode,       // OP_EXIT
  
  bytecodeCode,   // OP_DIP
  bytecodeCode,   // OP_LOOP
  bytecodeCode,   // OP_WRITELINE
  bytecodeCode,   // OP_LOAD
  bytecodeCode,   // OP_LOADFD
  bytecodeCode,   // OP_LOADSTR
  bytecodeCode,   // OP_SAVE
  bytecodeCode,   // OP_REPL
  bytecodeCode,   // OP_DEFMEM

  stoCode,         // OP_STO
  rclCode,         // OP_RCL
};

void (*immediateOpJumpTable[N_IMMEDIATE_OPS])(void) = {
  pushintCode,     // OP_PUSH_INT
  pushbytesCode,   // OP_PUSH_BYTES
  pushbytesCode,   // OP_PUSH_CODE
  pushbytesCode,   // OP_PUSH_ITEM
  pushbufCode,     // OP_PUSH_BUF
  callnameCode,    // OP_CALL_NAME
  staticcallCode,  // OP_CALL_STATIC
  branchCode,      // OP_BR
  branchCode,      // OP_BRF
  branchCode,      // OP_BRT
};
