#include "ono.h"
#include "env.h"
#include "parser.h"
#include "bytecode.h"
#include "escapechars.h"
#include "optimiser.h"
#include <stdlib.h>
#include <ctype.h>
#include <string.h> // for memcpy(), strlen()
#include <limits.h>

extern const int stdinFD;
extern const int stdoutFD;

extern Int  ioWrite(int fd, char* s, Word len);
static void setPC(Code newPC, Code newPE);
static void advancePC(void);

///////////
// printing

// macro is smaller than the calls
#define TO_DIGIT(i) (i <= 9 ? '0' + i : 'A' - 10 + i)

#define INT_BUF_SIZE (3*WORDSIZE + 1)

static void printWord(Word x, char **pp) {
  char *p = *pp;
  while (x > 0) {
    *(--p) = TO_DIGIT(x % 10);
    x /= 10;
  }
  if (p == *pp)
    *(--p) = '0';
  *pp = p;
}

static void printInt(Int x, char** pp) {
  bool neg = false;
  if (x < 0) {
    neg = true; x = -x;
  }
  char *p = *pp;
  printWord((Word)x,&p);
  if (neg) *(--p) = '-';
  *pp = p;
}

// see [12]

static void writeStackItem(Word *sp, int fd);

void writeChar(char c, int fd) {
  ioWrite(fd,&c,1);
}

void writeNL(int fd) { ioWrite(fd,"\n",1); }

static Int writeBool(bool b, int fd) {
  const InstructionInfo* ii = &(opInfoTable[b ? OP_TRUE : OP_FALSE]);
  return ioWrite(fd,ii->name,strlen(ii->name));
}

#define EMITCHAR(C) writeChar(C,fd)

static void writeInt(Int x, bool sgnd, int fd) {
  char intbuf[INT_BUF_SIZE];
  char *end = intbuf + INT_BUF_SIZE;
  char *p = end;
  if (sgnd)
    printInt(x,&p);
  else
    printWord((Word)x,&p);
  Int n = end - p;
  ioWrite(fd,p,n);
}

static void writeString(Seg src, int fd) {
  EMITCHAR('"');
  for (;src.cursor < src.end;src.cursor++) {
    char ec;
    if (charToEsc(*src.cursor,&ec)) {
      EMITCHAR('\\');
      EMITCHAR(ec);
    } else if (!isprint(*src.cursor)) {
      EMITCHAR('\\');  
      EMITCHAR('x');
      EMITCHAR(TO_DIGIT((*src.cursor >> 4)));
      EMITCHAR(TO_DIGIT((*src.cursor & 0x0f)));
    } else {
      EMITCHAR(*src.cursor);
    }
  }
  EMITCHAR('"');
}

static void writeStruct(Word *sp, int fd) {
  Word* top = sp - 3;
  Word* p = top;
  Int n = sp[-2];
  EMITCHAR('[');
  for (int i=n;i > 0;i--) {
    p = top;
    for (int j=1;j < i;j++)
      p -= stackItemSize(p);
    if (i < n) { EMITCHAR(' '); }
    writeStackItem(p,fd);
  }
  EMITCHAR(']');
}

// see [4]
static void writeBytecode(Seg src, int fd) {
  EMITCHAR('(');
  bool first = true;
  while (src.cursor < src.end) {
    if (!first)
      {
      EMITCHAR(' ');
      }
    first      = false;
    Code ip    = src.cursor;
    Word len, n;
    Opcode op = decodeInstruction(ip,&n,&len);
    src.cursor += len;
    if (op < FIRST_IMMEDIATE_OP) {
      const InstructionInfo* ii = &(opInfoTable[op]);
      ioWrite(fd,ii->name,strlen(ii->name));
    } else {
      Seg subsrc;
      switch (op) {
        case OP_PUSH_INT:
        case OP_PUSH_BYTES: 
        case OP_PUSH_ITEM:
          {
          Code savePc = env.pc;
          env.pc = ip;
          immediateOpJumpTable[op-FIRST_IMMEDIATE_OP]();
          writeStackItem(env.sp,fd);
          popCode();
          env.pc = savePc;
          }
          break;
        case OP_PUSH_CODE:
          subsrc.cursor = src.cursor - n;
          subsrc.end    = src.cursor;
          writeBytecode(subsrc,fd);
          src.cursor = subsrc.end;
          break;
        case OP_CALL_NAME:
          ioWrite(fd,(char *)(src.cursor - n),n);
          break;
        case OP_CALL_STATIC:
          EMITCHAR('*');
          break;
        default:
          return; // unknown instruction, internal error
          break;
      } // end of instruction switch
    } // end of else not a builtin op
  } // end of bytecode loop
  EMITCHAR(')');
}

static void writeStackItem(Word *sp, int fd) {
  Seg src;
  switch(*sp) {
    case TAG_TRUE:
    case TAG_FALSE:
      writeBool(*sp == TAG_TRUE,fd);
      break;
    case TAG_INT:
      writeInt(sp[-1],true,fd);
      break;
    case TAG_BYTES:
      src.cursor = (Byte *)itemBase(sp);
      src.end    = src.cursor + sp[-1];
      writeString(src,fd);
      break;
    case TAG_CODE:
      src.cursor = (Byte *)itemBase(sp);
      src.end    = src.cursor + sp[-1];
      writeBytecode(src,fd);
      break;
    case TAG_STRUCT:
      writeStruct(sp,fd);
      break;
    case TAG_BUF:
      writeChar('$',fd);
      writeInt(sp[-2],false,fd);
      writeChar(':',fd);
      writeInt(sp[-1],false,fd);
      break;
  }
}

void overflowCheck(Word x) {
  if (env.sp + x >=  env.lsp) 
    THROW(ERR_OUT_OF_MEMORY);
}

static bool isBool(Tag t) { return t == TAG_TRUE || t == TAG_FALSE; }

void pushInt(Int x) {
  overflowCheck(2);
  *(++env.sp) = (Word)x;
  *(++env.sp) = TAG_INT;
}

Int popInt(void) {
  env.sp--;
  return (Int)(*(env.sp--));
}

void pushBytes(void* buf, int len, Tag tag) {
  Word sz = alignedSize(len);
  overflowCheck(sz + 2);
  memmove((Byte *)(env.sp+1),buf,len);
  env.sp += sz;
  *(++env.sp) = len;
  *(++env.sp) = tag;
}

void pushBuf(void* p, Word len) {
  overflowCheck(3);
  *(++env.sp) = (Word)p;
  *(++env.sp) = len;
  *(++env.sp) = TAG_BUF;
}

static void reverseWords(Word* btm, Word* top) {
  Word tmp;
  while (btm < top) {
    tmp = *top;
    *top-- = *btm;
    *btm++ = tmp;
  }
}

////////////////
// lexical stack

/*
Lexical (return) stack grows downwards by words. 
Stack frames look like this:

                              ^
                              |
           +------------- +   |
  fp ----> | previous fp  | --+
           +--------------+
           | previous lsp |
           +--------------+
           | previous pc  |
           +--------------+
           | previous pe  |
           +--------------+
           |              |
           | stack alloc  |
           |              |

  lsp ---> |              |
           +--------------+
*/

void pushFrame(void) {
  overflowCheck(4);
  Word* nfp = env.lsp - 1;
  nfp[0]    = (Word)env.fp;
  nfp[-1]   = (Word)env.lsp;
  nfp[-2]   = (Word)env.pc;
  nfp[-3]   = (Word)env.pe;
  env.fp    = nfp;
  env.lsp   = nfp - 3;
}

void popFrame(void) {
  // pop stack frame
  setPC(      (Code)env.fp[-2],
              (Code)env.fp[-3]);
  env.lsp = (Word *)env.fp[-1];
  env.fp  = (Word *)env.fp[0];
}

#define RESETFRAME env.lsp = env.fp - 3

// PRE: env.pc < env.pe, pointing to instruction that
//      invoked call
void call(Code code, Code codeEnd, bool dynamic) {
  advancePC();
  if (env.pc >= env.pe && env.fp < env.limit) {
    // tail call - just deallocate and use existing frame
    // but only if there is a frame
    RESETFRAME;
  } else {
    pushFrame();
  }
  if (dynamic) {
    Word codesz = codeEnd - code;
    Word sz = alignedSize(codesz);
    overflowCheck(sz);
    env.lsp -= sz;
    memmove(env.lsp,code,codesz);
    setPC((Code)env.lsp,((Code)env.lsp) + codesz);
  } else {
    setPC(code,codeEnd);
  }
}

//////////////////////////
// bytecode implementation

void catCode(void);
void quoCode(void);
void swpCode(void);

#define TRY if ((env.err = setjmp(env.catch)) == 0)

void nopCode (void) {  }

void exitCode(void) { env.err = ERR_EXIT; }

void popCode (void) { env.sp -= stackItemSize(env.sp); }

void dupCode(void) {
  Word sz = stackItemSize(env.sp);
  overflowCheck(sz);
  memcpy(&(env.sp[1]),&(env.sp[1-sz]),sz*WORDSIZE);
  env.sp += sz;
}

#define SWAP_MODE_SWP 0
#define SWAP_MODE_SWU 1
#define SWAP_MODE_SWA 2 
#define SWAP_MODE_DIG 3
#define SWAP_MODE_BRY 4

void swpImpl(int mode) {
  Word aSz = stackItemSize(env.sp);
  Word *b = stack2();
  Word bSz = stackItemSize(b);
  Word cSz = mode ? stackItemSize(b - bSz) : 0;
  Word* btm = b - bSz - cSz + 1;
  Word* top = env.sp;
  Word* boundary = btm + aSz - 1;
  if (mode == SWAP_MODE_SWU) {
    top = b;
    boundary = btm + bSz - 1;
  } else if (mode == SWAP_MODE_DIG) {
    boundary = btm + aSz + bSz - 1;
  }
  reverseWords(btm,top);
  reverseWords(btm,boundary);
  if (mode == SWAP_MODE_SWA) {
    Word* swaBoundary = btm + aSz + bSz - 1;
    reverseWords(boundary+1,swaBoundary);
    boundary = swaBoundary;
  }
  boundary += 1;
  reverseWords(boundary,top);
}

void swpCode(void) { swpImpl(*env.pc - OP_SWP); }

void upCode(void) {
  Int n = popInt();
  Word* p = env.sp;
  while (n > 0) {
    p = next(p);
    if (p <= env.dt) THROW(ERR_STACK_UNDERFLOW);
    n--;
  }
  Word sz = stackItemSize(p);
  overflowCheck(sz);
  memcpy(env.sp+1,itemBase(p),sz*WORDSIZE);
  env.sp += sz;
}

void catCode(void) {
  Tag t            = env.sp[0];
  Word* b          = stack2();
  Word aByteSize   = env.sp[-1];
  Word bByteSize   = b[-1];
  Word* bBase      = itemBase(b);
  Word newByteSize = aByteSize + bByteSize;
  if (newByteSize >= MAX_INSTRUCTION_PAYLOAD)
    THROW(ERR_OUT_OF_MEMORY);
  memmove(((Code)bBase)+bByteSize,(Code)(b+1),aByteSize);
  env.sp      = bBase + alignedSize(newByteSize);
  *(env.sp++) = newByteSize;
  *env.sp     = t;
}

// see [13]
void quoCode(void) {
  Opcode op = OP_PUSH_ITEM;
  Word len; // existing payload copy size
  overflowCheck(4);
  Code base = (Code)itemBase(env.sp);
  if (env.sp[0] == TAG_CODE) {
    len = env.sp[-1];
    op = OP_PUSH_CODE;
    env.sp--;
  } else {
    len = stackItemSize(env.sp)*WORDSIZE;
    env.sp++;
  }
  // INV: env.sp points to word after the payload to be shuffled
  Byte lgSz = encodedWordLogSize(len);
  Word n = sizeFromLogSize(lgSz);
  memmove(base+1+n,base,len); // shuffle up
  *base = opcodeWithLogSize(op,lgSz);
  encodeWord(base+1,len,n);
  Word newLen = 1 + n + len;
  env.sp     += alignedSize(newLen) - alignedSize(len);
  *(env.sp)   = newLen;
  *(++env.sp) = TAG_CODE;
}

void runCode(void) {
  Word codesz    = env.sp[-1];
  Code code      = (Code)itemBase(env.sp);
  popCode();
  call(code,code+codesz,true);
}

void trueCode(void)  { *(++env.sp) = TAG_TRUE; }
void falseCode(void) { *(++env.sp) = TAG_FALSE; }
void andCode(void) {
  env.sp[-1] = (env.sp[-1] == TAG_TRUE && env.sp[0] == TAG_TRUE) ? TAG_TRUE : TAG_FALSE;
  env.sp--;
  }
void orCode(void) {
  env.sp[-1] = (env.sp[-1] == TAG_TRUE || env.sp[0] == TAG_TRUE) ? TAG_TRUE : TAG_FALSE;
  env.sp--;
  }
void notCode(void) {
  env.sp[0] = env.sp[0] == TAG_TRUE ? TAG_FALSE : TAG_TRUE;
}

// see [2]
#define MATHOP(NAME,OPERATOR) void NAME(void) {\
  Int x = popInt();\
  Int y = popInt();\
  pushInt(y OPERATOR x);\
}

MATHOP(addCode,+)
MATHOP(subCode,-)
MATHOP(mulCode,*)

void divCode(void) {
  Int x = popInt();
  if (x == 0) THROW(ERR_DIVIDE_BY_ZERO);
  Int y = popInt();
  pushInt(y / x);
}

void modCode(void) {
  Int x = popInt();
  if (x == 0) THROW(ERR_DIVIDE_BY_ZERO);
  Int y = popInt();
  Int r = y % x;
  pushInt(r < 0 ? r + (x < 0 ? -x : x) : r);
}

MATHOP(bitandCode,&)
MATHOP(bitorCode,|)
MATHOP(bitxorCode,^)

void bitshiftCode(void) {
  Int a = popInt();
  Int x = popInt();
  pushInt(a < 0 ? x >> -a : x << a);
}

void bitnotCode(void) { pushInt(~popInt()); }

void cmpCode(void) {
  Tag aTag = env.sp[0];
  Word* b  = stack2();
  Tag bTag = b[0];
  int r = 0;
  if (isBool(aTag)) {
    if (!isBool(bTag))
      THROW(ERR_TYPE_MISMATCH);
    if (aTag == TAG_TRUE && bTag == TAG_FALSE)      r = -1;
    else if (aTag == TAG_FALSE && bTag == TAG_TRUE) r = 1;
    else                                            r = 0;
  } else {
    if (bTag != aTag) 
      THROW(ERR_TYPE_MISMATCH);
    switch (aTag) {
      case TAG_INT:
        subCode();
        return;
      case TAG_BYTES:
        r = b[-1] - env.sp[-1];
        if (r == 0)
          r = strncmp((char *)itemBase(b),
                      (char *)itemBase(env.sp),
                      env.sp[-1]);
        break;
      default:
        THROW(ERR_INTERNAL);
    } // tag type switch
  } // else not bool
  popCode();
  popCode();
  pushInt(r);
}

#define CMPCODE(NAME,COMPOP)\
void NAME(void) {\
  cmpCode(); popInt() COMPOP 0 ? trueCode() : falseCode();\
}
CMPCODE(eqCode,==)
CMPCODE(neqCode,!=)
CMPCODE(ltCode,<)
CMPCODE(lteCode,<=)
CMPCODE(gtCode,>)
CMPCODE(gteCode,>=)

void packCode(void) {
  Int n = popInt();
  overflowCheck(2);
  if (n < 0) THROW(ERR_ARRAY_BOUNDS);
  Word* p = env.sp;
  Word sz = 1;
  for (int i=0;i < n;i++) {
    if (p <= env.dt) THROW(ERR_STACK_UNDERFLOW);
    Word psz = stackItemSize(p);
    sz += psz;
    p -= psz;
  }
  *(++env.sp) = n;
  *(++env.sp) = sz*WORDSIZE;
  *(++env.sp) = TAG_STRUCT;
}

void unpackCode(void) { 
  *(--env.sp) = TAG_INT; // so elegant
  popCode();
}

Word* indexStruct(Int m, Word* s) {
  Int n = s[-2];
  if (m < 0 || m >= n) THROW(ERR_ARRAY_BOUNDS);
  Word* p = s - 3;
  m++;
  while (m < n) {
    p = next(p);
    m++;
  }
  return p;
}

void tupgetCode(void) {
  Int m    = popInt();
  Word* p  = indexStruct(m,env.sp);
  Word pSz = stackItemSize(p);
  popCode();
  memmove(env.sp+1,itemBase(p),pSz*WORDSIZE);
  env.sp += pSz;
}

void tupsetCode(void) {
  Int m    = popInt();
  Word aSz = stackItemSize(env.sp);
  Word* b  = stack2();
  Word* p  = indexStruct(m,b);
  Word pSz = stackItemSize(p);
  Int shuffleSize = pSz > aSz ? -(pSz - aSz) : (aSz - pSz);
  if (shuffleSize != 0) {
    Word* d = p + 1;
    memmove(d + shuffleSize,d,(env.sp - d + 1)*WORDSIZE);
    env.sp += shuffleSize;
  }
  memcpy(p-pSz+1,env.sp-aSz+1,aSz*WORDSIZE);
  env.sp -= aSz;
  env.sp[-1] += shuffleSize*WORDSIZE;
}

void chooseCode(void) {
  popCode();
  if (env.sp[1] == TAG_FALSE)
    swpImpl(SWAP_MODE_SWP);
  popCode();
}

void ifeCode(void) {
  swpImpl(SWAP_MODE_DIG);
  chooseCode();
  runCode();
}

void dipCode(void) {
  swpImpl(SWAP_MODE_SWP);
  quoCode();
  catCode();
  runCode(); 
}

void strCode(void) {
  switch (env.sp[0]) {
    case TAG_TRUE:
      popCode();
      pushBytes("true",4,TAG_BYTES);
      break;
    case TAG_FALSE:
      popCode();
      pushBytes("false",5,TAG_BYTES);
      break;
    case TAG_INT:
      {
      Int x = popInt();
      char intbuf[INT_BUF_SIZE];
      char *end = intbuf + INT_BUF_SIZE;
      char *p = end;
      printInt(x,&p);
      pushBytes(p,end-p,TAG_BYTES);
      }
      break;
    case TAG_BUF:
      {
      Word* a = env.sp;
      popCode();
      pushBytes((void *)a[-2],a[-1],TAG_BYTES);
      }
      break;
    case TAG_BYTES:
      break;
    default:
      THROW(ERR_TYPE_MISMATCH);
  }
}

void chrCode(void) {
  int n = popInt();
  env.sp++;
  Byte* a     = (Byte *)env.sp;
  a[0]        = (Byte)n;
  *(++env.sp) = 1;
  *(++env.sp) = TAG_BYTES;
}

void lenCode(void) {
  int n = 0;
  switch(env.sp[0]) {
    case TAG_BYTES:
    case TAG_BUF:
      n = env.sp[-1];
      break;
    case TAG_STRUCT:
      n = env.sp[-2];
      break;
  }
  popCode();
  pushInt(n);
}

void slcCode(void) {
  Int len    = popInt();
  Int offset = popInt();
  if (offset < 0) THROW(ERR_BAD_ARGUMENT);
  Word bufLen = env.sp[-1];
  if (*env.pc == OP_SLC)
    if (len > offset)
      len -= offset;
  if (len == -1)
    len = bufLen - offset;
  if (len < 0 || offset + len > bufLen) THROW(ERR_ARRAY_BOUNDS);
  Tag tg = (Tag)env.sp[0];
  if (tg == TAG_BUF) {
    env.sp[-1] = len;
    env.sp[-2] += offset;
  } else  {
    Byte* base = (Byte*)itemBase(env.sp);
    memmove(base,base + offset,len);
    popCode();
    env.sp += alignedSize(len);
    *(++env.sp) = len;
    *(++env.sp) = TAG_BYTES;
  }
}

void brkCode(void) {
  overflowCheck(1);
  Int m = popInt();
  Int n = env.sp[-1];
  if (m < 0 || m > n)
    THROW(ERR_ARRAY_BOUNDS);
  Int o = n - m;
  Tag tg = (Tag)env.sp[0];
  if (tg == TAG_BUF) {
    env.sp[-1] = m;
    pushBuf((void *)(env.sp[-2]+m),o);
  } else {
    popCode();
    Word mSz = alignedSize(m);
    memmove(env.sp+mSz+3,((Byte*)(env.sp+1))+m,o);
    env.sp += mSz;
    *(++env.sp) = m;
    *(++env.sp) = TAG_BYTES;
    env.sp += alignedSize(o);
    *(++env.sp) = o;
    *(++env.sp) = TAG_BYTES;
  }
}

void strgetCode(void) {
  Int i = popInt();
  if (i < 0 || i >= env.sp[-1])
    THROW(ERR_ARRAY_BOUNDS);
  popCode();
  Byte* buf = (Byte *)(env.sp + 1);
  pushInt(buf[i]);
}

void strsetCode(void) {
  Int i = popInt();
  Int x = popInt();
  if (x < 0 || x > 255)
    THROW(ERR_BAD_ARGUMENT);
  if (i < 0 || i >= env.sp[-1])
    THROW(ERR_ARRAY_BOUNDS);
  Byte* buf = (Byte *)itemBase(env.sp);
  buf[i] = (Byte)x;
}

void parseintCode(void) {
  Word strSz = env.sp[-1];
  Byte* buf = (Byte *)itemBase(env.sp);
  popCode();
  Seg in = {.cursor = buf, .end = buf + strSz };
  while (in.cursor < in.end && isspace(*in.cursor)) in.cursor++;
  Err err = ERR_OK;
  Int x = 0;
  if (in.cursor >= in.end) {
    err = ERR_INCOMPLETE;
  } else if (!(*in.cursor == '-' || isdigit(*in.cursor))) {
    err = ERR_UNEXPECTED_CHARACTER;
  } else {
    err = consumeInt(&in,&x);
  }
  if (err == ERR_OK) {
    Word n = in.cursor - buf;
    pushInt(x);
    pushInt(in.cursor - buf);
  } else {
    pushInt(0);
    pushInt(0);
  }
}

void stdinCode(void) { pushInt(stdinFD); }
void stdoutCode(void) { pushInt(stdoutFD); }

// see [7]

static Byte* locateInput(Seg* src, Word offset) {
  Code srcStart  = (Code)itemBase(env.sp);
  src->cursor = srcStart + offset;
  src->end = srcStart + env.sp[-1];
  return srcStart;
}

static Byte* locateOutput(Seg* code, Word offset) {
  Word maxCodeLen = AVAILABLE_BYTES - 2*WORDSIZE;
  Code startCodeStart = (Code)(env.sp + 1);
  code->cursor = startCodeStart + offset;
  code->end = startCodeStart + maxCodeLen;
  return startCodeStart;
}

static Word capAccumulatingProc(Seg* code, Code startCodeStart) {
  Word newCodeSize = code->cursor - startCodeStart;
  env.sp += alignedSize(newCodeSize);
  *(++env.sp) = newCodeSize;
  *(++env.sp) = TAG_CODE;
  return newCodeSize;
}

void parsePartCode(void) {
  Err err = ERR_OK;
  swpImpl(SWAP_MODE_SWP);
  Int n = popInt();
  if (n < 1) {
    err = ERR_BAD_ARGUMENT;
    goto final_return;
  }
  swpImpl(SWAP_MODE_SWP); // accumulating proc to top
  if (env.sp[0] != TAG_CODE) {
    err = ERR_TYPE_MISMATCH;
    goto final_return;
  }

  Int startCodeSize = env.sp[-1];
  // pop the lid off the accumulating proc so we can add to it
  popCode();
  Seg code;
  Code startCodeStart = locateOutput(&code,startCodeSize);

  // set up buffer
  Word bufLen = env.sp[-1];
  Seg src;
  Code srcStart = locateInput(&src,0);

  bool gotOne = false;
  while (true) {
    Code limit = src.end;
    if (bufLen > TOKEN_LENGTH_LIMIT)
      limit -= TOKEN_LENGTH_LIMIT;
    if (src.cursor >= limit) break;

    Code tokenStart = src.cursor;
    Byte c = *(src.cursor);
    if (isspace(c)) {
      err = parseSpaces(&src,&code);
    } else if (c == '#') {
      err = parseComment(&src,&code);
    } else if (isalpha(c)) {
      err = parseSymbol(&src,&code);
    } else if (isdigit(c) || c == '-') {
      err = parseInt(&src,&code);
    } else if (c == '"') {
      err = parseStr(&src,&code);
    } else if (c == '(') {
      (src.cursor)++;
      // remember input offset
      Word srcCursorOffset = src.cursor - srcStart;

      capAccumulatingProc(&code,startCodeStart);

      // push it down to be new nested proc
      swpImpl(SWAP_MODE_SWP);
      n++; // see [10]

      srcStart       = locateInput(&src,srcCursorOffset);
      startCodeStart = locateOutput(&code,0);
    } else if (c == ')') {
      if (n <= 1) {
        err = ERR_UNEXPECTED_CHARACTER;
      } else {
        (src.cursor)++;
        // remember input offset
        Word srcCursorOffset = src.cursor - srcStart;

        Word newCodeSize = capAccumulatingProc(&code,startCodeStart);

        // restore nested proc and cat this
        quoCode();
        swpImpl(SWAP_MODE_SWU); // pull next nested proc up
        n--;
        Word* procUnder = stack2();
        if (procUnder[0] != TAG_CODE) {
          err = ERR_TYPE_MISMATCH;
          swpImpl(SWAP_MODE_SWP); // pop bad non-proc thing
          popCode();
        } else {
          catCode();
        }

        // uncap
        newCodeSize = env.sp[-1];
        popCode();
        startCodeStart = locateOutput(&code,newCodeSize);
        srcStart       = locateInput(&src,srcCursorOffset);
      }
    } else {
      err = ERR_UNEXPECTED_CHARACTER;
    }

    if (err == ERR_INCOMPLETE && gotOne) {
      err = ERR_OK;
      src.cursor = tokenStart;
      break;
    }

    if (err != ERR_OK)
      break;

    gotOne = true;
  } // end of while there are more tokens

  // put the lid back on
  Word newCodeSize = capAccumulatingProc(&code,startCodeStart);

  swpImpl(SWAP_MODE_SWP);         // buf to top
  pushInt(n);
  swpImpl(SWAP_MODE_SWP);         // n below buf above procs
  pushInt(src.cursor - srcStart); // prune consumed chars from buf
  brkCode();
  swpImpl(SWAP_MODE_SWP);
  popCode();

  final_return:
  pushInt(err);
}

void overwriteCode(void) {
  void* src;
  Word srcLen = env.sp[-1];
  Word* a = stack2();
  if (env.sp[0] == TAG_BYTES) {
    src = (void *)(a + 1);
  } else {
    src = (void *)env.sp[-2];
  }
  env.sp = a;
  Word dstLen = env.sp[-1];
  if (srcLen > dstLen)
    THROW(ERR_ARRAY_BOUNDS);
  memmove((void *)(env.sp[-2]),src,srcLen);
  popCode();
}

void peekpokeCode(void) {
  Byte op = *env.pc;
  bool isPoke = op == OP_POKEINT || op == OP_POKE;
  bool isInt  = op == OP_POKEINT || op == OP_PEEKINT;
  int x = 0;
  int offset = popInt();
  if (isPoke) x = popInt();
  void* buf = (void *)env.sp[-2];
  if (offset < 0 || (offset+1)*(isInt ? WORDSIZE : 1) > env.sp[-1])
    THROW(ERR_ARRAY_BOUNDS);
  popCode();
  if (isInt) {
    Int* ip = ((Int*)buf) + offset;
    if (isPoke)
      *ip = x;
    else
      pushInt(*ip);
  } else {
    Byte *bp = ((Byte*)buf) + offset;
    if (isPoke)
      *bp = (Byte)x;
    else
      pushInt((Int)*bp);
  }
}

// NOTE must be in same order as Err enum in ono.h

static char* errorStrings[] = {
  "ok",
  "bye",
  "internal error",
  "unexpected character",
  "incomplete parse",
  "token too long",
  "out of memory",
  "bad argument",
  "stack underflow",
  "type mismatch",
  "array bounds error",
  "unknown symbol",
  "io error",
  "already defined",
  "divide by zero",
  "unknown error" };

void printError(void) {
  char *s = errorStrings[env.err < ERR_UNKNOWN ? 
                         env.err : ERR_UNKNOWN];
  ioWrite(stdoutFD,s,strlen(s));
  if (env.err == ERR_TYPE_MISMATCH ||
      env.err == ERR_STACK_UNDERFLOW) {
    if (env.pc[0] < LAST_BASIC_OP) {
      const InstructionInfo* ii = &(opInfoTable[env.pc[0]]);
      strncpy(env.errWord,ii->name,ERR_WORD_BUFSIZE);
      env.errWord[ERR_WORD_BUFSIZE-1] = '\0';
    } else {
      env.err = ERR_UNKNOWN;
    }
  }
  if (env.err == ERR_UNKNOWN_SYMBOL  || 
      env.err == ERR_ALREADY_DEFINED ||
      env.err == ERR_TYPE_MISMATCH   ||
      env.err == ERR_STACK_UNDERFLOW) {
    writeChar(' ',stdoutFD);
    ioWrite(stdoutFD,env.errWord,strlen(env.errWord));
  }
  writeNL(stdoutFD);
}

void errstrCode(void) {
  Int n = popInt();
  if (n < 0 || n > ERR_UNKNOWN) n = ERR_UNKNOWN;
  char *p = errorStrings[n];
  pushBytes(p,strlen(p),TAG_BYTES);
}

void availCode(void) {
  pushInt((env.lsp - env.sp - 2)*WORDSIZE);
}

void wordsizeCode(void) {
  pushInt((Int)WORDSIZE);
}

Word *findDef(bool throwIfMissing) {
  Buf buf;
  buf.len   = env.sp[-1], 
  buf.data  = (Byte *)itemBase(env.sp);
  Search s;
  lookup(&s,&buf);
  Word* def = s.dp;
  if (!def && throwIfMissing) 
    unknownSymbol((char *)buf.data,buf.len,ERR_UNKNOWN_SYMBOL);
  return def;
}

void defCode(void) {
  swpImpl(SWAP_MODE_SWP); // put name on top for dictionary format
  if (findDef(false)) {
    Buf buf;
    buf.len   = env.sp[-1], 
    buf.data  = (Byte *)itemBase(env.sp);
    unknownSymbol((char *)buf.data,buf.len,ERR_ALREADY_DEFINED);
  }
  Word* b = stack2(); // b is thing
#ifdef AUTOQUOTE
  if (b[0] != TAG_CODE) {
    swpImpl(SWAP_MODE_SWP);
    quoCode(); // can overflow
    swpImpl(SWAP_MODE_SWP);
    b = stack2();
  }
#endif
  Word aSz = stackItemSize(env.sp); // name size
  Word bSz = stackItemSize(b); // thing size
  Word* c = next(b); 
  Word cSz = c - env.dt;
  if (cSz > 0) { // swap def down onto dict top
    Word* btm = env.dt+1;
    reverseWords(btm,env.sp);
    reverseWords(btm,env.sp - cSz);
    reverseWords(btm + aSz + bSz,env.sp);
  }
  env.dt += aSz + bSz;
}

void allocstaticCode(void) {
  Int len = popInt();
  if (len < 0 || findDef(false)) 
    THROW(ERR_BAD_ARGUMENT);
  Word memBlockSize = alignedSize(len) + 2;
  Word shuffleSize = memBlockSize + 2;
  overflowCheck(shuffleSize);
  Word *btm = env.ft+1;
  // open up a gap
  memmove(
    (void *)(btm+shuffleSize),
    (void *)btm,
    (env.sp-env.ft)*WORDSIZE);
  env.sp += shuffleSize;
  env.dt += shuffleSize;
  // write header
  env.ft += shuffleSize;
  env.ft[-3] = len;
  env.ft[-2] = TAG_BYTES;
  env.ft[-1] = 0; // dummy name
  env.ft[0]  = TAG_BYTES;
  pushBuf(btm,len);
}

void freezeCode(void) {
   Err err = freezeAndLink();
   if (err)
     THROW(err);
}

void bytecodeCode(void) {
  const BytecodeRef* bc = &bytecodePrimTable[(*env.pc)-FIRST_BYTECODE_OP];
  call(bc->start,bc->end,false);
}

void pushintCode(void) {
  pushInt(decodeInt(env.pc+1,logSizeFromOpcode(*env.pc)));
}

void pushbytesCode(void) {
  Word len, n;
  Opcode op = decodeInstruction(env.pc,&n,&len);
  Word sz = alignedSize(n);
  overflowCheck(sz);
  memmove((Byte *)(env.sp+1),env.pc+len-n,n);
  env.sp += sz;
  if (op != OP_PUSH_ITEM) {
    overflowCheck(2);
    *(++env.sp) = n;
    *(++env.sp) = op == OP_PUSH_BYTES ? TAG_BYTES : TAG_CODE;
  }
}

void pushbufCode(void) {
  overflowCheck(3);
  Word p;
  Word len,n; 
  decodeInstruction(env.pc,&n,&len);
  memcpy(&p,env.pc+len-WORDSIZE,WORDSIZE); // see [14]
  pushBuf((void *)p,n);
}

void callnameCode(void) {
  // see [3]
  Buf buf;
  Word len, n;
  decodeInstruction(env.pc,&n,&len);
  buf.len = n; buf.data = env.pc + len - n;
  Search s;
  lookup(&s,&buf);
  Word* dp = s.dp;
  if (!dp) 
    unknownSymbol((char *)buf.data,buf.len,ERR_UNKNOWN_SYMBOL);
  dp = next(dp);
  Code code = (Code)itemBase(dp);
  len = dp[-1];
  if (dp[0] == TAG_CODE)
    call(code,code + len,true); // see [14]
  else // assume TAG_BYTES for mem bufferr
    pushBuf((void *)code,len);
}

void staticcallCode(void) {
  Word n;
  Word *x;
  decodePrefix(env.pc,(Word *)(&x),&n);
  Word sz = x[0];
  Code code = (Code)(x - alignedSize(sz));
  if (x[1] == TAG_CODE)
    call(code,code+sz,false);
  else
    pushBuf((void *)code,sz);
}

void branchCode(void) {
  Word len, n;
  Opcode op = decodeInstruction(env.pc,&n,&len);
  if (op != OP_BR) {
    Word tag = env.sp[0];
    if (tag != TAG_TRUE && tag != TAG_FALSE)
      THROW(ERR_TYPE_MISMATCH);
    bool cond = env.sp[0] == TAG_TRUE;
    popCode();
    if (op == OP_BRT && cond) {
      n = -n;
    } else if (op == OP_BRF && !cond) {
      
    } else 
      n = 0;
  }
  env.pc = env.pc + 2 + n;
  env.pcAdjusted = true;
}

void stoCode(void) {
  overflowCheck(1);
  // push lightweight frame, must exactly match recall and not
  // get mixed up with full frames
  *(--env.lsp) = (Word)env.fp;
  env.fp = env.lsp;
  Word sz = stackItemSize(env.sp);
  Word* base = itemBase(env.sp);
  env.lsp -= sz;
  memmove((void *)env.lsp,base,sz*WORDSIZE);
  popCode();
}

void rclCode(void) {
  Word* itemtop = env.fp - 1;
  Word sz = stackItemSize(itemtop);
  memmove((void *)(env.sp+1),env.lsp,sz*WORDSIZE);
  env.sp += sz;
  // pop lightweight frame
  env.lsp = env.fp;
  env.fp = (Word *)(*(env.lsp++));
}

///////////////////////
// bytecode interpreter

void typeCheck(Opcode op) {
  if (op >= FIRST_IMMEDIATE_OP) return;
  char *typec = opInfoTable[op].type;
  Word* sp = env.sp;
  while (*typec) {
    if (sp <= env.dt)
      THROW(ERR_STACK_UNDERFLOW);
    Word tag = sp[0];
    bool ok = true;
    switch (*typec) {
      case 'a': break;
      case 'b':
        if (!(tag == TAG_TRUE || tag == TAG_FALSE)) ok = false;
        break;
      case 'o': 
        if (!(tag == TAG_INT  || tag == TAG_BYTES ||
              tag == TAG_TRUE || tag == TAG_FALSE)) 
          ok = false;
        break;
      case 'r':
        if (tag != TAG_CODE) ok = false;
        break;
      case 't':
        if (tag != TAG_STRUCT) ok = false;
        break;
      case 'p':
        if (tag != TAG_BUF) ok = false;
        break;
      case 'n':
        if (tag != TAG_INT) ok = false;
        break;
      case 's':
        if (tag != TAG_BYTES) ok = false;
        break;
      case 'l':
        if (!(tag == TAG_BYTES || tag == TAG_BUF || tag == TAG_STRUCT))
          ok = false;
          break;
    }
    if (!ok)
      THROW(ERR_TYPE_MISMATCH);
    sp = next(sp);
    typec++;
  }
}

static void setPC(Code newPC, Code newPE) {
  env.pc = newPC;
  env.pe = newPE;
  env.pcAdjusted = true;
}

static void advancePC(void) {
  Word len, n;
  decodeInstruction(env.pc,&n,&len);
  env.pc += len;
  // needed for correct tail-call optimisation
  while (env.pc < env.pe) {
    Byte op = *env.pc;
    if (op == OP_BR) {
      branchCode();
      continue;
    }
    if (op == OP_NOP) {
      env.pc++; // trust this is one byte
      continue;
    }
    break;
  }
  env.pcAdjusted = true;
} 

void interpretLoop(void) {
  while (env.err == ERR_OK) {
    if (env.pc >= env.pe) {
      if (env.fp >= env.limit) break; // all done
      popFrame();
      continue;
    }
    Code ip        = env.pc;
    Byte opByte    = env.pc[0];
    Opcode op      = decodeOpcode(opByte);
    env.pcAdjusted = false;
    if (op <= LAST_BASIC_OP) {
      // see [9]
      typeCheck(op);
      basicOpJumpTable[op]();
    } else if (op <= LAST_IMMEDIATE_OP) {
      immediateOpJumpTable[op-FIRST_IMMEDIATE_OP]();
    } else {
      THROW(ERR_INTERNAL);
    }
    if (!env.pcAdjusted) // if not touched by eg branch prims
      advancePC();
  } // end of interpret loop
}

void reset(Opcode op) {
  env.fp  = env.lsp = env.limit;
  env.err = ERR_OK;
  const BytecodeRef* interpRef = 
    &(bytecodePrimTable[op-FIRST_BYTECODE_OP]);
  setPC(interpRef->start,interpRef->end);
}

///////////////
// REPL support

void undefCode(void) {
  Word* def   = findDef(true);
  popCode();
  if (def <= env.ft) THROW(ERR_UNKNOWN_SYMBOL); // see [15]
  Word* b      = next(def);
  Word gapsize = stackItemSize(def) + stackItemSize(b);
  memmove((Code)itemBase(b),(Code)(def+1),(env.sp-def)*WORDSIZE);
  env.dt -= gapsize;
  env.sp -= gapsize;
}

void isdefCode(void) {
  bool found = (findDef(false) != NULL);
  if (!found) 
    found = lookupPrim((char *)itemBase(env.sp),env.sp[-1]) >= 0;
  popCode();
  *(++env.sp) = found ? TAG_TRUE : TAG_FALSE;
}

void writeDefBody(Word *dp, int fd) {
  Int len = dp[-1];
  if (dp[0] == TAG_BYTES) {
    writeInt(len,false,fd);
  } else {
    Seg src;
    src.cursor = (Code)itemBase(dp);
    src.end    = src.cursor + dp[-1];
    writeBytecode(src,fd);
  }
}

#define MAX_LINE_LENGTH 40

void eolCheck(int* n) {
  if (*n > MAX_LINE_LENGTH) {
    writeNL(stdoutFD);
    *n = 0;
  }
}
    
void listCode(void) {
  int n = 0;
  Word sz = 0;
  for (int i=0;i <= LAST_PARSEABLE_OP;i++) {
    eolCheck(&n);
    const InstructionInfo* ii = &(opInfoTable[i]);
    sz = strlen(ii->name);
    ioWrite(stdoutFD,ii->name,sz);
    writeChar(' ',stdoutFD);
    n += sz + 1;
  }
  if (n > 0) writeNL(stdoutFD);

  n = 0;
  Word* dp = env.dt;
  if (env.dt > env.base) {
    ioWrite(stdoutFD,"-- user --",10);
    writeNL(stdoutFD);
  }
  while (dp > env.base) {
    eolCheck(&n);
    if (dp == env.ft) {
      if (n > 0) writeNL(stdoutFD);
      ioWrite(stdoutFD,"-- frozen --",12);
      writeNL(stdoutFD);
      n = 0;
    }
    sz = dp[-1];
    if (sz > 0) {
      ioWrite(stdoutFD,(char *)itemBase(dp),sz);
      writeChar(' ',stdoutFD);
    }
    dp = next(dp);
    dp = next(dp);
    n += sz + 1;
  }
  if (n > 0) writeNL(stdoutFD);
}

void showCode(void) {
  Word *def = findDef(true);
  popCode();
  def = next(def);
  if (def[0] == TAG_BYTES)
    ioWrite(stdoutFD,"*buffer* ",9);
  writeDefBody(def,stdoutFD);
  writeNL(stdoutFD);
}

void savefdCode(void) {
  Int fd = popInt();
  Word* dp = env.dt;
  while (dp > env.ft) {
    Word sz = dp[-1];
    EMITCHAR('"');
    ioWrite(fd,(char *)itemBase(dp),sz);
    EMITCHAR('"');
    writeNL(fd);

    dp = next(dp);
    writeDefBody(dp,fd);
    writeNL(fd);
    ioWrite(fd,"def",3);
    if (dp[0] == TAG_BYTES)
      ioWrite(fd,"-mem",4);
    writeNL(fd); writeNL(fd);
    dp = next(dp);
  }
}

void clearCode(void) {
  env.sp = env.dt;
}

void resetCode(void) {
  env.dt = (env.ft = (env.sp = env.base));
  reset(OP_REPL);
}

void writestackCode(void) {
  Word* sp = env.sp;
  while (sp > env.dt) {
    ioWrite(stdoutFD,"| ",2);
    writeStackItem(sp,stdoutFD);
    writeNL(stdoutFD);
    sp -= stackItemSize(sp);
  }
}

void repl(void) {
  while (env.err != ERR_EXIT) {
    reset(OP_REPL);
    TRY {
      interpretLoop();
    } else {
      printError(); // see [1]
    }
  }
}

////////
// Notes
/*
[1] There seems to be a bug in SDCC where the value of env.err
    is not correct here (it has value 0) despite being inside
the else env.err != ERR_OK branch. Putting in a printf, or
passing it as an argument to printError seems to work around it.

[2] This works but is bigger:
  env.sp[-3] = ((Int)env.sp[-3]) OPERATOR ((Int)env.sp[-1]);
  env.sp -= 2;

[3] This can made of a "look-up-and-run" prim. But that's probably
    not that useful, and in any case we should keep the opcode 
    though to save a byte.

[4] We could make this non-recursive and use the lexical stack.

[5] Space for minimal code (3) plus status int (2).
    Source gets shifted from arg to lexical stack, and some of 
it back again.

[6] Define err-ok err-incomplete print-err.

[7] sdcc doesn't have memmem().

void strfindCode(void) {
  Word aSz = stackItemSize(env.sp);
  Word* b = env.sp - aSz;
  Word bSz = stackItemSize(b);
  char *bChars = (char *)(b - bSz + 1);
  void* r = memmem(
    (void *)bChars, b[-1],
    (void *)(b + 1), env.sp[-1]);
  env.sp = b - bSz;
  if (r == NULL)
    pushInt(-1);
  else
    pushInt(((char *)r) - bChars);
}

[8] Argument (return) stack grows upwords by words. 
    Stack items look like this generically. Not all have size
(and there may be different more efficiencly encoded tags
and sizes for specific word sizes in the future).

           +-------------+ 
  sp ----> |     tag     |
           +-------------+
           |    size     |
           +-------------+
           |             |
               content
           |             |
           +-------------+
           |     tag     |
           +-------------+
           |     ...     |

  [10] Maybe limit this. Should run out of memory before n overflows
       though.

  [11] Assumption of single-byte lenghts.

  [12] The stuff from writeStackItem proto through to its definition
       is de-compilation stuff that can be dropped from a
   non-interactive interpreters. Not just this, also undef through
   repl code.

   [13] Quoted code can be much more compact by individually 
        generating code for each tag type. This avoids carrying
   around the item tag and size as full words. We do this for code
   so that parseItem doesn't build bloated code.

   [14] Problem here was that code that shuffled the dictionary
        around (static-alloc, undef, freeze) could be ran from
   dictionary. Fixed by always copying dict code to lexical
   stack, which is bad because of the speed and memory waste.
   And particularly bad because almost all code doesn't cause.

   Can fix by recording "do I shuffle ?" flag (yes/no/unknown)
   in the dict entry, and keeping it updated on def and undef.

   [15] Should be "can't undef frozen symbol" message.
*/
