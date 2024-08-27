#include "optimiser.h"
#include "bytecode.h"
#include "env.h"
#include <string.h> // for memcpy, memmove

/////////
// Linker

Err peepholeDefToStackTop(int i, Word* sz, Word* originalSize) {
  Word* dp = indexDict(i);
  dp = next(dp);
  Seg in;
  in.cursor         = (Code)itemBase(dp);
  *originalSize = dp[-1];
  in.end            = in.cursor + dp[-1];
  Seg out;
  out.cursor = (Code)(env.sp+1);
  out.end    = out.cursor + AVAILABLE_BYTES;
  Code outStart     = out.cursor;
  Err err           = peepholeOptimise(&in,&out);
  if (err) return err;
  *sz = out.cursor - outStart;
  return ERR_OK;
}

static bool linkProc(Seg code) {
  while (code.cursor < code.end) {
    Word x,len;
    Opcode op = decodeInstruction(code.cursor,&x,&len);
    if (op == OP_PUSH_CODE) {
      Seg sub;
      sub.end = code.cursor + len;
      sub.cursor = sub.end - x;
      if(!linkProc(sub)) return false;
    } else if (op == OP_CALL_STATIC) {
      Word* dp = indexDict(x);
      dp = next(dp);
      dp--; // we point at the size word of the target def
      encodeWord(code.cursor+1,(Word)dp,WORDSIZE); // see [7]
    }
    code.cursor += len;
  }
  return true;
}

Err freezeAndLink(void) {
  Word nUnfrozenDefs = dictSize(false);

  // pass 1 : check for def closure, calculate max intermediate size
  //          and total new size needed

  Int  totalAdditionalSize = 0;
  for (int i=nUnfrozenDefs-1;i >= 0;i--) {
    Word sz, originalSize;
    Err err = peepholeDefToStackTop(i,&sz,&originalSize);
    if (err) return err;
    totalAdditionalSize += (sz - originalSize);
  }

  if (totalAdditionalSize > 0 && totalAdditionalSize >= AVAILABLE_BYTES)
    return ERR_OUT_OF_MEMORY;

  // pass 2 : peephole opt and write call instructions with def number
  //          shuffle each one into final place

  for (int i=nUnfrozenDefs-1;i >= 0;i--) {
    Word sz,originalSize;
    Err err           = peepholeDefToStackTop(i,&sz,&originalSize);
    if (err) return err; // should not happen
    Word* dp = indexDict(i);
    dp = next(dp);
    Word* base = itemBase(dp);
    Word oldSize = alignedSize(dp[-1]);
    Word newSize = alignedSize(sz);
    Int  dSize = newSize > oldSize ? -(oldSize - newSize) : newSize - oldSize;
    if (dSize != 0) {
      Word* oldRest = base + oldSize;
      memmove(base + newSize,
              oldRest,
              (env.sp+1+newSize-oldRest)*WORDSIZE);
      env.sp += dSize;
      env.dt += dSize;
    }
    base[newSize] = sz;
    memcpy(base,env.sp+1,newSize*WORDSIZE);
  }

  // pass 3 : link

  for (int i=0;i < nUnfrozenDefs;i++) {
    Word* dp = indexDict(i);
    dp = next(dp);
    Seg code;
    code.cursor = (Code)itemBase(dp);
    code.end = code.cursor + dp[-1];
    linkProc(code);
  }

  // then move frozen top up

  env.ft = indexDict(0);
  return ERR_OK;
}

///////////
// Peephole

typedef bool PeepholeTransformation(Seg *, Byte**);

typedef struct PeepholeEntry {
  Opcode                  op;
  int                     arity;
  PeepholeTransformation* f;
} PeepholeEntry;

bool ifeTrans(Seg*,Byte**);
bool dipTrans(Seg*,Byte**);
bool loopTrans(Seg*,Byte**);

PeepholeEntry peepholeTable[] = {
  { OP_IFE,  2, ifeTrans  },
  { OP_DIP,  1, dipTrans  },
  { OP_LOOP, 1, loopTrans },
};

const int nPeepholeEntries = sizeof(peepholeTable) / sizeof(PeepholeEntry);

#define OVERFLOW_CHECK(N) if (out->cursor + (N) >= out->end) return false

// see [1]
Err peepholeOptimise(Seg* in, Seg* out) {
  Byte* stackedProcs[2];
  int nStackedProcs = 0;
  while (in->cursor < in->end) {
    Word sz, n, len;
    Byte lgSz = 0;
    n = 0;

    Opcode op = decodeInstruction(in->cursor,&sz,&len);
    if (op == OP_CALL_NAME || op == OP_PUSH_CODE)  {
      lgSz = encodedWordLogSize(sz);
      n    = sizeFromLogSize(lgSz);
    }

    if (op == OP_CALL_NAME) { // see [6]

      Buf qName = { .len = sz, .data = in->cursor + 1 + n };
      Search s;
      lookup(&s,&qName);
      if (s.dp == NULL) {
        unknownSymbol((char *)qName.data,qName.len,ERR_UNKNOWN_SYMBOL);
        return ERR_UNKNOWN_SYMBOL;
      }
      OVERFLOW_CHECK(1+WORDSIZE);
      out->cursor[0] = opcodeWithLogSize(OP_CALL_STATIC,logSizeFromSize(WORDSIZE));
      out->cursor++;
      encodeWord(out->cursor,s.n,WORDSIZE);
      out->cursor += WORDSIZE;
      in->cursor += len;

    } else if (op == OP_PUSH_CODE) {

      stackedProcs[1] = stackedProcs[0];
      Seg sub;
      sub.cursor = in->cursor + 1 + n;
      sub.end = sub.cursor + sz;
      stackedProcs[0] = out->cursor;
      // assume that new size will be same n as old
      out->cursor += 1 + n;
      Err err = peepholeOptimise(&sub,out);
      if (err != ERR_OK) return err;
      sz           = out->cursor - (stackedProcs[0] + 1 + n);
      Byte outLgSz = encodedWordLogSize(sz);
      Word outN    = sizeFromLogSize(outLgSz);
      out->cursor  = stackedProcs[0];
      if (lgSz != outLgSz) { // size bytes changed, need to shuffle
        if (outN > n) {
          OVERFLOW_CHECK(outN - n);
        }
        memmove(out->cursor + 1 + outN,
                out->cursor + 1 + n,
                sz);
      }
      out->cursor[0] = opcodeWithLogSize(OP_PUSH_CODE,outLgSz);
      out->cursor++;
      encodeWord(out->cursor,sz,outN);
      out->cursor += outN + sz;
      if (nStackedProcs < 2) nStackedProcs++;
      in->cursor = sub.cursor;
    } else {
      int i = 0;
      for (;i < nPeepholeEntries;i++) {
        PeepholeEntry* pep = &peepholeTable[i];
        if (op == pep->op && nStackedProcs >= pep->arity) {
          pep->f(out,stackedProcs);
          // peephole fn responsible for updating out cursor
          break;
        }
      }
      if (i >= nPeepholeEntries) {
        OVERFLOW_CHECK(len);
        memmove(out->cursor,in->cursor,len);
        out->cursor += len;
      }
      in->cursor += len;
      nStackedProcs = 0;
    }
  }
  return ERR_OK;
}

bool ifeTrans(Seg* out, Byte** stacked) {
  Byte* cursor = stacked[1];
  Word cSz, cN;
  Byte cLgSz = decodePrefix(cursor,&cSz,&cN);
  Byte* aStart = stacked[0];
  Word aSz, aN;
  Byte aLgSz = decodePrefix(aStart,&aSz,&aN);

  Word sz          = cSz + 1 + aN; // need to jump past BR as well
  Byte lgSz        = encodedWordLogSize(sz);
  Word n           = sizeFromLogSize(lgSz);
  if (n != cN) { // need to shuffle
    if (n > cN) {
      OVERFLOW_CHECK(n - cN);
    }
    memmove(cursor+1+n,cursor+1+cN,cSz+1+aN+aSz);
  }
  cursor[0] = opcodeWithLogSize(OP_BRF,lgSz);
  encodeWord(cursor+1,sz,n);
  cursor += 1 + n + cSz;
  // cursor should now be pointing at alternative pushcode op
  // which happens to have correct size to become the BR
  cursor[0] = opcodeWithLogSize(OP_BR,aLgSz);
  out->cursor = cursor + 1 + aN + aSz;
  return true;
}

bool dipTrans(Seg* out, Byte** stacked) {
  Byte* cursor = stacked[0];
  Word sz, n;
  decodePrefix(cursor,&sz,&n);
  cursor[0] = OP_STO;
  cursor++;
  memmove(cursor,cursor+n,sz);
  cursor += sz;
  cursor[0] = OP_RCL;
  out->cursor = cursor+1;
  return true;
}

bool loopTrans(Seg* out, Byte** stacked) {
  Byte* cursor = stacked[0];
  Word sz, n;
  decodePrefix(cursor,&sz,&n);
  memmove(cursor,cursor+1+n,sz);
  cursor += sz;
  // need to check that the offset needed for the branch
  // doesn't have n larger than the code body n
  // first guess sz + 1 + n
  sz += 1+n;
  Byte lgSz = encodedWordLogSize(sz);
  Word newN = sizeFromLogSize(lgSz); // might be larger
  if (newN > n) {
    OVERFLOW_CHECK(newN-n);
  }
  // assume that changing from n to newN won't bump
  // it up _another_ size category
  sz -= n;
  sz += newN; // second guess original sz + 1 + newSz
  //lgSz = encodedWordLogSize(sz);
  //n    = sizeFromLogSize(lgSz); // the size of _that_ size
  cursor[0] = opcodeWithLogSize(OP_BRT,lgSz);
  cursor++;
  encodeWord(cursor,sz,newN);
  out->cursor = cursor + newN;
  return true;
}

// needs to wait for in-out transformation
//void ifokTrans(Seg* code, Byte** stacked) {
//}

// Misc

////////
// Notes
/*
[1] We could try to handle type errors that we spot eg
    (A) (B) (C) ife => (A) is not bool

[3] Branches require co-operation of instruction-pointer
    advance code to correctly implement tail-call optimisation.
After a call, any tail of unconditional branches or nops that
leads to the end of code means that call is a tail call.

[4] sto and rcl require perfect nesting with respect to all
    the other uses of the lexical stack.

[5] For simplicity we make a pessimistic assumption about
    needed space to write the output code ie that encoded
sizes are a whole word.

[6] On encountering named calls, the peephole optimiser looks
    up the name in the global env dictionary and writes its
number (0 is topmost) into the destination part of the
instruction. A later pass needs to resolve these.

When compiling the bytecode prims this should not occur as
there are no named calls in those definitions.

[7] This will be a problem if the pointer-to-word size is larger
    than the word size.
*/