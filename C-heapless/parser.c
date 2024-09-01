#include "ono.h"
#include "escapechars.h"
#include "parser.h"
#include "env.h"
#include <stdlib.h>
#include <ctype.h>
#include <string.h> // for memcpy

static int digitInt(int c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'A' && c <= 'F') return c - 'A' + 10;
  if (c >= 'a' && c <= 'f') return c - 'a' + 10;
  return -1;
}

#define ADVANCE() \
(in->cursor)++; \
if (in->cursor >= in->end) return ERR_INCOMPLETE

#define OUTPUT_CHECK(N)\
if (out->cursor + N >= out->end) return ERR_OUT_OF_MEMORY

void emitByte(Seg* out, Byte x) { *(out->cursor++) = x; }

bool isWordChar(Byte c) {
  return isalpha(c) || c == '-' || c == '/' || isdigit(c);
}

Err parseSpaces(Seg* in, Seg* out) {
  while (in->cursor < in->end && isspace(*(in->cursor)))
    in->cursor++;
  return ERR_OK;
}

// PRE: before end, pointing at hash char
Err parseComment(Seg* in, Seg* out) {
  Byte* start = (in->cursor)++;
  while (in->cursor < in->end && *(in->cursor) != '\n')
    in->cursor++;
  if (in->cursor - start > TOKEN_LENGTH_LIMIT)
    return ERR_TOKEN_TOO_LONG;
  return ERR_OK;
}

// PRE: before end, pointing at alpha char
Err parseSymbol(Seg* in, Seg* out) {
  // collect word
  Byte* wordStart = in->cursor;
  while (true) {
    if (in->cursor >= in->end || !isWordChar(*(in->cursor)))
      break;
    (in->cursor)++;
  }

  Word symbolSize = in->cursor - wordStart;
  if (symbolSize > TOKEN_LENGTH_LIMIT) return ERR_TOKEN_TOO_LONG;

  // see if it is a built-in

  int i = lookupPrim((char *)wordStart,symbolSize);
  if (i >= 0) {
    OUTPUT_CHECK(1);
    emitByte(out,i & 0xff);
    return ERR_OK;
  }

  // otherwise emit name call

  OUTPUT_CHECK(1 + WORDSIZE + symbolSize); // see [3]
  Byte lgSz = encodedWordLogSize(symbolSize);
  Word n = sizeFromLogSize(lgSz);
  out->cursor[0] = opcodeWithLogSize(OP_CALL_NAME,lgSz);
  encodeWord(out->cursor+1,symbolSize,n);
  out->cursor++;
  memmove(out->cursor+n,wordStart,symbolSize);
  out->cursor += n+symbolSize;
  return ERR_OK;
}

static Int consumeHexInt(Seg *in) {
  Int x = 0;
  while (in->cursor < in->end) {
    if (!isxdigit(*(in->cursor))) return x;
    x = x*16 + digitInt(*(in->cursor++));
  }
  return x;
}

Err consumeInt(Seg* in, Int* r) {
  Byte* start = in->cursor;
  bool negated = false;
  if (in->cursor[0] == '-') {
    negated = true;
    ADVANCE();
    if (!isdigit(in->cursor[0])) return ERR_UNEXPECTED_CHARACTER;
  }
  Int x = 0;
  if (in->cursor+2 < in->end &&
      in->cursor[0] == '0'   &&
      in->cursor[1] == 'x') {
    if (!isxdigit(in->cursor[2])) return ERR_UNEXPECTED_CHARACTER;
    in->cursor += 2;
    x = consumeHexInt(in);
  } else {
    while (true) {
      if (in->cursor >= in->end || !isdigit(*(in->cursor))) break;
      x = x*10 + digitInt(*(in->cursor++));
    }
  }
  if (in->cursor - start > TOKEN_LENGTH_LIMIT)
    return ERR_TOKEN_TOO_LONG;
  if (negated) x *= -1;
  *r = x;
  return ERR_OK;
}

// PRE: before end, starts with digit or -
Err parseInt(Seg* in, Seg* out) {
  Int x;
  Err err = consumeInt(in,&x);
  if (err != ERR_OK) return err;
  OUTPUT_CHECK(1+WORDSIZE); // see [4]
  out->cursor += encodePushIntInstruction(out->cursor,x);
  return ERR_OK;
}

Err emitBlock(Seg* in, Seg* out, Word len, Opcode op) {
  OUTPUT_CHECK(1+WORDSIZE);
  Byte lgSz = encodedWordLogSize(len);
  Word n = sizeFromLogSize(lgSz);
  memmove(out->cursor + 1 + n,out->cursor,len);
  out->cursor[0] = opcodeWithLogSize(op,lgSz);
  out->cursor++;
  encodeWord(out->cursor,len,n);
  out->cursor += n + len;
  return ERR_OK;
}

// PRE: before end, pointing at double quote char
Err parseStr(Seg* in, Seg* out) {
  Byte* start = in->cursor;
  ADVANCE();
  Word len = 0;
  while (true) {
    Byte c = *(in->cursor);
    if (c == '"') {
      in->cursor++;
      return emitBlock(in,out,len,OP_PUSH_BYTES);
    }
    if (c == '\\') {
      if (++in->cursor >= in->end) break;
      c = *(in->cursor);
      if (c == 'x') {
        in->cursor++;
        if (in->cursor + 2 >= in->end) break;
        if (!isxdigit(in->cursor[0]) || !isxdigit(in->cursor[1]))
            return ERR_UNEXPECTED_CHARACTER;
        c = (Byte)(digitInt(in->cursor[0])*16 + 
                   digitInt(in->cursor[1]));
        in->cursor += 2;
      } else {
        if (!escToChar(c,(char *)&c)) return ERR_UNEXPECTED_CHARACTER;
        in->cursor++;
      }
    } else {
      in->cursor++;
    }
    OUTPUT_CHECK(1);
    out->cursor[len] = c;
    len++;
    if (in->cursor - start > TOKEN_LENGTH_LIMIT)
      return ERR_TOKEN_TOO_LONG;
    if (in->cursor >= in->end) break;
  }
  return ERR_INCOMPLETE;
}

////////
// Notes
/*
[3] Pessimisticly assume worse-case size bytes.

[4] Since maximum token (and thus symbol) size is < 256, we can 
    use a single byte payload length.
*/
