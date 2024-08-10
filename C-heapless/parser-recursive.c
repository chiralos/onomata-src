#include "ono.h"
#include "parser.h"
#include "parser-recursive.h"
#include <string.h> // for memmove
#include <ctype.h>

extern Env env;

// HERE add depth args, fail properly on bad stop.
//      let parseSeq have empty content

Err parseItem(Seg* in, Seg* out, int n);
Err parseSeq(Seg* in, Seg* out, int n);

Err parseSeq(Seg* in, Seg* out, int n) {
  parseSpaces(in,out);
  while (in->cursor < in->end) {
    if (*(in->cursor) == ')') return ERR_OK;
    Int err = parseItem(in,out,n);
    if (err) return err;
    parseSpaces(in,out);
  }
  return n == 0 ? ERR_OK : ERR_INCOMPLETE; // see [3]
}

#define ADVANCE() \
(in->cursor)++; \
if (in->cursor >= in->end) return ERR_INCOMPLETE

#define OUTPUT_CHECK(N)\
if (out->cursor + N >= out->end)  return ERR_OUT_OF_MEMORY

// PRE: in pointing before end, not pointing at space
Err parseItem(Seg* in, Seg* out, int n) {

  Byte c = *(in->cursor);

  if (in->cursor >= in->end) return ERR_INCOMPLETE;

  if (isspace(c)) return ERR_UNEXPECTED_CHARACTER;

  if (c == '#') return parseComment(in,out);

  if (isalpha(c)) return parseSymbol(in,out);

  if (isdigit(c) || c == '-')  return parseInt(in,out);
    
  if (c == '"') return parseStr(in,out);

  if (c == '(') {
    ADVANCE();
    Byte* bodyStart = out->cursor;

    Err err = parseSeq(in,out,n+1);
    if (err) return err;

    if (in->cursor >= in->end || *(in->cursor) != ')') 
      return ERR_INCOMPLETE;
    (in->cursor)++; // skip the ')'

    Word len = out->cursor - bodyStart;
    if (len > MAX_INSTRUCTION_PAYLOAD) 
      THROW(ERR_OUT_OF_MEMORY);
    out->cursor = bodyStart;

    return emitBlock(in,out,len,OP_PUSH_CODE);
  }

  return ERR_INCOMPLETE;
}

////////
// Notes
/*

*/
