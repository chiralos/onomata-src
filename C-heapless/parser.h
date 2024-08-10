#ifndef _ONOMATA_PARSER_H
#define _ONOMATA_PARSER_H

#include "ono.h"

// see [1]
#define TOKEN_LENGTH_LIMIT 96

void emitByte(Seg* out, Byte x);
Err  consumeInt(Seg* in, Int* x);
void emitIntPush(Seg* out, Int x);
Err emitBlock(Seg* in, Seg* out, Word len, Opcode op);

Err parseSpaces (Seg* in, Seg* out);
Err parseComment(Seg* in, Seg* out);
Err parseSymbol (Seg* in, Seg* out);
Err parseInt    (Seg* in, Seg* out);
Err parseStr    (Seg* in, Seg* out);

#endif

////////
// Notes
/*
[1] Allow reasonably long numbers, literal strings, identifiers
    and comments. We ensure that parsing from a buffer never cuts
tokens by ensuring that we always have at least a token's worth
of characters left in a buffer we are parsing from - unless the
buffer is shorter than the token length, which indicates end
of input.
*/

