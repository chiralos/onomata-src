#ifndef _ONOMATA_ENV_H
#define _ONOMATA_ENV_H

#include "ono.h"

Word alignedSize(Word bytes);
Word stackItemSize(Word *sp);

Word* next(Word* sp);
Word* stack2(void);
Word* itemBase(Word *p);

Word* indexStruct(Int m, Word* s);

Word* indexDict(int n);
void  lookup(Search* s, Buf* qName);
int   dictSize(bool includeFrozen);
int   lookupPrim(char* wordStart, Word wordLen);

void unknownSymbol(char*s, int len, Err err);
void overflowCheck(Word x);

#endif

////////
// Notes
