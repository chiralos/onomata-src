#ifndef _ONOMATA_STACK_H
#define _ONOMATA_STACK_H

#include "ono.h"

Word alignedSize(Word bytes);
Word stackItemSize(Word *sp);

Word* next(Word* sp);
Word* stack2(void);
Word* itemBase(Word *p);

Word*  indexDict(int n);
void  lookup(Search* s, Buf* qName);
int   dictSize(bool includeFrozen);

#endif

////////
// Notes
