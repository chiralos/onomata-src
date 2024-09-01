#include "env.h"
#include <string.h> // for strncmp

extern Env env;

Word alignedSize(Word bytes) { 
  return (bytes / WORDSIZE) + ((bytes & (WORDSIZE-1)) ? 1 : 0);
}

Word stackItemSize(Word *sp) {
  switch(*sp) {
    case TAG_FALSE:  return 1;
    case TAG_TRUE:   return 1;
    case TAG_INT:    return 2;
    case TAG_BYTES:
    case TAG_CODE:
    case TAG_STRUCT: return 2 + alignedSize(*(sp-1));
    case TAG_BUF:    return 3;
  }
  return ERR_INTERNAL;
}

Word* next(Word* sp) { return sp - stackItemSize(sp); }
Word* stack2(void) { return next(env.sp); }
Word* itemBase(Word* p) { return next(p) + 1; }

// error symbol

void unknownSymbol(char* s, int len, Err err) {
  int copylen = len >= ERR_WORD_BUFSIZE ? 
                ERR_WORD_BUFSIZE-1 : len;
  memcpy(env.errWord,s,copylen);
  env.errWord[copylen] = '\0';
  THROW(err);
}

/////////////
// dictionary

// at the moment the dictionary is just alternating
// bytes (for name) on top of definition stack item

static bool probeDict(bool (*f)(Word *, void *),void *x,Word *limit) {
  Word* dp = env.dt;
  while (dp > limit) {
    if (!f(dp,x)) return false;
    dp = next(dp);
    dp = next(dp);
  }
  return true;
}

Word* indexDict(int n) {
  Word* dp = env.dt;
  while (n > 0) {
    dp = next(dp);
    dp = next(dp);
    n--;
  }
  return dp;
}

static bool compareEntryName(Word *dp, Buf* qName) {
  Word entryNameSz = dp[-1];
  return entryNameSz == qName->len &&
         strncmp((char *)qName->data,
                 (char *)itemBase(dp),entryNameSz) == 0;
}

static bool lookupFunc(Word *dp, void *x) {
  Search* s = (Search*)x;
  if (compareEntryName(dp,&(s->name))) {
    s->dp = dp;
    return false;
  }
  (s->n)++;
  return true;
}

void lookup(Search *s, Buf* qName) {
  s->name.data = qName->data; // see [2]
  s->name.len  = qName->len;
  s->dp        = NULL;
  s->n         = 0;
  probeDict(lookupFunc,(void *)s,env.base);
}

static bool counterFunc(Word *dp, void *x) {
  (*((int*)x))++;
  return true;
}

int dictSize(bool includeFrozen) {
  int n = 0;
  probeDict(counterFunc,(void*)&n,includeFrozen ? env.base : env.ft);
  return n;
}

int lookupPrim(char* wordStart, Word wordLen) {
  for (int i=0;i <= LAST_PARSEABLE_OP;i++) {
    const InstructionInfo* ii = &(opInfoTable[i]);
    int nameLen = strlen(ii->name);
    if (wordLen == nameLen && 
        strncmp(wordStart,ii->name,nameLen) == 0)
      return i;
  }
  return -1;
}

////////
// Notes
/*
[1] Argument (return) stack grows upwords by words. 
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

[2] Work around SDCC lack of structure assignment.
*/