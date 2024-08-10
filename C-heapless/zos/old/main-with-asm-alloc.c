#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include "zos_vfs.h"
#include "zos_sys.h"

#include "parser.h"
#include "ono.h"

Env env;

// things missing from SDCC

int digittoint(int c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return c - 'f';
  if (c >= 'A' && c <= 'F') return c - 'F';
  return 0;
}

// we can't just use alloca() with SDCC since SDCC
// controls the stack 

void* alloca(Word sz) __naked {
__asm
  ex   de, hl ; parameter in de for subtraction
  ld   h, #0
  ld   l, #0
  add  hl, sp ; load sp into hl
  sbc  hl, de ; calc new sp after alloc
  inc  hl     ; we get to use the return address word
  inc  hl
  pop  de     ; return address in de
  ld   sp, hl ; allocate
  ex   de, hl ; ready to return in de
  jp   (hl)   ; popped return address from de now in hl
__endasm;
}

void *runCode(void) __naked {
__asm
  ; preamble
  push ix ; save ix
  ld   ix, #0
  add  ix, sp ; we need a stack from so we can deallocate after alloca

  ; Word codesz = env.sp[-1];
  ld   hl, (#(_env + 6) + 0)
  dec  hl
  ld   b, (hl)
  dec  hl
  ld   c, (hl) ; bc = codesz

  ; Byte* stackCode = alloca(codesz);
  ld   h, b
  ld   l, c ; codesz now in bc and hl
  call  _alloca ; bc=codesz de=stackCode

  ; we are after the alloca, so we can use the stack
  ; so we go nuts
  push bc ; stack: codesz
  push de ; stack: stackCode codesz

  ; Word sz = stackItemSize(env.sp);
  ld   hl, (#(_env + 6) + 0)
  call _stackItemSize
  ; hl free here, de=sz, stack: stackCode codesz
  ex   de, hl
  pop  de ; de=stackCode
  pop  bc ; bc=codesz
  push de ; stack: stackCode
  push bc ; stack: codesz stackCode
  push hl ; stack: sz codesz stackCode
  push de ; stack: stackCode sz codeSize stackCode
  push bc ; stack: codesz stackCode sz codeSize stackCode
  ex   de, hl ; de=sz

  ; memcpy(stackCode,&(env.sp[1-sz]),codesz);
  ld   a, #0x01
  sub  a, e
  ld   l, a
  sbc  a, a
  sub  a, d
  ld   h, a
  add  hl, hl
  ld   bc, (#(_env + 6) + 0)

  add  hl, bc ; hl now correct source
              ; stack: codesz stackCode sz codeSize stackCode
  pop  bc     ; stack: stackCode sz codeSize stackCode
  pop  de     ; stack: sz codeSize stackCode
  ld   a, b
  or   a, c
  jr   Z, 00103$
  ldir
00103$:

  ; env.sp -= sz;
  ld   bc, (#(_env + 6) + 0)
  pop  hl ; hl=sz, stack: codeSize stackCode
  add  hl, hl
  ld   a, c
  sub  a, l
  ld   c, a
  ld   a, b
  sbc  a, h
  ld   b, a
  ld   ((_env + 6)), bc

  ; (void)interpret(stackCode,stackCode + codesz); // continuation call stuff
  pop  bc ; bc=codeSz, stack: stackCode
  pop  de ; de=stackCode stack:
  ld   a, c
  add  a, e
  ld   l, a
  ld   a, b
  adc  a, d
  ld   h, a
  ex   de, hl
  call _interpret

  ; return NULL;
  ld   sp, ix      ; deallocates
  pop  ix          ; restore ix
  ld   de, #0x0000 ; return null
  ret              ; return
__endasm;
}

///////
// main

#define DEFAULT_MEM_SIZE_WORDS 256
Word mem[DEFAULT_MEM_SIZE_WORDS];

#define BUFSIZE 256
char inBuf[BUFSIZE];
Byte codeBuf[BUFSIZE];

int main(int argc, char** argv) {

  printf("word size: %u\n", WORDSIZE);

  env.base  = mem;
  env.limit = env.base + DEFAULT_MEM_SIZE_WORDS;
  env.dt    = env.sp = env.base;
  env.err   = ERR_OK;

  while (env.err != ERR_EXIT) {
    uint16_t n = 4;
    write(DEV_STDOUT,"Ti> ",&n); // see [1]
    n = sizeof(inBuf);
    zos_err_t err = read(DEV_STDIN,inBuf,&n);
    if (err != ERR_SUCCESS) return 1;
    Seg src = { .cursor = inBuf,   .end = (Byte *)(inBuf + n) };
    Seg out = { .cursor = codeBuf, .end = codeBuf + sizeof(codeBuf) };

    bool ok = true;
    if ((env.err = setjmp(env.parseFrame)) == 0) {
      parseSeq(&src,&out);
    } else {
      printError();
      ok = false;
    }

    if (ok) {
      if((env.err = setjmp(env.interpFrame)) == 0) {
        (void)interpret(codeBuf,out.cursor);
      } else {
        printError();
      }
      dumpStack();
    }
  }

  exit ();
  return 0;
}

////////
// Notes
/*
[1] printf without newline doesn't seem to flush to output.
*/
