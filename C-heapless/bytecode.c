#include "bytecode.h"

// encode

Byte encodedWordLogSize(Word x) {
  if (x <= 0xff) return 0;
  if (WORDSIZE >= 2 && x <= 0xffff) return 1;
  if (WORDSIZE >= 4 && x <= 0xffffffff)  return 2; // see [1]
  if (WORDSIZE == 8) return 3;
  return 0;
}

// see [2]
void encodeWord(Byte* pc, Word x, Byte n) {
  if (n == 1)
    *((uint8_t *)pc) = (uint8_t)x;
  else if (WORDSIZE >= 2 && n == 2)
    *((uint16_t *)pc) = (uint16_t)x;
  else if (WORDSIZE >= 4 && n == 4) // see [1]
    *((uint32_t *)pc) = (uint32_t)x;
  else if (WORDSIZE == 8)
    *((uint64_t *)pc) = (uint64_t)x;
}

bool   isImmediatePrim(Byte opByte) { return opByte & 0x80; }
Byte   opcodeWithLogSize(Opcode op, Byte lgSz) { return op | (lgSz << 5); }
Opcode opcodeWithoutLogSize(Byte opByte) { return opByte & 0x9f; }
Byte   sizeFromLogSize(Byte lgSz) { return 1 << lgSz; }
Byte   logSizeFromOpcode(Byte opByte) { return (opByte & 0x60) >> 5; }

Byte   logSizeFromSize(Byte sz) {
  Byte n = 0;
  while (sz > 1) { n++; sz = sz >> 1; }
  return n;
}


// decode

Opcode decodeOpcode(Byte opByte) {
  return isImmediatePrim(opByte) ? 
         opcodeWithoutLogSize(opByte) : (Opcode)opByte;
}

static Word decodeWord(Byte *pc, Byte lgSz) {
  if (lgSz == 0) 
    return (Word)(*((uint8_t *)pc));
  if (WORDSIZE >= 2 && lgSz == 1) 
    return (Word)(*((uint16_t *)pc));
  if (WORDSIZE >= 4 && lgSz == 2) 
    return (Word)(*((uint32_t *)pc));
  if (WORDSIZE == 8 && lgSz == 3) 
    return (Word)(*((uint64_t *)pc));
  return 0;
}

static Word instructionLengthSuffix(Opcode op, Word n) {
  switch (op) {
    case OP_PUSH_INT:
    case OP_CALL_STATIC:
    case OP_BR:          
    case OP_BRF:
    case OP_BRT:           
      return 0;
    case OP_PUSH_BYTES:
    case OP_PUSH_CODE:
    case OP_PUSH_ITEM:
    case OP_CALL_NAME:     
       return n;
      break;
    case OP_PUSH_BUF:
      return sizeof(Word);
      break;
    default:
      break;
  }
  return 0; // error
}

Opcode decodeInstruction(Byte* pc, Word* x, Word* iLen) {
  Byte opByte = *pc;
  if (!isImmediatePrim(opByte)) {
    *iLen = 1; return (Opcode)opByte;
  }
  Word n;
  Byte lgSz = decodePrefix(pc,x,&n);
  Opcode op = opcodeWithoutLogSize(opByte);
  *iLen = 1 + n + instructionLengthSuffix(op,*x);
  return op;
}

Byte decodePrefix(Byte* pc, Word* x, Word* n) {
  Byte lgSz = logSizeFromOpcode(*pc);
  *n = sizeFromLogSize(lgSz);
  *x = decodeWord(pc+1,lgSz);
  return lgSz;
}

// signed int stuff

static Byte encodedIntLogSize(Int x) {
  x = x < 0 ? -x : x;
  if (x <= 0x7f) return 0;
  if (WORDSIZE >= 2 && x <= 0x7fff) return 1;
  if (WORDSIZE >= 4 && x <= 0x7fffffff)  return 2; // see [1]
  if (WORDSIZE == 8) return 3;
  return 0;
}

// see [2]
static void encodeInt(Byte* pc, Word x, Byte lgSz) {
  if (lgSz == 0)
    *((int8_t *)pc) = (int8_t)x;
  else if (WORDSIZE >= 2 && lgSz == 1)
    *((int16_t *)pc) = (int16_t)x;
  else if (WORDSIZE >= 4 && lgSz == 2) // see [1]
    *((int32_t *)pc) = (int32_t)x;
  else if (WORDSIZE == 8)
    *((int64_t *)pc) = (int64_t)x;
}

Word encodePushIntInstruction(Byte*pc, Int x) {
  Byte lgSz = encodedIntLogSize(x);
  *pc = opcodeWithLogSize(OP_PUSH_INT,lgSz);
  encodeInt(pc+1,x,lgSz);
  return 1+sizeFromLogSize(lgSz);
}

Int decodeInt(Byte *pc, Byte lgSz) {
  if (lgSz == 0) 
    return (Int)(*((int8_t *)pc));
  if (WORDSIZE >= 2 && lgSz == 1) 
    return (Int)(*((int16_t *)pc));
  if (WORDSIZE >= 4 && lgSz == 2) 
    return (Int)(*((int32_t *)pc));
  if (WORDSIZE == 8 && lgSz == 3) 
    return (Int)(*((int64_t *)pc));
  return 0;
}

////////
// Notes
/*
[1] The WORDSIZE conditions hopefully get the compiler to 
    eliminate code branches on smaller word count architectures.

[2] These may have to use memcpy for architectures with memory
    alignment restrictions (if the compiler doesn't deal with that).
*/