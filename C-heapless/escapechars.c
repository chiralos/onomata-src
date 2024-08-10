#include "escapechars.h"

const char escapeChars[] = 
  "\\\\"
  "\"\""
  "0\0"
  "t\t"
  "n\n"
  "r\r"
  "f\f"
  "v\v";

bool charToEsc(char c, char* esc) {
  for (int i = 0;i < sizeof(escapeChars);i += 2)
    if (escapeChars[i+1] == c) {
      *esc = escapeChars[i]; return true;
    }
  return false;
}

bool escToChar(char esc, char* c) {
  for (int i = 0;i < sizeof(escapeChars);i += 2)
    if (escapeChars[i] == esc) {
      *c = escapeChars[i+1]; return true;
    }
  return false;
}
