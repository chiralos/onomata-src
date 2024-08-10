#ifndef _ONOMATA_ESCAPECHARS_H
#define _ONOMATA_ESCAPECHARS_H

#include <stdbool.h>

bool charToEsc(char c, char* esc);
bool escToChar(char esc, char* c); 

#endif
