#ifndef _ONOMATA_OPTIMISER_H
#define _ONOMATA_OPTIMISER_H

#include "ono.h"

Err freezeAndLink(void);
Err peepholeOptimise(Seg* in, Seg *out);

#endif
