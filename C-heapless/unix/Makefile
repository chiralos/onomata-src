#######################################
# Onomata C heapless hosts makefile

#### Generic Unix host

# building on OS X so probably lots of annoying tool differences
# on OS X, -ggdb works with lldb debugger

TARGETDIR=unix
INTTYPE=intptr_t
WORDTYPE=uintptr_t
CC=cc
CFLAGS=-std=c99 -I. -I..
OBJS=body.o escapechars.o parser.o iolib.o main.o \
info-table.o jump-table.o bytecode-prim-table.o \
bytecode.o optimiser.o env.o

body.o : ../ono.c
	${CC} ${CFLAGS} -o body.o -c ../ono.c

bytecode.o : ../bytecode.c
	${CC} ${CFLAGS} -o bytecode.o -c ../bytecode.c

env.o : ../env.c
	${CC} ${CFLAGS} -o env.o -c ../env.c

optimiser.o : ../optimiser.c
	${CC} ${CFLAGS} -o optimiser.o -c ../optimiser.c

escapechars.o : ../escapechars.c
	${CC} ${CFLAGS} -o escapechars.o -c ../escapechars.c

parser.o : ../parser.c
	${CC} ${CFLAGS} -o parser.o -c ../parser.c

iolib.o : iolib.c
	${CC} ${CFLAGS} -c iolib.c

main.o : main.c
	${CC} ${CFLAGS} -c main.c

info-table.o : ../info-table.c
	${CC} ${CFLAGS} -o info-table.o -c ../info-table.c

jump-table.o : ../jump-table.c
	${CC} ${CFLAGS} -o jump-table.o -c ../jump-table.c

bytecode-prim-table.o : ../make-bytecode-prims.c
	cd .. ; ${MAKE} TARGETDIR=${TARGETDIR} INTTYPE=${INTTYPE} WORDTYPE=${WORDTYPE} make-bytecode-prims
	./make-bytecode-prims >bytecode-prims.c
	${CC} ${CFLAGS} -o bytecode-prim-table.o -c bytecode-prims.c

onoi : ${OBJS}
	${CC} ${CFLAGS} -o onoi ${OBJS}

# utils

clean :
	rm -f *.o make-bytecode-prims bytecode-prims.c 
	rm -f onoi
