#######################################
# Onomata C heapless hosts makefile

#### Generic Unix host

# building on OS X so probably lots of annoying tool differences
# on OS X, -ggdb works with lldb debugger

TARGETDIR=unix
INTTYPE=intptr_t
WORDTYPE=uintptr_t
CC=cc -I.. -ggdb
CFLAGS=-std=c99
OBJS=body.o escapechars.o parser.o iolib.o main.o \
info-table.o jump-table.o bytecode-prim-table.o \
bytecode.o optimiser.o stack.o

body.o : ../ono.c
	${CC} ${CFLAGS} -I. -o body.o -c ../ono.c

bytecode.o : ../bytecode.c
	${CC} ${CFLAGS} -I. -o bytecode.o -c ../bytecode.c

stack.o : ../stack.c
	${CC} ${CFLAGS} -I. -o stack.o -c ../stack.c

optimiser.o : ../optimiser.c
	${CC} ${CFLAGS} -I. -o optimiser.o -c ../optimiser.c

escapechars.o : ../escapechars.c
	${CC} ${CFLAGS} -o escapechars.o -c ../escapechars.c

parser.o : ../parser.c
	${CC} ${CFLAGS} -o parser.o -c ../parser.c

iolib.o : iolib.c
	${CC} ${CFLAGS} -I. -c iolib.c

main.o : main.c
	${CC} ${CFLAGS} -c main.c

info-table.o : ../info-table.c
	${CC} ${CFLAGS} -o info-table.o -c ../info-table.c

jump-table.o : ../jump-table.c
	${CC} ${CFLAGS} -o jump-table.o -c ../jump-table.c

bytecode-prim-table.o : ../make-bytecode-prims.c
	cd .. ; ${MAKE} TARGETDIR=${TARGETDIR} INTTYPE=${INTTYPE} WORDTYPE=${WORDTYPE} make-bytecode-prims
	./make-bytecode-prims >bytecode-prims.c
	${CC} ${CFLAGS} -I. -o bytecode-prim-table.o -c bytecode-prims.c

ono-unix : ${OBJS}
	${CC} ${CFLAGS} -o ono-unix ${OBJS}

# utils

clean :
	rm -f *.o make-bytecode-prims bytecode-prims.c 
	rm -f ono-unix
