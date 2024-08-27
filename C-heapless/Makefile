####################
# Onomata C Heapless

CFLAGS = -ggdb -DWORDTYPE=${WORDTYPE} -DINTTYPE=${INTTYPE}
OBJS = parser.o parser-recursive.o escapechars.o info-table.o \
env.o optimiser.o bytecode.o

make-bytecode-prims : make-bytecode-prims.o ${OBJS}
	cc -o ${TARGETDIR}/make-bytecode-prims make-bytecode-prims.o ${OBJS}

clean :
	rm -f *.o
	rm -f make-bytecode-prims
	cd unix ; make clean ; cd ..
	cd zos ; make clean ; cd ..
