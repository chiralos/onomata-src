###################################
# Onomata C heapless hosts Makefile

#### ZOS target

# building with SDCC. See Zeal OS kernel build instructions
# need to set up
#   ZOS_PATH and SDCC_BASE
# tested with Zeal 8-bit Computer

ZOS_C_HEADERS=${ZOS_PATH}/kernel_headers/sdcc/include
ZOS_CRT0=${ZOS_PATH}/kernel_headers/sdcc/bin/zos_crt0.rel
SDCC_LIB=${SDCC_BASE}/share/sdcc/lib

CC=sdcc
LD=sdldz80
OBJCOPY=gobjcopy
INTTYPE=int16_t
WORDTYPE=uint16_t
TARGETDIR=zos

CFLAGS=-mz80 --std-c99 -c --codeseg TEXT -I${ZOS_C_HEADERS} -I.. --disable-warning 85
LDFLAGS=-n -mjwx -i -b _HEADER=0x4000 $(SDLD_FLAGS) -k $(ZOS_PATH)/kernel_headers/sdcc/lib -l z80
RELS=main.rel body.rel bytecode.rel iolib.rel escapechars.rel parser.rel \
info-table.rel jump-table.rel bytecode-prim-table.rel env.rel optimiser.rel

body.rel : ../ono.c
	${CC} ${CFLAGS} -o body.rel -c ../ono.c

escapechars.rel : ../escapechars.c
	${CC} ${CFLAGS} -o escapechars.rel -c ../escapechars.c

parser.rel : ../parser.c
	${CC} ${CFLAGS} -o parser.rel -c ../parser.c

bytecode.rel : ../bytecode.c
	${CC} ${CFLAGS} --disable-warning 94 --disable-warning 126 -o bytecode.rel -c ../bytecode.c

env.rel : ../env.c
	${CC} ${CFLAGS} -o env.rel -c ../env.c

iolib.rel : iolib.c
	${CC} ${CFLAGS} -o iolib.rel -c iolib.c

optimiser.rel : ../optimiser.c
	${CC} ${CFLAGS} -o optimiser.rel -c ../optimiser.c

main.rel : main.c
	${CC} ${CFLAGS} -o main.rel -c main.c

info-table.rel : ../info-table.c
	${CC} ${CFLAGS} -o info-table.rel -c ../info-table.c

jump-table.rel : ../jump-table.c
	${CC} ${CFLAGS} -o jump-table.rel -c ../jump-table.c

bytecode-prim-table.rel : ../make-bytecode-prims.c
	cd .. ; ${MAKE} TARGETDIR=${TARGETDIR} WORDTYPE=${WORDTYPE} INTTYPE=${INTTYPE} make-bytecode-prims
	./make-bytecode-prims >bytecode-prims.c
	${CC} ${CFLAGS} -o bytecode-prim-table.rel -c bytecode-prims.c

onoi : ${RELS}
	${LD} ${LDFLAGS} ono-zos.ihx ${ZOS_CRT0} ${RELS}
	${OBJCOPY} --input-target=ihex --output-target=binary ono-zos.ihx onoi

# utils

clean :
	rm -f *.bin *.lis *.o *.ihx *.map *.noi *.lst *.asm *.rel *.sym
	rm -f *.mem *.lk
	rm -f onoi make-bytecode-prims bytecode-prims.c
