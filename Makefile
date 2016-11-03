all: kcmon.rom

kcmon.rom: kcmon.bin
	bin2rom kcmon.bin

kcmon.bin: mon.bin 6502load.bin glue.exe
	glue.exe

mon.bin: io.o mon.o
	ld65 -C simple.cfg -vm -m mon.map -o mon.bin io.o mon.o

mon.o: mon.asm
	ca65 -g -l mon.lst mon.asm

io.o: io.asm
	ca65 -g -l io.lst io.asm

6502load.bin: 6502load.asm
	as1600 -o 6502load.bin -l 6502load.lst 6502load.asm

glue.exe: glue.c
	gcc -o glue.exe glue.c

clean:
	$(RM) *.o *.lst *.map *.bin *.rom 6502load.cfg glue.exe