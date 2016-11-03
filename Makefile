all: mon.bin

mon.bin: io.o mon.o
	ld65 -C simple.cfg -vm -m mon.map -o mon.bin io.o mon.o

mon.o: mon.asm
	ca65 -g -l mon.lst mon.asm

io.o: io.asm
	ca65 -g -l io.lst io.asm

clean:
	$(RM) *.o *.lst *.map *.bin