all: mon.bin

mon.bin: mon.o
	ld65 -t none -vm -m mon.map -o mon.bin mon.o

mon.o: mon.asm
	ca65 -g -l mon.lst mon.asm

clean:
	$(RM) *.o *.lst *.map *.bin