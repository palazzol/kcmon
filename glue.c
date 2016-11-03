
// glue.c - Merge two binaries into cartridge image

#include <stdio.h>

int main(int argc, char* argv[])
{
	FILE *fpin1, *fpin2, *fpout;
	int count;
	int fill;
	int i;
	char c;

	fpin1 = fopen("6502load.bin","rb");
	count = 0;
	fgetc(fpin1);
	while (!feof(fpin1)) {
	   fgetc(fpin1);
	   count++;
	}
	fill = 0x2000-count;
	fclose(fpin1);

	fpin1 = fopen("6502load.bin","rb");
	fpin2 = fopen("mon.bin","rb");
	fpout = fopen("kcmon.bin","wb");

	for(i=0;i<count;i++)
	{
		fread(&c,1,1,fpin1);
		fwrite(&c,1,1,fpout);
	}
	c = 0xff;
	for(i=0;i<fill;i++)
	{
		fwrite(&c,1,1,fpout);
	}
	for(i=0;i<4096;i++)
	{
		c = 0x00;
		fwrite(&c,1,1,fpout);
		fread(&c,1,1,fpin2);
		fwrite(&c,1,1,fpout);
	}
	return 0;
}
