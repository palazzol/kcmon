# kcmon

Original Apple II monitor code, ported to the unreleased Mattel Intellivision Keyboard Component

Back in 2003, I ported the original Apple II Monitor software to help me reverse-engineer the KC.  
The KC adds a 6502-based computer to the Intellivision Master Component.  I packaged the Apple II monitor
with a CP1610 bootloader, which I could load as an image using the standard Intellivision cartridge slot.

The first time around, I used a somewhat "modified" Intellicart to do this.  This was difficult to get to work.
I also used a build environment based on DOS-era tools.

This time around, I am using a stock Cuttle Cart 3, which can be used without mods on the KC system to load the software.

Binaries can be found in the bin directory

The following tools are required to build the cartridge image:
- cc65.exe and ld65.exe, from the cc65 distribution, found here:  http://cc65.github.io/cc65/
- as1600.exe and bin2rom, from the jzintv distribution, found here: http://spatula-city.org/~im14u2c/intv/
- gcc, to build a simple C utility (glue.exe) to merge the images.

Status:
- Currently generates the 2003 image, works but not pretty.  
- Also, the code is not well-documented at this time.
