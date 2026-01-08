#!/bin/bash
NAME=floppy2_commented
VERSION=3

ASL=../../../ExorsetFloppy/bin/asl
P2B=../../../ExorsetFloppy/bin/p2bin

# Make zeros
dd if=/dev/zero of=zeros1 bs=1 count=$((0x800))

# now is the Exordisk ROM
${ASL} -cpu 6800 -L ${NAME}.asm > out.txt
${P2B} ${NAME}.p exordisk.bin >> out.txt
rm ${NAME}.p
cat out.txt | grep rror   # Check for Errors

cat zeros1 exordisk.bin exbug.bin > Exbug_ROM_${VERSION}.bin
rm zeros1 out.txt