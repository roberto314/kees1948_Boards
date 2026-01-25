#!/bin/bash
NAME=floppy2_commented
VERSION=4

ASL=../../../ExorsetFloppy/bin/asl
P2B=../../../ExorsetFloppy/bin/p2bin

# Make zeros
dd if=/dev/zero of=zeros1 bs=1 count=$((0x800))
dd if=/dev/zero of=zeros2 bs=1 count=$((0x400))

# now is the Exordisk ROM
${ASL} -cpu 6800 -L ${NAME}.asm > out.txt
${P2B} ${NAME}.p exordisk.bin >> out.txt
rm ${NAME}.p
cat out.txt | grep rror   # Check for Errors

cat zeros1 exordisk.bin zeros2 exbug11.bin > Exbug_ROM_${VERSION}.bin
rm zeros1 zeros2 out.txt

# upload
minipro -p "X2864AP" -w Exbug_ROM_${VERSION}.bin