#!/bin/bash
NAME=floppy2_commented

ASL=../../../ExorsetFloppy/bin/asl
P2B=../../../ExorsetFloppy/bin/p2bin

# Make zeros
dd if=/dev/zero of=zeros1 bs=1 count=$((0xE800))
dd if=/dev/zero of=zeros2 bs=1 count=$((0x400))

# now is the Exordisk ROM
${ASL} -cpu 6800 -L ${NAME}.asm > out.txt
${P2B} ${NAME}.p exordisk.bin >> out.txt
rm ${NAME}.p
cat out.txt | grep rror   # Check for Errors

#cat zeros1 exordisk.bin exbug11.bin > exbug.bin # my firmware
cat zeros1 exordisk_sim.bin exbug11.bin > exbug.bin
rm zeros1 zeros2 out.txt

# copy it to exorsim directory
cp exbug.bin ../../../exorsim/
# upload
#minipro -p "X2864AP" -w Exbug_ROM_${VERSION}.bin