#!/bin/bash
#
VERSION=6

OSTR=~/Data/sync/Programmieren/Python/Ostrich/Ostrich1/ostrich1.py
PORT=/dev/ttyUSB1
MAX=0x7FFFF
#ADDR=0x7FD00
ADDR=$((0x80000-8192))
#echo $ADDR

ASL=../../../ExorsetFloppy/bin/asl
P2B=../../../ExorsetFloppy/bin/p2bin
CPU=6800
#DIR="/home/rob/Projects/mc-6502_mini/"
PWD=`pwd`
#echo $PWD
#######################################################################
#
# Assemble everything here and convert it to binary and s19
#
#######################################################################
## Use as0
#${AS0} ${NAME}.asm -L CRE C S > ${NAME}.lst 

NAME=floppy2_commented
## Use asl
# ${ASL} -cpu ${CPU} -L ${NAME}.asm > out.txt
# ${P2B} ${NAME}.p ${NAME}_b.bin >> out.txt
# cat out.txt | grep rror   # Check for Errors

${ASL} -cpu ${CPU} -L ${NAME}.asm 1> /dev/null
retVal=$?
if [ $retVal -ne 0 ]; then
    echo "Error in assembly"
    exit $retVal
fi
echo "No error in assembly"

${P2B} ${NAME}.p ${NAME}_b.bin 1> /dev/null
retVal=$?
if [ $retVal -ne 0 ]; then
    echo "Error in converting"
    exit $retVal
fi
rm ${NAME}.p
echo "No error in converting"


# Make zeros
dd if=/dev/zero of=zeros1 bs=1 count=$((0x800)) >/dev/null 2>&1
#dd if=/dev/zero of=zeros2 bs=1 count=$((0x100))

echo "Building the binary"
cat zeros1 ${NAME}_b.bin  exbug11.bin > Exbug_ROM_${VERSION}.bin
rm zeros1

#exit

# upload
#minipro -p "X2864AP" -w Exbug_ROM_${VERSION}.bin

echo "Uploading the binary..."
#Set Baud to 921600
${OSTR} --device $PORT --baud 7 cb0 1> /dev/null
${OSTR} --device $PORT write Exbug_ROM_${VERSION}.bin --address ${ADDR} > /dev/null 2>&1
#${OSTR} --device $PORT read 0 ${MAX} ${FILE}_rb 
#Set Baud to 115200
${OSTR} --device $PORT --baud 0 cb7 1> /dev/null
echo "Done"
