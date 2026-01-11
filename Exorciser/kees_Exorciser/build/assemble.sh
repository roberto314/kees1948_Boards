#!/bin/bash
#

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
${ASL} -cpu ${CPU} -L ${NAME}.asm > out.txt
${P2B} ${NAME}.p ${NAME}_b.bin >> out.txt
rm ${NAME}.p
cat out.txt | grep rror   # Check for Errors
