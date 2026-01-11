#!/bin/bash
#

ASL=../bin/asl
P2B=../bin/p2bin

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

NAME=extra
## Use asl
${ASL} -cpu ${CPU} -L ${NAME}.asm > out.txt
${P2B} ${NAME}.p ${NAME}.bin >> out.txt
rm ${NAME}.p
cat out.txt | grep rror   # Check for Errors
