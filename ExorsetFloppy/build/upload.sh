#!/bin/bash
VERSION=3
OSTR=~/Data/sync/Programmieren/Python/Ostrich/Ostrich1/ostrich1.py
PORT=/dev/ttyUSB1
MAX=0x7FFFF
ADDR=0x7FD00


#Set Baud to 921600
${OSTR} --device $PORT --baud 7 cb0

${OSTR} --device $PORT write extra.bin --address ${ADDR}

#${OSTR} --device $PORT read 0 ${MAX} ${FILE}_rb 

#Set Baud to 115200
${OSTR} --device $PORT --baud 0 cb7

#rm  out.txt