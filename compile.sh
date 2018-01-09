#!/bin/sh

./xm2tiatune
if [ "$?" = "0" ]
then
  ./acme main.asm
  if [ "$?" = "0" ]
  then 
    stella test.bin
  fi
fi
#stella -debug test.bin