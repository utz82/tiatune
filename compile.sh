#!/bin/sh

./xm2tiatune
if [ "$?" = "0" ]
then
  acme -Wno-bin-len -r tiatune.lst main.asm
  if [ "$?" = "0" ]
  then
    stella tiatune.bin
  fi
fi
#stella -debug tiatune.bin
