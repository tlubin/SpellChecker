#!/bin/sh
FILE="/usr/share/dict/web2"
NUM=`wc -l $FILE`
./spellchecker $FILE $NUM