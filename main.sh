#!/bin/sh

# file paths for dictionary and ranker
DFILE="dict/dict_txt"
RFILE="dict/en_50k.txt"

# line lengths and names for each file
DARGS=`wc -l $DFILE`
RARGS=`wc -l $RFILE`

./spellcheckeropt $DARGS $RARGS