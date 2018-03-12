#!/bin/bash

# Citation: https://www.unr.edu/it/research-resources/the-grid/using-the-grid/bash-commands 

./read foo bar baz           > tstOutput
./read " " 42 'h'            >> tstOutput
./read                       >> tstOutput
./read help                  >> tstOutput
./read -help                 >> tstOutput
./read --help                >> tstOutput
./read -help great           >> tstOutput
./read great -help           >> tstOutput
./read -length foo bar baz   >> tstOutput
./read -length " " 42 'h'    >> tstOutput
./read -length               >> tstOutput
./read -length help          >> tstOutput
./read great -length         >> tstOutput
./read great -length great   >> tstOutput
./read -length -length great >> tstOutput

#Citation: https://stackoverflow.com/questions/3611846/bash-using-the-result-of-a-diff-in-a-if-statement
#Citation: https://unix.stackexchange.com/questions/81998/understanding-of-diff-output

DIFF=$(diff tstOutput tstExpected)
if [ "$DIFF" != "" ]; then 
    exit -1
fi
