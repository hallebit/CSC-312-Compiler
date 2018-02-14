#!/bin/bash

# Citation: https://www.unr.edu/it/research-resources/the-grid/using-the-grid/bash-commands 

./bareBonesProj foo bar baz           > tstOutput
./bareBonesProj " " 42 'h'            >> tstOutput
./bareBonesProj                       >> tstOutput
./bareBonesProj help                  >> tstOutput
./bareBonesProj -help                 >> tstOutput
./bareBonesProj --help                >> tstOutput
./bareBonesProj -help great           >> tstOutput
./bareBonesProj great -help           >> tstOutput
./bareBonesProj -length foo bar baz   >> tstOutput
./bareBonesProj -length " " 42 'h'    >> tstOutput
./bareBonesProj -length               >> tstOutput
./bareBonesProj -length help          >> tstOutput
./bareBonesProj great -length         >> tstOutput
./bareBonesProj great -length great   >> tstOutput
./bareBonesProj -length -length great >> tstOutput

#Citation: https://stackoverflow.com/questions/3611846/bash-using-the-result-of-a-diff-in-a-if-statement
#Citation: https://unix.stackexchange.com/questions/81998/understanding-of-diff-output

DIFF=$(diff tstOutput tstExpected)
if [[ "$DIFF" != "" ]]; then 
    exit -1
fi
    

