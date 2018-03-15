#!/bin/bash

./compiler ../tests/test1.src                            > ../tests/test1.result 

 DIFF=$(diff ../tests/test1.out ../tests/test1.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler -lex ../tests/test1.src                       > ../tests/test1.lex.result

 DIFF=$(diff ../tests/test1.lex.out ../tests/test1.lex.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler -parse ../tests/test1.src                     > ../tests/test1.parse.result

 DIFF=$(diff ../tests/test1.parse.out ../tests/test1.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi