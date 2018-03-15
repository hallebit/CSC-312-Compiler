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

./compiler ../tests/test2.src                            > ../tests/test2.result 

 DIFF=$(diff ../tests/test2.out ../tests/test2.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler -lex ../tests/test2.src                       > ../tests/test2.lex.result

 DIFF=$(diff ../tests/test2.lex.out ../tests/test2.lex.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler -parse ../tests/test2.src                     > ../tests/test2.parse.result

 DIFF=$(diff ../tests/test2.parse.out ../tests/test2.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler ../tests/test3.src                            > ../tests/test3.result 

 DIFF=$(diff ../tests/test3.out ../tests/test3.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler -lex ../tests/test3.src                       > ../tests/test3.lex.result

 DIFF=$(diff ../tests/test3.lex.out ../tests/test3.lex.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler -parse ../tests/test3.src                     > ../tests/test3.parse.result

 DIFF=$(diff ../tests/test3.parse.out ../tests/test3.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi