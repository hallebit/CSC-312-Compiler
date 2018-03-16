#!/bin/bash

./compiler.byte ../tests/test1.src                            > ../tests/test1.result 

 DIFF=$(diff ../tests/test1.out ../tests/test1.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte -parse ../tests/test1.src                     > ../tests/test1.parse.result 

 DIFF=$(diff ../tests/test1.parse.out ../tests/test1.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte ../tests/test2.src                            > ../tests/test2.result 

 DIFF=$(diff ../tests/test2.out ../tests/test2.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte -parse ../tests/test2.src                     > ../tests/test2.parse.result 

 DIFF=$(diff ../tests/test2.parse.out ../tests/test2.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte ../tests/test3.src                            > ../tests/test3.result 

 DIFF=$(diff ../tests/test3.out ../tests/test3.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte -parse ../tests/test3.src                     > ../tests/test3.parse.result 

 DIFF=$(diff ../tests/test3.parse.out ../tests/test3.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte ../tests/test4.src                            > ../tests/test4.result 

 DIFF=$(diff ../tests/test4.out ../tests/test4.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte -parse ../tests/test4.src                     > ../tests/test4.parse.result 

 DIFF=$(diff ../tests/test4.parse.out ../tests/test4.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi