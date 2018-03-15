#!/bin/bash

./compiler.byte ../tests/test1.src                            > ../tests/test1.result 

 DIFF=$(diff ../tests/test1.out ../tests/test1.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte ../tests/test2.src                            > ../tests/test2.result 

 DIFF=$(diff ../tests/test2.out ../tests/test2.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte ../tests/test3.src                            > ../tests/test3.result 

 DIFF=$(diff ../tests/test3.out ../tests/test3.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi