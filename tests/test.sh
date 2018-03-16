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

./compiler.byte ../tests/test5.src                            > ../tests/test5.result 

 DIFF=$(diff ../tests/test5.out ../tests/test5.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte -parse ../tests/test5.src                     > ../tests/test5.parse.result 

 DIFF=$(diff ../tests/test5.parse.out ../tests/test5.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte ../tests/test6.src                            > ../tests/test6.result 

 DIFF=$(diff ../tests/test6.out ../tests/test6.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte -parse ../tests/test6.src                     > ../tests/test6.parse.result 

 DIFF=$(diff ../tests/test6.parse.out ../tests/test6.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte ../tests/test7.src                            > ../tests/test7.result 

 DIFF=$(diff ../tests/test7.out ../tests/test7.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte -parse ../tests/test7.src                     > ../tests/test7.parse.result 

 DIFF=$(diff ../tests/test7.parse.out ../tests/test7.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte ../tests/test8.src                            > ../tests/test8.result 

 DIFF=$(diff ../tests/test8.out ../tests/test8.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte -parse ../tests/test8.src                     > ../tests/test8.parse.result 

 DIFF=$(diff ../tests/test8.parse.out ../tests/test8.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte ../tests/test9.src                            > ../tests/test9.result 

 DIFF=$(diff ../tests/test9.out ../tests/test9.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi

./compiler.byte -parse ../tests/test9.src                     > ../tests/test9.parse.result 

 DIFF=$(diff ../tests/test9.parse.out ../tests/test9.parse.result )
  if [ "$DIFF" != "" ]; then 
    exit -1
  fi