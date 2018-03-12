#!/bin/bash

# Citation: https://www.unr.edu/it/research-resources/the-grid/using-the-grid/bash-commands 

./compiler ./tests/testIntAdd                              >  ./tests/OutputResult                  
./compiler ./tests/testIntDivide                           >> ./tests/OutputResult
./compiler ./tests/testIntDivideByNestedZero               &>> ./tests/OutputResult
# Prints: Fatal error: exception Failure("Dividing by zero is bad")
./compiler ./tests/testIntDivideByZero                     &>> ./tests/OutputResult
# Fatal error: exception Failure("Dividing by zero is bad")
./compiler ./tests/testIntMultiply                         >> ./tests/OutputResult
./compiler ./tests/testIntNegativeNestedOps                >> ./tests/OutputResult
./compiler ./tests/testIntNegativeResultAdd                >> ./tests/OutputResult
./compiler ./tests/testIntNegativeResultDivide             >> ./tests/OutputResult
./compiler ./tests/testIntNegativeResultMulitplication     >> ./tests/OutputResult
./compiler ./tests/testIntNegativeResultSubtraction        >> ./tests/OutputResult
./compiler ./tests/testIntNestedOps                        >> ./tests/OutputResult
# Fatal error: exception Failure("Unexpected end of file encountered")                  Do I want this behavior?
./compiler ./tests/testIntOnly                             &>> ./tests/OutputResult
./compiler ./tests/testIntSubtract                         >> ./tests/OutputResult
./compiler ./tests/testLParenMismatch                      &>> ./tests/OutputResult
# Fatal error: exception Failure("Unexpected end of file encountered")
./compiler ./tests/testNegativeInput                       &>> ./tests/OutputResult
# Fatal error: exception Failure("Unexpected token found: -")
./compiler ./tests/testRParenMismatch                      &>> ./tests/OutputResult
# Fatal error: exception Failure("Unexpected token found: +")
./compiler ./tests/testUnknownOperator                     &>> ./tests/OutputResult
# Fatal error: exception Failure("Unexpected character found: %")

#Citation: https://stackoverflow.com/questions/3611846/bash-using-the-result-of-a-diff-in-a-if-statement
#Citation: https://unix.stackexchange.com/questions/81998/understanding-of-diff-output

 DIFF=$(diff ./tests/OutputResult ./tests/ExpectedResult)
 if [ "$DIFF" != "" ]; then 
  exit -1
 fi