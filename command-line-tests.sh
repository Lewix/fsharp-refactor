#!/bin/bash

functioncode="let f a b = a+b"
expressioncode="1+2+3+4"

oneTimeSetUp()
{
    mkdir testfiles
}

setUp()
{
    echo $functioncode > testfiles/functioncode.fs
    echo $expressioncode > testfiles/expressioncode.fs
}

oneTimeTearDown()
{
    rm testfiles/functioncode.fs testfiles/expressioncode.fs
    rmdir testfiles
}

testRename()
{
    result=$(mono CommandLine.exe rename 1:7 c testfiles/functioncode.fs)
    assertEquals "Can rename an identifier"\
        "let f c b = c+b" "$result"

    result=$(mono CommandLine.exe rename 2:3 c testfiles/functioncode.fs 2>&1)
    assertEquals "Can report an error when an incorrect position is given"\
        "No identifier found at the given range" "$result"
        
    result=$(mono CommandLine.exe rename 1:1 c testfiles/functioncode.fs 2>&1)
    assertEquals "Can report an error when the position is not in an identifier"\
        "No identifier found at the given range" "$result"

    result=$(mono CommandLine.exe rename 1:1 c testfiles/doesntexist.fs 2>&1)
    assertEquals "Can report an error when given a non-existant file"\
        "The file does not exist" "$result"

    result=$(mono CommandLine.exe rename 1:14 c testfiles/functioncode.fs 2>&1)
    assertEquals "Can report an error when the specified identifier is not declared in the source"\
        "The specified identifier was not declared in the given source" "$result"
}

testExtractFunction()
{
    result=$(mono CommandLine.exe extract-function 1:1 1:8 f testfiles/expressioncode.fs)
    assertEquals "Can extract an expression into a function"\
        "let f = 1+2+3+4 in (f)" "$result"

    result=$(mono CommandLine.exe extract-function 1:1 1:3 g testfiles/functioncode.fs 2>&1)
    assertEquals "Can report an error when expression range does not contain an expresion"\
        "No expression found at the given range" "$result"

    result=$(mono CommandLine.exe extract-function 1:1 2:4 f testfiles/expressioncode.fs 2>&1)
    assertEquals "Can report an error when expression range is not withing the file"\
        "No expression found at the given range" "$result"

    result=$(mono CommandLine.exe extract-function 1:4 1:4 f testfiles/expressioncode.fs 2>&1)
    assertEquals "Can report an error when expression range is not a valid range"\
        "No expression found at the given range" "$result"
}

testAddArgument()
{
    result=$(mono CommandLine.exe add-argument 1:13 arg 0 testfiles/functioncode.fs)
    assertEquals "Can add an argument to a binding"\
        "let f arg a b = a+b" "$result"

    result=$(mono CommandLine.exe add-argument 1:4 arg 0 testfiles/expressioncode.fs 2>&1)
    assertEquals "Can report an error when position is not inside a binding"\
        "No binding found around the given position" "$result"

    result=$(mono CommandLine.exe add-argument 2:4 arg 0 testfiles/functioncode.fs 2>&1)
    assertEquals "Can report an error when position is not within the file"\
        "No binding found around the given position" "$result"
}

. /usr/share/shunit2/src/shunit2
