#!/bin/bash

print_result() {
    total=$(cat ~/results.csv | grep "$1" | wc -l)
    succ=$(cat ~/results.csv | grep "$1" | grep "Succeeded" | wc -l)
    changed=$(cat ~/results.csv | grep "$1" | grep "Succeeded" | grep "true" | wc -l)
    fail=$(cat ~/results.csv | grep "$1" | grep "Failed" | wc -l)
    fchanged=$(cat ~/results.csv | grep "$1" | grep "Failed" | grep "true" | wc -l)
    exc=$(cat ~/results.csv | grep "$1" | grep "Exception" | wc -l)

    succp=$(echo "100*$succ/$total" | bc)
    changedp=$(echo "100*$changed/$succ" | bc)
    failp=$(echo "100*$fail/$total" | bc)
    fchangedp=$(echo "100*$fchanged/$fail" | bc)
    excp=$(echo "100*$exc/$total" | bc)

    echo $(echo "$1" | tr '[a-z]' '[A-Z]')
    echo "Succeeded: $succ/$total = $succp%"
    echo "  changed: $changed/$succ = $changedp%"
    echo "Failed:    $fail/$total = $failp%"
    echo "  changed: $fchanged/$fail = $fchangedp%"
    echo "Exception: $exc/$total = $excp%"
    echo ""

    return 0
}

print_result "rename"
print_result "extract-function"
print_result "add-argument"
