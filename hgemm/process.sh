#!/bin/bash

for file in `ls -v job_*.sh.*.out`; do
    problem_size=$(grep PROBLEM_SIZE $file)
    dp_time=$(grep -A 1 "Double-precision" $file | grep -v "Double-precision")
    hp_time=$(grep -A 1 "Half-precision" $file | grep -v "Half-precision")
    ratio=$(python -c "print($dp_time/$hp_time if $hp_time != 0.0 else 'NaN')")
    echo "$problem_size $dp_time $hp_time $ratio" >> processed.txt
done

