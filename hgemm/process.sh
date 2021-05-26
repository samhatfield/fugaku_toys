#!/bin/bash

for file in `ls -v job_*.sh.*.out`; do
    echo $file
    problem_size=$(grep PROBLEM_SIZE $file)
    dp_time=$(grep -A 1 "Double-precision$" $file | grep -v "Double-precision")
    sp_time=$(grep -A 1 "Single-precision$" $file | grep -v "Single-precision")
    hp_time=$(grep -A 1 "Half-precision$" $file | grep -v "Half-precision")
    dp_ratio=$(python -c "print($dp_time/$hp_time if $hp_time != 0.0 else 'NaN')")
    sp_ratio=$(python -c "print($sp_time/$hp_time if $hp_time != 0.0 else 'NaN')")
    echo "$problem_size $dp_time $hp_time $dp_ratio $sp_ratio" >> processed.txt
done

