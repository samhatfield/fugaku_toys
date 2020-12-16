#!/bin/bash

#PJM -L "node=1"

./main

# Convert "X.XXD+XX" to "X.XXE+XX" in output file so Numpy can read it
sed -i "s/D/E/g" output.txt
