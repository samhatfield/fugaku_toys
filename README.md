# lorenz63_fugaku
This repository is for performing initial explorations into the use of
half-precision on the Fugaku supercomputer. It contains a simple Fortran 90
program for integrating the Lorenz '63 equations using a modified Euler scheme.

# How to run
- `make <prec>` where `<prec>` is `double`, `single`, or `half`
- `./main`
- `python plot.py` (assumes that Matplotlib and Numpy are available)

Collaboration between ECMWF and RIKEN-CCS.
