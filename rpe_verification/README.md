# Reduced Precision Emulator Verification

This program is for verifying that the
[Reduced Precision Emulator](https://github.com/aopp-pred/rpe) (rpe) is
correctly emulating the IEE 754 half-precision type, by comparing with the
natively implemented FP16 type on Fugaku. 1024 numbers are generated between the
lower and upper limits of 2^-24 and 2^17 (roughly 10^-7 and 10^5, respectively),
which covers the half-precision range. The number are spaced by powers of two.
The numbers are generated at double-precision and then placed in variables for
rpe and Fugaku so that the rounding errors can be compared.

# How to run
- `make`
- `pjsub ./job.sh`
