# FFTW benchmark

This simple program benchmarks the 1D real-to-complex Fast Fourier Transform
made available by the FFTW library. It performs a direct-inverse transform
cycle on a large 1D array many times over and measures the wallclock time.

It is based on [this](https://gist.github.com/appleparan/
c048c44668ede7ef28ba63c660b6dcf3) GitHub Gist.

## Building
First define the `FFTWF_LIB` environment variable, pointing it to the location
of the FFTW library directory. For example, on Fugaku I set
```
export FFTWF_LIB=-L/vol0004/apps/oss/spack-v0.16/opt/spack/linux-rhel8-a64fx/\
    fj-4.3.1/fftw-3.3.8-b6z4s4rfp5ju6m6pa3zr5a2xpbws7zum/lib\
    -Wl,-rpath,/vol0004/apps/oss/spack-v0.16/opt/spack/linux-rhel8-a64fx/\
    fj-4.3.1/fftw-3.3.8-b6z4s4rfp5ju6m6pa3zr5a2xpbws7zum/lib -lfftw3f
```
Also if you don't want to use the Fujitsu compiler you must also set the
`FORTCOMP` environment variable to your compiler of choice, and set the
`FORTOPT` environment variable with your optimisation flags.
