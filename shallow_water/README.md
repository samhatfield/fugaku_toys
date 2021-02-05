# Shallow water model

This repository contains a simple shallow water, based on one of Peter Dueben,
which itself was based on one of David Marshall.
 
# How to run
- `make <prec>` where `<prec>` is `double`, `single`, or `half`
- `pjsub ./job.sh`

# How to view output
- `python convert_to_netcdf.py` (assumes that Iris and Numpy are available)
- `ncview output.nc`

