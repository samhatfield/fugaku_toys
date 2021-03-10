#===============================================================================
# Makefile for Lorenz '63 model used for investigating half-precision support on
# Fugaku
#
# ECMWF/RIKEN-CCS
#===============================================================================

# Set compiler to Fujitsu if not already set
ifeq ($(FORTCOMP),)
FORTCOMP := frtpx
endif

# Define main target
main: main.F90
	$(FORTCOMP) -o $@ $^

# Define clean task
.PHONY: clean
clean:
	rm -f main

