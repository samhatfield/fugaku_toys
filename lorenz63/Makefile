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

# Define bytewidths for different real kinds
double : PREC = 8
single : PREC = 4
half : PREC = 2

# Define precision targets
double : main
single : main
half : main

# Define main target
main: main.o
	$(FORTCOMP) -o $@ $^

# Define generic target for Fortran 90
%.o: %.F90
	$(FORTCOMP) -c -cpp -DPREC=$(PREC) $< -o $(basename $<).o

# Define clean task
.PHONY: clean
clean:
	rm -f *.o main

