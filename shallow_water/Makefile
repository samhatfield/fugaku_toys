#===============================================================================
# Makefile for shallow water model used for investigating half-precision support
# on Fugaku
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

# Set compiler options
# All precisions use standard optimisation: -Kfast,parallel
# Half-precision also uses -Knofz to turn off automatic flush-to-zero behaviour
double : COMPOPT = -Kfast,parallel
single : COMPOPT = -Kfast,parallel
half : COMPOPT = -Kfast,parallel,nofz

# Define precision targets
double : main
single : main
half : main

# Define main target
main: params.o io.o dynamics.o main.o
	$(FORTCOMP) $(COMPOPT) -o $@ $^

# Define generic target for Fortran 90
%.o: %.F90
	$(FORTCOMP) $(COMPOPT) -c -cpp -DPREC=$(PREC) $< -o $(basename $<).o

# Define clean task
.PHONY: clean
clean:
	rm -f *.o *.mod main
