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

ifeq ($(FORTOPT),)
FORTOPT := -Kfast
endif

# Define main target
main: main.o
	$(FORTCOMP) $(FORTOPT) $(FFTWF_LIB) -o $@ $^

# Define generic target for Fortran 90
%.o: %.F90
	$(FORTCOMP) $(FORTOPT) $(FFTWF_LIB) -c $< -o $(basename $<).o

# Define clean task
.PHONY: clean
clean:
	rm -f *.o main

