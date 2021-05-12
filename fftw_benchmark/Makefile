#===============================================================================
# Makefile for FFTW benchmark.
#
# ECMWF/RIKEN-CCS
#===============================================================================

# Set compiler to Fujitsu if not already set
ifeq ($(FORTCOMP),)
FORTCOMP := frtpx
endif

# Set optimisation flag to -Kfast if not already set
ifeq ($(FORTOPT),)
FORTOPT := -Kfast
endif

# Link to libfjcrt when compiling on Fugaku
ifeq ($(FORTLIB),)
FORTLIB := -lfjcrt
endif

# Define main target
main: main.o
	$(FORTCOMP) $(FORTOPT) $(FFTWF_LIB) $(FORTLIB) -o $@ $^

# Define generic target for Fortran 90
%.o: %.F90
	$(FORTCOMP) $(FORTOPT) $(FFTWF_LIB) -c $< -o $(basename $<).o

# Define clean task
.PHONY: clean
clean:
	rm -f *.o main

