#===============================================================================
# Makefile for Reduced Precision Emulator verification program
#
# ECMWF/RIKEN-CCS
#===============================================================================

# Set compiler to Fujitsu if not already set
ifeq ($(FORTCOMP),)
FORTCOMP := frtpx
endif

# Define main target
main: rp_emulator.o main.o
	$(FORTCOMP) -o $@ $^

# Define generic target for Fortran 90
%.o: %.f90
	$(FORTCOMP) -c $< -o $(basename $<).o

# Define clean task
.PHONY: clean
clean:
	rm -f *.o main
