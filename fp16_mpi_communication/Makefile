#===============================================================================
# Makefile for half-precision MPI communication test
#
# ECMWF/RIKEN-CCS
#===============================================================================

# Set compiler to Fujitsu if not already set
ifeq ($(FORTCOMP),)
FORTCOMP := mpifrtpx
endif

# Define main target
main: main.F90
	$(FORTCOMP) -o $@ $^

# Define clean task
.PHONY: clean
clean:
	rm -f main

