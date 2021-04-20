#===============================================================================
# Makefile for program testing support for HGEMM.
#
# ECMWF/RIKEN-CCS
#===============================================================================

# Set compiler to Fujitsu if not already set
ifeq ($(FORTCOMP),)
FORTCOMP := frtpx
COMPARG := -SSL2 -Kfast,parallel
endif

# Define main target
main: main.o
	$(FORTCOMP) $(COMPARG) -o $@ $^

# Define generic target for Fortran 90
%.o: %.F90
	$(FORTCOMP) $(COMPARG) -c -DPROBLEM_SIZE=${PROBLEM_SIZE} $< -o $(basename $<).o

# Define clean task
.PHONY: clean
clean:
	rm -f *.o main

