# Makefile for SWE1D program

# Environment settings
FC90=gfortran
OPTS=-O2 -fdefault-real-8 -Wall

swe1D: constants.F90 swe1D.F90 setup.F90 timeloop.F90 derivx.F90 write_result.F90
	$(FC90) $(OPTS) -o swe1D constants.F90 swe1D.F90 setup.F90 timeloop.F90 derivx.F90 write_result.F90

clean:
	rm swe1D constants.mod

