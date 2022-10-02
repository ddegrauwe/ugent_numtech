# compilation of fft test program

# settings
FC90=gfortran
OPTS=-O2 -fdefault-real-8
FFTDIR=..

# compile!
test_fft: test_fft.F90
	$(FC90) $(OPTS) -o test_fft test_fft.F90 -I $(FFTDIR)/include/ -L $(FFTDIR)/lib/ -lfft

clean:
	rm test_fft
