FC=gfortran
OPTS=-fdefault-real-8 -O3 -Wall -pedantic -fbounds-check
MODS=fft_mod.mod
OBJS=fft_mod.o fft992.o rpassf.o qpassf.o set99.o

lib/libfft.a: $(MODS) $(OBJS)
	ar rs ../lib/libfft.a $(OBJS)
	cp $(MODS) ../include/

%.mod: %.F90
	$(FC) -c $(OPTS) $?

%.o: %.F90
	$(FC) -c $(OPTS) $?

%.o: %.F
	$(FC) -c $(OPTS) $?

clean:
	rm ../lib/libfft.a ../include/fft_mod.mod $(OBJS) $(MODS)
	
