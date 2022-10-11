# Script to compile and run advection equation program

# remove existing executable
rm adveq 2>/dev/null

# compile (including fft library)
gfortran -fdefault-real-8 -O2 -o adveq \
	-I /user/gent/407/vsc40744/ugent/numtech/aux/fft/include/ constants.F90 setup.F90 \
	timeloop.F90 timestep.F90 write_result.F90 exact_solution.F90 \
	adveq.F90 /user/gent/407/vsc40744/ugent/numtech/aux/fft/lib/libfft.a

# execute
./adveq
