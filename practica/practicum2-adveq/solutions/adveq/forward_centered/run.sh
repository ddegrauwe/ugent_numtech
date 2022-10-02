# Script to compile and run advection equation program

# remove existing executable
rm adveq 2>/dev/null

# compile
gfortran -fdefault-real-8 -O2 -o adveq constants.F90 setup.F90 exact_solution.F90 timeloop.F90 timestep.F90 write_result.F90 adveq.F90

# execute
./adveq
