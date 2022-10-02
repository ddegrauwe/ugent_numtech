# Script to compile and run the oscillation equation program

# remove executable file
rm osceq

# Compile
echo "Compiling"
gfortran constants.F90 setup_constants.F90 timeloop.F90 write_result.F90 osceq.F90 -o osceq

# Run
echo "Running"
./osceq