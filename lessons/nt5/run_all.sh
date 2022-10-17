#!/bin/bash

# SWE1D
cd swe1D
make
./swe1D
cd ..

# diffusion equation
cd diffusion
gfortran diffeq.F90
./a.out
rm a.out
cd ..


