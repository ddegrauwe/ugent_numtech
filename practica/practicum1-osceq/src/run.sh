#!/bin/bash

rm osceq output.dat

gfortran constants.F90 setup_constants.F90 timeloop.F90 write_result.F90 osceq.F90 -o osceq

./osceq

