#!/bin/bash

# run all osceq exercises

# loop over directories
for dd in osceq_*; do 
	cd ${dd}
	./run.sh
	cd ..
done

