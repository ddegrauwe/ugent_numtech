#!/bin/bash

for exercise in upstream forward_spectral leapfrog_centered trapezium_spectral forward_centered heun_centered matsuno_centered; do
	cd ${exercise}
	./run.sh
	cd ..
done
