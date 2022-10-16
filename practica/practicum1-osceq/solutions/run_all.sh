#!/bash/bin

# run all osceq exercises

# loop over directories
for dd in osceq*; do 
	cd ${dd}
	./run.sh
	cd ..
done

