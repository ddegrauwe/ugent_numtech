#!/bin/bash
#

# create figures for Boyd.I paper

# function to convert tex file in eps figure
function tex2eps {
	# argument : file name
	fName=$1
	
	# compile tex file to ps
	cp -sf ${fName}.tex tmp.tex
	latex --interaction=nonstopmode tmp > tmp.texout
	dvips -q tmp

	# convert ps file to eps file
	echo "%%!PS-Adobe-3.0 EPSF-3.0" > ${fName}.eps
	tail -n +1 tmp.ps >> ${fName}.eps

	# clean up
	rm -f tmp.* texput.log
	
	echo "Created ${fName}.eps"
}

# function to convert tex file in pdf figure
function tex2pdf {
	# argument : file name
	fName=$1
	
	# compile tex file to pdf
	cp -sf ${fName}.tex tmp.tex
	pdflatex --interaction=nonstopmode tmp > tmp.texout
	mv tmp.pdf ${fName}.pdf

	# clean up
	rm -f tmp.* texput.log

	echo "Created ${fName}.pdf"
}

# make figures
FORMAT=eps
tex2${FORMAT} time_extrapolation
