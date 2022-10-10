#!/bin/bash

ltxf() {
	ff=$1
	cp ${ff}.tex tmp.tex
	pdflatex --interaction=nonstopmode tmp.tex > /dev/null 2>&1
	if [[ ! -f tmp.pdf ]]; then exit 1; fi
	pdflatex --interaction=nonstopmode tmp.tex > /dev/null 2>&1
	pdflatex --interaction=nonstopmode tmp.tex
	mv tmp.pdf ${ff}.pdf
	echo -e "\nCreated ${ff}.pdf\n"
	rm tmp.aux tmp.log tmp.nav tmp.out tmp.snm tmp.toc tmp.tex
}

# current directory
currdir=${PWD}

### slides
cd slides
ffs="nt0-welcome nt2-time nt4-spectral nt6-sisl nt-projects nt1-stability nt3-space nt5-extras nt7-parallel"
ffs="nt3-space"
#ffs=""

for ff in ${ffs}; do
	ltxf ${ff}
done

### practica
cd ${currdir}
cd practica/practicum1-osceq/doc
ffs="practicum1-osceq"
ffs=""
for ff in ${ffs}; do
	ltxf ${ff}
done

cd ${currdir}
cd practica/practicum2-adveq
ffs="practicum2-adveq"
ffs=""
for ff in ${ffs}; do
	ltxf ${ff}
done

cd solutions/adveq
ffs="practicum2-adveq-solutions"
ffs=""
for ff in ${ffs}; do
	ltxf ${ff}
done


