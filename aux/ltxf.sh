#!/bin/bash

ff=$1
cp ${ff}.tex tmp.tex
pdflatex --interaction=nonstopmode tmp.tex > /dev/null 2>&1
if [[ ! -f tmp.pdf ]]; then exit 1; fi
pdflatex --interaction=nonstopmode tmp.tex > /dev/null 2>&1
pdflatex --interaction=nonstopmode tmp.tex
mv tmp.pdf ${ff}.pdf
echo -e "\nCreated ${ff}.pdf\n"
rm -f tmp.aux tmp.log tmp.nav tmp.out tmp.snm tmp.toc tmp.tex
