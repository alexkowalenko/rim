# make rimref

all : rimref24.pdf rimint24.pdf

rimref24.pdf : rimref24.tex 
	lualatex rimref24 
	makeindex rimref24
	lualatex rimref24 

rimint24.pdf : rimint24.tex
	lualatex rimint24

clean : 
	rm -f rimref24.pdf rimint24.pdf
	rm -f *.log *.idx *.out *.toc *.aux *.lot *.lof *.ilg *.ind
