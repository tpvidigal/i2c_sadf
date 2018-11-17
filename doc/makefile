## I2C SADF Makefile ####################

filename=""

report : 
	$(MAKE) gen_pdf filename=report
	$(MAKE) clean filename=report


## AUXILIARY FUNCTIONS ##################

gen_pdf: 
	@pdflatex ${filename}.tex

clean:
	rm -f ${filename}.ps
	rm -f ${filename}.log
	rm -f ${filename}.aux
	rm -f ${filename}.out
	rm -f ${filename}.dvi
	rm -f ${filename}.bbl
	rm -f ${filename}.blg

clean_all:
	$(MAKE) clean filename=*
	rm -f *.pdf
