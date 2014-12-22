FILENAME = BClib_manual

.PHONY: clean  pdf

pdf:
	pdflatex  $(FILENAME)

all:
	pdflatex  $(FILENAME)
	pdflatex  $(FILENAME)
	bibtex    $(FILENAME)
	pdflatex  $(FILENAME)
	pdflatex  $(FILENAME)

clean:
	@-rm -f *~
	@ rm -f   *.aux  *.bbl  *.blg  *.log  *.out  *.toc

full_clean:
	make  clean
	rm  -r  -f   $(FILENAME).pdf
