MAIN_DOCUMENT?=main

export LATEX_MAIN_DOCUMENT=$(MAIN_DOCUMENT).tex

all: $(MAIN_DOCUMENT).tex */*.tex */*.bib
	./list_includes.sh --compare
	@pdflatex -shell-escape $(MAIN_DOCUMENT).tex
	@pdflatex -shell-escape $(MAIN_DOCUMENT).tex
	makeindex $(MAIN_DOCUMENT).nlo  -s nomencl.ist -o $(MAIN_DOCUMENT).nls
	bibtex $(MAIN_DOCUMENT)
	@pdflatex -shell-escape $(MAIN_DOCUMENT).tex
	pdflatex -shell-escape $(MAIN_DOCUMENT).tex
