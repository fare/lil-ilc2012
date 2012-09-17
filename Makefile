LIL := lil-ilc2012
LILA := ${LIL}-abstract

LILSRC = ds.scrbl bibliography.scrbl utils.rkt

export PLTCOLLECTS:=$(shell pwd):${PLTCOLLECTS}

all: pdf PDF
html: ${LIL}.html # ${LILA}.html
pdf: ${LIL}.pdf
PDF: ${LIL}.PDF
abstract: ${LILA}.pdf ${LILA}.html ${LILA}.PDF

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	xpdf -z width -aa yes $<

%.pdf: %.scrbl ${LILSRC}
	scribble --dest-name $@ --pdf $<

%.html: %.scrbl ${LILSRC}
	scribble --dest-name $@ --html $<

%.latex: %.scrbl ${LILSRC}
	scribble --latex --dest tmp $<

clean:
	rm ${LIL}.pdf ${LIL}.html ${LILA}.pdf ${LILA}.html
	rm -rf tmp
