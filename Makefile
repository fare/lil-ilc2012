LIL := lil-ilc2012
LILA := ${LIL}-abstract

LILSRC = ds.scrbl bibliography.scrbl utils.rkt

export PLTCOLLECTS:=$(shell pwd):${PLTCOLLECTS}

all: ${LIL}.pdf ${LIL}.PDF
abstract: ${LILA}.pdf ${LILA}.PDF

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	xpdf $<

%.pdf: %.scrbl ${LILSRC}
	scribble --dest-name $@ --pdf $<

%.html: %.scrbl ${LILSRC}
	scribble --dest-name $@ --html $<

%.latex: %.scrbl ${LILSRC}
	scribble --latex --dest tmp $<

clean:
	rm ${LIL}.pdf ${LIL}.html ${LILA}.pdf ${LILA}.html
	rm -rf tmp
