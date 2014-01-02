LIL := lil-ilc2012

LILSRC = ds.scrbl bibliography.scrbl utils.rkt

export PLTCOLLECTS:=$(shell pwd):${PLTCOLLECTS}

all: long-slides # asdf # PDF
html: ${LIL}.html
pdf: ${LIL}.pdf
PDF: pdf ${LIL}.PDF

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	xpdf -z width -aa yes $<

%.pdf: %.scrbl ${LILSRC}
	scribble --dest-name $@ --pdf $<

${LIL}.html: ${LIL}-html.scrbl ${LILSRC}
	scribble --dest-name $@ --html $<

%.latex: %.scrbl ${LILSRC}
	scribble --latex --dest tmp $<

clean:
	rm -f ${LIL}.pdf ${LIL}.html *.css *.js
	rm -rf tmp

mrproper:
	git clean -xfd

rsync: html pdf
	rsync -av ${LIL}.html ${LIL}.pdf common-lisp.net:~frideau/public_html/lil-ilc2012/

slides: lil-slides.rkt utils.rkt
	racket $<

long-slides: lil-slides-long.rkt utils.rkt
	racket $<

asdf: asdf.scrbl utils.rkt
	racket $<
