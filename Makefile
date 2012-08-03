LIL=lil-ilc2012

all: ${LIL}.PDF

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	xpdf $<

${LIL}.pdf: *.scrbl utils.rkt
	PLTCOLLECTS=`pwd`:${PLTCOLLECTS} \
	scribble --dest-name $@ --pdf ds.scrbl

${LIL}.html: *.scrbl utils.rkt
	PLTCOLLECTS=`pwd`:${PLTCOLLECTS} \
	scribble --dest-name $@ --html ds.scrbl

clean:
	rm lil-ilc2012.pdf

latex:
	PLTCOLLECTS=`pwd`:${PLTCOLLECTS} \
	scribble --latex --dest tmp ds.scrbl
