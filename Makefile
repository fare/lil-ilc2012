LIL := lil-ilc2012

LILSRC = ${LIL}.scrbl utils.rkt

all: ${LIL}.PDF

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	xpdf $<

${LIL}.pdf: ${LILSRC}
	PLTCOLLECTS=`pwd`:${PLTCOLLECTS} \
	scribble --dest-name $@ --pdf ${LIL}.scrbl

${LIL}.html: ${LILSRC}
	PLTCOLLECTS=`pwd`:${PLTCOLLECTS} \
	scribble --dest-name $@ --html $<

latex: ${LILSRC}
	PLTCOLLECTS=`pwd`:${PLTCOLLECTS} \
	scribble --latex --dest tmp $<

clean:
	rm ${LIL}.pdf ${LIL}.html
	rm -rf tmp
