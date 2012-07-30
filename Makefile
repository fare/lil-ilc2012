LIL=lil-ilc2012

all: ${LIL}.PDF

#all: ds.W
#
#%.html: %.scrbl
#	exscribe -I $${FARE}/fare/www -o $@ $<
#
#%.pdf: %.scrbl
#	exscribe -I $${FARE}/fare/www -P -o $@ $<
#

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	xpdf $<

${LIL}.pdf: *.scrbl utils.rkt
	PLTCOLLECTS=`pwd`:${PLTCOLLECTS} \
	scribble --dest-name $@ --pdf ds.scrbl \

${LIL}.html: *.scrbl utils.rkt
	PLTCOLLECTS=`pwd`:${PLTCOLLECTS} \
	scribble --dest-name $@ --html ds.scrbl \

clean:
	rm lil-ilc2012.pdf

latex:
	PLTCOLLECTS=`pwd`:${PLTCOLLECTS} \
	scribble --latex --dest tmp ds.scrbl
