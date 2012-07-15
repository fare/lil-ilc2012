all: ds.W

%.html: %.scrbl
	exscribe -I $${FARE}/fare/www -o $@ $<

%.ps: %.scrbl
	exscribe -I $${FARE}/fare/www -P -o $@ $<

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc
