#lang at-exp racket

(require scribble/base scribble/manual scriblib/autobib "utils.rkt")

(provide (all-defined-out))

(define-bib Rideau-IPS
  #:title "Interface-passing style"
  #:author "François-René Rideau"
  #:date 2010
  #:url "http://fare.livejournal.com/155094.html")

(define-bib LIL2012
  #:title "lisp-interface-library"
  #:author "François-René Rideau" ;; and Eric O'Connor ??
  #:date 2012
  #:url "http://github.com/fare/lisp-interface-library/")

(define-bib Implementing-Type-Classes
  #:title "Implementing Type Classes"
  #:author "John Peterson and Mark Jones"
  #:date 1993
  #:url "http://web.cecs.pdx.edu/~mpj/pubs/pldi93.html")

(define-bib Units-Flatt-Felleisen
  #:title "Units: Cool Modules for HOT Languages"
  #:author "Matthew Flatt and Matthias Felleisen"
  #:location (proceedings-location "PLDI 98")
  #:date 1998
  #:url "http://www.ccs.neu.edu/scheme/pubs/pldi98-ff.ps.gz")

(define-bib MOOPUM
  #:title "Modular Object-Oriented Programming with Units and Mixins"
  #:author "Robert Bruce Findler and Matthew Flatt"
  #:location (proceedings-location "ICFP 98")
  #:date 1998
  #:url "http://www.ccs.neu.edu/scheme/pubs/icfp98-ff.pdf")

(define-bib Okasaki
  #:title "Purely Functional Data Structures"
  #:author "Chris Okasaki"
  #:date "1996"
  #:url "http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf")

(define-bib DSST-Persistent
  #:title "Making Data Structures Persistent"
  #:author "J. R. Driscoll, N. Sarnak, D. D. Sleator and R. E. Tarjan"
  #:date 1989
  #:location (journal-location "Journal of Computer and System Sciences, Vol. 38, No. 1")
  #:url "http://www.cs.cmu.edu/~sleator/papers/Persistence.htm")

#|(define-bib Baker-TreeShadow
  #:title "Worlds in Collision: A Mostly Functional Model of Concurrency Control and Recovery"
  #:date 1990
  #:url "http://home.pipeline.com/~hbaker1/TreeShadow.html")|#

(define-bib Baker-ShallowArrays
  #:title "Shallow Binding Makes Functional Arrays Fast"
  #:date 1991 ; 1990-1991
  #:url "http://home.pipeline.com/~hbaker1/ShallowArrays.html")

(define-bib cl-containers
  #:title "cl-containers"
  #:author "Gary King and contributors" ; and...
  #:date 2005 ; 2005-2011
  #:url "http://common-lisp.net/project/cl-containers/")

@(XXX #|
See discussion on 2012-08-03 on #racket
<asumu> I don't think this combination is used much.
<asumu> Partly, I think, because units are a fairly heavyweight feature.
<asumu> (to clarify: mixins & units *are* used together, but not in this particular pattern)
<asumu> (see the DrRacket tool API for an example of their use)
|#)
