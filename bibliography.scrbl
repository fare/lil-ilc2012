#lang at-exp racket

(require scribble/base scribble/manual scriblib/autobib "utils.rkt")

(provide (all-defined-out))

(define-bib Rideau-IPS
  #:title "Interface-passing style"
  #:author "François-René Rideau"
  #:date 2010
  #:url "http://fare.livejournal.com/155094.html")

(define-bib LIL
  #:title "lisp-interface-library"
  #:author "François-René Rideau and Eric O'Connor"
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
  #:date 1998)

(define-bib MOOPUM
  #:title "Modular Object-Oriented Programming with Units and Mixins"
  #:author "Robert Bruce Findler and Matthew Flatt"
  #:location (proceedings-location "ICFP 98")
  #:date 1998)

(define-bib Okasaki
  #:title "Purely Functional Data Structures"
  #:author "Chris Okasaki"
  #:url "http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf")

(define-bib DSST-Persistent
  #:title "Making Data Structures Persistent"
  #:author "J. R. Driscoll, N. Sarnak, D. D. Sleator and R. E. Tarjan"
  #:url "http://www.cs.cmu.edu/~sleator/papers/Persistence.htm"
  #:location (journal-location "Journal of Computer and System Sciences, Vol. 38, No. 1")
  #:date 1989)

#|(define-bib Baker-TreeShadow
  #:title "Worlds in Collision: A Mostly Functional Model of Concurrency Control and Recovery"
  #:url "http://home.pipeline.com/~hbaker1/TreeShadow.html"
  #:date 1990)|#

(define-bib Baker-ShallowArrays
  #:title "Shallow Binding Makes Functional Arrays Fast"
  #:url "http://home.pipeline.com/~hbaker1/ShallowArrays.html"
  #:date 1991) ; 1990-1991

(define-bib cl-containers
  #:title "cl-containers"
  #:author "Gary King and contributors" ; and...
  #:url "http://common-lisp.net/project/cl-containers/"
  #:date 2005) ; 2005-2011

@(XXX #|
See discussion on 2012-08-03 on #racket
<asumu> I don't think this combination is used much.
<asumu> Partly, I think, because units are a fairly heavyweight feature.
<asumu> (to clarify: mixins & units *are* used together, but not in this particular pattern)
<asumu> (see the DrRacket tool API for an example of their use)
|#)
