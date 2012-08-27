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
  #:date 1993)

(define-bib Units-Flatt-Felleisen
  #:title "Units: Cool Modules for HOT Languages"
  #:author "Matthew Flatt and Matthias Felleisen"
  #:location (proceedings-location "PLDI 98")
  #:date 1998)

@(define-bib MOOPUM
  #:title "Modular Object-Oriented Programming with Units and Mixins"
  #:author "Robert Bruce Findler and Matthew Flatt"
  #:location (proceedings-location "ICFP 98")
  #:date 1998)

@(XXX #|
See discussion on 2012-08-03 on #racket
<asumu> I don't think this combination is used much.
<asumu> Partly, I think, because units are a fairly heavyweight feature.
<asumu> (to clarify: mixins & units *are* used together, but not in this particular pattern)
<asumu> (see the DrRacket tool API for an example of their use)

http://www.cs.cmu.edu/~sleator/papers/Persistence.htm
Making Data Structures Persistent
J. R. Driscoll, N. Sarnak, D. D. Sleator, R. E. Tarjan, Making Data Structures Persistent, Journal of Computer and System Sciences, Vol. 38, No. 1, 1989

Henry Baker
Shallow binding makes functional arrays fast


pure vs stateful
persistent vs ephemeral   ==> persist vs ephemeralize ?

|#)
