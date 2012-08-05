#lang at-exp racket
;;-*- Scheme -*-

;;(display "In utils.rkt!\n")

(require
  scribble/base
  scriblib/autobib
  scribble/manual
  scribble/bnf
  (for-syntax syntax/parse))

(provide
  XXX IPS CL gf gfs
  cl clcode clblock <>
  ~cite define-bib generate-bib
  long short abstract-only)

;;(define-syntax-rule (~cite what ...) "")


(define abstract-only (make-parameter #f))
(define-syntax-rule (long x ...) (unless (abstract-only) (list x ...)))
(define-syntax-rule (short x ...) (when (abstract-only) (list x ...)))

(define (IPS) "Interface-Passing Style")
(define (CL) "Common Lisp")
(define (gf) "generic function")
(define (gfs) "generic functions")

(define-syntax (clblock stx)
  (syntax-parse stx
    [(_ #:line-numbers ln str ...)
     #'@nested[#:style "smaller"]{
        @codeblock[;;#:keep-lang-line? #f
                   #:line-numbers ln
                   #:line-number-sep 3
                   str ...]}]
    [(_ str ...)
     #'(clblock #:line-numbers 0 str ...)]))

(define-syntax (clcode stx)
  (syntax-parse stx
    [(_ str ...) #'(clblock #:line-numbers #f str ...)]))

(define-syntax-rule (cl str ...)
  @code[#|#:lang "cl"|# str ...])

(define-syntax-rule (<> x) (cl "<" x ">"))

(define-cite ~cite cite-noun generate-bib)

(define-syntax-rule (define-bib name stuff ...)
  (define name (make-bib stuff ...)))

;;(define-syntax-rule (p+ x ...) (list x ...))

(define-syntax-rule (XXX x ...) '())
