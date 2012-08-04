#lang racket

;;(display "In utils.rkt!\n")

(require scribble/base
         scriblib/autobib)


(provide ~cite IPS <> define-bib p+ XXX generate-bib long short abstract-only)

;;(define-syntax-rule (~cite what ...) "")


(define abstract-only (make-parameter #f))
(define-syntax-rule (long x ...) (unless (abstract-only) (list x ...)))
(define-syntax-rule (short x ...) (when (abstract-only) (list x ...)))

(define (IPS) "Interface-Passing Style")
(define (<> x) (tt "<" x ">"))

(define-cite ~cite cite-noun generate-bib)

(define-syntax-rule (define-bib name stuff ...)
  (define name (make-bib stuff ...)))

(define-syntax-rule (p+ x ...) (list x ...))

(define-syntax-rule (XXX x ...) '())
