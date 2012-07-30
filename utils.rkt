#lang at-exp racket

(require scribble/base
         scribble/manual
         scribble/bnf
         scriblib/autobib
         (for-syntax syntax/parse))

(provide ~cite IPS <> define-bib p+ XXX generate-bib)

;;(define-syntax-rule (~cite what ...) "")

(define (IPS) "Interface-Passing Style")
(define (<> x) (tt "<" x ">"))

(define-cite ~cite cite-noun generate-bib)

(define-syntax-rule (define-bib name stuff ...)
  (define name (make-bib stuff ...)))

(define-syntax-rule (p+ x ...) (list x ...))

(define-syntax-rule (XXX x ...) '())
