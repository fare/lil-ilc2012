#lang at-exp slideshow ;;-*- Scheme -*-
(require slideshow
         slideshow/code
         scheme/gui/base
         "utils.rkt")

(define ~ @t{ })
(define *blue* (make-object color% "blue"))
(define *red* (make-object color% "red"))
(define *grey* (make-object color% 200 200 200))
(define (url x) (colorize (tt x) *blue*))
(define (red x) (colorize x *red*))
(define (grey x) (colorize x *grey*))

(define (title x) (text x (cons 'bold 'default) 38))

(slide #:title "Toward a Linear Lisp"
   @bt{A Lisp based on Linear Logic}
   ~ ~
   (para #:align 'center @t{François-René Rideau,} @it{Google})
   @t{ILC'2012}
   #;@url{http://fare.tunes.org/computing/asdf.scrbl})

(slide #:title "Linearity in Commutative Algebra of Numbers"
@t{Linear in A and B:}
(code (* A B)) ~
@t{Linear in A, B, nonlinear in C:}
(code (+ (* A B C) (* (- A) B C C))) ~
@t{Linear in A, affine in B:}
(code (+ (* A B) (* (- A) 2))))

(slide #:title "Linearity, Algebra of Program Terms"
@t{Linear in A and B:}
(code (cons A B)) ~
@t{Linear in A, B, nonlinear in C:}
(code (if A (* B C) (* B C C))) ~
@t{Linear in A, affine in B:}
(code (if A B 2)))

(slide #:title "Linear Logic"
@t{An linear object is referenced exactly once}~ ~
@t{If you're manipulating one, you're it} ~ ~
@t{You might duplicate objects})

(slide #:title "Explicit Resource Management"
@t{creation:}
(code(let ((x (create))) ... x ...)) ~
@t{duplication:}
(code(call/dup x (λ (x1 x2) (* x1 x2)))) ~
@t{destruction:}
(code(destroy x)))

(slide #:title "Explicit Nonlinearity"
@t{"Exponential", in persistent heap:}
(code (! A)) ~
@t{Handle is still linear value}
(code (let ((H (! A))) (... B ...))) ~
@t{Dereference as often as you like with $}
(code (call/dup H (λ (H1 H2) ... ($ H1) ... ($ H2) ...))))

(slide #:title "Model of resource ownership"
@t{Resource: Memory buffers, OS handles} ~ ~
@t{Owner: Frame, Thread} ~ ~
@t{Dynamic stack extent})

(slide #:title "Safe low-level programming"
@t{No resource leak} ~ ~
@t{Trivial GC, Mutual Exclusion} ~ ~
@t{Linear continuations are cheap})

(slide #:title "Power of classical Lisp"
@t{Implicit heap-allocation, duplication} ~ ~
@t{GC, synchronization rightside up} ~ ~ ;; Building Linear on Top of Classical is an abstraction inversion
@t{Scheme: nonlinear continuations})

(slide #:title "Linear Lisp"
@t{Henry Baker: "Look Ma, No Garbage", etc.} ~ ~
@t{See also Henry Baker at ILC 2005} ~ ~ ;; http://international-lisp-conference.org/2005/speakers.html#henry_baker
@t{See Alan Bawden's thesis})

(slide #:title "Syntactic extensions"
@t{Non-interfering Observation:}
(code (if (observe-only A) (use A) (also-use A))) ~
@t{Affine: implicit drop}
(code (when A B)) ~
@t{Unify Pure and Stateful}
@t{Automatic style transformation})
