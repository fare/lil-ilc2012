#lang at-exp racket ;;-*- Scheme -*-
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

(define slides
  (make-keyword-procedure
   (lambda (kws kvs repeats . lines)
     (for ([i repeats])
       (let ((m (λ (xs) (map (λ (x) (x i)) xs))))
	 (keyword-apply
	  slide kws (m kvs) (m lines)))))))

(define (always x) (lambda (i) x))
(define (repeat-fun test n iftesttrue [iftestfalse ~] [ifnorepeat ~])
  (lambda (i)
    (cond
     ((not i) ifnorepeat)
     ((test i n) iftesttrue)
     (#t iftestfalse))))
(define (if= n x [y ~] [z ~]) (repeat-fun = n x y z))
(define (if<= n x [y ~] [z ~]) (lambda (i) (if (<= i n) x y)))
(define (if>= n x [y ~] [z ~]) (lambda (i) (if (>= i n) x y)))

(define (?highlight n m object [shaded grey] [highlit red] [normal identity])
  (if n
      (if (eqv? n m)
	  (highlit object)
	  (shaded object))
      (normal object)))

(define (tLIL)
  (title "Lisp Interface Library"))

(define (cover n [text ~] [text2 ~])
  (slide #:title (?highlight n 0 (tLIL) identity)
    (?highlight n 1 (bt "CLOS reaches higher-order,"))
    (?highlight n 2 (bt "sheds identity,"))
    (?highlight n 3 (bt "and has a transformative experience"))
    ~ text text2))

(define (cover2 n text text2)
  (cover n text ~)
  (cover n text text2))

;#;
(begin
(cover #f (t "François-René Rideau <tunes@google.com> — ILC 2012"))
#| Hi.
My name is François-René Rideau.
I am a Lisp hacker at ITA Software, now part of Google.

Today I'll be telling you about the work I did on
LIL, the Lisp Interface Library
that I presented at ILC 2012 —
the International Lisp Conference

I am honored to be in Kyoto today
at this 2012 International Lisp Conference
to present the work I did on
LIL, the Lisp Interface Library.

So what is LIL? 
|#

(cover2 0
 (t "Data Structure Library for Common Lisp")
 (t "in Interface-Passing Style (IPS)"))
#|
As the name says, it's a Data Structure Library for Common Lisp.
More than that, I claim that it is THE most fun way
to develop and use Data Structures, in Common Lisp, and in many languages.

What's so special about LIL?
|#
#|
It is written in Interface-Passing Style,
which I abbreviate as IPS,
a style of programming I came up with two years ago,
where, as the name implies, you pass around interfaces.

And what does that change?
|#

(cover2 1
 (t "Parametric Polymorphism")
 (t "married with Ad hoc Polymorphism"))
#|
@(IPS) allows for Parametric Polymorphism,
which was not previously available in Common Lisp.
This means that you can design not just data structures,
but higher-order data structures:
that is, functions — or "functors" as they are called —
that take data structures or functors
— or types, numbers or arbitrary objects —
as arguments, and build new more elaborate data structures, etc.

If you have ever used ML, OCaml or Haskell,
or maybe done advanced work with
C++ templates, Java Generics, F# or Scala,
you know what a joy it is to be able to use Parametric Polymorphism
when developing generic, reusable, libraries.
|#
#|
However, what is important, as we achieve this Parametric Polymorphism,
we integrate very well with the Ad Hoc Polymorphism provided by
CLOS, the Common Lisp Object System.
We can take advantage of the many features that decades later
still make it a bleeding edge system for Ad Hoc Polymorphism:
multiple dispatch, multiple inheritance, method combination,
meta-object protocol.

These features combined allow for nicely incremental, decentralized
specification of software behavior.

[The concepts of Ad-hoc polymorphism vs Parametric polymorphism
have been formalized by Christopher Strachey in his seminal 1967 article.]

And so that's why LIL is about CLOS reaching higher-order:
we marry CLOS's Ad hoc Polymorphism with Parametric Polymorphism
previously not available to CLOS.
|#

(cover2 2
 (t "Pure (and Stateful) Data Structures")
 (t "Ad hoc Polymorphism without State"))
#|
Thanks to this power,
I have built a library of Data Structures, both Pure and Stateful.

I mean pure, as in pure functional languages:
without side-effects,
where each value is immutable and therefore without identity,
persistent in preserving its structure unmodified forever,
till the Great Garbage Collector does us part,
while new values come into the spotlight
through the declarative exploration of updated variants of previous values.
This contrasts with Stateful Data Structures,
where each ephemeral object maintains a transient State
that potentially changes at every operation
through imperative side-effects
that irreversibly modify its intimate structure,
its only constant being its identity.

Using @IPS, I demonstrate once again (if anyone doubted it)
how the panoply of features of Ad Hoc Polymorphism,
far from being tied to stateful objects with identity,
applies very well to empowering the development of
pure functional computing without identity.
|#
#|
More than that, the very interfaces in @IPS themselves
are fundamentally instances of Ad hoc Polymorphism without State.

So is @IPS just a small change of point of view
on well-known programming techniques?
Yes, but it's a change of point of view that enables cool new things.
Which brings me to...
|#
(cover2 3
 (t "Automatic Transformations between Styles")
 (t "thanks to Linear Logic")))
#|
The transformative experience.

Thanks to @IPS,
I could easily achieve in practice
what so far as I know
has only been done in theory:
the automatic transformation of software
and between Pure Programming Style and Stateful Programming Style.
While I was at it, and to prove @IPS is directly applicable
to existing programs written in traditional Object-Oriented style,
I also implemented an automatic transformation
from @IPS to traditional Object-Oriented Style.
So you can write in @IPS things that would be extremely painful
to write without Parametric Polymorphism in traditional OOP,
yet be safe that your code is not lost
to people who'd refuse to use @IPS but would demand traditional OOP.

There was a secret technique thanks to which I could write these transformation,
and this secret is...
|#
#|
Linear Logic.

Linear Logic is
the idea that you can model computation as formal transformations
that are mindful that entities in general are resources
that are consumed as we use or abuse them,
and that we must explicitly duplicate
if we want to be able to use more than one copy.

Thanks to Linear Logic, we can view these various programming styles
as but different encodings of the very same programs,
and write the transformations.
The interfaces of @IPS happen to be the perfect entities
to encapsulate the programs that are to be transformed.
|#

(slide #:title (title "Interface Passing Style")
       ~ @t{(a.k.a. IPS)} ~)
#|
So let's start with Interface Passing Style,
also known as IPS.
Just like Object-Oriented Programming is OOP.
|#

(slide #:title (title "Interface Passing Style")
  (t "Let's lookup a map")
  ~
  (code (lookup map key)))
#|
|#

(slide #:title (title "Interface Passing Style")
  (t "But is it an alist? a hash-table? A binary tree?")
  ~
  (code (lookup map key)))
#|
|#

(slide #:title (title "Interface Passing Style")
  (t "But is it an alist? a hash-table? A binary tree?")
  (t "FP: have the correct function in scope")
  (code (_lookup map key)))
#|
The function itself is monomorphic.
To manipulate different data structures,
you'd need different functions, each with a different name.
For the expression to be polymorphic,
some outer mechanism for polymorphism would arrange to bind
the correct function to that name before entering the scope.
Pushing polymorphism back to whoever binds the function.
For instance, using the equivalent of PLT units.
|#

(slide #:title (title "Interface Passing Style")
  (t "But is it an alist? a hash-table? A binary tree?")
  (t "OOP: dispatch on object meta-data")
  (code (lookup _map key)))
#|
|#

(slide #:title (title "Interface Passing Style")
  (t "But is it an alist? a hash-table? A binary tree?")
  (t "IPS: One little extra argument for the meta-data")
  (code (lookup _<interface> map key)))

(slide #:title (title "Interface Passing Style")
  ~
  ~
  (code (lookup <alist> map key)))

(slide #:title (title "Interface Passing Style")
  ~
  ~
  (code (lookup <hash-table> map key)))

(slide #:title (title "Interface Passing Style")
  ~
  ~
  (code (lookup (<avl-tree> <french-string>) map key)))

(slide #:title (title "Interface Passing Style")
  ~
  ~
  (code (lookup (<alist> <equal>) map key)))



#|
|#

#|

|#
#|
|#
#|
|#
#|
|#
#|
|#
#|
|#
#|
|#
#|
|#
#|
|#

(slide #:title @title{Questions?}
  (url "http://github.com/fare/lisp-interface-library")
  (url "http://github.com/fare/lil-ilc2012")
  (code (quicklisp:quickload :lil))
  (t "Have fun!"))
#|
You can find the source code and my paper on github.

LIL is also available via quicklisp.

If I only had only thing to say about LIL,
it's that programming with it is fun!

If you need to use pure or stateful data structures,
I enjoin you to come use those available in LIL;
and if any you need are missing,
we can have fun together porting them to this framework.

Thank you for listening to my talk.

Any questions?
|#

;;------>8------>8------>8------>8------>8------>8------>8------>8------>8------
#|
Ad hoc polymorphism is not just for imperative objects
Parametric polymorphism is not just for pure computations
linear logic unifies pure and stateful.
It's more than time for a Linear Lisp — any taker?

It's important to distinguish data and meta-data
you can join them as convenient - but just THINKING about them separately
is a conceptual step that simplifies many things,
and avoids the horrific complications when you need to formalize their types.

Exposing the innards of a programming language feature is a rich device.
If you language feature doesn't have clean explainable semantics
without introducing such innards, then it is the wrong primitive,
and founding your language on it is an abstraction inversion.
Make the innards the primitive.
|#
