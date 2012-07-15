(style 'fare-style)

(define (IPS) "Interface-Passing Style")
(define (<> x) (tt "<" x ">"))

@document[
  :title @'{LIL: higher-order datastructures meet CLOS}
  :author (FRR)
]{
@abstract{
LIL, the Lisp Interface Library,
uses an explicit interface-passing style
to implement an algorithmic data-structure library.
By separating algorithmic information
from the concrete representation of data
and encapsulating it in first-class interface objects,
LIL simultaneously allows for parametric polymorphism
(abstracting over types, classes, functions, data)
and sharing of code and data fragments
through inheritance of CLOS mixins.
LIL provides both pure functional and stateful datastructure interfaces,
with functions to transform ones into the others.
Finally, judicious Lisp macros allow developers to avoid boilerplate and
to abstract away interface objects to expose classic-looking Lisp APIs.
}
@apply*[bibliography
'((misc Rideau-IPS
   (title "Interface-passing style")
   (author "François-René Rideau")
   (year "2010")
   (url "http://fare.livejournal.com/155094.html"))
  (misc LIL
   (title "lisp-interface-library")
   (author "François-René Rideau and Eric O'Connor")
   (year "2010-2012")
   (url "http://github.com/fare/lisp-interface-library/"))
  (article Implementing-Type-Classes
   (title "Implementing Type Classes")
   (author "John Peterson and Mark Jones")
   (year "1993")))
]
@section[:title @'{Introduction}]{
@p+{
In dynamically typed languages such as Common Lisp or Python,
programmers usually rely on ad-hoc polymorphism
to provide a uniform interface to multiple kinds of situations:
a same function can accept arguments of any type thanks to dynamic typing,
then dispatch on the type of these arguments to select an appropriate behavior.
Object-oriented programming via user-defined classes or prototypes
may then provide extension mechanisms
by which new types of objects may be specified that fit existing interfaces;
this extension can be incremental through the use of inheritance
in a class (or prototype) hierarchy.
More advanced object systems such as the Common Lisp Object System (CLOS)
have further mechanisms to make inheritance more expressive, such as
mixins (multiple inheritance) and method combinations,
that allow for a more decentralized specification of program behavior.

In statically typed languages such as OCaml or Haskell,
a given function requires its arguments to have fixed types.
While sum types allow to a point for dispatch of algorithmic behavior
depending on the provided arguments,
using such dispatch to conflate conceptually different entities
increases local code complexity, is limited in power,
cumbersome to the user, and not extensible for the developer.
To write generic algorithms applicable to a large range of situations,
programmers instead rely on parametric polymorphism,
using higher-order types to abstract over
types and statically associated functions.
This importantly allows for composable datastructure building-blocks,
"functors" (in ML) or "classes" (in Haskell).
Their composability allows for very elegant designs,
and makes it easier to reason about programs in modular ways;
it also enables the bootstrapping of more elaborate versions
of a same datastructure interface type from simpler components.

Of course, in the past
there have been static languages with ad-hoc polymorphism
(such as the original C++),
and dynamic languages with parametric polymorphism
(such as units in PLT Scheme).
In a more recent past, many languages,
usually static languages (C++, OCaml, Haskell, Scala, etc.),
have offered some combination of these two forms of polymorphism,
with varied results.
In this paper, we present LIL, the Lisp Interface Library,
which brings parametric polymorphism to Common Lisp
and allows ad-hoc polymorphism and parametric polymorphism
to complement together in specifying abstract algorithms
in the context of a dynamic programming language.

In a first part, we describe
the @[IPS] @ref[:bib "Rideau-IPS"] in which LIL is written:
meta-data about the current algorithm is encapsulated
in a first-class interface object,
and this object is then explicitly passed around
in computations that may require specialization based on it.
We describe syntactic extensions provided
to make the interface-passing implicit rather than explicit,
and show how some things are easier one way or the other.

In a second part, we demonstrate how we use this style to implement
a library of classic datastructures, both pure and stateful.
We show how our library makes good use of @[IPS]
to build up interesting datastructures:
ad-hoc polymorphism allows us to share code fragments through mixins
various tree implementations can share most of their code
yet differ where it matters;
parametric polymorphism allows the composition of datastructures
and the bootstrapping of more efficient ones
from simpler but less efficient variants;
first-class interfaces allow a very same object
as implementing a given type of interface in different ways.
Finally, we show how adequate macros allow to bridge between
pure and stateful variants of our datastructures without
requiring the programmer to introduce a lot of boilerplate.

We conclude with a short statement about how @[IPS] in Lisp
relates to idioms in other programming languages:
how it compares to existing mechanisms for polymorphism in these languages
or to their underlying implementation;
how it could be applied in some languages and with what limitations.
}}
@section[:title @'{Interface-Passing Style}]{
@subsection[:title @'{Simple Interfaces}]{
@p+{
Examples: @<>{eq}, @<>{hashable}
}}
@subsection[:title @'{Parametric Interfaces}]{
@p+{Examples: @<>{alist}

:parametric - automatic function. Memoization.

:singleton - automatic special variable.
}}
@subsection[:title @'{Making Interfaces Implicit}]{
@p+{
with-interface
}}}
@section[:title @'{Classic Data-Structure}]{
@subsection[:title @'{Tree Mixins}]{
@p+{
Power of CLOS:
From naive binary trees to balanced binary trees in one method.
}}
@subsection[:title @'{Bootstrapping Datastructures}]{
@p+{
Power of Parametric Composition:
pure hash-tables bootstrapped from pure trees of hash buckets and pure alists as buckets.

Example from Okasaki.
}}
@subsection[:title @'{Same Data, Multiple Interfaces}]{
@p+{
Implicit in the above bootstrapping.

Multiply-indexed tuple store.
}}
@subsection[:title @'{From Pure to Stateful and Back}]{
@p+{
put pure state in mutable box.

put mutable object in use-once box.

Provide all wrappers for all the methods via macros.
}}}
@section[:title @'{Conclusion}]{
@p+{
@[IPS] is an effective tool with which to write software libraries.
However, the underlying principle is hardly original,
as @[IPS] is typically how existing languages with parametric polymorphism
have implicitly implemented this polymorphism for decades:
for instance, that is how
Haskell implements Type Classes @[ref :bib "Implementing-Type-Classes"],
PLT Scheme implements Units@[XXX 'ref :bib], and
ML implements functors@[XXX 'ref :bib].

However, there are some minor innovations in our library.
First, instead of hiding the interfaces behind a language abstraction,
we embraced the opening up of the implementation details
and deferred any hiding of interfaces to later facilities.
On the one hand makes, this is
a rather low-level way of achieving parametric polymorphism,
and is more cumbersome to use than methods that abstract interfaces away.
On the other hand, it gives programmers more control and
makes it easy to leverage the power of CLOS,
whereas it is always possible to use Lisp macros to
build higher-level abstractions as additional layers on top of this mechanism.

@XXX{
Haskell has the advantage that it will automatically infer the type class for you and pass it around as an implicit argument. In a dynamic language like Common Lisp, you have to explicitly pass the interface around, which on the one hand is cumbersome, but on the other, gives you more control: you can express parametric polymorphism, existential quantification, dependent types, and multiple ways to view a very same data structure as being an instance of the same interface protocol, which is especially great when bootstrapping elaborate versions of a protocol from simpler versions of the same (as is done with hash-tables). Of course, using Haskell, you could also go from one point of view to the other by wrapping objects inside view-specific constructors; but then, if your algorithm switches point of view constantly as for a Necker Cube, you may waste a lot of space and possibly leak as you keep creating new wrapped objects; whereas with a first-class interface separate from the data, you can just switch interface without consing.
}

}}
@section[:number #f :title @'{Bibliography}]{
@font[:size -1 (print-bibliography :all #t)]}
}

#|
The call for paper is here:
http://international-lisp-conference.org/2012/call-for-papers.html

   Important Dates:

     Please send contributions before the submission deadline, including
     abstracts of 4 pages for technical papers and abstracts of 2 pages
     for all other categories.

     Deadline for abstract submissions: July 15, 2012
     Notification of acceptance or rejection: July 31, 2012
     Deadline for final paper submissions: August 31, 2012

A complete technical paper is up to 15 pages and must describe original results.

Another category we might vie for is Demonstrations: Abstracts of up
to 2 pages for demonstrations of tools, libraries and applications.

|#
