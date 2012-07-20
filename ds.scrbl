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
@subsection[:title @'{Using Interfaces}]{
@p+{
}}
@subsection[:title @'{Simple Interfaces}]{
@p+{
Example: @<>{eq}, interface for objects with an equality predicate.
Algorithms that depend on that interface (or any interface that inherits from it)
may rely on the existing of a method for gf test-function (interface x y)
on a suitable class of objects that will be used by the algorithm.
Test-function defaults to eql.
We could have been a default to undefined,
but we prefer usable defaults, which fits better with Lisp programming style.
}}
@subsection[:title @'{Interface Inheritance}]{
@p+{
Example: @<>{hashable}, inherits from @<>{eq},
clients may assume a method on gf hash (interface x);
servers must provide such a method.
There again, we default to sxhash (which matches equal, not eql).
@<>{equal}, inherits from both @<>{eq} and @<>{hashable},
uses equal for its test-function.
}}
@subsection[:title @'{Parametric Interfaces}]{
@p+{
Example: @<>{alist}. Takes an @<>{eq} interface as parameter.

Define-interface extension option :parametric automatically generates
a function to create such interface objects.
This function further uses memoization so interfaces with identical parameters
end up being the same interface rather than new objects every time.

Define-interface extension option :singleton relies on the previous
(a trivial version of which is assumed if not present)
to automatic a special variable.
Therefore clients can use the symbol @<>{alist}
to refer to the one such interface, instead of
having to either create a new instance every time with (make-instance '<alist>)
or to call function (<alist>) with the default test function.
}}}
@section[:title @'{Classic Data-Structure}]{
@subsection[:title @'{Mixins}]{
@p+{
Power of CLOS:
From naive binary trees to balanced binary trees in one method
(plus a little bit of boilerplate).
}}
@subsection[:title @'{Bootstrapping Datastructures}]{
@p+{
Power of Parametric Composition:
pure hash-tables bootstrapped from pure trees of hash buckets and pure alists as buckets.

Example of bootstrapped datastructure from Okasaki. TBD.
}}
@subsection[:title @'{Same Data, Multiple Interfaces}]{
@p+{
Implicit in the above bootstrapping.

Multiply-indexed-organized tuple store.
Each node is simultaneously node of multiple trees.
Depending on which index you're using,
they have to be viewed as a tree in the according way,
and appropriate accessors have to be chosen for say subtree access.
The same tree-manipulation routines can be used on the same tree nodes
with completely different results depending on which interface you use.
DISCLAIMER: example TBD as of 20120715.

Because the interface is not tied to the data, the data can remain unchanged
while the interface changes.
In some algorithms, you might want to constantly switch
your point of view on the same data, as for a Necker Cube.
When using classes to control behavior, this requires
constant rewriting of the object,
either by global rewriting
or by rewrapping and unwrapping.
If creating new datastructures with these objects,
this can even leak memory
(or some kind of memoization must be done,
either through a table which is somewhat slow,
or by adding an ad-hoc memoization field to relevant classes,
neither of which is nice if you wanted to preserve purity from side-effects).
First class interfaces separate behavior from representation
and avoid this issue.
DISCLAIMER: example TBD as of 20120715.
}}}
@section[:title @'{Interface Transformations}]{
@subsection[:title @'{Making Interfaces Implicit}]{
@p+{
Local bindings with
(with-interface-methods (interface &key methods prefix) &body body).
DISCLAIMER: macros TBD as of 20120715.

Global definitions with
(define-interface-methods interface &key methods prefix package).
DISCLAIMER: macros TBD as of 20120715.

TBD: from implicit interfaces back to explicit ones?
}}
@subsection[:title @'{From Pure to Stateful and Back}]{
@p+{
Put pure datum in a mutable box.

Put mutable object in use-once box.

Provide wrappers for all relevant methods via macros.
Also need to identify for every method
which position in argument and/or return values
holds the object or datum to wrap/unwrap.
DISCLAIMER: macros TBD as of 20120715.
}}
@subsection[:title @'{From Interfaces to Classes and Back}]{
@p+{
An interface can be viewed as "detached" class information,
where an object's "virtual method table" is passed
as an explicit "self" argument.
From any object-oriented API
(set of classes and generic functions that dispatch on objects),
you can therefore mechanically derive an interface-passing API
(set of interfaces and generic functions that dispatch on interfaces),
simply by passing around the object itself as the interface
that drives the dispatch.
DISCLAIMER: macros TBD as of 20120715.

Conversely, you can view classes as "subjective" interfaces,
where no explicit state object is passed, but rather
where any state has been moved inside the interface itself.
Once you build an elaborate interface API
by composing several parametric interfaces,
you can obtain a class-style API by having a class
derive from your interface
(but specifying the :allocation :class option for its slots)
and your interface's object class
(with :allocation :instance for its slots),
and by automatically deriving subjective variants
of the interface-passing style generic functions
and appropriate wrappers.
DISCLAIMER: macros TBD as of 20120715.
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
and is more cumbersome to use than methods that abstract interfaces away;
for instance, in static languages such as Haskell or ML,
type inference allows the language to
automatically pick the right interface at every call site,
instead of the user having to explicitly pass interfaces around.
On the other hand, it gives programmers more control
(a same datum can be seen through multiple interfaces;
your interfaces can depend on first-class data, not just second-class types and functions)
and makes it easy to leverage the power of CLOS,
whereas it is always possible to use Lisp macros to
build higher-level abstractions as additional layers on top of this mechanism.

cl-containers: mixins and find-or-create-class.
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

     Deadline for abstract submissions: August 5, 2012 (was July 15, 2012)
     Notification of acceptance or rejection: August 25, 2012 (was July 31, 2012)
     Deadline for final paper submissions: September 25, 2012 (was August 31, 2012)

A complete technical paper is up to 15 pages and must describe original results.
|#
