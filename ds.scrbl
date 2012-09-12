#lang sigalternate @nocopyright

@;;(display "In ds!\n")

@(require scribble/base scribble/manual
          scriblib/autobib scriblib/footnote
          (only-in scribble/core style)
          "utils.rkt" "bibliography.scrbl")

@(provide doc)

@title{LIL: higher-order datastructures meet CLOS}

@authorinfo["François-René Rideau" "Google" "tunes@google.com"]
@;@authorinfo["Eric O'Connor" "Mercer University" "oconnore@gmail.com"]

@conferenceinfo["ILC 2012" "October 26--27, Kyoto, Japan."]
@copyrightyear{2012}
@;@copyrightdata{123-4-5678-9012-3/45/67}

@abstract{
LIL, the Lisp Interface Library,
uses an explicit interface-passing style
to implement an algorithmic datastructure library.
By separating algorithmic information
from the concrete representation of data
and encapsulating it in first-class interface objects,
LIL simultaneously allows for both parametric polymorphism
(abstracting over types, classes, functions, data)
and ad-hoc polymorphism
(sharing of code and data fragments through CLOS inheritance and mixins).
LIL provides interfaces to both
pure functional (persistent) and stateful (ephemeral) datastructure,
with a common fragment for read-only methods.
Based on metadata describing side-effects of methods,
macros can transform pure datastructures into stateful interfaces
and the other way around,
automatically wrapping methods with proper boxing and linearization.
Finally, judicious Lisp macros allow developers to avoid boilerplate and
to abstract away interface objects to expose classic-looking Lisp APIs.
}

@section{Introduction}

In dynamically typed languages such as @[CL] or Python
(but also in some statically typed languages like the original C++),
programmers usually rely on ad-hoc polymorphism
to provide a uniform interface to multiple kinds of situations:
a same function can accept arguments of many types thanks to dynamic typing,
then dispatch on the type of these arguments to select an appropriate behavior.
Object-oriented programming via user-defined classes or prototypes
may then provide extension mechanisms
by which new types of objects may be specified that fit existing interfaces;
this extension can be incremental through the use of inheritance
in a class (or prototype) hierarchy.
More advanced object systems such as the @[CL] Object System (CLOS)
have further mechanisms such as multiple inheritance,
multiple dispatch, and method combinations,
that allow for a more decentralized specification of program behavior.

In statically typed languages such as ML or Haskell
(but also in some dynamically typed languages such as
PLT Scheme with its units @~cite[Units-Flatt-Felleisen]),
programmers usually rely on parametric polymorphism
to write generic algorithms applicable to a large range of situations:
algorithmic units be parameterized with types, functions
and other similar algorithmic units.
These units can then be composed, allowing for elegant designs
that make it easier to reason about programs in modular ways;
the composition also enables the bootstrapping of more elaborate versions
of a same datastructure interface type from simpler components.

In the past, many languages,
usually statically typed languages (C++, OCaml, Haskell, Java, Scala, etc.),
but also dynamically typed languages (PLT Scheme@~cite[MOOPUM]),
have offered some combination of both
ad-hoc polymorphism and parametric polymorphism,
with a variety of results.
In this paper, we present LIL, the Lisp Interface Library@~cite[LIL],
which brings parametric polymorphism to @[CL],
in a way that nicely fits into the language
and its existing ad-hoc polymorphism,
taking full advantage of the advanced features of CLOS.

In a first part, we describe
the @[IPS] @~cite[Rideau-IPS] in which LIL is written:
meta-data about the current algorithm is encapsulated
in a first-class interface object,
and this object is then explicitly passed around
in computations that may require specialization based on it.
We show basic mechanisms by which this make it possible
to express both ad-hoc and parametric polymorphism.

In a second part, we demonstrate how we use this style to implement
a library of classic datastructures, both pure (persistent) and stateful (ephemeral).
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

In a third part, we show how adequate macros allow to bridge
between different programming styles.
We demonstrate macros that bridge between
syntactically implicit or explicit interfaces.
We demonstrate macros that bridge between
pure functional and stateful datastructures
based on metadata describing effects of methods
according to a simple model of such effects.
We demonstrate macros that bridge between
"detached" interfaces and "subjective" objects.
All these macros allow programmers to choose a programming style
that best fit the problem at hand and their own tastes
while still enjoying the full benefits of @[IPS] libraries.

We conclude with a short statement about how @[IPS] in Lisp
relates to idioms in other programming languages:
how it compares to existing mechanisms for polymorphism in these languages
or to their underlying implementation;
how it could be applied in some languages and with what limitations.

@section{Interface-Passing Style}
@subsection{Using Interfaces: An Overview}

@subsubsection{Interface 101: An Extra Argument}

From the point the user of a library written in @[IPS],
interfaces are just one extra argument (more rarely, two or more)
passed as the first argument (or arguments) to appropriate function calls.
Each such interface argument provides these functions with
some contextual information about
which specific variant of some algorithm or datastructure is being used.

@subsubsection{Trivial Example: Finite Maps}

Our library as it is mainly implements finite maps.
Assuming you have an object @cl{map}
that according to an interface @cl{<i>} maps keys to values,
you can lookup to what value a given key is mapped (say, string @cl{"some key"})
with:
@;
@clcode{(lookup <i> map "some key")}
@;
and you can insert a new mapping of key to value into said map
(say, as a key the string @cl{"other key"} and as a value the number @cl{42})
as follows:
@;
@clcode{(insert <i> map "other key" 42)}

Depending on what interface @cl{<i>} you specified,
these generic functions (in the sense of CLOS)
will select an algorithm and appropriately query or update
the datastructure bound to variable @cl{map},
returning the expected results.

@subsubsection{Pure vs Stateful Interfaces}

Which results are expected from a function call
of course may vary with the interface.

For instance, the simplest such interface is @<>{alist},
which like the name implies stores maps as association lists
in usual @[CL] tradition:
a list of pairs (@cl{cons} cells),
containing a key (as the cell's @cl{car})
and a value (as the cell's @cl{cdr}).
The interface is pure, meaning that the maps it manipulates
are never modified in place, but
new lists and association pairs are created as required.
In particular, this means that the function @cl{insert}
when applied to an object with an @<>{alist} interface
will return a new alist object. e.g.
@;
@clcode{
(insert <alist>
  '((conference . "ILC") (year . 2010))
  'year 2012)}
@;
will return a new alist
@clcode{((conference . "ILC") (year . 2012))}
without modifying any previous data cell.

If instead of alists,
we were using the interface @cl{stateful:<hash-table>}
and a hash-table object,
the function @cl{insert} would have returned no values,
instead modifying the existing hash-table in place.

Because @cl{insert} means something quite different
for pure and stateful datastructures, with incompatible invariants,
our library actually defines two different generic functions,
@cl{pure:insert} and @cl{stateful:insert},
each in its own package.
(Packages are the standard first-class namespace mechanism of @[CL].)

By contrast, there is only one lookup function,
@cl{interface:lookup}
which is shared by a the pure and stateful datastructures
and imported in both the @cl{pure} and @cl{impure} packages.
In both cases, returns two values:
the value associated to the given key if a mapping was found,
and a boolean that it true iff a mapping was found.

@subsubsection{Variable Interfaces}

Now, the interface doesn't have to be a compile-time constant;
you can pass interfaces around as first-class objects,
write functions that abstract over interface objects,
create new interface objects, etc.
By abstracting over the interface object,
accepting it as an argument and passing it to other functions,
you can write algorithms that are independent of the specifics
of the underlying datastructure.

For instance, you can write functions that fold over a map (reduce it)
without knowing how the map is implemented internally
(or indeed whether it's pure or stateful, computing serially or in parallel),
and be able to apply such functions to a wide variety of situations,
in each of which the map will be implemented in a way suitable optimized
to the context at hand:
@clcode{
(defmethod sum-all-values ((<i> pure:<map>) map)
  (multiple-value-bind (m1 m2) (pure:divide <i> map)
    ;; we rely on a promised invariant of pure:divide
    (cond
      ((empty-p <i> m1) ; m1 and m2 both are empty
       0)
      ((empty-p <i> m2) ; m1 has only one mapping
       (nth-value 1 (first-key-value <i> m1)))
      (t
       (+ (sum-all-values <i> m1)
          (sum-all-values <i> m2))))))}

The method above abstracts over interface @cl{<i>},
and will work with all pure map interfaces implementing
the @cl{pure:divide} API function.
This is a standardized function in our library,
whereby a map @cl{map} is divided in two submaps @cl{m1} and @cl{m2},
with the promised invariant that these submaps will each have strictly
fewer mappings than the original map, unless said map
has zero or one mapping, in which case @cl{m2} is empty,
and @cl{m1} is the same as @cl{map}.

@subsubsection{Caveat}

@[IPS] is somewhat low-level, in that
the user is given both full control and full responsibility
with respect to passing around appropriate interfaces.
If the user fails to ensure consistency
between the interfaces being used and datastructures being passed as arguments,
unspecified behavior may ensue
(usually resulting in a runtime error at some point),
as generic functions may or may not check their arguments for consistency.
On the other hand, the user may enjoy
the ability to explicitly specify an interface
in some uncommon cases where the "canonical" interface isn't what he wants,
or where there isn't any "canonical" interface to begin with.

@subsection{Simple Interfaces}

Example: @<>{eq}, interface for objects with an equality predicate.
Algorithms that depend on that interface (or any interface that inherits from it)
may rely on the existing of a method for @[gf] @cl{test-function (interface x y)}
on a suitable class of objects that will be used by the algorithm.
@cl{test-function} defaults to @cl{eql},
the equality comparison function always used as a default in @[CL].
We could have decided not to define a default,
but we prefer usable defaults, which better fits with @[CL] programming style.

Also, because CLOS has multiple dispatch,
our generic functions can dispatch on more than the first argument,
thus preserving the language's object-oriented style
on arguments beyond the initial interface argument.
In a language with single-dispatch, we couldn't do that,
at least not directly,
as dispatching on the interface would use up the object-oriented ability
to specialize behavior depending on arguments.

@subsection{Interface Inheritance}

Example: @<>{hashable}, inherits from @<>{eq},
clients may assume a method on @[gf] @cl{hash (interface x)};
servers must provide such a method.

@<>{equal} inherits from both @<>{eq} and @<>{hashable},
uses @cl{equal} for its @cl{test-function}
and @cl{sxhash} as its @cl{hash} function.

New interfaces can be defined with macro @cl{define-interface}.
It is an extension of the standard @[CL] macro @cl{defclass},
with various new options,
some of which we will describe in this article.

@subsection{Parametric Interfaces}

@subsubsection{Blah blah}

If instead of using alists, you were using some kind of balanced binary tree,
ordered as per @cl{string<} or as per some Unicode collating sequence,
you would pass the appropriate interface instead of @<>{alist}.



Example: @<>{alist}. Takes an @<>{eq} interface as parameter.

Note that as a syntactic convention,
we often use angle brackets around the names of interface classes
(such as class @<>{alist}),
of functions returning interfaces
(such as function @<>{alist}
taking an optional parameter to specify which interface to use
for equality),
or of variables bound to a singleton interface object
(such as variable @<>{alist},
bound to an object of class @<>{alist}
using the default equality interface @<>{eq}).

@cl{define-interface} extension option
@cl{:parametric} automatically generates
such a function to create such interface objects.
This function further uses memoization so interfaces with identical parameters
end up being the same interface rather than new objects every time.

@cl{define-interface} extension option @cl{:singleton}
relies on the previous
(a trivial version of which is assumed if not present)
to automatically define such a special variable.
Therefore clients can use the symbol @<>{alist}
to refer to the one such interface,
instead of having to either create a new instance every time
with @cl{(make-instance '<alist>)}
or to call function @cl{(<alist>)} with the default equality interface @<>{eq}.

@section{Classic Data-Structure}
@subsection{Mixins}

Power of CLOS:
From naive binary trees to balanced binary trees in one method
(plus a little bit of boilerplate).

@subsection{Bootstrapping Datastructures}

Power of Parametric Composition:
pure hash-tables bootstrapped from pure trees of hash buckets and pure alists as buckets.

Example TBD of bootstrapped datastructure from Okasaki.

@subsection{Same Data, Multiple Interfaces}

Implicit in the above bootstrapping.

Multiply-indexed-organized tuple store.
Each node is simultaneously node of multiple trees.
Depending on which index you're using,
they have to be viewed as a tree in the according way,
and appropriate accessors have to be chosen for say subtree access.
The same tree-manipulation routines can be used on the same tree nodes
with completely different results depending on which interface you use.
DISCLAIMER: example TBD.

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
DISCLAIMER: example TBD.

@section{Interface Transformations}
@subsection{Making Interfaces Implicit or Explicit}

Local bindings with
@clcode{
with-interface (interface functions-spec
                &key prefix package) &body body
}

Global definitions with
@clcode{
define-interface-specialized-functions
  interface functions-spec &key prefix package
}

Define methods with
@clcode{
(define-interface-methods (i <myinterface>)
  (:method ...) ...)
}

Example TBD of how to build an implicit interface from explicit functions.

@subsection{From Pure to Stateful and Back}

Put pure datum in a mutable box. DONE.

Put mutable object in use-once box. DONE.

Provide wrappers for all relevant methods via macros.
Also need to identify for every method
which position in argument and/or return values
holds the object or datum to wrap/unwrap.

@subsection{From Interfaces to Classes and Back}

An interface can be viewed as "detached" class information,
where an object's "virtual method table" is passed
as an explicit "self" argument.
From any object-oriented API
(set of classes and generic functions that dispatch on objects),
you can therefore mechanically derive an interface-passing API
(set of interfaces and generic functions that dispatch on interfaces),
simply by passing around the object itself as the interface
that drives the dispatch.
DISCLAIMER: macros TBD.

Conversely, you can view classes as "subjective" interfaces,
where no explicit state object is passed, but rather
where any state has been moved inside the interface itself.
Once you build an elaborate interface API
by composing several parametric interfaces,
you can obtain a class-style API by having a class
derive from your interface
(but specifying the @cl{:allocation :class} option for its slots)
and your interface's object class
(with @cl{:allocation :instance} for its slots),
and by automatically deriving subjective variants
of the interface-passing style generic functions
and appropriate wrappers.
DISCLAIMER: macros TBD.

@section{Conclusion}

@subsection{Related Works}

While the underlying principle behind @[IPS] is hardly original,
we found it an effective tool to implement a generic datastructure library,
and a particularly good fit to @[CL]
thanks to the way we can leverage CLOS and macros.

As for its lack of originality,
@[IPS] is typically how existing implementations
of languages with parametric polymorphism
have implicitly implemented this feature for decades, under the hood.
For instance, that is how
Haskell implements Type Classes @~cite[Implementing-Type-Classes],
PLT Scheme implements Units @~cite[Units-Flatt-Felleisen], and
ML implements functors@[XXX 'ref :bib]:
an extra interface argument is implicitly passed around
to the lower-level functions implementing these various constructs,
and this extra argument encapsulates the parameters to said constructs.

However, there are several ways
in which our @[IPS] differ from any of the above-mentioned systems;
these ways, some of them innovative, are all related to our embracing
the powers and limitations of @[CL] in implementing parametric polymorphism:
@itemlist[
 @item{
   @emph{We do not rely on static information},
   either purely syntactic (via scoping as in PLT)
   or somewhat semantic (via type inference as in Haskell),
   to statically resolve interfaces,
   or otherwise hide them behind a language abstraction.
   Instead, we embrace the dynamic nature of @[CL]
   and let interfaces be first-class objects
   that may be determined at runtime at any call site.
   This can be viewed either as
   a restriction on the capabilities of our technique, or as
   the absence of a restriction on its applicability.}
 @item{
   @emph{Interface arguments are explicit passed around rather than implicitly}.
   We embrace the opening up of what in other systems is an implementation detail.
   This gives our library a low-level flavor of control and responsibility;
   while the responsibility is indeed sometimes burdensome,
   we can take advantage of that control to access
   a same datastructure through multiple interfaces.}
 @item{
   @emph{Our interfaces can be parameterized by arbitrary first-class data}.
   The parameters are not constrained to be second-class entities
   to allow for termination of a type inference algorithm.
   This does raise the difficulty for authors of compilers to optimize our code,
   or for authors of proof systems to accommodate the complexity.}
 @item{
   @emph{We make it easy for users to hide these interfaces in usual cases}
   thanks to @[CL] macros,
   with facilities both syntactic (such as @cl{with-interface})
   and semantic (such as our macros TBD to go from interfaces to classes).
   In common cases, we can therefore eschew the burden
   of explicitly passing around interface objects.}
 @item{
   @emph{We support ad-hoc polymorphism by explicitly dispatching on interface arguments}.
   These interfaces need not be uniform dictionaries
   (like the implicit arguments in the respective implementations
   of the above-mentioned systems),
   but can objects of arbitrary user-defined classes,
   subject to the usual object-oriented dispatch techniques.}
 @item{
   @emph{Our ad-hoc polymorphism is scoped outside of parameters, not inside}.
   This lambda lifting of interface objects matters a lot for @[CL],
   for @[CL] doesn't have first-class class combinators
   or cheap portable anonymous classes,
   but instead has a global public namespace
   that favors dynamic linking of new methods to existing generic functions
   and dynamic instantiation of new interface objects with runtime parameters.
   Note that this starkly constrasts with classes inside parameterized units,
   as done in the PLT unit article @~cite[MOOPUM],
   where parameterized class are statically linked and strict scoped
   via an assemblage of units;
   though the PLT approach allows for dynamic instantiation of unit assemblages
   with runtime parameters, any such assemblage is semantically sealed
   and unextensible after instantiation.
   Once again, the @[CL] approach has a lower-level feel overall.
   }
 @item{
   @emph{We rely on multiple-dispatch to
   not sacrifice object-oriented style when using interface dispatch}.
   In a language with single-dispatch,
   dispatch on our explicit interface argument will use up
   the programmer's ability to rely on ad-hoc polymorphism
   to express his algorithms.
   In LIL, we leverage the multimethod capabilities of CLOS
   to dispatch on our interface objects and
   still be able to dispatch on further arguments
   as per normal object-oriented style.}
]

Our solution would fit any other dynamic language,
especially if it also has multiple dispatch and/or syntax extension facilities.

Thanks to @[CL] syntax extension, we could also achieve
a few interesting features
beside the addition of parametric polymorphism to @[CL]:

@itemlist[
 @item{
   Our library provides both pure and stateful datastructures,
   that share a common interface for read-only methods.}
 @item{
   Macros make interfaces implicit again in the usual cases.}
 @item{
   For the sake of the above, we associate @[gfs] to interfaces.}
 @item{
   Additionally, we annotate @[gfs] with trivial metadata about their side-effects.}
 @item{
   Based on such metadata, macros to automate bridging between
   pure (persistent) and corresponding stateful (ephemeral) datastructure.}
]

@subsection{Current Limitations and Future Work}

LIL at this point is already a usable datastructure library
that has contributed features not previously available to @[CL] users:
not only does it offer infrastructure for users to develop their own
parametrically polymorphic datastructures,
it sports a generic map interface with pure and stateful variants,
and implementations as balanced binary trees, hash-tables or patricia trees.

Yet, in many ways, LIL is still in its early stages;
at the current moment it is a usable proof-of-concept
rather than full-fledged library.
It sports as few usable features as necessary to illustrate its concepts,
and each of its features is as bare as possible while remaining functional.
There are thus many axes for development,
both in terms of actually provided algorithms
and in terms of linguistic abstraction.

Obviously, more known datastructures could be provided:
Stacks, queues, double-queues, arrays, heaps, sets, multi-sets,
both pure and stateful,
could be fit in the existing framework.
We notably intend to port the algorithms from
Chris Okasaki's now classic book@~cite[Okasaki]
and other currently popular pure functional datastructures;
and of course matching interfaces to well-known stateful variants.

Just as obviously, our linguistic features could offer more bells and whistles:
users could have more flexibility in
mapping names, parameters and other aspects of their interfaces
when translating between variants of algorithms,
pure and stateful, interface-based and class-based.
These transformations could be more mindful of interface and class hierarchies
rather than operate on all the generic functions of one pair of APIs at a time.
The packaging of the current features could be improved,
with internals being refactored and exported.

However, here are a few less-obvious ways in which we'd like to improve LIL.

Firstly, we'd like to explore how algorithms can be developed in terms
of combining small individual features, each embodied in an interface mixin:
controlling whether any given property is implemented
as a slot or a user-defined method;
controlling whether some data is indirectly accessible through a box
or inlined in the current object;
combining multiple datastructures to achieve better access guarantees
(i.e. records are both nodes of a hash-table for constant-time lookup
and of a doubly-linked list for preserving insertion order),
or implementing the same datastructure twice with a different view
(i.e. records are part of several trees that index several fields
or computed expressions from fields).
Mixins would be combined in the style of
@tt{cl-containers}@~cite[cl-containers]
and its macro @cl{find-or-create-class},
which implements first-class class combination
on top of @[CL]'s second-class class object-system
but first-class reflection.
Some protocol would manage the several classes of objects
associated to an interface, and combine them all (or the relevant subset)
with additional mixins when such mixins are specified;
this would also be used extracting "classified" APIs from @[IPS] APIs.
Possibly, we could also provide some way for abstract interfaces
to provide a default concrete implementations;
thus, in simple cases one could obtain a full implementation
just by specifying the high-level properties of the desired algorithm,
yet in more complex cases, manual specialization would be possible.

Second, we'd like to explore how both pure and stateful variants
of some algorithms can be extracted from a single specification:
the specification would essentially combine a pure node-building algorithm
with annotations on which object identities are to be preserved
between original and new objects.
The pure variant would just create new values and drop the identities.
A stateful variant would clobber old identities
using change-class on previous nodes.
New variants could purely pass around or statefully effect
an additional explicit store object.
The main ambition though is that a single specification should simply
make all the variants possible, so that each user may
fine-tuning which variant makes sense for him
while being able to share his algorithms with other users that
need the "same" algorithm viewed from a totally different angle.

Third, and relatedly, we'd like to explore how to improve
the so far trivial language by which we currently express "effects"
of API functions.
The current first-order specifications can probably be generalized
into some kind of higher-order type system.
Presumably, API transformations could be automatically extracted
from expressions in that more elaborate effect specification language.
Possibly, simple API implementations themselves
could in some cases be automatically extracted
from the API specification itself.
If the specifications also include annotations about performance guarantees,
this opens a venue for a more declarative approach
to datastructure development.

The ultimate goal we would be reaching for
is that it should be possible to write programs
out of small individual contributions,
each written in the style its author considers simplest
to express what he means.
These contributions should be automatically aligned
along a common semantic framework thanks to declarative specifications
of the style in which they are intended
(some more constrained bits of code can be viewed in many ways).
And it should thereafter be possible to seamlessly combine
these contributions into a common result,
made available to the user according to whichever point of view
best suits his needs.

@(generate-bib)

@;@section[#:style (style #f '(hidden unnumbered))]{}

@;@bold{Acknowledgment:} Thanks to ...


@XXX{
The call for paper is here:
http://international-lisp-conference.org/2012/call-for-papers.html

   Important Dates:

     Deadline for final paper submissions: September 25, 2012 (was August 31, 2012)

A complete technical paper is up to 15 pages and must describe original results.

}
