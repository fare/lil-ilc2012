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
LIL, the @[LIL],
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
In this paper, we present LIL, the @[LIL]@~cite[LIL2012],
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

We conclude about
how @[IPS] in Lisp relates to idioms in other programming languages
and compares to existing or potential mechanisms
for polymorphism in these languages or to their underlying implementation,
and what are the current limitations of our library and
our plans of future developments.


@section{Interface-Passing Style}
@subsection{Using Interfaces}

@subsubsection{Interface Passing: An Extra Argument}

For the user of a library written in @[IPS],
interfaces are just one extra argument (more rarely, two or more)
passed as the first argument (or arguments) to appropriate function calls.
Each such interface argument provides these functions with
some contextual information about
which specific variant of some algorithm or datastructure is being used.

As a syntactic convention followed by our library,
symbols that denote interface classes,
variables bound to interface objects,
or functions returning interface objects
will usually start and end with respective angle brackets @cl{<} and @cl{>}.
For instance, the interface to objects that may be empty is @<>{emptyable},
whereas a prototypical interface variable would be @<>{i}.

@subsubsection{Trivial Example: Maps}

The most developed API in our library currently deals with (finite) maps,
i.e. a finite set of mappings from keys to values.
Our examples will mainly draw from this API.
Maps notably include traditional Lisp alists (association lists) and hash-tables.

Thus, whereas a traditional object-oriented API might feature a function
@clcode{(lookup map key)}
that would dispatch on the class of the object @cl{map}
to determine how said map associates a value to the given @cl{key},
an interface-passing API will instead feature a function
@clcode{(lookup <i> map key)}
where information on which precise algorithm to use
is instead encapsulated in the extra argument @cl{<i>},
an interface.

You could thus lookup the year
in an alist of data about a conference with code such as:
@clcode{
(lookup <alist>
  '((name . "ILC") (year . 2010) (topic . "Lisp"))
  'year)}
In this case, the library-exported variable @cl{<alist>}
is bound to an interface for association lists.

Similarly, to insert a new key-value mapping in an existing map,
you would call a function
@clcode{(insert <i> map key value)}
and depending on what interface @cl{<i>} you specified,
the generic function (in the sense of CLOS)
would select an algorithm and appropriately update
the datastructure bound to variable @cl{map},
returning the expected results.

@subsubsection{Pure vs Stateful Interfaces}

Which effects a function call has and which results it returns
of course may vary with the interface as well as the function.

For instance, our simplest of interface, @cl{<alist>},
as the name implies, implements maps as association lists
in usual @[CL] tradition:
a list of pairs (@cl{cons} cells),
each specifying a key (as the cell's @cl{car})
and a value (as the cell's @cl{cdr}).
Our alist interface is pure,
meaning that the maps it manipulates are never modified in place, but
new lists and association pairs are created as required.
In particular, the function @cl{insert}
when applied to our @<>{alist} interface,
will return a new alist object:
@clcode{
(insert <alist>
  '((name . "ILC") (year . 2010) (topic . "Lisp"))
  'year 2012)}
will return
@clcode{((name . "ILC") (year . 2012) (topic . "Lisp"))}
or some equivalent alist,
without modifying any previous data cell,
instead reusing the unmodified cells as possible.

If instead of alists,
we had been using the interface @cl{<hash-table>}
and a hash-table object,
the function @cl{insert} would have returned no values,
instead modifying the existing hash-table in place.

Because @cl{insert} means something quite different
for pure and stateful datastructures, with incompatible invariants,
our library actually defines two different generic functions,
@cl{pure:insert} and @cl{stateful:insert},
each in its own package.
@note{@smaller{
Packages are the standard first-class namespace mechanism of @[CL].
The syntax for a symbol can either leave the package implicit,
or explicitly specify a package name as a prefix
followed by one or two colons and the symbol name.
}}

By contrast, there is only one function @cl{interface:lookup}
that is shared by all pure and stateful interfaces
and imported in both the @cl{pure} and @cl{stateful} packages.
Indeed, lookup has the same specification in both cases:
it takes an interface, a map and a key as parameters, and
it returns two values,
the value associated to the given key if a mapping was found,
and a boolean that it true iff a mapping was found.

@subsubsection{First-Class Interfaces}

Interfaces are first-class objects.
They don't have to be compile-time constants.
Functions can abstract over interface objects,
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
(defmethod sum-values ((<i> pure:<map>) map)
  (let ((submaps (divide/list <i> map)))
    ;; see promised invariant of divide/list
    (cond
      ((null submaps) ; no element
       0)
      ((null (rest submaps))
       ;; only one mapping, extract its value
       (nth-value 1 (first-key-value <i> map)))
      (t ;; general case: recurse and map-reduce
       (reduce #'+
         (mapcar (λ (m) (sum-values <i> m))
                 submaps))))))}

The method above abstracts over interface @cl{<i>},
and will work with all pure map interfaces implementing
the @cl{divide/list} signature function.
This is a standardized function in our library,
whereby a map @cl{map} is divided in a list of non-empty submaps
each with strictly fewer mappings than the original map,
unless said map has exactly one mapping, in which case
a singleton list containing that map.
The above method could be trivially parallelized by replacing
@cl{mapcar} and/or @cl{reduce} by parallelizing variants.

@subsubsection{Caveat: No Type Checking}

@[IPS] is somewhat low-level, in that
the user is given both full control and full responsibility
with respect to passing around appropriate interfaces.

The downside is that if the user fails to ensure consistency
between the interfaces being used and datastructures being passed as arguments,
unspecified behavior may ensue
(usually resulting in a runtime error at some point),
as generic functions may or may not check their arguments for consistency.
While our library does provide a function @cl{check-invariant}
as part of the signature of interface @<>{type},
most of the methods we provide do not call said function,
and instead trust the user to call as appropriate,
typically at the entry points of his code or while debugging.

The upside of this lack of automatic type-based interface control is that
the user can explicitly specify an interface
in some uncommon cases where the "canonical" interface
that could be deduced by type inference isn't what he wants,
and even in cases where
there isn't any such "canonical" interface to begin with.

@subsection{Defining Interfaces}

@subsubsection{define-interface}

Interfaces can be defined with the @cl{define-interface} macro,
which is an extension to @cl{defclass}
that handles several features specific to interfaces.

For instance, here is
a stripped-down excerpt from our library:

@clcode{
(define-interface <emptyable> (<type>) ()
  (:abstract)
  (:generic empty (<emptyable>)
   (:values object) (:out 0)
   (:documentation "Return an empty object"))
  (:generic empty-p (<emptyable> object)
   (:in 1) (:values boolean)
   (:documentation "Is object empty?")))}

It defines an interface @<>{emptyable},
the name of which is passed as first argument to the macro,
as in @cl{defclass}.

The second argument is a list specifying super-interfaces
from which to inherit behavior, as in @cl{defclass}.
In this case, there is one and only one super-interface, @<>{type}.
In our library, @<>{type} is an abstract interface
specifying that some datatype is associated to the interface.
@<>{emptyable} extends it with the notion that
elements of that type may be empty.

As in @cl{defclass}, the third argument is a list of slots.
A non-empty list of slots is how parametric polymorphism is achieved.
In this case, this list is empty, as
there are no parameters defined by this interface.

Finally, the @cl{define-interface} macro accepts a list of options,
just like @cl{defclass}. But it also accepts additional options
not part of the @cl{defclass} specification.
For instance, this interface uses the @cl{:abstract} option,
and should not be instantiated,
because it doesn't implement all its declared functions.

This interface also uses the @cl{:generic} option
to declare two @[gfs] that are part of the signature of the interface,
@cl{empty} and @cl{empty-p}.
Furthermore, with each function a return value convention may be defined
as well as a calling convention, and indeed are defined in this case.
The first function takes no argument beyond the interface;
the @cl{(values object)} specifies that
it returns one value named object, and the
@cl{(:out 0)} specifies that return argument in first position
(indexes are 0-based) is of the target type.
and returns one value, an object, of said type.
Similarly, the second function is a predicate,
takes one argument of the target type and returns one boolean value.

We will now go over each of these features in more detail.

@subsubsection{Inheritance of Interfaces}

Interfaces may inherit from other interfaces
and be organized in inheritance hierarchies.

For instance,
@<>{type} is an interface with an associated datatype.
@<>{eq}, is an interface that inherits from @<>{type},
for datatypes with an equality comparison predicate @cl{==}.
@<>{hashable}, is an interface that inherits from @<>{eq},
for datatypes with a function @cl{hash} such
that two equal values (as compared by @<>{==}) have the same hash.
@<>{equal} is an interface that inherits from @<>{hashable},
and implements equality with the standard @[CL] predicate @cl{equal}
and @cl{hash} with the standard @[CL] function @cl{sxhash}.
@<>{eql} is an interface that inherits from @<>{eq},
and implements equality with the standard @[CL] predicate @cl{eql};
since there is no standard @[CL] hash function that corresponds to it,
it doesn't inherit from @<>{hashable}.

@subsubsection{Multiple Inheritance of Interfaces}

Interfaces may inherit from any number of super-interfaces.
Indeed, our interfaces are CLOS classes,
and since CLOS supports multiple-inheritance,
they may easily inherit from multiple other such classes.
Interfaces may only inherit from other interfaces.
Internally they are instances of
the CLOS metaclass @cl{interface-class}.

As an example of multiple inheritance,
our @cl{pure:<tree>} map interface inherits from both
@cl{interface:<tree>}, an interface
specifying readonly signature functions on trees,
and @cl{pure:<map>}, an interface specifying signature functions for maps
with pure update as well as mere lookup.

@subsubsection{Interface Mixins}

Our library also relies on multiple-inheritance extensively
in the form of mixins:
small interface classes implement a small aspect of the interface.
Oftentimes, a mixin will be used to simply deduce
the implementation of some signature functions from other signature functions.
Depending on which signature functions are more "primitive" for a given concrete datastructure,
opposite mixins may be used that deduce some functions from the others or the other way around.

For instance, the @<>{eq} interface actually has two associated functions,
@cl{(== <i> x y)}  that compares two objects @cl{x} and @cl{y}, and equivalently
@cl{(eq-function <i>)} that returns a function object that may be passed
as an argument to various higher-order functions.
The mixin @<>{eq-from-==} will automatically deduce @cl{eq-function}
from @cl{==} while the converse deduction is provided by the mixin
@<>{eq-from-eq-function}.

@subsubsection{Parametric Interfaces}

Interfaces may be parameterized by other interfaces
as well as by any object.

For instance, consider the current definition of @<>{alist} in our library:

@clcode{
(define-interface <alist>
    (map-simple-empty map-simple-decons
     map-simple-update-key
     map-divide/list-from-divide
     map-simple-map/2 map-simple-join
     map-simple-join/list <map>)
  ((key-interface
    :initarg :key-interface
    :reader key-interface))
  (:parametric (&optional (eq <eql>))
     (make-interface :key-interface eq))
  (:singleton))}

The super-interface list contains several mixins
to deduce various methods from primitive methods,
together with the interface @<>{map} that provides the signature.

But most importantly,
The list of slots contains a single slot @cl{key-interface}.
Indeed, association lists crucially depend on an equality predicate
with which to compare keys when looking up a given key.
Our @<>{alist} interface therefore has this slot,
the value of which must inherit from @<>{eq},
that will specify how to compare keys.

Slot definitions such as these are how
we achieve parametric polymorphism in @[IPS]:
an interface class with such a slot get instantiated
as interface objects with a specific value in that slot,
and a method defined on this interface class
can extract the value in said slot as a parameter to its behavior.

Our definition of @<>{alist} also uses
two options recognized by @cl{define-interface}
that are not provided by @cl{defclass}:
@cl{:parametric} and @cl{:singleton}.

@subsubsection{Concrete Parametric Interfaces}

The @cl{:parametric} option automatically generates
a function to instantiate parameterized interface objects.
This function further uses memoization so interfaces with identical parameters
end up being the actual same interface object
rather than a new object every time.

In the above @<>{alist} example,
the function takes one optional parameter that defaults to @<>{eql}
(itself a variable bound to a singleton interface).
This means that if no @<>{eq} interface is specified,
we will follow the Lisp convention and tradition in providing
the @cl{eql} function as the default comparison function.
The body of the parametric function creates the interface object using
the locally defined function @cl{make-interface}
that handles memoization of an underlying CLOS @cl{make-instance}.

Our library implements datastructures more elaborate than alists.
For instance, you could use balanced binary tree, in which case
you would have to provide the tree interface with
a parameter @cl{key-interface} that inherits from @<>{order},
so that keys may be compared.
Thus,
@cl{(stateful:<avl-tree> <number>)}
will return an interface
that is ideal to maintain a sorted index of numbered records, whereas
@cl{(pure:<avl-tree> <string>)}
is suitable to build persistent dictionary structures.
However, if you want your dictionary not in ASCIIbetical order
but rather in a proper collating sequence for Japanese, you'll have to build
an interface @cl{<japanese-collation>} around a Unicode library,
and pass it as an argument to @cl{pure:<avl-tree>}, or
to a variant thereof that caches the collation key.

@subsubsection{Singleton Interface Variable}

The @cl{define-interface} extension option @cl{:singleton}
automatically defines a special variable bound
to a one instance of a concrete interface class.
If a @cl{:parametric} function if explicitly defined,
it will call this function with default values.
Otherwise, it automatically defines a trivial version of such a function.

Clients can therefore use the variable @<>{alist}
to refer to the one default such interface,
instead of having either to create a new instance every time
with @cl{(make-instance '<alist>)}
or to call function @cl{(<alist>)}
with the default equality interface @<>{eql}.

@subsubsection{Multiple Dispatch}

Because CLOS has multiple dispatch,
our generic functions can dispatch on more than the first argument,
thus preserving the language's object-oriented style
on arguments beyond the initial interface argument.
In a language with single-dispatch, we couldn't do that,
at least not directly,
as dispatching on the interface would use up the object-oriented ability
to specialize behavior depending on arguments.

As the simplest example, an interface @<>{empty-object}
could implement the @<>{emptyable} signature functions as follows,
given a class @cl{empty-object} for its empty objects:
@clcode{
(defmethod empty-p
    ((<i> <empty-object>) (x t))
  nil)
(defmethod empty-p
    ((<i> <empty-object>) (x empty-object))
  t)}
Non-empty objects would be matched by the first method,
while empty objects would be matched by the more specific second method.

More complex examples could involve more methods,
with bigger class hierarchies or dispatch on more than two arguments.

@subsubsection{Interface Signatures}

To each interface is attached a set of functions declared
as part of the interface's signature.
The functions from the interface's super-interfaces are inherited;
additional functions can be directly declared
using the @cl{:generic} option of @cl{declare-interface}.

Some interfaces, such as @<>{emptyable} above,
exist for the sole purpose of declaring such functions,
while leaving full freedom to sub-interfaces as to how to implement them.
That is why @<>{emptyable} was marked as @cl{:abstract}:
it is not meant to be instantiated,
only to be inherited from other interfaces,
and dispatched upon in some methods.

We saw that some abstract interfaces have the opposite purpose:
they implement one or several signature functions
in terms of other signature functions,
that may be more "primitive" in various concrete interfaces.

Finally, some interfaces do implement the complete declared signature,
either directly or through inheritance of appropriate mixins.
They are concrete interfaces meant to be instantiated,
such as @cl{pure:<alist>} above.
Instantiation usually happens through a function declared by
the @cl{:parametric} option
or a variable declared by the @cl{:singleton} option.
These options are incompatible with the @cl{:abstract} option.

The fact that some interfaces are concrete makes is one notable difference
between our interfaces and interfaces in other languages such as Java.
Another notable difference is that
interfaces are not to be implemented by a class of objects,
with the first argument to every function in the signature
being treated specially and
having to be of an object class implementing the interface.
Instead, our signature functions treat all arguments uniformly,
and none of these arguments have to be restricted to any particular class.
In particular, there is no problem whatsoever with having "binary methods";
no special status is required for "constructor" methods
that create an object of some target type
when there was no object yet on which to dispatch methods;
and there is no dilemma regarding contravariance or covariance of types
when inheriting from a parametric interface.
Note that many of these issues could be avoided or glossed over @[CL]
thanks to its multimethods and dynamic typing;
but the interface-based approach would solve these issues
even in a language without single dispatch and/or with static typing.
@XXX{
The functions that are part of an interfaces' signature
and the meta-information associated to these functions
will matter later on when we automatically transform interfaces.
For now, they may be considered as mostly documentation
that trivially expands into according @cl{(defgeneric ...)} statements.
}

@section{Revisiting Classic Structures}

@subsection{Pure and Stateful Datastructures}

@subsubsection{Pure, Stateful, their Intersection, and Beyond}

We built LIL, the @[LIL],
with the ambition that it should become
the definitive library for datastructures in @[CL].
While we initially chose @[IPS] to achieve parametric polymorphism,
previously unavailable in @[CL],
this style was also helpful to address other issues
in developing our library.

For instance, to be able to improve on all existing libraries,
we decided to provide both
pure functional (persistent) datastructures and
stateful (ephemeral) datastructures.
Furthermore, we decided to do the Right Thing™
by sharing as much as possible of the interface and implementation
between the two styles of datastructures,
with APIs so congruent with each other that it is possible to build
automated bridges between the two styles.

The interfaces in @[IPS] proved to be a great locus in which formalize both
the commonalities and divergences between pure and stateful datastructures.

@subsubsection{Common Interfaces: Read-Only Access}

The @cl{interface::<map>} interface declares functions
@cl{lookup}, @cl{first-key-value}, @cl{fold-left}, @cl{fold-right},
@cl{map-alist} that access an existing map in a read-only way.
It also declares a function @cl{alist-map}
that creates a complete map from an alist,
and is quite useful for initializing non-empty maps.
These functions are applicable to pure as well as to stateful maps.
There are also inherited functions such as @cl{check-invariant},
@cl{empty} and @cl{empty-p}.

Thus, it is possible to write generic read-only tests for map datastructures
that work for all map implementations, pure and stateful as well.
Indeed, LIL includes such a tests in its test suite.

@subsubsection{Interface Divergence: Update}

We mentioned how the two distinct functions @cl{pure:insert}
and @cl{stateful:insert} have different signatures as far as return values go.
The same difference exists between the
@cl{pure:drop} and @cl{stateful:drop} functions:
both have the same input signature@[linebreak]
@cl{(<map> map key)} @cl{(:in 1)}.@[linebreak]
But whereas the former has the output signature@[linebreak]
@cl{(:values map value foundp)} @cl{(:out 0)},@[linebreak]
the latter has the output signature@[linebreak]
@cl{(:values value foundp)} @cl{(:out t)}.@[linebreak]
This means that the pure function returns an updated version
of the original map datastructure as its first return value,
whereas the stateful function omits this return value and
instead side-effects the map passed as argument.

Other functions that update map datastructures
have similar differences in their signatures:
pure methods tend to return updated new maps as additional values,
whereas such return values are omitted by stateful methods
that instead update existing maps in place through side-effects.
@XXX{
Yet there is a sense in which these two functions do the same thing;
we'll see what that is in detail
when we discuss automated interface transformations
in the next section.
}

@subsubsection{Incremental Layers of Functionality}

We have strived to implement our datastructures
in small incremental layers.

For instance, here is the complete implementation
of stateful AVL (self-balanced) trees on top of previous layers:

@clcode{
(define-interface <avl-tree>
    (interface::<avl-tree>
     <heighted-binary-tree>
     <post-self-balanced-binary-tree>) ()
  (:abstract))

(defclass avl-tree-node
    (interface::avl-tree-node
     heighted-binary-tree-node) ())

(defmethod node-class ((i <avl-tree>))
  'avl-tree-node)

(defmethod balance-node ((i <avl-tree>)
	   		 (node empty-object))
  (values))

(defmethod balance-node ((i <avl-tree>)
	   		 (node avl-tree-node))
  (ecase (node-balance node)
    ((-1 0 1) ;; already balanced
     (update-height node))
    ((-2)
     (ecase (node-balance (left node))
       ((-1 0)
        (rotate-node-right node))
       ((1)
        (rotate-node-left (left node))
        (rotate-node-right node))))
    ((2)
     (ecase (node-balance (right node))
       ((-1)
        (rotate-node-right (right node))
        (rotate-node-left node))
       ((0 1)
        (rotate-node-left node))))))
}

The superclasses already handle the read-only aspect of avl-trees
(mostly invariant checking, in this case),
and the stateful aspects of maintaining the tree height
and having to rebalance after updates.
The only incremental code we need is the specification of
how to rebalance nodes.

This decomposition of interfaces into lots of incremental interfaces
makes for a very clean programming style where each interface
is a small mixin that is quite easy to understand.

Note however the current burden of having to explicitly maintain
two class hierarchies, one for the interfaces, and one for
each type of object that the interfaces may manipulate.
We have hopes to eliminate this boilerplate in some future,
by having @cl{define-interface} manage for each interface
such a set of object classes;
but we have no actual solution yet.

@subsection{Interface Tricks and Puns}

@subsubsection{Dispatch without Object}

A clear disadvantage of @[IPS],
as compared to means by which other languages achieve similar expression,
is that it imposes upon the user the cost of keeping track of
the interface objects that are passed around.
But as a tradeoff, there are some advantages to balance the equation.
We are going to enumerate a few of these advantages,
from the most trivial to the most advanced.

First, interfaces as a separate detached argument allows
to manipulate data where there is no object to dispatch on.
This is very clear in the case of the @cl{pure:<alist>} interface:
it operates on primitive entities (@cl{cons} cells, @cl{nil})
without requiring these entities to be full-fledged objects,
and without requiring the user to retroactively add
a superclass to these entities for dispatch purposes.
(Note however that normal CLOS multiple dispatch
also works without this limitation.)
This is also clear for constructor functions,
where no object exists yet on which to dispatch.

@subsubsection{Bootstrapping Datastructures}

A second advantage of explicit interfaces is that
different variants of a same interface can apply to the same object.
You can see an array as a sequence of elements one way, or the reverse way;
no need to actually reverse the elements in memory,
just look through the lens of a reversing interface.

One way that we put this technique to profit on LIL is
in how we bootstrap pure hash-tables.

The construction is of our @cl{pure:<hash-table>}
is relatively straightforward:
from a slow generic map of keys to values
(generic meaning that keys can be anything),
and a fast specialized map of key hashes to bucket
(specialized meaning that keys are integers),
we bootstrap a fast generic map of keys to values.
By default our slow generic map is @cl{(<alist> <equal>)}
and our fast specialized map is @cl{(<avl-tree> <number>)},
under the nickname @cl{<number-map>};
but these parameters are under user control.
@note{
@smaller{
A hash-table is generic implementation of a finite map with fast access time,
supposing the existence of a hash function (typically from keys to integers)
as well as an equality predicate.
The hash function, the value of which hopefully can be quickly computed,
will hopefully distinguish with high probability any two objects
that may be used as keys in the map.
The location of the entry mapping the key to its value can then be
quickly computed from the key hash;
in case several keys collide (have the same hash),
some compensation strategy is used,
such as putting all those keys in the same bucket,
or using an alternate key.
In the stateful case, the key-indexed table is typically implemented as
a random-access array with @math{O(1)} access time.
In the pure case, the key-indexed table will typically be
a balanced binary tree, which has slightly worse
@math{O(log n)} access time but allows for persistent datastructures
(i.e. old copies are still valid after update).

The @[CL] standard specifies a class @cl{hash-table},
but this only provides a stateful variant of hash-tables.
LIL has an interface @cl{stateful:<hash-table>}
that matches the @cl{stateful:<map>} API
while using those standard hash-tables underneath,
but also needed a @cl{pure:<hash-table>}
as a generic pure map mechanism.
}}

Our @cl{pure:<hash-table>} is defined parametrically as follows:
@clcode{
(define-interface <hash-table>
    (map-simple-join map-simple-update-key
     map-simple-map/2 <map>)
  ((key-interface
    :reader key-interface
    :initarg :key)
   (hashmap-interface
    :reader hashmap-interface
    :initarg :hashmap)
   (bucketmap-interface
    :reader bucketmap-interface
    :initarg :bucketmap))
  (:parametric
       (&key (key <equal>)
             (hashmap <number-map>)
             (bucketmap (<alist> key)))
     (make-interface :key key
       :hashmap hashmap :bucketmap bucketmap))
  (:singleton)
  (:documentation "pure hash table"))
}

Methods are then straightforward, but notice the pun:
@clcode{
(defmethod insert
    ((<i> <hash-table>) map key value)
  (let ((hash (hash (key-interface <i>) key)))
    (insert
     (hashmap-interface <i>) map hash
     (insert
      (bucketmap-interface <i>)
      (multiple-value-bind (bucket foundp)
           (lookup (hashmap-interface <i>)
	           map hash)
         (if foundp
	     bucket
	     (empty (bucketmap-interface <i>))))
      key
      value))))
}
Indeed the very same object @cl{map}
is passed as an argument to
the same function @cl{pure:insert} through no fewer than
three different @cl{pure:<map>} interfaces:
first the original @cl{<i>} which is a @cl{pure:<hash-table>};
then the outer @cl{(hashmap-interface <i>)},
which is presumably a @cl{pure:<number-map>},
to locate the proper hash bucket (if any);
and finally the inner @cl{(bucketmap-interface <i>)}
which is presumably a @cl{pure:<alist>},
once the proper bucket has been found or a new empty one created.

We don't need to wrap and unwrap objects
to see them through a different view point;
we can simply change the interface,
and the same object can be punned into meaning usefully different things.

In the future, we would like to bootstrap more datastructures this way,
for instance following some of the algorithms documented by Chris Okasaki
in @~cite[Okasaki].

@subsubsection{Same Data, Multiple Interfaces}

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
Also, provisions for safe concurrent access to datastructures
could be introduced.

Just as obviously, our linguistic features could offer more bells and whistles:
users could have more flexibility in
mapping names, parameters and other aspects of their interfaces
when translating between variants of algorithms,
pure and stateful, interface-based and class-based,
single-threaded or concurrent, etc.
These transformations could be more mindful of interface and class hierarchies
rather than operate on all the generic functions of one pair of APIs at a time.
The packaging of the current features could be improved,
with internals being refactored and exported.
We could use ContextL@~cite[contextl-soa]
or similar Context-Oriented Programming techniques
to dynamically bind extra implicit arguments to our function calls
to trivially reexpose an @[IPS] API as a classic Object Oriented style API.

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
