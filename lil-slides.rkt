#lang at-exp racket ;;-*- Scheme -*-
(require slideshow
	 slideshow/code
	 scheme/gui/base
	 "utils.rkt")

;; See TODO at the end.


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
I am François-René Rideau.
I am a [2014: former] Lisp hacker at ITA Software, now a part of Google Travel.

#-ILC
Today I'm presenting
LIL, the Lisp Interface Library
on which I published a paper at ILC 2012 —
the International Lisp Conference

#+ILC
I am honored to be in Kyoto today
at this 2012 International Lisp Conference
to present the work I did on
LIL, the Lisp Interface Library.

You may have noticed my talk has a mystical sounding title.

I stand by every word.

So what is LIL?
|#

(cover2 0
 (t "Data Structure Library for Common Lisp")
 (t "in Interface-Passing Style"))
#|
As the name says, it's a Data Structure Library for Common Lisp,
and it is written in special style called Interface-Passing Style.

And my claim is that it is THE most fun way
to develop and use Data Structures,
in Common Lisp, or in any language.
|#

(cover2 1
 (t "Parametric Polymorphism")
 (t "married with Ad hoc Polymorphism"))
#|
@(IPS) allows for Parametric Polymorphism,
which was not previously available in Common Lisp.
This means that you can design not just data structures,
but higher-order data structures:
that is, functions
that take as arguments data structures, or functions,
or types, or numbers, or arbitrary objects,
and from them build new more elaborate data structures, etc.

If you have ever used functors in ML or OCaml,
or Type Classes in Haskell,
or maybe done advanced work with
C++ templates, Java Generics, F# or Scala,
you may know what a joy it is to be able to use Parametric Polymorphism
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

[The concepts of Ad hoc polymorphism vs Parametric polymorphism
have been formalized by Christopher Strachey in his seminal 1967 article.]

And so that's why LIL is about CLOS reaching higher-order:
we marry the unsurpassed Ad hoc Polymorphism of CLOS
with Parametric Polymorphism previously not available to CLOS.
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
 (t "thanks to Linear Logic"))
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

(slide #:title (title "I. Interface Passing Style")
       ~ @t{(a.k.a. IPS)} ~)
#|
So let's start with Interface Passing Style.

As the name implies, it a style of programming
that involves passing around interfaces.

Just like Object-Oriented Programming is OOP,
Functional Programming is FP,
and Continuation-Passing Style is CPS,
Interface Passing Style is IPS.
|#

(slide #:title (title "Polymorphism")
  (t "Protocol of functions and data structures") ~
  (t "Users and Implementers") ~
  (bt "Get more for Less"))
#|
Our goal is Polymorphism:
introduce formal abstractions between
users and implementers of data structures,
so both can get more for less:
larger applicability with fewer lines of code,
more cooperation with less coordination,
higher division of labor with higher specialization.

Let's examine an extremely common such protocol.
|#
(slide #:title (title "Finite Maps")
  ~ (t "look up a key in a map") ~
  (code (lookup map key)) ~)
#|
A finite map is
a data structure that encodes the mapping
from a finite number of keys each to one value.

Here's how we'd like to write code that can
look up a key in an arbitrary map
and find the associated value.
|#
(slide #:title (title "Dispatch the correct algorithm")
  ~ (t "But is it an alist? a hash-table? A binary tree?") ~
  (code (lookup map key)) ~)
#|
Now when you tell the system to lookup a map,
how is it going to match the algorithm used to look up the key
with the type of data structure used to encode the map?
|#
(slide #:title (title "Bad monomorphism: incompatible protocols")
  (code (assoc map key) => pair?)
  (code (gethash key map) => value foundp)
  (t "binary tree: DIY"))
#|
In the worst case,
implementers have to build each and every data structure separately from scratch,
and users have to learn a slightly different way to use each of them.

Not only are functions monomorphic,
that is, capable of processing only one kind of data structure;
they do not implement a same protocol,
except in a very abstract sense outside the language itself.

That's the situation for built-in CL maps.
|#
(slide #:title (title "Better monomorphism: uniform signature")
  (code
   (_alist-lookup map key)
   (_hash-table-lookup map key)
   (_binary-tree-lookup map key)
   =>
   value foundp))
#|
A better way to organize your code is that
whereas your functions are still monomorphic,
they all possess a uniform signature.

You will have different functions with different names,
such as alist-lookup, hash-table-lookup, binary-tree-lookup.

But they all follow the same calling convention.
They will all take the same arguments,
in this case a map and a key;
they will all return the same things,
in this case the value found if any,
and a boolean indicating whether a value was found.

Users and implementers may have to do as much work,
but at least they only have to remember one pattern.
The protocol has been moved oh so slightly inside the language.
It is kind of polymorphic at the meta-level,
but remains monomorphic in the base language.

But this is not actually solving the issue of polymorphism,
just pushing it back to whoever chooses the function to use or implement.
|#
(slide #:title (title "Pseudo-polymorphism: Namespaces")
  (code
   (_alist:lookup map key)
   (_hash-table:lookup map key)
   (_binary-tree:lookup map key)
   =>
   value foundp))
#|
You could have a same name, lookup, but different namespaces.
In Common Lisp, that would be packages.
Each data structure would be implemented in its own package,
but using a uniform signature,
so you "just" have to select the correct package.
|#
(slide #:title (title "Pseudo-polymorphism: Namespaces")
  (code
   (in-package _:binary-tree)
   (lookup map key)))
#|
At the meta-language level, the code is polymorphic.
The source code looks the same, and by selecting the correct package,
you can omit the package prefix.
But by having him select the correct package,
you reduce the programmer to doing the linker's job.
And once things are linked one way at compile-time,
so the code is actually monomorphic.
If your language supports EVAL or reflection,
you could programmatically build polymorphism on top,
but that's going to be very ugly and inefficient.
|#
(slide #:title (title "Functional Programming")
  (t "Use higher-order functions")
  ~
  (t "Bind the lookup function within scope")
  ~
  (code (_lookup map key)))
#|
Now if your monomorphic functions follow the same protocol,
and your language supports higher-order functions,
then you can achieve parametric polymorphism.

Users can simply say (lookup map key), and
the system will dispatch on the binding of the function
to determine which algorithm to use.

However, in setting up the scope, the user has to somehow
bind all the protocol functions
to coherent algorithms that manipulate the same data structures.

If you have to manually bind each and every function in the protocol
in each and every scope, you'll have a lot of binding bureaucracy to deal with.
|#
(slide #:title (title "Parametric Polymorphism")
  (t "ML: Modules and Functors")
  ~
  (t "Haskell: Type Classes")
  ~
  (t "Racket: Units"))
#|
To reduce this bureaucracy,
modern functional programming languages support abstractions such as
Modules and Functors (in ML),
Type Classes (in Haskell), or
Units (in Racket).
Each in its own way, they encapsulate
the simultaneous binding of
a signature of functions and data types.

New modules can extend older ones;
and functors allow implementers
to build more elaborate modules from previous ones.


Something that can be both a feature or a limitation of parametric polymorphism
is that it is often quite static and rigid.
Once you've bound your parameters,
the function values inside the scope are monomorphic.
|#

(slide #:title (title "Object Oriented Programming")
  (t "Use generic functions")
  ~
  (t "dispatch method on object meta-data")
  ~
  (code (lookup _map key)))
#|
There exists a very different and in some way dual approach:
Instead of dispatching the correct algorithm based on the functional object,
we can dispatch the correct algorithm based on the data value.

In Object-Oriented Programming, or OOP,
the map data structure contains
some self-descriptive meta-information, its class.
The same generic function lookup
can examine the class of the map object passed as argument,
and select the correct method to use
in that particular type of data structure.
|#
(slide #:title (title "Ad Hoc Polymorphism")
  (t "Most languages: Classes")
  ~
  (t "Haskell: Type Classes")
  ~
  (t "Common Lisp: classes, and more"))
#|
Most modern programming languages have adopted
some form or variant of Object-Oriented Programming
to provide Ad Hoc Polymorphism.

Programmers define classes,
that can be refined and extended into subclasses.
The subclasses can inherit structure and behavior from their superclasses,
so implementers can reuse code from the superclasses,
and users can reuse code for all subclasses.

In dynamically typed languages such as Smalltalk, Lisp,
and after them Python, Ruby, etc., (maybe Java)
the objects are examined as runtime to determine the correct method.

In a statically typed language such as C++ or Haskell, (maybe Java),
the class can be determined statically,
and the proper function chosen at compile-time.
|#
(slide #:title (title "Implementing Polymorphism")
  (t "Reduction to monomorphism")
  ~
  (t "FP: dictionary, linkage table")
  ~
  (t "OOP: self, virtual method table"))
#|
To implement polymorphism,
calls to polymorphic functions
can be reduced to monomorphic functions at compile-time or runtime.
Every source call site may exists in multiple copies,
one per combination of classes to which it applies.

But whether for parametric polymorphism or ad hoc polymorphism,
whether statically at compile-time or dynamically at runtime,
the implementation of this reduction relies on
internally passing around an extra argument containing meta-data
based on which to dispatch the correct function to use:
a dictionary of functions,
a virtual method table,
a self object reference,
a method resolution context.

At that leads us to
|#
(slide #:title (title "Interface Passing Style")
  (t "Reconcile FP and OOP") ~
  (t "Expose the internals"))
#|
Interface Passing Style.

This is contribution to reconciling
Functional Programming and Object-Oriented Programming.

Instead of leaving the polymorphism meta-data
as a hidden piece of internals,
I'm exposing it as a first-class entity.
|#
(slide #:title (title "Interface Passing Style")
  (t "IPS: One little extra argument for the meta-data")
  ~
  (code (lookup _<interface> map key)))
#|
And so Interface-Passing Style is nothing but
explicit passing around such meta-data as an extra argument
to generic functions.

We don't hide the plumbing, we flaunt it proudly,
and we give that piece of first-class meta-data the
glorious name of Interface.

And thanks to that trick, we can have both
ad hoc polymorphism and parametric polymorphism together.
|#
(slide #:title (title "Using an alist")
 (code
  (lookup <alist>
    '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO"))
    2010)
     =>
     ?))
#|
Let's see how to use it.

The most trivial implementation for a finite map in Lisp
is an association list, or alist,
which is a list of key-value pairs.
Here, we have an alist that maps year
to city code of the corresponding recent ILC conference.

And so if you pass the <alist> interface to the function lookup,
it will know that the map argument is an alist.

Can anyone in the audience tell me what this function call returns?
|#
(slide #:title (title "Using an alist")
 (code
  (lookup <alist>
    '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO"))
    2010)
     =>
     "RNO" T))
#|
It returns two values,
the string "RNO" and
the boolean T to confirm the key was found.
|#
(slide #:title (title "Using a hash-table")
 (code
  (lookup _<hash-table>
    '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO"))
    2010)
  =>
  ?))
#|
Now if instead of using the alist interface,
I use the <hash-table> interface,
what do you think this expression will return?
|#
(slide #:title (title "Using a hash-table - FAIL")
 (code
  (lookup <hash-table>
    '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO"))
    2010)
  =>!
  TYPE-ERROR: ... is not of type HASH-TABLE))
#|
Well, it will not return, it will issue a type error,
because you're telling the system to look up a hash-table,
but you are providing an alist.

Before we may look up at hash-table, we have to create one.
And the easiest way to do it happens to be...
|#
(slide #:title (title "Using a hash-table")
 (code
  (lookup <hash-table>
    (alist-map <hash-table>
      '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO")))
    2010)
  =>
  "RNO" T))
#|
... transforming an alist literal into a hash-table
using the function alist-map.
alists are thus slightly privileged in Lisp
because there is a built-in literal syntax for them;
but other data structures are just one function call away.
|#

(slide #:title (title "Using a balanced binary tree")
 (code
  (lookup <number-map>
    (alist-map <number-map>
      '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO")))
    2010)
  =>
  "RNO" T))
#|
We could also use a binary tree, by using the interface <number-map>.
Under the hood, this is an avl-tree using real numbers as its ordered keys.
|#
(slide #:title (title "Inserting in a map"))
#|
Now, it's important to look up a key in a map,
but we also want to be able to insert a key value association in a map.
|#
(slide #:title (title "Inserting in a map")
 (code (insert <i> map key value)))
#|
We do it with the function insert.

Did you notice how we follow the convention
of starting and ending the names of our interface variables
with angle brackets?
|#
(slide #:title (title "Inserting in a map")
 (code
  (insert <alist>
   '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO"))
   2012 "UKY")
  =>
  ?))
#|
Let's say we notice that
the airport code for Kyoto is actually UKY, not KYO,
and we want to fix the alist.

So what happens if you insert a key and value in an alist?
|#
(slide #:title (title "Inserting in an alist")
 (code
  (insert <alist>
   '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO"))
   2012 "UKY")
  =>
  '((2009 . "BOS") (2010 . "RNO") (2012 . "UKY"))))
#|
It returns a new alist where
the mapping for the key is associated to the value,
overriding any previous association.
|#
(slide #:title (title "Inserting in an alist")
 (code
  (insert <alist>
   '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO"))
   2012 "UKY")
  =>
  '((2012 . "UKY") (2009 . "BOS") (2010 . "RNO"))))
#|
Of course, the implementation does not have to preserve
the order of the mappings, so it might return any equivalent alist.

In any case, alists are best seen as a pure data structure,
and it is both easier and safer to create a new one everytime,
that shares structure with the previous one.
|#
(slide #:title (title "Inserting in a hash-table")
 (code
  (insert <hash-table>
   (alist-map <hash-table>
     '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO")))
   2012 "UKY")
  =>
  ?))
#|
Now, a trick question for those familiar with Common Lisp.
What happens if I insert a key-value association in a hash-table?
|#
(slide #:title (title "Inserting in a hash-table - FAIL")
 (code
  (insert <hash-table>
   (alist-map <hash-table>
     '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO")))
   2012 "UKY")
  =>
  \; no value))
#|
It returns nothing,
because in Common Lisp,
the built-in hash-table data structures are stateful.
insert happens through side-effect;
it does not return a new hash-table, it modifies the existing one.
The same hash-table now associates a different value to the provided key.

Note that in the example displayed,
we failed to keep a reference to the object,
so it's lost.
|#
(slide #:title (title "Inserting in a hash-table")
 (code
  (defvar *table*
    (alist-map <hash-table>
     '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO"))))

  (insert <hash-table> *table* 2012 "UKY")

  ... *table* ...))
#|
Instead, stateful data structures are used in a different style,
where you keep a reference to the object and reuse it many times.

But the point is that insert means something quite different
for pure and stateful data structures, with incompatible invariants.
Our library actually defines two different generic functions:
|#
(slide #:title (title "Pure vs Stateful")
  (code pure:insert ≠ stateful:insert) ~
  (code pure:lookup = stateful:lookup = interface:lookup))
#|
pure:insert, in the package pure,
inserts in a pure map and returns a new updated map.
stateful:insert, in the package stateful,
modifies a stateful map by side-effect and returns no value.
These are two different functions, each in its own package.

By contrast, pure:lookup and stateful:lookup are the same function,
with the same signature and the same invariants.
It is part of a read-only fragment common to pure and stateful maps.
Both packages pure and stateful import it from a common package interface,
and reexport it.
|#
(slide #:title (title "Pure vs Stateful, defaults")
  (code <alist> = pure:<alist>) ~
  (code <hash-table> = stateful:<hash-table>) ~
  (code lookup = interface:lookup) ~
  (code insert = pure:insert OR stateful:insert))
#|
In some cases where the context is otherwise ambiguous,
I'll assume some implicit defaults, following common usage:
An unqualified alist is a pure alist.
An unqualified hash-table is a stateful hash-table.
Later on, when I discuss stateful alists and pure hash-tables,
I will keep explicit package qualifiers.

But the defaults make the context ambiguous,
I will omit the package qualifier.
So I'll write lookup for interface:lookup,
and insert for whichever of pure:insert or stateful:insert
makes sense for the data structure at hand.
|#
(slide #:title (title "Parametric Interfaces"))
#|
Now let's discuss Parametric Interfaces
|#
(slide #:title (title "Parametric Interfaces")
  (code
   (lookup <alist>
     '(("BOS" . 2009) ("RNO" . 2010) ("UKY" . 2012))
     "BOS")
   =>
   ?))
#|
Let's try to lookup an alist that is
the reverse mapping from the previous one,
mapping city codes to years of recent ILC conference.

Can someone familiar with Common Lisp tell me
what the above function call returns?
|#
(slide #:title (title "Parametric Interfaces")
  (code
   (lookup <alist>
     '(("BOS" . 2009) ("RNO" . 2010) ("UKY" . 2012))
     "BOS")
   =>
   NIL NIL))
#|
It returns the two values NIL NIL
(in Common Lisp, NIL serves as the boolean value for false).

Why? Because in Common Lisp,
the default way to compare things is
to use the EQL predicate that compares its arguments for identity.
These two strings "BOS" and "BOS" are mutable strings with different identity,
and thus the key cannot be found.
|#
(slide #:title (title "Parametric Interfaces")
  (code
    <alist> = (<alist> <eql>)))
#|
So if <eql> (in angle brackets) is
the interface for comparing objects with EQL,
then the interface <alist> is the same thing as (<alist> <eql>),
where the former <alist> is a variable or constant
bound to the default <alist> interface,
and the latter <alist> is a function to construct
different <alist> interfaces with specialized comparison functions.
|#
(slide #:title (title "Parametric Interfaces")
  (code
   (lookup (<alist> <equal>)
     '(("BOS" . 2009) ("RNO" . 2010) ("UKY" . 2012))
     "BOS")
   =>
   2009 T))
#|
So we can use the EQUAL predicate
that compares built-in objects by value rather than identity,
using (<alist> <equal>),
where <equal> (between angle brackets) is an interface
that specifies this comparison.
|#
(slide #:title (title "Parametric Interfaces")
  (code
   (lookup (<alist> <string>)
     '(("BOS" . 2009) ("RNO" . 2010) ("UKY" . 2012))
     "BOS")
   =>
   2009 T))
#|
We could also use the <string> interface
that specifically knows how to compare strings for equality,
and also knows how to order them.
In this case, though, we don't rely on order,
and it would behave the same as using <equal>.
|#
(slide #:title (title "Parametric Interfaces")
  (code
   (lookup (<alist> <case-insensitive-string>)
     '(("BOS" . 2009) ("RNO" . 2010) ("UKY" . 2012))
     "bos")
   =>
   2009 T))
#|
However, we could have a different behavior with
<case-insensitive-string>,
which doesn't make a difference between uppercase and lowercase.

The important point here is that @IPS allows for Parametric Polymorphism.

In standard @OOP and without Parametric Polymorphism,
you would have to define at compile-time
a new class for every kind of alist that you want.
In @IPS, you can build it at runtime.

The number of classes to create would increase
polynomially with the size of your parameter space,
and exponentially with the number of parameters;
and if the parameters can be arbitrary interface expressions,
you just can't do it without Parametric Polymorphism.
|#
(slide #:title (title "Passing Interfaces Around"))
#|
Now that we have seen how to make individual calls to interface functions,
let's combine them into larger functions and see
how we can write polymorphic code.
|#
(slide #:title (title "Passing Interfaces Around")
  (code
   (defmethod sum-values ((<i> pure:<map>) map)
     (let ((submaps (divide/list <i> map)))
       (cond
	((null submaps)
	 0)
	((null (rest submaps))
	 (nth-value 1 (first-key-value <i> map)))
	(t
	 (reduce #'+
	   (mapcar (λ (m) (sum-values <i> m))
		   submaps))))))))
#|
In this function, we sum the values in a pure map
through a polymorphic divide-and-conquer strategy.

We rely on two interface functions,
divide/list and first-key-value.
divive/list divides the map into a list of non-empty submaps.
If the map was empty, it returns the empty list,
and the sum is 0.
If the map was a singleton, it returns the singleton list with the map,
and we extract its contents with first-key-value,
and we drop the key (nth-value indices are 0 based,
so the 0th value is the association key,
and the first value is the association value).
Otherwise, the list contains at least two submaps,
and we recurse using a simple map-reduce strategy.

Note that we could trivially parallelize this function
with parallel versions of map and reduce.
Note also that with my system LAMBDA-READER,
you could actually use the symbol λ instead of LAMBDA.

But the takeaway is that you can write very polymorphic algorithms
that work on arbitrary data structures.
All I rely on here is that I have a pure map that maps keys to numbers;
and I could generalize numbers to any monoid
if I bothered to abstract away the sum operator into another interface.
|#
(slide #:title (title "Functions of multiple interfaces")
  (t "Most functions take one interface as input")
  (code (join <map> map1 map2))
  ~
  (t "A few take multiple interfaces")
  (code (convert <dest> <orig> object)))
#|
Most functions take one and only one interface object as an argument,
the first argument.

Our join function, for instance,
takes two maps
that use the same data structure representation,
after a single interface argument that describes that representation.
The pure:join function returns a new map that joins the mappings of the two;
the stateful:join function adds the mapping of the latter to the former.

But there are also some functions take multiple interfaces as parameters.

For instance, the convert function,
that creates an object that follows the destination interface
from an object that follows the origin interface.
|#
(slide #:title (title "Multiple interfaces")
  (t "Mostly use higher-order interfaces.")
  (code
   (defgeneric <federate> (<db1> <db2>)
     (:documentation
      "Create federated db interface"))))
#|
Typically though, the way to deal with multiple interfaces
is to combine them into a single interface using parametric polymorphism,
then use the combined interface as THE interface to pass around to functions.

And so, most functions of multiple interfaces are actually
constructors for more elaborate interfaces,
such as a hypothetical <federate> function
that could federate database by building federated database interfaces
from elementary database interfaces.
|#

(slide #:title (title "Beware! No Type Checking")
  (t "No implicit check")
  (code (check-invariant <type> object)) ~
  (t "Runtime errors — or worse") ~
  (t "No integration to typep (yet)"))
#|
Following the tradition of Common Lisp,
our library does NOT provide static typechecking.
You could combine parametric and ad hoc polymorphism
in the context of static typechecking, as in C++ or Scala,
but in this case, this is Common Lisp, so we don't.

We do not implicitly include type checks everywhere.
They are expensive and in general have a cost proportional to the size of the objects,
so you don't want them for everyday operation.
But we do provide a function check-invariant,
that takes a <type> interface and an object and checks the object is of the type,
and throws an error if it doesn't.

When running tests, you should check invariants a lot.
In my tests I do and I caught many bugs that way.
You should probably also check invariants
in the various the entry points of a module
to validate any untrusted user-provided inputs.

So what happens when you run code with the wrong argument?
We already saw that in some cases,
Lisp will eventually throw a runtime error at you
as you try to do an illegal operation.
In other cases, you might get worse: the wrong answer.

So beware, you have the great power to do anything you want;
with great power comes great responsibility.

We also don't provide integration of interfaces with Common Lisp's typep yet.
I know how to do that, and it involves using ASDF-FINALIZERS.
But I haven't bothered yet.
|#

(slide #:title (title "Defining Interfaces"))
#|
Now that we've seen how to use interfaces,
defining interfaces is rather straightforward.
|#

(slide #:title (title "Abstract Interfaces")
  (code
    (define-interface <emptyable> (<type>) ()
      (:abstract)
      (:generic empty (<emptyable>)
        (:values object) (:out 0)
        (:documentation "Return an empty object"))
      (:generic empty-p (<emptyable> object)
        (:in 1) (:values boolean)
        (:documentation "Is object empty?")))))
#|
Here is a simple interface, <emptyable>,
representing types of objects that may be empty.

We define it with define-interface,
which can be seen as an extension to defclass,
and creates a class where the metaclass is interface-class.

So first, we have a list of super-interfaces,
in this case, there is one super-interface, <type>,
the interface of interfaces notionally associated to a type.

Then there is a list of slot, which is empty,
since this is not a parametric interface.

Then we have a list of options, and that's where we extend defclass.

The :abstract option indicates it's an abstract function,
that doesn't fully implement all declared functions.
It cannot be instantiated.

The :generic options each introduce one generic function.

The empty function takes an abstract interface as argument,
and returns one value, an object, the empty object.
The (:out 0) says that this 0th return value is of the target type.

The empty function takes an abstract interface as argument,
and an object, and returns a boolean which is true iff the object was empty.
The (:in 1) says that the 1st argument (remember it's 0-based)
is of the target type.
|#

(slide #:title (title "Abstract interfaces")
  (t "It's abstract, can't be instantiated")
  ~
  (t "It's a mixin! (a.k.a.  trait)")
  ~
  (t "This mixin defines part of a signature"))
#|
So we could define an abstract interface.

It is defines a single aspect shared by many interfaces,
and is supposed to be used via multiple inheritance:
it's a mixin — what other languages such as smalltalk or Scala call a trait.

And this particular mixin defines part of a signature,
i.e. a list of functions with their calling conventions.
|#
(slide #:title (title "Abstract Interfaces")
  (code
    (define-interface <empty-is-nil>
      (<emptyable>) ()
      (:abstract))

    (defmethod empty ((<i> <emptyable>))
      nil)

    (defmethod empty-p ((<i> <emptyable>) object)
      (null object))))
#|
Here is another abstract interface.

This one proposes a partial implementation of the previous one.

If implements the method empty by returning nil.

If implements the method empty-p by returning T
if and only if the provided object is NIL,
using the built-in NULL predicate.
|#
(slide #:title (title "Abstract Interfaces")
  (t "Also an abstract mixin.")
  ~
  (t "This mixin defines part of an implementation"))
#|
It's also an abstract mixin.

But this mixin defines part of an implementation,
rather than part of a signature.

It is the complement of the previous.

Actually, <empty-is-nil> already defines all there is to be defined,
so it could trivially be made into a concrete interface.
We still make it abstract to insist on the intent as a mixin,
but we could have made it a concrete interface.
|#

(slide #:title (title "Concrete Interface")
  (code
   (define-interface <null> (<empty-is-nil>) ()
     (:singleton))))
#|
Here is a concrete interface trivially deduced from <empty-is-nil>

The :singleton means that a variable <empty-is-nil> is defined
with an singleton instance of the interface class.
|#
(slide #:title (title "Multiple Dispatch")
  (code
   (defmethod empty-p ((<i> <empty-is-nil>)
		       (object t))
     nil)

   (defmethod empty-p ((<i> <empty-is-nil>)
		       (object null))
     t)))
#|
Note that we could have implemented <empty-is-nil>
differently, in a more object-oriented style,
using multiple dispatch as follows:

We define two methods for empty-p,
both that match interfaces of the class <empty-is-nil>,
and that dispatch on the second (or first, depending on how you count) interface.

The default behavior is to return NIL (i.e. false) for any object,
i.e. objects of class T, the class of all objects.

But objects of class NULL, i.e. the value NIL,
will match the more specific method,
and instead return T.
|#

(slide #:title (title "Multiple Dispatch")
  (t "Same mixin.") ~
  (t "Using multiple-dispatch") ~
  (t "Not sacrificing OO style") ~
  (t "Extensible!"))
#|
This is the same mixin, done differently.

It illustrates that thanks to Multiple Dispatch,
we didn't sacrifice Ad Hoc polymorphism to the dispatch on the interface argument.
We can still write our methods in object-oriented style.

What does that buy us? Extensibility!
We could have multiple kinds of empty object.

Our interface could allow for a bit-bucket,
a magic object that always remains empty.

Or it could allow for proxy objects,
where you don't know whether the object is empty until you query a server.

Or it could allow for lazy objects with a deferred computation queue.
You have to flush your journal before you can tell whether the object is empty.

In any case, the usual arguments for object-oriented programming apply here,
and that's the point.
|#
(slide #:title (title "Concrete Parametric Interfaces")
  (code
   (define-interface <alist>
     (<map-empty-as-nil>
      <map-decons-from-first-key-value-drop>
      <map-update-key-from-lookup-insert-drop>
      <map-divide/list-from-divide>
      <map-map/2-from-fold-left-lookup-insert-drop>
      <map-join-from-fold-left-insert>
      <map-join/list-from-join>
      <map>)
    ((key-interface :type <eq>
     :initarg :key-interface
     :reader key-interface))
    (:parametric (&optional (eq <eql>))
      (make-interface :key-interface eq))
    (:singleton))))
#|
Here is the full definition of <alist>.

Let's examine its parts...
|#
(slide #:title (title "Super-Interfaces")
  (code
     (<map-empty-as-nil>
      <map-decons-from-first-key-value-drop>
      <map-update-key-from-lookup-insert-drop>
      <map-divide/list-from-divide>
      <map-map/2-from-fold-left-lookup-insert-drop>
      <map-join-from-fold-left-insert>
      <map-join/list-from-join>
      <map>)))
#|
First, it has plenty of implementation mixins,
that implement some functions in terms of other functions.
For instance, <map-decons-from-first-key-value-drop>
can implemenent the destructuring function decons
from the function first-key-value that extract one key-value association
and the function drop that can remove that association.

The last mixin is <map> itself, that defines the signature.
It could have been left out, since it's already inherited via
all the previous mixins.
|#
(slide #:title (title "Interface Parameters")
  (code
    ((key-interface :type <eq>
       :initarg :key-interface
       :reader key-interface))))
#|
Then we have one slot, key-interface,
which makes it a parametric interface.

key-interface is constrained to be an interface of class <eq>.
initarg defines how to initialize it, and the reader how to extract it.
There is no writer, so it's read-only.
|#
(slide #:title (title "Interface Constructors")
  (code
    (:parametric (&optional (eq <eql>))
      (make-interface :key-interface eq))
    (:singleton)))
#|
The parametric option creates a function <alist>
that takes one optional argument eq, that default to the <eql> interface,
and instantiates an interface using that as the key interface.

make-interface is just a memoizing version of make-instance,
so that we always return the same interface object
when it's called with the same argument.
alist interfaces have no identity, only a value.

The singleton, as above, creates a variable <alist>
using the function <alist> and its default value <eql>.
|#
(slide #:title (title "Multiple namespaces")
  (code <alist>)
  (t "the class")
  (t "the function")
  (t "the variable")
  (bt "Do not confuse!"))
#|
Common Lisp is a Lisp-N. It possesses multiple namespaces.

Do not confuse the multiple meanings of the symbol <alist>,
depending on the namespace context.

As a class, it is the class of alist interfaces,
that can be inherited from,
based on which to dispatch behavior.

As a function, it is bound to the constructor that
takes an optional key signature as argument,
and creates an alist interface.

As a variable, it is bound to the default <alist> interface object.
|#
);begin skip

(slide #:title @title{II. Building Data Structures})
#|
We have seen how to use and define interfaces.

I'd like to illustrate a few ways that we take advantage
of the power of both CLOS-style Ad hoc polymorphism
and parametric polymorphism in building these data structures.
|#
(slide #:title @title{Building Data Structures}
  @t{CLOS: incrementality} ~
  @t{Parametric: composability})
#|
Thanks to the power of CLOS,
we could organize our data structures
in thin incremental layers of functionality,
each small and easy to understand.

Thanks to parametric polymorphism,
we could build composable abstractions,
not just a collection of data structures.
|#
(slide #:title @title{Current LIL API}
  @t{pure:<map> stateful:<map>} ~
  @t{<type> <eq> <order>} ~
  @t{pure:<iterator> stateful:<iterator>} ~
  @t{<box>})
#|
So far, the data structures provided by LIL, the Lisp-Interface-Library,
are mostly implementations of finite maps, both pure and stateful.

But to build those maps, I had to define a few fundamental interfaces:
for object types, for equality and order comparisons, iteration.

LIL also provides a few ways to box data into boxes:
mutable boxes, linear boxes, lazy boxes;
these boxes will notably help when later we transform interfaces.

Let's focus on maps.
|#
(slide #:title @title{Map API methods}
  @t{inherited: make check-invariant convert size empty empty-p empty!} ~
  @t{read: lookup first-key-value fold-left fold-right map-alist alist-map} ~
  @t{write: insert drop decons join join/list divide divide/list map/2 ...})
#|
Here is a list of generic functions available to manipulate maps.
They provide a nice way to manipulate arbitrary maps.
|#
(slide #:title @title{Map Implementations}
  @t{pure: <alist> <avl-tree> <hash-table> <trie>} ~
  @t{stateful: <avl-tree> <hash-table>})
#|
And here is a list of available useful data structures.
|#
(slide #:title @title{Method Combinations}
  (code
(define-interface <post-self-balanced-binary-tree>
    (<binary-tree>) ()
  (:abstract))))
#|
And here in three slides is the complete implementation
of self-balancing stateful trees, using method combination.

First, define a mixin.
|#
(slide #:title (title "Method Combinations")
       (code
(defmethod insert :after
    ((i <post-self-balanced-binary-tree>)
     node key value)
  (declare (ignore key value))
  (balance-node i node))))
#|
Then add two methods to re-balance the nodes
after a modication:
when you insert a new node.
|#
(slide #:title (title "Method Combinations")
       (code
(defmethod drop :after
    ((i <post-self-balanced-binary-tree>)
     node key)
  (declare (ignore key))
  (balance-node i node))))
#|
Or when you drop a node.

That's all you need to say.
And that's really how things should be in any language.
|#
(slide #:title (title "Stateful AVL-tree")
  (code
(define-interface <avl-tree>
    (interface::<avl-tree>
     <heighted-binary-tree>
     <post-self-balanced-binary-tree>) ()
  (:abstract))
(defclass avl-tree-node
    (interface::avl-tree-node
     heighted-binary-tree-node) ())
))
#|
And on top of re-balancing,
here is are stateful AVL trees,
also in three slides.

First define a mixin for the interface,
and a mixin for the actual structure.

Yes, in the future,
I hope that I integrate
the structure mixin definitions
as part of the interface mixin definitions.

The issue is that in general, your interfaces may manage
multiple data structures or variants of a data structure;
therefore, you can't limit yourself to
a single hierarchy of data structures that follows
the hierarchy of interfaces.
|#
(slide #:title (title "Stateful AVL-tree")
  (code
(defmethod node-class ((i <avl-tree>))
  'avl-tree-node)

(defmethod balance-node ((i <avl-tree>)
	   		 (node empty-object))
  (values))
))
#|
Here are trivial ancillary methods.

Function node-class helps abstract tree functions
specify what class to use to instantiate concrete trees.
This method tells us what to use for avl-trees.

And function balance-node needs to be told
that empty objects are already balanced.
|#
(slide #:title (title "Stateful AVL-tree")
  (code
(defmethod balance-node
    ((i <avl-tree>) (node avl-tree-node))
  (ecase (node-balance node)
    ((-1 0 1) (update-height node))
    ((-2)
     (ecase (node-balance (left node))
       ((-1 0))
       ((1) (rotate-node-left (left node))))
     (rotate-node-right node))
    ...))
))
#|    ((2)
     (ecase (node-balance (right node))
       ((-1)
        (rotate-node-right (right node)))
       ((0 1)))
     (rotate-node-left node))))

Finally, here the meat of the functionality:
the rebalancing function.
For brevity, I omitted the case
where the tree is skewed to the right
and we need to rotate it left,
as it's just symmetrical to the case displayed
where the tree is skewed to the left
and we need to rotate it right.

See how once again we only need to specify
small increments of functionality,
and take advantage of both multiple inheritance
and method combination, through the previous layer
of post-self-balancing-binary-tree
|#
(slide #:title (title "Composing Interfaces")
  (code
(define-interface <hash-table>
    (<map-join-from-fold-left-insert>
     <map-join/list-from-join>
     <map-update-key-from-lookup-insert-drop>
     <map-map/2-from-fold-left-lookup-insert-drop>
     <map>)
    ...)))
#|
And now for taking advantage of parametricity.

We saw that Common Lisp provides stateful hash-tables.
But it doesn't provide pure hash-tables,
so here is how we implement them.

We start with plenty of appropriate mixins.
|#
(slide #:title (title "Composing Interfaces")
  (code
(define-interface <hash-table> (...)
  ((key-interface :type <hashable>
    :reader key-interface
    :initarg :key)
   (hashmap-interface :type <map>
    :reader hashmap-interface
    :initarg :hashmap)
   (bucketmap-interface :type <map>
    :reader bucketmap-interface
    :initarg :bucketmap))
  ...)))
#|
And there is the interesting part.

What is a hash-table?
It's a way to compose
a slow but generic algorithm that can use arbitrary objects as keys,
and a fast but specialized algorithm that can only use integers as keys.

Each key is hashed into an integer.
This is specified by the key-interface parameter.

Hashed integer values are organized in a fast access data structure
(for stateful hash-tables, that would be an array;
for pure hash-tables, that would be an self-balancing binary tree).
This is specified by the hashmap-interface parameter.

For each value in the table of hashed integers,
we have a bucket of actual entries, using the slow but generic algorithm.
If our hash function, each entry will be very small,
so it doesn't matter that the generic algorithm is slow.
This is specified by the bucketmap-interface parameter.
|#
(slide #:title (title "Composing Interfaces")
  (code
(define-interface <hash-table> (...) (...)
  (:parametric
       (&key (key <equal>)
             (hashmap <number-map>)
             (bucketmap (<alist> key)))
     (make-interface :key key
       :hashmap hashmap :bucketmap bucketmap))
  (:singleton)
  (:documentation "pure hash table"))))
#|
Finally, we make this a concrete interface,
with proper defaults:

<equal> for the key-interface
(because Common Lisp has no hashing interface
for its otherwise default EQL comparison —
that would require standardized interaction
with the garbage collector).

<number-map>, i.e. avl-trees with numeric keys,
as the fast hashmap interface.

and last but not least
<alist> as the slow but generic map interface
for the bucket interface.

Now we have to define a handful of methods
to actually implement hash-tables.
|#
(slide #:title (title "Mind the Pun")
  (code
(defmethod insert ((_<i> <hash-table>) _map key value)
  (nest
   (let ((hash (hash (key-interface <i>) key))))
   (multiple-value-bind (bucket foundp)
       (lookup (hashmap-interface <i>) map hash))
   (let ((old-bucket
          (if foundp
	      bucket
	      (empty (bucketmap-interface <i>))))))
   (let ((new-bucket
          (insert (bucketmap-interface <i>)
                  old-bucket key value))))
   (insert (hashmap-interface <i>) map
           hash new-bucket)))))

(slide #:title (title "Mind the Pun")
  (code
(defmethod insert ((<i> <hash-table>) map key value)
  (nest
   (let ((hash (hash (key-interface <i>) key))))
   (multiple-value-bind (bucket foundp)
       (lookup (_hashmap-interface _<i>) _map hash))
   (let ((old-bucket
          (if foundp
	      bucket
	      (empty (bucketmap-interface <i>))))))
   (let ((new-bucket
          (insert (bucketmap-interface <i>)
                  old-bucket key value))))
   (insert (_hashmap-interface _<i>) _map
           hash new-bucket)))))

#|
Here is a typical such method, for insertion.

It is better to read it inside out.

First you compute the hash of the key
using the key-interface.

Then you lookup the bucket for this hash in the hash-map.

If no bucket is found, then start from an empty bucket.

Insert the actual key in the slow for small bucket.

And insert the new bucket in the hash-map.

Mind the pun:
the very same object map
is used both as viewed by interface <i>,
and as a viewed by interface (hashmap-interface <i>).

This is a very nice technique that is made possible
by Interface-Passing Style.
Without @IPS, you'd have to wrap each and every object
in a wrapping constructor to change the point of view;
that can be ugly and inefficient.

But while punning is nice,
the real important feature is composability.
We have built a data structure that has nice properties,
namely being both fast and generic,
from simpler map data structures that each had only
some of these properties.

That's something you can't do in a general way
using plain object-oriented CLOS style.
|#
(slide #:title (title "III. Transformations"))
#|
You may say,
combining Ad hoc and Parametric polymorphism
is something that several programming languages provide already.
However, here is a novel feature
that is enabled by using Interface Passing Style
specifically in Common Lisp:
useful automatic code transformations using macros.
|#
(slide #:title (title "Making interfaces implicit")
  (code
(with-interface
  (interface functions-spec &key prefix package)
  &body body)
))
#|
The most obvious annoyance when using @IPS is
to have to explicitly pass around all these interfaces,
even though most of the time, it's obvious from the context
which interface to pass around.

And the simplest and most obvious syntactic improvement to @IPS
is therefore to allow the elision of the explicit interface argument.

Thus, here is a macro with-interface.
You provide an concrete runtime interface object INTERFACE,
a compile-time specification of functions in which to elide the interface,
which usually just the name of an abstract interface,
and various options,
and in the BODY of the macro,
calls to the specified interface functions will implicitly
pass around the the provided interface object.
|#
(slide #:title (title "Using interfaces made implicit")
  (code
(defun insertion-sort (alist)
  (with-interface (<number-map> <map>)
    (let ((m (alist-map alist)))
      (fold-right m #'acons nil))))
))
#|
Here is a trivial example use,
which sorts an alist with numeric keys
using avl-trees (the <number-map> interface).
First we build a balanced binary tree by inserting the entries of the alist,
using alist-map —
notice how alist-map does not explicitly take the <number-map> interface.
Then we walk that tree and extract a sorted list of entries.

Because we create the tree using the generic constructor alist-map,
our algorithm could use pure trees as well as a stateful trees.
That's something that's not possible with plain object-oriented style,
where that can be no generic constructors.

Of course, in the above example,
we only elided interface passing in two function calls,
so with-interface didn't buy us much.
But in more complex functions with many interface function calls,
it can buy us a lot.
|#
(slide #:title (title "Defining interfaces implicit style")
  (code
(define-interface <eq-from-==> (<eq>) ()
  (:abstract)
  (:method> eq-function ()
    (λ (x y) (== x y))))
))
#|
Because interface functions always take the interface as their first argument,
another obvious syntactic improvement is to provide a way to define
interface methods with an implicit with-interface.

Therefore define-interface accepts a :method> option
to define methods this way.

Here this trivial implementation mixin, <eq-from-==>.
The <eq> interface defines two functions, == and eq-function.
== compares two values,
while eq-function returns a function that can do comparisons,
and can be passed to higher-order functions such as find.
This mixin allows you to automatically deduce the latter from the former.

Notice how the call to == does not take an interface argument.
Interface-passing happens implicitly.

Also notice that @IPS can express so-called binary methods
without any dissymmetry between the arguments,
and without any problem with covariance or contravariance.
That's a positive feature of having separated meta-data and data:
because interface meta-data and the actual data-structures
follow separate hierarchy, we are not stuck with contradictory
constraints of covariance and contravariance.
|#
(slide #:title (title "Defining interfaces implicit style")
  (code
    (define-interface <empty-is-nil>
      (<emptyable>) ()
      (:abstract)
      (:method> empty ()
	 nil)
      (:method> empty-p (object)
	 nil)
      (:method> empty-p ((object null))
	 t))))
#|
We can now revisit the earlier example of <empty-is-nil>
Here is the definition with elided interface passing.

Note how we are implicitly using multiple dispatch.
The empty-p methods dispatch on both
the implicit zeroth interface argument
and explicit first object argument.
|#
(slide #:title (title "IPS to FP")
  (code
(define-interface-specialized-functions
  interface functions-spec &key prefix package)))
#|
Now if there's a data structure you use all the time,
and you don't want to either use with-interface
or pass extra arguments all the time,
you can use the define-interface-specialized-functions macro.
It will create a traditional Common Lisp API
from your structures built in @IPS.
|#
(slide #:title (title "IPS to FP")
  (code
(defpackage :pure-hash-table ...)
(in-package :pure-hash-table)
(define-interface-specialized-functions
  pure:<hash-table> pure:<map>)
(let ((h (empty)))
  (insert h 1 "a")
  (join h (alist-map '((2 . "b") (3 . "c")))))))
#|
Here is a trivial use of the macro.

We define a package pure-hash-table that export all the functions we want,
then in that package we simply evaluate the macro,
and that's all.

This way, you can create your data structures
with all the power of @IPS,
and export them for use by your customers
who don't want to hear anything about @IPS.

There is no more polymorphism in the provided API,
though polymorphism was used (and how!)
in building the data structure.
|#
(slide #:title (title "From Pure to Stateful and Back")
  (t "put pure objects in mutable box") ~
  (t "put stateful objects in use-once box"))
#|
And now for the really interesting transformations.

With LIL, you can define a data structure in pure style,
with all its nice performance properties,
and use it in the context of a stateful algorithm.
Or you can define a data structure in stateful style,
with all its nice performance properties,
and use it in the context of a pure algorithm.
|#
(slide #:title (title "Pure to Stateful")
  (code
(in-package :stateful)
(define-mutating-interface
  <mutating-map> (stateful:<map>) (pure:<map>)
  ()
  ...
  (:parametric (interface)
    (make-interface :pure-interface interface)))
))
#|
With the define-mutating-interface macro,
we can build a <mutating> variant of any pure interface.
Here is how we use it to generically create stateful maps
from any given pure map interface.

Elided are a few method definitions that I'll tell you about later.

The transformation is straightforward.
Objects in the mutating variant are just a mutable box
that contains as its current state the pure variant.

Input arguments to a stateful function are unboxed
before being passed to the pure function;
when there is a side effect,
the pure function will output a new value among its return results;
the stateful function captures this new value and updates the box.
|#
(slide #:title (title "Pure to Stateful")
  (code
(defmethod lookup
    ((<interface> <mutating-map>) map key)
  (let ((<pure-interface>
          (pure-interface <interface>))
        (pure-map (box-value map)))
    (lookup <pure-interface> pure-map key)))))
#|
Here is how the lookup method is automatically generated.
In practice, the macro uses gensyms, here I cleaned up the macro output.

See how to lookup a mutating map,
we first extract the pure interface from the mutating interface,
and we unbox the map object to get the pure map value.
We do the lookup and return the results.

Here, we have a read-only function,
that only has an input map, no output map.

How does the transformation know about that?
How does it know to unbox the map argument, but not the key argument
and to not box the value and foundp results?
|#
(slide #:title (title "Linear Types")
  (code
   (:generic lookup
     (<map> map key) (:in 1)
     (:values value foundp))

   (:generic pure:insert
     (<map> map key value) (:in 1)
     (:values updated-map) (:out 0))

   (:generic pure:empty
     (<map>)
     (:values object) (:out 0))))
#|
Well, remember those :in and :out annotations?
They weren't mere documentation after all.

They are actually a trivial linear type system
that specify how certain input arguments
are actually objects being processed,
and how they correspond to an output result.

Linear here refers to Linear Logic,
a formalism build around are resources
that are preserved during computations.
In linear logic, every function uses
every linear argument value exactly once
in any actually executed branch.
When an object's value you have a new value for the object,
and can't use the old value anymore.
If you want to stick to the old value,
you have to make an explicit copy.
This exactly expresses the correspondance between
our pure functions and their stateful variants.
|#
(slide #:title (title "Linear Types")
  (code
   (:generic lookup
     (<map> map key) (:in 1)
     (:values value foundp))

   (:generic stateful:insert
     (<map> map key value) (:in 1)
     (:values)             (:out t))

   (:generic stateful:empty
     (<map>)
     (:values object) (:out 0))))
#|
And so in the stateful functions,
you see that some arguments are side-effected
rather than new values being passed.

Notice that we chose three simple and representative functions:
a read-only function,
a side-effecting function,
and a constructor.
|#
(slide #:title (title "Pure to Stateful")
  (code
(defmethod stateful:insert
    ((<interface> <mutating-map>) map key value)
  (let* ((<pure-interface>
          (pure-interface <interface>))
         (pure-map (box-value map))
         (updated-map
          (pure:insert <pure-interface>
                       pure-map key value)))
      (set-box-value _updated-map map)
      (values)))))
#|
And so here is the insert method.

We unbox the input map,
we call the pure method,
and we update the box with the updated map value.
Note that there is a bug in the printed version of the article.
The slide correctly specifies _update-map as an argument to set-box-value.
The macro expansion also does the right thing.
It's my fault in the paper to not properly proofread the result
of my partly copy-pasted macro expansion cleanup.
|#
(slide #:title (title "Pure to Stateful")
  (code
(defmethod stateful:empty
    ((<interface> <mutating-map>))
  (let* ((<pure-interface>
          (pure-interface <interface>))
         (pure-empty
          (pure:empty <pure-interface>)))
    (box! pure-empty)))
))
#|
Finally, here is the empty method.

It trivially creates a new box with an empty value.
|#
(slide #:title (title "Limits of our Trivial Types")
  (code
(:method> stateful:divide/list (map)
   (let ((list
         (pure:divide/list
           (pure-interface <mutating-map>)
           (box-value map))))
     (when list
       (set-box-value (first list) map)
       (cons map
             (mapcar #'box! (rest list))))))
))
#|
Out of over a dozen interface functions that were designed
before I wrote the transformations,
most fit our trivial linear type system.
Unhappily, two fail: join/list and divide/list
that respectively take as argument and return as result
a list of submaps.
My type system can't express the preservation of identity in list arguments.
Therefore I explicitly wrote methods for these functions.

Here is divide/list that we used earlier
in our generic divide and conquer reduction algorithm.

We unbox the pure map, call the pure method,
and we box each of the results.
To preserve the contract,
we specially preserve the identity of the original map
with the first submap as its new value.
|#
(slide #:title (title "Useful in Practice!")
  (t "How we do stateful:alist"))
#|
This transformation is not just an idle exercise.
In practice, that's how we implement stateful alists,
as the boxing of pure alists;
If you want to avoid boxing, you run into trouble
when you try to side-effect an empty alist,
which is represented as the constant NIL.

Or you may find that sometimes you want to be able
to hold on to old values of your objects;
in that case, it can be much cheaper to use
the mutating variant of a pure data structure
than to do a deep copy of your object every time.
|#
(slide #:title (title "Stateful to Pure")
  (code
(in-package :pure)
(define-linearized-interface
  <linearized-map> (pure:<map>) (stateful:<map>)
  ()
  (:method> join/list (list) ...)
  (:method> divide/list (map)
     (let ((list
            (stateful:divide/list
             (stateful-interface <linearized-map>)
             (box-ref map))))
       (and list
            (mapcar 'one-use-value-box list))))
  (:parametric (interface)
    (make-interface
      :stateful-interface interface)))
))
#|
The transformation from Stateful to Pure is similar,
with the same manual method definitions.
|#
(slide #:title (title "Stateful to Pure")
  (code
(defmethod lookup
    ((<interface> <linearized-map>) map key)
  (let ((<stateful-interface>
         (stateful-interface <interface>))
        (stateful-map (box-value map)))
    (lookup <stateful-interface>
            stateful-map key)))))
#|
Here is the lookup method.
It is exactly the same as for the pure to stateful transformation.
That's because it's read-only either way.
|#
(slide #:title (title "Stateful to Pure")
  (code
(defmethod pure:insert
    ((<interface> <linearized-map>) map key value)
  (let* ((<stateful-interface>
           (stateful-interface <interface>))
         (stateful-map (_box-ref map)))
     (stateful:insert <stateful-interface>
                      stateful-map key value)
     (let* ((updated-map
              (one-use-value-box stateful-map)))
       updated-map)))
))
#|
However, for the insert method,
see how we put new value in a fresh one-use-value-box.
We do not reuse the old box.
Instead we extract the value with box-ref instead of box-value,
and this invalidates the original one-use-value-box.
Therefore, anyone trying to use the old value will get an error.

Indeed, after we do the insertion in the stateful data structure,
the old value is gone.
If you wanted to hold on to the old value,
you should have made an explicit copy and used it.

That's because our two transformations are actually reverse isomorphisms,
but only in Linear Logic.
However, the Lisp language itself doesn't enforce the linearity constraint,
and it doesn't prevent you from using a variable or values more than once.
That's why we have those boxes to throw an error if you do it.
|#
(slide #:title (title "Stateful to Pure")
  (code
(defmethod empty
    ((<interface> <linearized-map>))
  (let* ((<stateful-interface>
           (stateful-interface <interface>))
         (empty-object
          (empty <stateful-interface>)))
    (one-use-value-box empty-object)))))
#|
Finally, the constructor simply puts the object in a box.
|#
(slide #:title (title "IPS to OOP")
  (code
(define-classified-interface-class
  >map< (object-box) stateful:<map>
  ((interface :initarg :interface))
  (:interface-argument (<interface> stateful:<map>)))
))

(slide #:title (title "Using code OOP style")
  (code
(let ((map (make-instance '>map<
                          :interface 'number-map)))
  (insert map 2010 50)
  (insert map 2012 60)
  (reduce #'+ (collection-values map)))
))
#|
Now, using the same Linear Type System,
we can automatically transform from @IPS to @OOP.

However, there's a catch: the treatment of constructors.

Here's how we could define a generic class >map<
parametrically from a <map> interface.

Objects are boxes that remember both
the interface used and the value for that interface.
|#
(slide #:title (title "IPS to OOP")
  (code
(defmethod lookup ((map >map<) key)
  (let ((<interface> (class-interface map))
        (map-data (box-ref map)))
    (interface:lookup <interface> map-data key)))))
#|
The read-only method would
trivially extract the interface and value from the box argument
and do the lookup.
|#
(slide #:title (title "IPS to OOP")
  (code
(defmethod insert ((map >map<) key value)
  (let ((<interface> (class-interface map))
        (map-data (box-ref map)))
    (stateful:insert <interface> map-data key value)))))
#|
Similarly, the side-effecting method
can simply extract the interface and datum from the argument,
and do the side-effect.
|#
(slide #:title (title "Issue with Constructors!")
  (code
(defmethod empty ((<interface> stateful:<map>))
  (let ((empty-data
          (interface:empty <interface>)))
    (make-instance '>map<
                   :interface <interface>
                   :value empty-data)))))
#|
But the constructor doesn't have an object as argument,
it has an object as a result.
Where is it going to get its interface from???

That's where things are tricky.

In the parametric version of the >map< class,
constructors have to take an extra argument,
from which we can deduce the interface to use.
In this case, we directly pass the interface to use as argument.

Notice that in plain CLOS style, objects are constructed with
make-instance,
which takes a class or class-name as parameter.
In other words,
in traditional @OO style,
constructors are part of the meta-class protocol,
not of the class protocol.
Whereas in @IPS,
constructors are a normal part of the interface protocol,
that treats them uniformly just like other protocol functions.

That's another way that @IPS is much cleaner than traditional @OOP.
|#
(slide #:title (title "IPS to OOP, monomorphic constructor")
  (code
(define-classified-interface-class
  >nm< (object-box) stateful:<number-map>
  ((interface :initform stateful:<number-map>
  	      :allocation :class))
  (:interface-keyword nil)
  (:extract-interface stateful:<number-map>))
))
#|
Another solution to the problem would be that
constructors just use a constant value for the interface.
|#
(slide #:title (title "IPS to OOP, monomorphic constructor")
  (code
(defmethod empty ()
  (let* ((<interface> <number-map>)
         (empty-data
          (interface:empty <interface>)))
    (make-instance '>nm< :value empty-data)))))
#|
Then the constructor does not need to take an extra argument.

But when you want to have several classes share the same protocol,
you need to rename your constructors, for instance with a prefix or suffix,
so every class has differently named constructors.
|#
(slide #:title @title{Conclusion})
#|
I'd like to conclude with a few lessons I learned
while developing LIL.
|#
(slide #:title @title{1. LIL is useful library}
  (t "Data Structures not in other libraries") ~
  (t "A pleasure to work with!") ~ ~
  (bt "Use it! Extend it!"))
#|
First, @IPS is actually a great way
to develop and use a data structure library.

LIL is a great library.
It provides various data structures that are not
available in any other @CL library,
and you can use them in either pure or stateful context at your leisure.

If you need a new data structure, please contribute it to LIL,
instead of starting yet another library.
You'll find both that it's much easier to write,
and that the result is more powerful.
|#
(slide #:title @title{2. Polymorphism for Everyone}
  (t "Ad hoc polymorphism for pure computations") ~
  (t "Parametric polymorphism for objects") ~ ~
  (bt "Enjoy our composable abstractions!"))
#|
Second,
Ad hoc polymorphism is not just for imperative objects, and
Parametric polymorphism is not just for pure computations.
We can have both together, and we can build composable abstractions.
There is no reason to use any less powerful formalism.
|#
(slide #:title @title{3. Powerful Transformations}
  (t "From Pure to Stateful and Back") ~
  (t "From IPS to OOP") ~
  (bt "Macros FTW"))
#|
Three, thanks to the power of Lisp Macros,
@IPS does not have to be cumbersome,
and if your users still don't like @IPS,
they don't even have to use it.
You can write algorithms in your choice of style,
and users can use them in their choice of style.
|#
(slide #:title @title{4. Better Formalisms Matter}
  (t "IPS cleaner than traditional OOP") ~
  (t "Linear Logic is underrated") ~ ~
  (bt "Next: a Linear Lisp?"))
#|
Fourth,
the use of clean and simple formalisms
can really improve your coding experience.

I claim that @IPS is cleaner than traditional @OOP
and I invite you to give it a try.

I also claim that Linear Logic is underrated,
and my next project will be a Lisp variant
that provides Linearity.
This was suggested by @hbaker long ago.
I think the time has come.
If you're interested, I'm looking for help.
|#
(slide #:title @title{5. Simpler Semantics}
  (t "Interfaces were everywhere, just hidden") ~
  (t "Separate Meta-Data from Data") ~
  (bt "Expose Internals!"))
#|
Finally,
what makes these formalisms cleaner is that
they expose what is implicit in other formalisms.

All other variants of polymorphisms
had some equivalent of interfaces internally;
by trying to hide the interfaces,
they end up having horribly complex semantics,
with plenty of hard issues.
If you don't believe me,
just look at the attempts by Luca Cardelli
to provide clean semantics for object systems.
Ouch.

By keeping meta-data and data separate,
we eschew a lot of problems that other systems have.

And so I invite you always expose the guts of your system.
Have nothing to hide!
@IPS is in the long tradition of @CPS, State Passing Style, etc.,
in exposing the innards of existing systems for fun and profit.
Anything else results in an abstraction inversion,
to reuse another idea by Henry Baker.
In other words, if you don't expose your internals,
you'll find that when you need to reason about those internals,
you'll end up putting the cart before the horses.
|#
(slide #:title @title{Questions?}
  (url "http://github.com/fare/lisp-interface-library")
  (url "http://github.com/fare/lil-ilc2012")
  (code (quicklisp:quickload :lil))
  (t "Have fun!")
  ~
  (bt "Wanna hack Lisp? Apply at Google! [2014: kind of]"))
#|
Thank you for listening to my talk.

You can find the source code and my paper on github.

LIL is also available via quicklisp.

If I only had only thing to say about LIL,
it's that programming with it is fun!

If you need to use pure or stateful data structures,
I enjoin you to come use those available in LIL;
and if any you need are missing,
we can have fun together porting them to this framework.

Any questions?
|#

;;------>8------>8------>8------>8------>8------>8------>8------>8------>8------

#|
1- Be better prepared, and finish faster
2- Since the public knows ad hoc polymorphism, insist on parametric polymorphism
3- Be ready to field questions on performance:
   compare to existing libraries.
|#
