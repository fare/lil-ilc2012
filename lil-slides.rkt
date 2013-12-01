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
I am François-René Rideau, from Google.

You may have noticed my talk has a mystical sounding title.

I stand by every word.
|#

(cover2 0
 (t "Data Structure Library for Common Lisp")
 (t "in Interface-Passing Style"))
#|
I'll be presenting a Library, for Common Lisp, that provides Data Structures.
It is written in special style called Interface-Passing Style.
|#

(cover2 1
 (t "Parametric Polymorphism")
 (t "married with Ad hoc Polymorphism"))
#|
This style enables Parametric Polymorphism,
which allows for higher-order data structures.
It does it in a way fully integrated
with the Ad Hoc Polymorphism provided by CLOS,
the Common Lisp Object System.
|#

(cover2 2
 (t "Pure (and Stateful) Data Structures")
 (t "Ad hoc Polymorphism without State"))
#|
It enables a programming without object identity and mutable state,
by providing Pure Data Structures as well as Stateful Data Structures.
The interface objects it manipulates are themselves without identity.
|#
(cover2 3
 (t "Automatic Transformations between Styles")
 (t "Enlightening Experience"))
#|
And it automates the transformation of data structures
from one style to the other.

Finally developing the library for this paper
was enlightening indeed to me.
I hope I can share this enlightenment with you.
|#
(slide #:title (title "I. Interface Passing Style")
       ~ @t{(a.k.a. IPS)} ~)
#|
So let's start with Interface Passing Style.

As the name implies, it a style of programming
that involves passing around interfaces.
|#

(slide #:title (title "Polymorphism")
  (t "Abstraction of data structures") ~
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
|#
(slide #:title (title "Finite Maps")
  ~ (t "look up a key in a map") ~
  (code (lookup map key)) ~)
#|
Our main example is finite maps:
data structures that encode
a finite number of associations from key to value.

Ideally, users would just write
  (lookup map key)
and the system would find the associated value (if any).
|#
(slide #:title (title "Dispatch the correct algorithm")
  ~ (t "But is it an alist? a hash-table? A binary tree?") ~
  (code (lookup map key)) ~)
#|
But how is the system going to match the algorithm used
with the type of data structure used to encode the map?
|#
(slide #:title (title "Bad monorphism: incompatible protocols")
  (code (assoc map key) => pair?)
  (code (gethash key map) => value foundp)
  (t "binary tree: DIY"))
#|
Without any polymorphism,
implementers have to build each each data structure separately from scratch;
not only are the functions monorphic,
and users have to learn a slightly different protocol each time.

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
You could have all your monomorphic functions
follow a same calling convention.
That would be polymorphism at the meta-level;
but not at the language level.
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

You must "just" make sure you function lookup is properly bound
in the scope you call it.

Of course, manually binding each and every function in the protocol
in each and every scope is a lot of bureaucracy.
|#
(slide #:title (title "Parametric Polymorphism")
  (t "ML: Modules and Functors")
  ~
  (t "Haskell: Type Classes")
  ~
  (t "Racket: Units"))
#|
To reduce this bureaucracy,
modern functional programming languages support abstractions
that encapsulate the simultaneous binding of many functions and data types.

However, parametric polymorphism
is that it is often quite static and rigid, in that
the function values inside the scope are monomorphic.
This can be both a feature or a limitation.
|#

(slide #:title (title "Object Oriented Programming")
  (t "Use generic functions")
  ~
  (t "dispatch method on object meta-data")
  ~
  (code (lookup _map key)))
#|
The dual approach is to
dispatch the algorithm to use based on the object,
we can dispatch the correct algorithm based on the data value.

In Object-Oriented Programming, or OOP,
the map data structure contains
some self-descriptive meta-information, its class.

Lookup is a generic function,
it will examine the class of the object passed as its map argument,
and select the correct combination of methods to use.
|#
(slide #:title (title "Implementing Polymorphism")
  (t "Reduction to monomorphism")
  ~
  (t "FP: dictionary, linkage table")
  ~
  (t "OOP: self, virtual method table"))
#|
One way or another,
whether at compile-time or runtime,
polymorphism is implemented by resolving any relevant binding
and dispatching an appropriate monomorphic method.
This dispatch is necessarily based on an extra piece of meta-data,
that has to be passed around, even if only at compile-time:
a dictionary of functions,
a virtual method table,
a self object reference,
a method resolution context.
|#   
(slide #:title (title "Interface Passing Style")
  (t "Expose the internals") ~
  (t "Reconcile FP and OOP"))
#|
In Interface Passing Style, we explicitly reify that meta-data,
and call it an interface,
exposing what is a usually a hidden internal.

We thereby contribute to reconciling
Functional Programming and Object-Oriented Programming.
|#
(slide #:title (title "Interface Passing Style")
  (t "IPS: One little extra argument for the meta-data")
  ~
  (code (lookup _<interface> map key)))
#|
To the user, Interface-Passing Style is nothing but
explicit passing around such meta-data as an extra argument
to generic functions.
|#
(slide #:title (title "Using an alist")
 (code
  (lookup <alist>
    '((2009 . "BOS") (2010 . "RNO") (2012 . "KYO"))
    2010)
     =>
     ?))
#|
The most trivial implementation for a finite map in Lisp
is an association list, or alist,
which is a list of key-value pairs.
Here, we have an alist that maps year
to city code of the corresponding recent ILC conference.

Here we pass the <alist> interface to the function lookup,
so it knows that the map argument is an alist.

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

What happens if you insert a key and value in an alist?
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
the built-in hash-table data structure is stateful:
insert happens through side-effect;
it does not return a new one, it modifies the existing one.
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
then the interface <alist> is actually the same thing as (<alist> <eql>).
Note that the former <alist> is
a variable bound to the default <alist> interface,
whereas the latter <alist> is a function to construct
<alist> interfaces with specialized equality comparison.
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
We could also use the <string> interface.
<string> knows how to compare strings for equality or for order.
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
In @IPS, you can build it at runtime,
based on user-provided information.

In standard @OOP and without Parametric Polymorphism,
you would have to define at compile-time
a new class for every kind of alist that your program uses.
If the parameter space is very large
(e.g. user-specified Unicode collation order),
or infinite,
then you can't directly express your class without Parametric Polymorphism.
|#
(slide #:title (title "Passing Interfaces Around"))
#|
We have seen how to make individual calls to interface functions.
Let's combine them into larger functions and see
how to write polymorphic code.
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
This function computes the sum the values in a pure map.
It uses a polymorphic divide-and-conquer strategy.

It relies on two interface functions,
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
Also note that with my system LAMBDA-READER,
you could actually use the symbol λ instead of LAMBDA.

But the takeaway is that you can write very polymorphic algorithms
that work on arbitrary data structures.
All I rely on here is a pure map from arbitrary keys to numbers.

I could generalize numbers to any monoid;
I would have to abstract away the sum operator into another interface;
that codomain interface could be passed as a second argument,
or extracted from the map interface.
|#
(slide #:title (title "Functions of multiple interfaces")
  (t "A few functions have multiple interface arguments")
  (code (convert <dest> <orig> object))
  (t "Mostly use higher-order interfaces")
  (code
   (defgeneric <federate> (<db1> <db2>)
     (:documentation
      "Create federated db interface"))))
#|
This leads me to functions with multiple interface parameters.

Some functions, such as the convert function,
that creates an object that follows the destination interface
from an object that follows the origin interface.

But most functions of multiple interfaces are
constructors for more elaborate interfaces.
This is consistent with separating meta-data level computations
from data-level computations.

For instance, a hypothetical <federate> function
would create an interface for a federated database
from interfaces of the elementary databases.
|#
(slide #:title (title "Multiple interfaces")
  (t "Most functions take one interface as input")
  (code (join <map> map1 map2)))
#|
But most functions take one and only one interface object as an argument,
the first argument.

For instance, the join function that merges two maps into one
takes only one interface argument;
all maps follow the specified representation strategy,
and the algorithm takes advantage of that for performance.

When dealing with multiple interfaces, the recommended solution
is to combine the multiple interfaces into one using parametric polymorphism,
then use the combined interface.

Interface combination is typically completed in an early phase of computation,
often at compile-time.
|#
(slide #:title (title "Beware! No Type Checking")
  (t "No implicit check")
  (code (check-invariant <type> object)) ~
  (t "Runtime errors — or worse") ~
  (t "No integration to typep (yet)"))
#|
Following the tradition of Common Lisp,
our library does NOT provide static typechecking.

You can EXPLICITLY check invariants with the method check-invariant
of the <type> interface.
However, its cost is usually proportional to the size of objects,
so it is not good around every operation.

It is great for testing, and helped me found many bugs,
and probably good around module boundaries.

So what happens when you run code with the wrong argument?
sometimes, a runtime error;
sometimes, worse: the wrong answer.

That's the usual Lisp attitude:
great power, and with it, great responsibility.

[Finally, there is no integration CL's typep yet.
It would involve using ASDF-FINALIZERS.]
|#

(slide #:title (title "Defining Interfaces"))
#|
We've seen how to use interfaces,
Defining interfaces is rather straightforward.
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
(defmethod insert
    ((_<i> <hash-table>) _map key value)
  (let ((hash (hash (key-interface <i>) key)))
    (insert
     (_hashmap-interface _<i>) _map hash
     (insert
      (bucketmap-interface <i>)
      (multiple-value-bind (bucket foundp)
           (lookup (_hashmap-interface _<i>)
	           _map hash)
         (if foundp
	     bucket
	     (empty (bucketmap-interface <i>))))
      key value))))))
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
(slide #:title (title "Making interfaces implicit")
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
(slide #:title (title "Making interfaces implicit")
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
(slide #:title (title "Making interfaces implicit")
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
  pure:<hash-table> pure:<map>)))
#|
Here is a trivial use of the macro.

We define a package pure-hash-table that export all the functions we want,
then in that package we simply evaluate the macro,
and that's all.

This way, you can create your data structures
with all the power of @IPS,
and export them for use by your customers
who don't want to hear anything about @IPS.
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
  (let* ((<pure-interface>
           (pure-interface <interface>))
         (pure-map (box-value map)))
    (multiple-value-bind (value foundp)
        (lookup <pure-interface>
	        pure-map key)
      (values value foundp))))
))
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
     (:values) (:out t))

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
         (pure-map (box-value map)))
    (multiple-value-bind (updated-map)
        (pure:insert <pure-interface>
		     pure-map key value)
      (set-box-value _updated-map map)
      (values)))))
(t "Erratum p. 72 section 4.2.3"))
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
           (pure-interface <interface>)))
    (multiple-value-bind (pure-empty)
        (pure:empty <pure-interface>)
      (let* ((empty-object (box! pure-empty)))
        empty-object))))))
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
Our of over a dozen interface functions that were designed
before I wrote the transformations,
most fit our trivial linear type system.
Unhappily, two fail: join/list and divide/list
that respectively take as argument and return as result
a list of submaps.
My type system can't express the preservation of identity in list arguments.
Therefore I have explicitly write methods for these functions.

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
  (let* ((<stateful-interface>
           (stateful-interface <interface>))
         (stateful-map (box-value map)))
    (multiple-value-bind (value foundp)
        (lookup <stateful-interface>
	        stateful-map key)
      (values value foundp))))
))
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
       updated-map))))
(t "Erratum p. 73 section 4.2.5"))
#|
However, for the insert method,
see how we put new value in a fresh one-use-value-box.
We do not reuse the old box.
Instead we we extract the value with box-ref instead of box-value,
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
           (stateful-interface <interface>)))
    (multiple-value-bind (empty-object)
        (empty <stateful-interface>)
      (let* ((one-use-empty
               (one-use-value-box empty-object)))
	one-use-empty))))
))
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
  (let* ((<interface> (class-interface map))
         (map-data (box-ref map)))
    (multiple-value-bind (value foundp)
        (interface:lookup <interface> map key)
      (values value foundp))))
))
#|
The read-only method would
trivially extract the interface and value from the box argument
and do the lookup.
|#
(slide #:title (title "IPS to OOP")
  (code
(defmethod insert ((map >map<) key value)
  (let* ((<interface> (class-interface map))
         (map-data (box-ref map)))
    (stateful:insert <interface> map key value)
    (values)))
))
#|
Similarly, the side-effecting method
can simply extract the interface and datum from the argument,
and do the side-effect.
|#
(slide #:title (title "Issue with Constructors!")
  (code
(defmethod empty ((<interface> stateful:<map>))
  (multiple-value-bind (empty-data)
      (interface:empty <interface>)
    (let* ((object (make-instance '>map<
                    :interface <interface>
		    :value empty-data)))
      object)))))
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
  (let* ((<interface> <number-map>))
    (multiple-value-bind (empty-data)
        (interface:empty <interface>)
      (let* ((object (make-instance '>nm<
      	    	        :value empty-data)))
        object))))))
#|
Then the constructor does not need to take an extra argument.

But then you want to have several classes share the same protocol,
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
If you're interested, I'd looking for help.
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
Anything else is results in an abstraction inversion,
to reuse another idea by Henry Baker.
In other words, if you don't expose your internals,
you'll find that when you need to express those internals,
you'll end up putting the cart before the horses.
|#
(slide #:title @title{Questions?}
  (url "http://github.com/fare/lisp-interface-library")
  (url "http://github.com/fare/lil-ilc2012")
  (code (quicklisp:quickload :lil))
  (t "Have fun!")
  ~
  (bt "Wanna hack Lisp? Apply at Google!"))
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
