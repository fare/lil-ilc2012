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

(slide #:title (title "ASDF and beyond")
   @bt{Building Common Lisp software}
   @bt{made slightly easier}
   ~ ~
   (para #:align 'center @t{François-René Rideau,} @it{Google}) ~
   @t{ILC'2012}
   #;@url{http://fare.tunes.org/computing/asdf.scrbl})

(slide #:title (title "Problem")
@t{Build a large Lisp system} ~
@t{Make it easy to create and maintain} ~
@t{Make it easy to share and configure} ~
@t{Make it reliable to reproduce})

(slide #:title (title "Solutions")
@t{Antiquity} ~
@t{ASDF} ~
@t{ASDF 2, ASDF 2.25} ~
@t{Other solutions} ~
@t{XCVB})

(slide #:title (title "Antiquity")
@t{All in one file} ~ ~
@t{Load script (QPX)} ~ ~
@t{Old Defsystems})

(slide #:title (title "Antiquity: all in one file")
@t{no dependency issues} ~
@t{trivial configuration} ~
@t{doesn't scale} ~
@t{no separate compilation})

(slide #:title (title "Load Script")
@t{"compile and load A, then B, then C"} ~
@t{Allows for separate compilation} ~
@t{expensive to manage and configure} ~
@t{No or little incremental compilation})

(slide #:title (title "Defsystem")
@t{"Before you load A, compile B"} ~
@t{"Before you compile B, load C"} ~
@t{incremental build} ~
@t{hard to manage dependencies} ~
@t{still a pain to configure paths})

(slide #:title (title "ASDF")
@para{cfg: @code[*central-registry*], @code[*load-pathname*]} ~
@t{declarative dependencies} ~
@t{incremental: transitive dependencies & timestamps} ~
@t{extensible: OO model} ~
@t{de facto standard CL defsystem facility since 2002})

(slide #:title (title "Issues with old ASDF:")
@t{Pathnames are not portable} ~
@t{No common configuration file} ~
@t{Plenty of bugs and performance issues} ~
@t{Extension is not declarative} ~
@t{Missing features or hooks})

(slide #:title (title "Stalemate with ASDF 1:")
@t{Everyone has his own subtly incompatible version} ~
@t{Once loaded, can't be changed} ~
@t{Can't depend on upgrade until everyone else upgrades} ~
@t{2009 rant: "Irresponsible Software"})

(slide #:title (title "ASDF 2 (2010)")
@t{more portable, configurable, robust} ~
@t{Key: self-upgradability (see paper at ILC 2010)} ~
@t{Also: pathname abstractions, configuration files} ~
@t{Since then, many bug fixes and new features})

(slide #:title (title "ASDF just works, everywhere")
@t{ASDF 1 ran on: SBCL, CCL, CLISP, CMUCL, ECL, LW, ACL, GCL 2.7} ~
@t{ASDF 2 added: ABCL, SCL, MKCL, XCL, Genera, Cormanlisp, RMCL} ~
@t{ASDF 2 works the same everywhere.} ~
@t{OSes: Unix, Windows, MacOS 9, Genera, JVM})

(slide #:title (title "ASDF 2: Plenty of small bug fixes")
@t{innumerable corner cases, notably pathnames, upgrade} ~
@t{better documentation, error messages} ~
@t{better support windows, old macos, Genera(!), URLs} ~
@t{clock skews, circularities} ~
@t{vastly improved regression test suite})

(slide #:title (title "ASDF 2: Notable ASDF feature fixes")
@t{logical pathname support} ~
@code[:force (system1 system2)] ~
@code[:defsystem-depends-on :default-component-class] ~
@t{system upgrade / reinitialization})

(slide #:title (title "ASDF 2 vs ASDF 1")
@t{Backwards compatible: "If it's not backwards..."} ~
@t{Almost no line untouched.} ~
@t{x3 size increase from ASDF 1 (200KB vs 76KB).} ~
@t{ASDF 2.25: +66% size increase from ASDF 2.000 (138KB).})

(slide #:title (title "ASDF 2: New Features")
@t{cl-source-file.cl, cl-source-file.lsp, source-file-type} ~
@t{better, cleaner hooks to the internals} ~
@t{Enable: Quicklisp integration, POIU, description} ~
@para{many utilities (notably pathnames): @code[ASDF-UTILS]})

(slide #:title (title "ASDF 2: Recent Features")
@para{@code[:force-not (system1 system2)], @code[require-system]} ~
@para{@code[:encoding] ==> @code[asdf-encodings]} ~
@para{@code[:around-compile] ==> @code[package-renaming]} ~
@para{@code[:compile-check] ==> @code[asdf-finalizers]})

(slide #:title (title "ASDF-ENCODINGS")
@para{now support for @code[:encodings :utf8], de facto standard} ~
@para{For more, @code[:defsystem-depends-on :asdf-encodings]} ~
@t{supports non-standard encodings, autodetection} ~
@para{backwards compatibility: default is @code[:default]} ~
@t{Yet out of 700 systems in Quicklisp, <10 with issues.})

(slide #:title (title "ASDF-BUNDLE")
@t{Deliver a single FASL for a system} ~
@t{Easier delivery for system or sys+deps} ~
@t{Works on SBCL, CCL; hopefully still ECL} ~
@para{Unsupported: @code[.so] from cffi-wrapper (except on ECL?)} ~
@t{Already works, but could use more features}
#;@t{Good for blaze})

(slide #:title (title ":AROUND-COMPILE")
@t{Can set specials, packages, readtables, etc.}  ~
@t{link model: not around loading} ~
@t{link model: not around entire modules})

(slide #:title (title "PACKAGE-RENAMING")
@t{local nicknames for packages?} ~
@para{@code[:around-compile], @code[with-effective-package-renamings]} ~
@t{(!) a package's canonical name must exist at runtime} ~
@para{e.g. @code[FOO.at.rt#2] has nickname @code[BAR] around this file})

(slide #:title (title "READER CHANGES:")
@para{LAMBDA-READER: Use @code[λ], not @code[LAMBDA]} ~
@para{uses @code[named-readtables] (future: @code[cl-syntax])} ~
@para{@code[READER-INTERCEPTION]: your own arbitrary reader})

(slide #:title (title "Extreme Makeover, Syntax Edition")
 (code
  (defclass py (py-source-file)
    ((source-file-type
      :initform "py")
     (around-compile
      :initform "cl-py:with-py-syntax")))) ~
 @t{There you are, compiling another language as CL!})

(slide #:title (title "LIST-OF")
@para{You want a monomorphic list type @code[(list-of string)]} ~
@para{expand to: @code[(and list (satisfies list-of-string-p))]} ~
@para{define @code[list-of-string-p] as part of @code[deftype] expansion} ~
@t{but side-effects from expansion not saved in FASL!})

(slide #:title (title "Solution: ASDF-Finalizers")
@para{Relies on @code[:around-compile], @code[:compile-check]} ~
(code(eval-at-toplevel '(ensure-predicate string))) ~
@t{evaluates it now, pushes it to a queue of final forms} ~
@para{@code[(final-forms)] at the end of the file empties queue})

(slide #:title (title "XCVB")
@t{Presented at ILC 2009} ~
@t{Also by me, before I took up ASDF maintainership} ~
@t{ASDF limitations: serial side-effects in one-world} ~
@t{XCVB: deterministic parallel file build in many worlds})

(slide #:title (title "Failure for XCVB to spread")
@t{Still not massively deployed} ~
@t{Network effects: high barrier to adoption} ~
@t{Still not 10x better than ASDF} ~
@t{Meanwhile, ASDF has gotten much better})

(slide #:title (title "XCVB TODO:")
@t{Viably replace ASDF on all active implementations} ~
@t{Integration with SLIME, with Quicklisp} ~
@t{Intent-indexed, content-addressed LRU cache} ~
@t{Concurrent backend} ~
@t{Better delivery with FFI wrappers}
#;@t{Support for blaze!})

(slide #:title (title "Success of XCVB derivatives")
@para{@code[inferior-shell]: no more shell scripts} ~
@para{@code[asdf-condition-control]: only interesting warnings} ~
@para{@code[lisp-invocation]: cross- compile, test, run software} ~
@para{@code[cl-launch], @code[command-line-arguments]: be shell client})
