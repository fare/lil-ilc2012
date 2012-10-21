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

(slide #:title "ASDF and beyond"
   @bt{Building Common Lisp software}
   @bt{made slightly easier}
   ~ ~
   (para #:align 'center @t{François-René Rideau,} @it{Google})
   @t{ILC'2012}
   #;@url{http://fare.tunes.org/computing/asdf.scrbl})

(slide #:title "Problem"
@t{How to build large Lisp systems}
@t{Make it easy to create and maintain?}
@t{Make it easy to share and configure?}
@t{Make it reliable to reproduce?}
)

(slide #:title "Solutions"
@t{Antiquity}
@t{ASDF}
@t{ASDF 2, ASDF 2.25}
@t{Other solutions}
@t{XCVB}
)

(slide #:title "Antiquity"
@t{All in one file}
@t{Load script (QPX)}
@t{Old Defsystems}
)

(slide #:title "Antiquity: all in one file"
@t{no dependency issues}
@t{trivial configuration}
@t{doesn't scale}
@t{no separate compilation}
)

(slide #:title "Load Script"
@t{"compile and load A, then B, then C"}
@t{Allows for separate compilation}
@t{expensive to manage and configure}
@t{No or little incremental compilation}
)

(slide #:title "Defsystem"
@t{"Before you load A, compile B"}
@t{"Before you compile B, load C"}
@t{incremental build}
@t{hard to manage dependencies}
@t{still a pain to configure paths}
)

(slide #:title "ASDF"
@t{config: *central-registry*, *load-pathname*}
@t{declarative dependencies}
@t{incremental: transitive dependencies & timestamps}
@t{extensible: OO model}
@t{de facto standard CL defsystem facility since 2002}
)

(slide #:title "Issues with old ASDF:"
@t{Pathnames are not portable}
@t{No common configuration file}
@t{Plenty of bugs and performance issues}
@t{Extension is not declarative}
@t{Missing features or hooks}
)

(slide #:title "Stalemate with ASDF 1:"
@t{Everyone has his own subtly incompatible version}
@t{Once loaded, can't be changed}
@t{Can't depend on upgrade until everyone else upgrades}
@t{2009 rant: "Irresponsible Software"}
)

(slide #:title "ASDF 2 (2010)"
@t{more portable, configurable, robust}
@t{Key: self-upgradability (see paper at ILC 2010)}
@t{Also: pathname abstractions, configuration files}
@t{Since then, many bug fixes and new features}
)

(slide #:title "ASDF just works, everywhere"
@t{ASDF 1 ran on: SBCL, CCL, CLISP, CMUCL, ECL, LW, ACL, GCL 2.7}
@t{ASDF 2 added: ABCL, SCL, MKCL, XCL, Genera, Cormanlisp, RMCL}
@t{ASDF 2 works the same everywhere.}
@t{OSes: Unix, Windows, MacOS 9, Genera, JVM}
)

(slide #:title "ASDF 2: Plenty of small bug fixes"
@t{innumerable corner cases, notably pathnames, upgrade}
@t{better documentation, error messages}
@t{better support windows, old macos, Genera(!), URLs}
@t{clock skews, circularities}
@t{vastly improved regression test suite}
)

(slide #:title "ASDF 2: Notable ASDF feature fixes"
@t{logical pathname support}
@t{:force (system1 system2)}
@t{:defsystem-depends-on :default-component-class}
@t{system upgrade / reinitialization}
)

(slide #:title "ASDF 2 vs ASDF 1"
@t{Backwards compatible: "If it's not backwards..."}
@t{Almost no line untouched.}
@t{x3 size increase from ASDF 1 (200KB vs 76KB).}
@t{ASDF 2.25: +66% size increase from ASDF 2.000 (138KB).}
)

(slide #:title "ASDF 2: New Features"
@t{cl-source-file.cl, cl-source-file.lsp, source-file-type}
@t{better, cleaner hooks to the internals}
@t{Enable: Quicklisp integration, POIU, description}
@t{many utilities (notably for pathnames): see ASDF-UTILS}
)

(slide #:title "ASDF 2: Recent Features"
@t{:force-not (system1 system2), require-system}
@t{encodings ==> asdf-encodings}
@t{around-compile ==> package-renaming}
@t{compile-check ==> asdf-finalizers}
)

(slide #:title "ASDF-ENCODINGS"
@t{asdf now supports :encodings :utf8, de facto standard}
@t{For more, :defsystem-depends-on :asdf-encodings}
@t{supports non-standard encodings, autodetection}
@t{backwards compatibility: default is :default}
@t{Yet out of 700 systems in Quicklisp, <10 with issues.}
)

(slide #:title "ASDF-BUNDLE"
@t{Deliver a single FASL for a system}
@t{Easier delivery for system or sys+deps}
@t{Works on SBCL, CCL; hopefully still ECL}
@t{Unsupported: .so from cffi-wrapper (except on ECL?)}
@t{Already works, but could use more features}
@t{Good for blaze}
)

(slide #:title ":AROUND-COMPILE"
@t{Can set specials, packages, readtables, etc.}
@t{link model: not around loading}
@t{link model: not around entire modules}
)

(slide #:title "PACKAGE-RENAMING"
@t{local nicknames for packages?}
@t{:around-compile, with-effective-package-renamings}
@t{(!) a package's canonical name must exist at runtime}
@t{e.g. FOO.at.rt#2 has nickname BAR around this file, BAR}
)

(slide #:title "READER CHANGES:"
@t{LAMBDA-READER: Use λ, not LAMBDA}
@t{depends on named-readtables}
@t{READER-INTERCEPTION: your own arbitrary reader}
)

(slide #:title "Extreme Makeover, Syntax Edition"
 (code
  (defclass py (py-source-file)
    ((source-file-type
      :initform "py")
     (around-compile
      :initform "cl-py:with-py-syntax"))))
 @t{There you are, compiling another language as CL!}
)

(slide #:title "LIST-OF"
@t{You want a monomorphic list type (list-of string)}
@t{expand to: (and list (satisfies list-of-string-p))}
@t{define list-of-string-p as part of deftype expansion}
@t{but side-effects from expansion not saved in FASL!}
)

(slide #:title "Solution: ASDF-Finalizers"
@t{Relies on :around-compile, :compile-check}
(code(eval-at-toplevel '(ensure-predicate string)))
@t{evaluates it now, pushes it to a queue of final forms}
@t{(final-forms) at the end of the file empties queue}
)

(slide #:title "XCVB"
@t{Presented at ILC 2009}
@t{Also by me, before I took up ASDF maintainership}
@t{ASDF limitations: serial side-effects in one-world}
@t{XCVB: deterministic parallel file build in many worlds}
)

(slide #:title "Failure for XCVB to spread"
@t{Still not massively deployed}
@t{network effects: high barrier to adoption}
@t{still not 10x better than ASDF}
@t{meanwhile, ASDF has gotten much better}
)

(slide #:title "XCVB TODO:"
@t{Viably replace ASDF on all active implementations}
@t{Integration with SLIME, with Quicklisp}
@t{Intent-indexed, content-addressed LRU cache}
@t{Concurrent backend}
@t{Better delivery with FFI wrappers}
;;@t{Support for blaze!}
)

(slide #:title "Success of XCVB derivatives"
@t{inferior-shell: no more shell scripts}
@t{asdf-condition-control: only interesting warnings}
@t{lisp-invocation: cross- compile, test, run software}
@t{cl-launch, command-line-arguments: be shell client}
)
