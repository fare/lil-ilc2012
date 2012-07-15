Recent advances in CL build systems

ASDF
* de facto standard CL defsystem facility since 2002
* ASDF 2 (2010): more portable, configurable, robust
* Key: self-upgradability (see paper at ILC 2010)
* Since then, many bug fixes and new features

ASDF just works, everywhere
* ASDF 1 ran on: SBCL, CCL, CLISP, CMUCL, ECL, LW, ACL, GCL 2.7
* But many discrepancies and version skews between implementations
* ASDF 2.000: uniform support, same versions, same semantics
* Since then: added ABCL, SCL, XCL, Genera, Cormanlisp, RMCL

Plenty of small ASDF bug fixes
* innumerable corner cases, notably pathnames, upgrade
* better documentation, error messages
* better support windows, old macos
* vastly improved regression test suite

Notable ASDF feature fixes
* logical pathname support
* :force (system1 system2)
* :defsystem-depends-on :default-component-class
* system upgrade / reinitialization

New ASDF features:
* cl-source-file.cl, cl-source-file.lsp, source-file-type
* many exported utilities (notably for pathnames)
* better, cleaner hooks to the internals
* Notably enabling: Quicklisp integration

Recent new ASDF features:
* :force-not (system1 system2), require-system
* encodings ==> asdf-encodings
* around-compile ==> package-renaming
* compile-check ==> asdf-finalizers

ASDF-ENCODINGS
* asdf now supports :encodings :utf8, de facto standard
* backwards compatibility: default is :default
* for more, :defsystem-depends-on :asdf-encodings
* supports non-standard encodings, autodetection

PACKAGE-RENAMING
* local nicknames for packages?
* :around-compile, with-effective-package-renamings
* (!) a package's canonical name must exist at runtime
* e.g. FOO@rt#2 has nickname BAR around this file, BAR

LIST-OF
* You want a monomorphic list type (list-of string)
* expand to: (and list (satisfies list-of-string-p))
* define list-of-string-p as part of deftype expansion
* but side-effects from expansion not saved in FASL!

Solution: ASDF-Finalizers
* Relies on :around-compile, :compile-check
* (eval-at-toplevel '(ensure-list-of-predicate string))
* evaluates it now, pushes it to a queue of final forms
* (final-forms) at the end of the file empties queue

Extreme Makeover, Syntax Edition
* (defclass py (py-source-file)
    ((source-file-type :initform "py")
     (around-compile :initform "cl-py:with-py-syntax")))
* There you are, compiling another language as CL!

XCVB
* Presented at ILC 2009
* Also by me, before I took up ASDF maintainership
* ASDF limitations: serial side-effects in one-world
* XCVB: deterministic parallel file build in many worlds

Failure for XCVB to spread
* Still not massively deployed
* network effects: high barrier to adoption
* still not 10x better than ASDF
* meanwhile, ASDF has gotten better

Success of XCVB derivatives
* inferior-shell: no more shell scripts
* asdf-condition-control: only interesting warnings
* lisp-invocation: cross- compile, test, run software
* cl-launch, command-line-arguments: be shell client

XCVB TODO:
* Viably replace ASDF on all active implementations
* Integration with SLIME, with Quicklisp
* Intent-indexed, content-addressed LRU cache
* Concurrent backend
