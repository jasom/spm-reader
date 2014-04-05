SPM Reader
==========

SPM Reader is a toy example of an alternative syntax for lisp symbols.

Package names in lisp have the problem of being global only.  FOO::BAR always
refers to a symbol in the package "FOO"  Someone (I forget who) Noted that this
problem was solved for symbols by having packages.  I then thought "what would
happen if we used symbols instead of names to designate packages at read-time.
After learning about PJB's implementation of a lisp reader, I figured it would
be a few hours work to prototype something, and I was right.

How to specify symbols:

This is built on top of Common Lisp's existing package system, so each package
still has a global name.  To use this global name, just use a keyword as the
package part of the symbol:

 ':COMMON-LISP::CONS 
 ':COMMON-LISP:CONS 

However, use-package-as lets you alias a package to an arbitrary symbol.  See
spm-example.lisp for a simple example that uses 2 separate json libraries with
aliases.

_NB_ In order to override the lisp reader, I install a reader macro function
for every single character.  This has a RAM overhead of over 1GB on both sbcl
and ccl on my system.  Needless to say, using this in production systems is not
recommended
