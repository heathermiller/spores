# Spores

Scala Spores, safe mobile closures

## Updates since the first draft (June 16th, 2013) of SIP-21

Many users expressed concern that spores would be both unusable with for-
expression syntax, and would be incompatible (without a lot of boilerplate)
with normal closures or higher-order functions which take normal functions as
arguments.

### Capture Syntax

To remedy the incompatibility with for-expressions, we propose a new `capture`
syntax.

### Stable Paths

A **stable path** is an expression which only contains selections and
identifiers (no applications, for example), and for which each selected entity
is _stable_. In this context, _stable_ means that the entity or object in
question is introduced by object definitions or by value definitions of non-volatile
types.

Adapted from the Scala Language specification (section 3.1), a _path_ is
defined to be one of the following:

- `C.this`, where `C` references a class. The path `this` is taken as a shorthand for `C.this` where `C` is the name of the class directly enclosing the reference.
- `x` where `x` is a package.
- `p.x` where `p` is a path and `x` is a stable member of `p`. _Stable members_ are packages or members introduced by object definitions or by value definitions of non-volatile types. (Section 3.6 of the SLS.)
- `C.super.x` or `C.super[M].x` where `C` references a class and `x` references a stable member of the super class or designated parent class `M` of `C`. The prefix `super` is taken as a shorthand for `C.super` where `C` is the name of the class directly enclosing the reference.

A path refers to an object, that is, it ends with an identifier.

### Uglies

- Right now, `capture` method has to be imported. This is less than desirable, but it's this way because we couldn't figure out how to obtain the symbol of the capture method if capture is defined inside the `spores` package object. Error:

    [error] /Users/hmiller/Dropbox/git-shared/spores/src/main/scala/scala/spores/package.scala:81: type mismatch;
    [error]  found   : spores.type
    [error]  required: AnyRef
    [error] Note that spores extends Any, not AnyRef.
    [error] Such types can participate in value classes, but instances
    [error] cannot appear in singleton types or in reference comparisons.
    [error]     val captureSym = typeOf[spores.type].member(newTermName("capture"))
    [error]                             ^
    [error] one error found

