# Spores

Scala Spores, safe mobile closures: [SIP-21](http://docs.scala-lang.org/sips/pending/spores.html)

## Building Spores

The Spores project is built and tested using sbt. It has two modules:
`spores-core` and `spores-pickling`. The `spores-core` module contains the
core type definitions and the `spore` macro. The `spores-pickling` module
integrates Spores with [scala/pickling](https://github.com/scala/pickling)
by providing picklers for Spores.

To build the core Spores module:
```
> project spores-core
> compile
```

To run the test suite:
```
> test
```

## Get Spores

The `spores-core` and `spores-pickling` modules for Scala 2.11 are available
on Maven Central and Sonatype. You can use Spores in your sbt project by
simply adding the following dependency to your build file:

```scala
libraryDependencies += "org.scala-lang.modules" %% "spores-core" % "0.2.4"
```

To enable integration with Pickling, add the following dependency:

```scala
libraryDependencies += "org.scala-lang.modules" %% "spores-pickling" % "0.2.4"
```

Or you can just directly download the jar files ([spores-core](http://search.maven.org/remotecontent?filepath=org/scala-lang/modules/spores-core_2.11/0.2.4/spores-core_2.11-0.2.4.jar), [spores-pickling](http://search.maven.org/remotecontent?filepath=org/scala-lang/modules/spores-pickling_2.11/0.2.4/spores-pickling_2.11-0.2.4.jar)).

## Updates since the first draft (June 16th, 2013) of SIP-21

Many users expressed concern that spores would be both unusable with for-
expression syntax, and would be incompatible (without a lot of boilerplate)
with normal closures or higher-order functions which take normal functions as
arguments.

### Capture Syntax

To remedy the incompatibility with for-expressions, we propose a new `capture`
syntax. Here is an exemplary use in the context of a hypothetical
`DCollection` type:

```scala
def lookup(i: Int): DCollection[Int] = ...
val indices: DCollection[Int] = ...

for { i <- indices
      j <- lookup(i)
} yield j + capture(i)

trait DCollection[A] {
  def map[B](sp: Spore[A, B]): DCollection[B]
  def flatMap[B](sp: Spore[A, DCollection[B]): DCollection[B]
}
```


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

- Need to make it more convenient to create a nullary spore
- Should objects be allowed in paths? The reason is that they are initialized lazily, so if we don't allow lazy vals, then allowing objects (which could end up being initialized only when the spore is applied) doesn't make a lot of sense.

