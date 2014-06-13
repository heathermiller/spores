/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.spores

trait NullarySpore[+R] extends Function0[R]

trait Spore[-T, +R] extends Function1[T, R] {
  def className: String
}

trait SporeC1[-T, +R] extends Spore[T, R] {
  type Captured
  var c1: Captured
}

trait Spore2[-T1, -T2, +R] extends Function2[T1, T2, R]

trait Spore3[-T1, -T2, -T3, +R] extends Function3[T1, T2, T3, R]

class NullarySporeImpl[+R](f: () => R) extends NullarySpore[R] {
  def apply(): R = f()
}

class SporeImpl[-T, +R](f: T => R) extends Spore[T, R] {
  def apply(x: T): R = f(x)
  def className = "SporeImpl"
}

class Spore2Impl[-T1, -T2, +R](f: (T1, T2) => R) extends Spore2[T1, T2, R] {
  def apply(x1: T1, x2: T2): R = f(x1, x2)
}

class Spore3Impl[-T1, -T2, -T3, +R](f: (T1, T2, T3) => R) extends Spore3[T1, T2, T3, R] {
  def apply(x1: T1, x2: T2, x3: T3): R = f(x1, x2, x3)
}