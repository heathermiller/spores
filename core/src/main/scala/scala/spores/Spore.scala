/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.spores

trait NullarySpore[+R] extends Function0[R] {

  /** The type of captured variables.
    *
    *  If this Spore captures multiple variables, this is
    *  a tuple type.
    */
  type Captured

  /** Enables creating an instance of a Spore subclass via reflection.
    */
  def className: String =
    _className

  protected[this] var _className: String =
    null
}

trait NullarySporeWithEnv[+R] extends NullarySpore[R] {

  /** Stores the environment of the Spore.
    *
    *  If the Spore captures multiple variables, this field
    *  stores a tuple.
    */
  val captured: Captured //= _

}

trait Spore[-T, +R] extends Function1[T, R] {

  /** The type of captured variables.
    *
    *  If this Spore captures multiple variables, this is
    *  a tuple type.
    */
  type Captured

  /** Enables creating an instance of a Spore subclass via reflection.
    */
  def className: String =
    _className

  protected[this] var _className: String = null
}

trait SporeWithEnv[-T, +R] extends Spore[T, R] {

  /** Stores the environment of the Spore.
    *
    *  If the Spore captures multiple variables, this field
    *  stores a tuple.
    */
  val captured: Captured // = _

}

trait Spore2[-T1, -T2, +R] extends Function2[T1, T2, R] {

  /** The type of captured variables.
    *
    *  If this Spore captures multiple variables, this is
    *  a tuple type.
    */
  type Captured

  /** Enables creating an instance of a Spore subclass via reflection.
    */
  def className: String =
    _className

  protected[this] var _className: String =
    null
}

trait Spore2WithEnv[-T1, -T2, +R] extends Spore2[T1, T2, R] {

  /** Stores the environment of the Spore.
    *
    *  If the Spore captures multiple variables, this field
    *  stores a tuple.
    */
  val captured: Captured // = _

}

trait Spore3[-T1, -T2, -T3, +R] extends Function3[T1, T2, T3, R] {

  /** The type of captured variables.
    *
    *  If this Spore captures multiple variables, this is
    *  a tuple type.
    */
  type Captured

  /** Enables creating an instance of a Spore subclass via reflection.
    */
  def className: String =
    _className

  protected[this] var _className: String =
    null
}

trait Spore3WithEnv[-T1, -T2, -T3, +R] extends Spore3[T1, T2, T3, R] {

  /** Stores the environment of the Spore.
    *
    *  If the Spore captures multiple variables, this field
    *  stores a tuple.
    */
  val captured: Captured // = _

}

class NullarySporeImpl[+R](val f: () => R) extends NullarySpore[R] {
  def apply(): R = f()
}

class SporeImpl[-T, +R](val f: T => R) extends Spore[T, R] {
  def apply(x: T): R = f(x)
  override def className = "SporeImpl"
}

class Spore2Impl[-T1, -T2, +R](val f: (T1, T2) => R)
    extends Spore2[T1, T2, R] {
  def apply(x1: T1, x2: T2): R = f(x1, x2)
}

class Spore3Impl[-T1, -T2, -T3, +R](val f: (T1, T2, T3) => R)
    extends Spore3[T1, T2, T3, R] {
  def apply(x1: T1, x2: T2, x3: T3): R = f(x1, x2, x3)
}
