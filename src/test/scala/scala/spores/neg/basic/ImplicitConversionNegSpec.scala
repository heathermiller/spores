package scala.spores
package neg
package basic

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import util._

@RunWith(classOf[JUnit4])
class ImplicitConversionNegSpec {

  @Test
  def `fails without SAM type inference`() {
    expectError("missing parameter type") {
      """
        import scala.spores._
        class Mappable[T](val x: T) {
          def map[S](f: Spore[T, S]): Mappable[S] =
            new Mappable(f(x))
          def flatMap[S](f: T => Mappable[S]) =
            new Mappable(f(x).x)
        }
        val m = new Mappable("hello")
        val m2: Mappable[Int] = m.map(s => s.length)
      """
    }
  }

}
