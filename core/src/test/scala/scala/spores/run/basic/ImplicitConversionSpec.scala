package scala.spores
package run
package basic

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


object Global {
  var x: Int = 0
}

@RunWith(classOf[JUnit4])
class ImplicitConversionSpec {
  @Test
  def simpleImplicitConversion() {
    val v1 = 10

    val s: Spore[Int, String] = /*spore*/ { (x: Int) =>
      val cc1 = capture(v1)
      s"arg: $x, cc1: $cc1"
    }

    assert(s(20) == "arg: 20, cc1: 10")
  }

  @Test
  def `fails without SAM type inference`() {
    import scala.spores._
    class Mappable[T](val x: T) {
      def map[S](f: Spore[T, S]): Mappable[S] =
        new Mappable(f(x))
      def flatMap[S](f: T => Mappable[S]) =
        new Mappable(f(x).x)
    }
    val m = new Mappable("hello")
    val m2: Mappable[Int] = m.map(s => s.length)
  }

  // this should be rejected
  // @Test
  // def illegalCapture() {
  //   // this is a var:
  //   var v1: Int = 10
  //   val s: Spore[Int, String] = { (x: Int) =>
  //     val cc1 = capture(v1)
  //     s"arg: $x, cc1: $cc1"
  //   }
  // }

  @Test
  def nullarySpore(): Unit = {
    val s1: NullarySpore[Unit] = () => {}
    val s2: NullarySpore[Unit] = () => {
      Global.x = 10
    }
    val s3: NullarySpore[Int] = () => {
      40 + 2
    }
    s2()
    assert(Global.x == 10)
    assert(s3() == 42)
  }

}
