package scala.spores
package run
package basic

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import Spore.capture

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
/*
  @Test
  def illegalCapture() {
    // this is a var:
    var v1: Int = 10
    val s: Spore[Int, String] = spore { (x: Int) =>
      val cc1 = capture(v1)
      s"arg: $x, cc1: $cc1"
    }
  }
*/
}
