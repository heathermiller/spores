package scala.spores
package neg
package basic

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import util._

@RunWith(classOf[JUnit4])
class NegSpec {
  @Test
  def `wrong shape, incorrect val def list`() {
    expectError("Only val defs allowed at this position") {
      """
        import scala.spores._
        val v1 = 10
        val s: Spore[Int, Unit] = spore {
          val c1 = v1
          println("hi")
          (x: Int) => println(s"arg: $x, c1: $c1")
        }
      """
    }
  }

  @Test
  def `only allowed to capture paths 1`() {
    expectError("Only stable paths can be captured") {
      """
        import scala.spores._
        import Spore.capture
        def compute(x: Int): Int = x * 5
        val s: Spore[Int, String] = spore { (x: Int) =>
          val cc1 = capture(compute(2))
          s"arg: $x, cc1: $cc1"
        }
      """
    }
  }

  @Test
  def `only allowed to capture paths 2`() {
    expectError("Only stable paths can be captured") {
      """
        import scala.spores._
        import Spore.capture
        // this is a var:
        var v1: Int = 10
        val s: Spore[Int, String] = spore { (x: Int) =>
          val cc1 = capture(v1)
          s"arg: $x, cc1: $cc1"
        }
      """
    }
  }

}
