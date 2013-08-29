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
}

//   @Test
//   def `no lazy vals allowed`() {
//     expectError("Only stable paths can be captured") {
//       """
//         import scala.spores._
//         import Spore.capture
//         lazy val v1 = 10
//         val s: Spore[Int, Unit] = spore {
//           (x: Int) => println(s"arg: $x, c1: ${capture(v1)}")
//         }
//       """
//     }
//   }
// }

@RunWith(classOf[JUnit4])
class StablePathNegSpec {
  @Test
  def `blocks aren't stable`() {
    expectError("Only stable paths can be captured") {
      """
        import scala.spores._
        val a = 12
        val s: Spore[Int, Unit] = spore {
          (x: Int) => capture({def x = ??? ; a })
        }
      """
    }
  }

  @Test
  def `only allowed to capture paths 1`() {
    expectError("Only stable paths can be captured") {
      """
        import scala.spores._
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
        // this is a var:
        var v1: Int = 10
        val s: Spore[Int, String] = spore { (x: Int) =>
          val cc1 = capture(v1)
          s"arg: $x, cc1: $cc1"
        }
      """
    }
  }

  @Test
  def `1 isn't a stable path`() {
    expectError("Only stable paths can be captured") {
      """
        import scala.spores._
        val s: Spore[Int, String] = spore { (x: Int) =>
          val capt = capture(1)
          s"$capt"
        }
      """
    }
  }

  @Test
  def `can't ascribe types in a stable path`() {
    expectError("Only stable paths can be captured") {
      """
        import scala.spores._
        val v = 10
        val s: Spore[Int, String] = spore { (x: Int) =>
          val capt = capture(v: Any)
          s"$capt"
        }
      """
    }
  }
}
