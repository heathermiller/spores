package scala.spores
package neg
package basic

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class NegSpec {
  @Test
  def `wrong shape, incorrect val def list`() {
    expectError("Only val defs allowed at this position") {
      """
        import scala.spores.Spore
        import scala.spores.Spore.spore
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