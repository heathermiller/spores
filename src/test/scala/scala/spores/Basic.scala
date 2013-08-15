package scala.spores
package basic

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class BasicSpec {
  @Test
  def `simple spore transformation`() {
    val v1 = 10

    val s: Spore[Int, String] = spore {
      val c1 = v1
      (x: Int) => s"arg: $x, c1: $c1"
    }

    assert(s(20) == "arg: 20, c1: 10")
  }
}