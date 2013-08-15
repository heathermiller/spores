package scala.spores
package neg
package basic

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class NegSpec {
  @Test
  def `missing symbol`() {
    expectError("not found: value kaboom") {
      """
        | kaboom
      """.stripMargin
    }
  }
}
