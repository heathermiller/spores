package scala.spores
package run
package pickling

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.pickling._
import json._

@RunWith(classOf[JUnit4])
class PicklingSpec {
  @Test
  def `pickle to JSON`() {
    val v1 = 10
    val s = spore {
      val c1 = v1
      (x: Int) => s"arg: $x, c1: $c1"
    }

    /*implicit*/ val pickler: SPickler[Spore[Int, String] { type Captured = Int }] =
    SporePickler.genSporePickler[Int, String, Int]

    val format = implicitly[PickleFormat]
    val builder = format.createBuilder

    val p = pickler.pickle(s, builder)
    val res = builder.result()

    assert(res.value == """{
  "tpe": "scala.spores.Spore[scala.Int,java.lang.String]",
  "className": "anonspore$macro$2",
  "c1": 10
}""")
  }

}
