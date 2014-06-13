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

    /*implicit*/ val pickler: SPickler[Spore[Int, String] { type Captured = Int }] with Unpickler[Spore[Int, String] { type Captured = Int }] =
    SporePickler.genSporePickler[Int, String, Int]

    val format = implicitly[PickleFormat]
    val builder = format.createBuilder

    val p = pickler.pickle(s, builder)
    val res = builder.result()

    assert(res.value == """{
  "tpe": "scala.spores.Spore[scala.Int,java.lang.String]",
  "className": "scala.spores.run.pickling.PicklingSpec$anonspore$macro$2$1",
  "c1": 10
}""")

    val reader = format.createReader(res.asInstanceOf[format.PickleType], scala.pickling.internal.currentMirror)
    val up = pickler.unpickle(???, reader)
    val us = up.asInstanceOf[Spore[Int, String]]
    val res2 = us(5)
    assert(res2 == "arg: 5, c1: 10")
  }

}
