package scala.spores
package run
package pickling

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.pickling._
import binary._

trait Emitter[T] {
  def emit(v: T)(implicit pickler: SPickler[T], tag: FastTypeTag[T], unpickler: Unpickler[T]): Unit
  def done(): Unit
}

@RunWith(classOf[JUnit4])
class PicklingBinarySpec {
  @Test
  def `pickle/unpickle to/from binary`() {
    val v1 = 10
    val s = spore {
      val c1 = v1
      (x: Int) => s"arg: $x, c1: $c1"
    }

    val pickler: SPickler[Spore[Int, String] { type Captured = Int }] with Unpickler[Spore[Int, String] { type Captured = Int }] =
    SporePickler.genSporePickler[Int, String, Int]

    val format = implicitly[PickleFormat]
    val builder = format.createBuilder

    builder.hintTag(implicitly[FastTypeTag[Spore[Int, String]]])
    pickler.pickle(s, builder)
    val res = builder.result()

    val reader = format.createReader(res.asInstanceOf[format.PickleType], scala.reflect.runtime.currentMirror)
    val up = pickler.unpickle(???, reader)
    val us = up.asInstanceOf[Spore[Int, String]]
    val res2 = us(5)
    assert(res2 == "arg: 5, c1: 10")
  }

  @Test
  def `generated nested classes`() {
    val s = spore {
      (elem: (Int, List[String]), emit: Emitter[(Int, List[String])]) =>
        if (elem._1 == 0) emit.emit(elem)
    }
    assert(true)
  }
}
