package scala.spores
package run
package pickling

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.pickling._
import Defaults._
import binary._

import SporePickler._


trait Emitter[T] {
  def emit(v: T)(implicit pickler: Pickler[T], unpickler: Unpickler[T]): Unit
  def done(): Unit
}

class TestEmitter extends Emitter[String] {
  val builder = new StringBuilder()
  def emit(v: String)(implicit pickler: Pickler[String], unpickler: Unpickler[String]): Unit =
    builder.append(v)
  def done(): Unit = {
    // do nothing
  }
}

@RunWith(classOf[JUnit4])
class PicklingBinarySpec {
  @Test
  def `pickle/unpickle to/from binary`(): Unit = {
    val v1 = 10
    val s = spore {
      val c1 = v1
      (x: Int) => s"arg: $x, c1: $c1"
    }

    val pickler: Pickler[Spore[Int, String] { type Captured = Int }] with Unpickler[Spore[Int, String] { type Captured = Int }] =
    SporePickler.genSporePickler[Int, String, Int]

    val format = implicitly[PickleFormat]
    val builder = format.createBuilder

    builder.hintElidedType(implicitly[FastTypeTag[Spore[Int, String]]])
    pickler.pickle(s, builder)
    val res = builder.result()

    val reader = format.createReader(res.asInstanceOf[format.PickleType])
    val up = pickler.unpickle("scala.spores.Spore[Int, String]", reader)
    val us = up.asInstanceOf[Spore[Int, String]]
    val res2 = us(5)
    assert(res2 == "arg: 5, c1: 10")
  }

  @Test
  def `generated nested classes`(): Unit = {
    val s = spore {
      implicit val p = implicitly[Pickler[(Int, List[String])]]
      implicit val u = implicitly[Unpickler[(Int, List[String])]]
      (elem: (Int, List[String]), emit: Emitter[(Int, List[String])]) =>
        if (elem._1 == 0) emit.emit(elem)
    }
    assert(true)
  }

  @Test
  def testSimpleSpore(): Unit = {
    val s: Spore[Int, Int] = spore { (x: Int) => x + 1 }
    val p = s.pickle
    val s2 = p.unpickle[Spore[Int, Int]]
    assert(s2(10) == 11)
  }

  @Test
  def testSpore2WithEnv(): Unit = {
    val maxSize = 20

    val s = spore {
        val chunkSize = maxSize / 2
        val chunkIndex = 0
        (elem: (String, Int), emit: Emitter[String]) =>
          val cond1 = elem._2 >= chunkIndex * chunkSize
          val plusOne = chunkIndex+1
          val cond2 = elem._2 < plusOne * chunkSize
          if (cond1 && cond2) emit.emit(elem._1)
    }

    val p = s.pickle
    val s2 = p.unpickle[Spore2[(String, Int), Emitter[String], Unit]]

    val testEmitter = new TestEmitter
    s2(("hello, " -> 0), testEmitter)
    s2(("world!" -> 9), testEmitter)
    assert(testEmitter.builder.toString == "hello, world!")
  }

  @Test
  def testSpore3WithoutEnv(): Unit = {
    val s = spore {
      (x: Int, y: String, z: Emitter[String]) =>
        if (x < 10) z.emit(y)
    }
    val p = s.pickle
    val s2 = p.unpickle[Spore3[Int, String, Emitter[String], Unit]]
    val testEmitter = new TestEmitter
    s2(9, "hello", testEmitter)
    assert(testEmitter.builder.toString == "hello")
  }

  @Test
  def testSpore3WithEnv(): Unit = {
    val maxSize = 10
    val factor  = 2
    val s = spore {
      val localMaxSize = maxSize
      val localFactor  = factor
      (x: Int, y: String, z: Emitter[String]) =>
        val limit = localMaxSize * localFactor
        if (x < limit) z.emit(y)
    }
    val p = s.pickle
    val s2 = p.unpickle[Spore3[Int, String, Emitter[String], Unit]]
    val testEmitter = new TestEmitter
    s2(19, "hello", testEmitter)
    assert(testEmitter.builder.toString == "hello")
  }
}
