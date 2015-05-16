package scala.spores
package run
package pickling

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.pickling._
import Defaults._
import json._

import SporePickler._

@RunWith(classOf[JUnit4])
class PicklingSpec {
  @Test
  def `pickle/unpickle to/from JSON`(): Unit = {
    val v1 = 10
    val s = spore {
      val c1 = v1
      (x: Int) => s"arg: $x, c1: $c1"
    }

    val pickler: Pickler[Spore[Int, String] { type Captured = Int }] with Unpickler[Spore[Int, String] { type Captured = Int }] =
      SporePickler.genSporePickler[Int, String, Int]

    val format = implicitly[PickleFormat]
    val builder = format.createBuilder

    builder.hintTag(implicitly[FastTypeTag[Spore[Int, String]]])
    pickler.pickle(s, builder)
    val res = builder.result()

    assert(res.value.toString.endsWith("""
  "captured": 10
}"""))

    val reader = format.createReader(res.asInstanceOf[format.PickleType])
    val up = pickler.unpickle("scala.spores.Spore[Int, String]", reader)
    val us = up.asInstanceOf[Spore[Int, String]]
    val res2 = us(5)
    assert(res2 == "arg: 5, c1: 10")
  }

  @Test
  def `simplified spore pickling`(): Unit = {
    val v1 = 10
    val s = spore {
      val c1 = v1
      (x: Int) => s"arg: $x, c1: $c1"
    }

    val pickler = SporePickler.genSporePickler[Int, String, Int]
    val builder = implicitly[PickleFormat].createBuilder
    val res = {
      builder.hintTag(implicitly[FastTypeTag[Spore[Int, String]]])
      pickler.pickle(s, builder)
      builder.result()
    }

    assert(res.value.toString.endsWith("""
      |  "captured": 10
      |}""".stripMargin))
  }

  @Test
  def `pickling spore with two captured variables`(): Unit = {
    val v1 = 10
    val v2 = "hello"
    val s = spore {
      val c1 = v1
      val c2 = v2
      (x: Int) => s"arg: $x, c1: $c1, c2: $c2"
    }

    val pickler = SporePickler.genSporeCMPickler[Int, String, (Int, String)]
    val format  = implicitly[PickleFormat]
    val builder = format.createBuilder
    val res = {
      builder.hintTag(implicitly[FastTypeTag[Spore[Int, String]]])
      pickler.pickle(s, builder)
      builder.result()
    }

    assert(res.value.toString.endsWith(""""captured": {
      |    "$type": "scala.Tuple2[scala.Int,java.lang.String]",
      |    "_1": 10,
      |    "_2": "hello"
      |  }
      |}""".stripMargin))

    val reader = format.createReader(res.asInstanceOf[format.PickleType])
    val up = pickler.unpickle("scala.spores.Spore[Int, String]", reader)
    val us = up.asInstanceOf[Spore[Int, String]]
    val res2 = us(5)
    assert(res2 == "arg: 5, c1: 10, c2: hello")
  }

  @Test
  def `simple pickling of spore with two captured variables`(): Unit = {
    val v1 = 10
    val v2 = "hello"
    val s = spore {
      val c1 = v1
      val c2 = v2
      (x: Int) => s"arg: $x, c1: $c1, c2: $c2"
    }
    val res  = s.pickle
    val up   = res.unpickle[Spore[Int, String]]
    val res2 = up(5)
    assert(res2 == "arg: 5, c1: 10, c2: hello")
  }

  @Test
  def `simple pickling of spore with one parameter`(): Unit = {
    val s = spore { (l: List[Int]) => l.map(_ + 1) }
    val res  = s.pickle
    val up   = res.unpickle[Spore[List[Int], List[Int]]]
    val res2 = up(List(1, 2, 3))
    assert(res2.toString == "List(2, 3, 4)")
  }

  @Test
  def `simple pickling of spore with two parameters`(): Unit = {
    val s = spore {
      (x: Int, s: String) => s"arg1: $x, arg2: $s"
    }
    val res  = s.pickle
    val up   = res.unpickle[Spore2[Int, String, String]]
    val res2 = up(5, "hi")
    assert(res2 == "arg1: 5, arg2: hi")
  }

  @Test
  def `simple pickling of spore with two parameters and two captured variables`(): Unit = {
    val v1 = 10
    val v2 = "hello"
    val s = spore {
      val c1 = v1
      val c2 = v2
      (x: Int, y: String) => s"args: $x, $y, c1: $c1, c2: $c2"
    }
    val res  = s.pickle
    val up   = res.unpickle[Spore2[Int, String, String]]
    val res2 = up(5, "hi")
    assert(res2 == "args: 5, hi, c1: 10, c2: hello")
  }

  @Test
  def `simple pickling of spore with three parameters`(): Unit = {
    val s = spore {
      (x: Int, s: String, c: Char) => s"arg1: $x, arg2: $s, arg3: $c"
    }
    val res  = s.pickle
    val up   = res.unpickle[Spore3[Int, String, Char, String]]
    val res2 = up(5, "hi", '-')
    assert(res2 == "arg1: 5, arg2: hi, arg3: -")
  }

  def doPickle[T <: Spore[Int, String]: Pickler: Unpickler](spor: T) = {
    val unpickler = implicitly[Unpickler[T]]
    val res       = spor.pickle
    val reader    = pickleFormat.createReader(res)
    val spor2     = unpickler.unpickleEntry(reader).asInstanceOf[Spore[Int, String]]
    assert(spor2(5) == spor(5))
    assert(spor2.getClass.getName == spor.getClass.getName)
  }

  @Test
  def testPickleUnpickleSporeWithTypeRefinement(): Unit = {
    val v1 = 10
    val v2 = "hello"
    val s = spore {
      val c1 = v1
      val c2 = v2
      (x: Int) => s"arg: $x, c1: $c1, c2: $c2"
    }
    doPickle(s)
  }
}
