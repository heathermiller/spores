package scala.spores
package run
package pickling

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.concurrent.Future

import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.json._

import SporePickler._


class LocalSilo[U, T <: Traversable[U]](val value: T) {
  def send(): Future[T] =
    Future.successful(value)
}

case class InitSiloFun[U, T <: Traversable[U]](fun: NullarySpore[LocalSilo[U, T]], refId: Int)


object GlobalFuns {
  private val summary = """
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt
in culpa qui officia deserunt mollit anim id est laborum."""

  private lazy val words = summary.replace('\n', ' ').split(" ")

  def randomWord(random: scala.util.Random): String = {
    val index = random.nextInt(words.length)
    words(index)
  }

  def populateSilo(numLines: Int, random: scala.util.Random): LocalSilo[String, List[String]] = {
    // each string is a concatenation of 10 random words, separated by space
    val buffer = collection.mutable.ListBuffer[String]()
    val lines = for (i <- 0 until numLines) yield {
      val tenWords = for (_ <- 1 to 10) yield randomWord(random)
      buffer += tenWords.mkString(" ")
    }
    new LocalSilo(buffer.toList)
  }
}


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

  @Test
  def `simple pickling of nullary spore`(): Unit = {
    val s = spore { delayed { List(2, 3, 4) } }
    val res  = s.pickle
    val up   = res.unpickle[NullarySpore[List[Int]]]
    val res2 = up()
    assert(res2.toString == "List(2, 3, 4)")
  }

  @Test
  def `simple pickling of nullary spore with one captured variable`(): Unit = {
    val x = 1
    val s = spore {
      val localX = x
      delayed {
        List(1, 2, 3).map(_ + localX)
      }
    }
    val res  = s.pickle
    val up   = res.unpickle[NullarySpore[List[Int]]]
    val res2 = up()
    assert(res2.toString == "List(2, 3, 4)")
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

  @Test
  def `pickle case class with nullary spore`(): Unit = {
    val s: NullarySpore[LocalSilo[String, List[String]]] = spore {
      delayed {
        GlobalFuns.populateSilo(10, new scala.util.Random(100))
      }
    }

    val init = InitSiloFun(s, 5)
    val p = init.pickle
    val up = p.unpickle[InitSiloFun[String, List[String]]]
    assert(up.refId == 5)
    assert(up.fun().value.size == 10)
  }
}
