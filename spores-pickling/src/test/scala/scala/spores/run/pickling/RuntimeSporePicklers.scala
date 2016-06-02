/*                     __                                               *\
 **     ________ ___   / /  ___     Scala API                            **
 **    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
 **  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
 ** /____/\___/_/ |_/____/_/ | |                                         **
 **                          |/                                          **
\*                                                                      */

package scala.spores

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.pickling.Defaults._
import scala.pickling._
import scala.pickling.json._
import scala.pickling.pickler.AnyPicklerUnpickler
import SporePicklers._

@RunWith(classOf[JUnit4])
class RuntimeSporePicklers {

  val s = spore[Int, String] { (x: Int) =>
    s"arg: $x, c1: World"
  }
  val pickled = s.pickle

  @Test
  def `pickle a spore as Any`() = {

    /* Any is not able to detect pickler/unpickler for super types
     * and it turns out that a spore has its own unique class */
    val picklerSporeAnyAny = implicitly[Pickler[Spore[Any, Any]]]
    import scala.pickling.internal.currentRuntime
    val tag = FastTypeTag.makeRaw(s.getClass)
    currentRuntime.picklers.registerPickler(tag.key, picklerSporeAnyAny)

    // Pickle using reflection, yay!
    val pickled2 = s.asInstanceOf[Any].pickle
    val s2 = pickled2.unpickle[Spore[Int, String]]

    // Only test same behaviour, not same instance
    val n = scala.util.Random.nextInt()
    assert(s(n) == s2(n))
  }

  @Test
  def `unpickle a spore as Any`() = {

    // Unpickle using reflection, yay!
    val r1 = pickleFormat.createReader(pickled)
    val s2 = AnyPicklerUnpickler
      .unpickle("scala.spores.Spore[scala.Int,java.lang.String]", r1)
      .asInstanceOf[Spore[Int, String]]

    // Only test same behaviour, not same instance
    val n = scala.util.Random.nextInt()
    assert(s(n) == s2(n))
  }

  @Test
  def `pickle/unpickle a spore as Spore[Any,Any]`() = {

    // Pickle using reflection, yay!
    val pickled2 = s.asInstanceOf[Spore[Any, Any]].pickle
    val s2 = pickled2.unpickle[Spore[Any, Any]]

    // Only test same behaviour, not same instance
    val n = scala.util.Random.nextInt()
    assert(s(n) == s2(n))
  }

}
