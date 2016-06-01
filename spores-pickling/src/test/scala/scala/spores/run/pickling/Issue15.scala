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
import scala.pickling.json._
import SporePicklers._

@RunWith(classOf[JUnit4])
class Issue15Spec {

  @Test
  def test(): Unit = {
    val s = spore {
      val pre = "hello"
      l: List[Int] => l.map(x => s"$pre $x")
    }

    val d = s.pickle
    System.out.println(d.value.toString)
    assert(d.value.toString.endsWith("""
      |  "captured": {
      |    "$type": "java.lang.String",
      |    "value": "hello"
      |  }
      |}""".stripMargin))

    val us = d.unpickle[Spore[List[Int], List[String]]]
    assert(us(List(1, 2, 3)).toString == "List(hello 1, hello 2, hello 3)")
  }

}
