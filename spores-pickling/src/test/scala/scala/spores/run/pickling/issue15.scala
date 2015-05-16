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
class Issue15Spec {
  @Test
  def test(): Unit = {
    val s = spore {
      val pre = "hello"
      l: List[Int] => l.map(x => s"$pre $x")
    }

    val d = s.pickle
    assert(d.value.toString.endsWith("""
      |    "value": "hello"
      |  }
      |}""".stripMargin))

    val us = d.unpickle[Spore[List[Int], List[String]]]
    assert(us(List(1, 2, 3)).toString == "List(hello 1, hello 2, hello 3)")
  }
}
