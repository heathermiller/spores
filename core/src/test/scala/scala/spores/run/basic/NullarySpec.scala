package scala.spores
package run
package basic

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class NullarySpec {
  @Test
  def simpleNoEnv(): Unit = {
    val s: NullarySpore[Int] = spore {
      delayed {
        42
      }
    }
    assert(s() == 42)
  }

  @Test
  def simpleEnv(): Unit = {
    val v1 = 10
    val s: NullarySpore[String] = spore {
      val c1 = v1
      delayed {
        s"c1: $c1"
      }
    }
    assert(s() == "c1: 10")
  }

  @Test
  def testInvocationTopLevelObject1(): Unit = {
    val s = spore {
      delayed {
        val s1 = somepackage.nested.TopLevelObject.f.m(0).asInstanceOf[String]
        s1 + "!"
      }
    }
    assert(s() == "example 0!")
  }

  @Test
  def testInvocationTopLevelObject2(): Unit = {
    val s = spore {
      delayed {
        val s1 = somepackage.nested.TopLevelObject.g.m(0).asInstanceOf[String]
        s1 + "!"
      }
    }
    assert(s() == "example 0!")
  }
}
