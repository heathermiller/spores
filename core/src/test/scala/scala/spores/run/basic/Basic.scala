package scala.spores
package run
package basic

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


class C {
  def m(i: Int): Any = "example " + i
}

trait D {
  def g = new C
}

package somepackage {
  package nested {
    object TopLevelObject extends D {
      val f = new C
    }
  }
}

@RunWith(classOf[JUnit4])
class BasicSpec {
  @Test
  def `simple spore transformation`(): Unit = {
    val v1 = 10
    val s: Spore[Int, String] = spore {
      val c1 = v1
      (x: Int) => s"arg: $x, c1: $c1"
    }

    assert(s(20) == "arg: 20, c1: 10")
  }

  @Test
  def testInvocationTopLevelObject1(): Unit = {
    val s = spore {
      (x: Int) =>
        val s1 = somepackage.nested.TopLevelObject.f.m(x).asInstanceOf[String]
        s1 + "!"
    }
    assert(s(5) == "example 5!")
  }

  @Test
  def testInvocationTopLevelObject2(): Unit = {
    val s = spore {
      (x: Int) =>
        val s1 = somepackage.nested.TopLevelObject.g.m(x).asInstanceOf[String]
        s1 + "!"
    }
    assert(s(5) == "example 5!")
  }
}


// this is just to test that `super` is judged by the framework as a stable path
class SuperTest {
  val name = "super test"
}

package stablePathPkg {
  object StablePathObj {
    val kitteh = "i can haz stable path"
  }
}

@RunWith(classOf[JUnit4])
class StablePathSpec extends SuperTest {
  override val name = "stable path spec"
  val v0 = 12

  @Test
  def `can capture this in a stable path`(): Unit = {
    val s: Spore[Int, String] = spore {
      (x: Int) => s"${capture(this.v0)}"
    }

    assert(s(42) == "12")
  }

  // we can't seem to have a super in paths because of S-1938, pity
  // https://issues.scala-lang.org/browse/SI-1938
  // @Test
  // def `can capture super in a stable path`() {
  //   val s: Spore[Int, String] = spore {
  //     (x: Int) => s"arg: $x, c1: ${capture(super.name)}"
  //   }

  //   assert(s(20) == "arg: 20, c1: super test")
  // }

  @Test
  def `can capture an innocuous simple stable path`(): Unit = {
    object Innocuous {
      val cute = "fluffy"
    }
    val s: Spore[Int, String] = spore {
      (x: Int) => s"${capture(Innocuous.cute)}"
    }

    assert(s(42) == "fluffy")
  }

  @Test
  def `can capture an innocuous stable path in a package`(): Unit = {
    val s: Spore[Int, String] = spore {
      (x: Int) => s"${capture(stablePathPkg.StablePathObj.kitteh)}"
    }

    assert(s(42) == "i can haz stable path")
  }
}
