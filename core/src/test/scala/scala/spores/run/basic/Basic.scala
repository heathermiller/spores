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



abstract class X { def g(f: Int => Unit) : Unit}
abstract class TestCl[T] {val x : X}


@RunWith(classOf[JUnit4])
class BasicSpec {


  /**
    * Test the following:
    * spore has variable a
    * a is part of PTT in spore
    * PTT needs to be correct, when 'a' is changed into x$macro$n,
    * PTT must change.
    *
    * Expansion with SporeConv.sporeConv is optional.
    */
  @Test
  def pTTParameterCapture(): Unit = {

    abstract class A {
      self =>
      type B
      val t: self.B
      def f(x: self.B) = {}
    }

    val s = spore{
      val y = 5
      (a: A) => {
        val k: a.B = a.t
        a.f(k)
      }
    }
  }

  /**
    * Same as pTTParameterCapture, but
    * the path-dependent type comes from a captured variable,
    * not from the parameter
    */
  @Test
  def pTTCapturedCapture(): Unit = {

    abstract class A {
      self =>
      type B
      val t: self.B
      def f(x: self.B) = {}
    }

    val s = spore{
      (a: A) => {
        val s2 = spore {
          val na = a
          (_: Unit) => {
            val k: na.B = na.t
            na.f(k)
          }
        }
      }
    }
  }

  /**
    * Same as pTTCapturedCapture, but with nullary spores (more code coverage)
    */
  @Test
  def pTTCapturedCaptureNullary(): Unit = {

    abstract class A {
      self =>
      type B
      val t: self.B
      def f(x: self.B) = {}
    }

    val s = spore{
      (a: A) => {
        val s2 = spore {
          val na = a
          delayed {
            val k: na.B = na.t
            na.f(k)
          }
        }
      }
    }
  }

  /**
    * Similar to pTTCapture(), but spores are nested.
    */
  @Test
  def nestedPTTCapture(): Unit = {

    abstract class A {
      self =>
      type B
      val t: self.B
      def f(x: self.B) = {}
    }

    val s = spore{
      val y = 5
      (_: Unit) => {
        spore {
          val y = 5
          (a: A) => {
            val k: a.B = a.t
            a.f(k)
          }
        }
        println()
      }
    }
  }



  @Test
  def sporeWithAnonClasses(): Unit = {

    class B {self => type C}

    val s = spore{
      (x: B) => {
        final class anon { type D = x.C }
        val y = new anon
      }
    }
  }




  @Test
  def `simple spore transformation`(): Unit = {
    val v1 = 10
    val v2 = 20
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

  @Test
  def testClassOfLiteral(): Unit = {
    val s = spore {
      val y = 3
      (x: Int) =>
        val name = classOf[C].getName
        name + (x * y)
    }
    assert(s(2).contains("C"))
  }

  // This is to test that path-dependent types in which captured arguments are part of the path are
  // transformed correctly.
  @Test
  def pathDependentTypes(): Unit = {
    class A {
      type B
      def f(x: B) = println(x)
    }

    val s = spore {
      (x: A) => {
        x.f(null.asInstanceOf[x.B])
      }
    }
  }

  // This is to test that path-dependent types in which captured arguments are part of the path are
  // transformed correctly.
  @Test
  def pathDependentTypesWithCapture(): Unit = {
    class A {
      type B
      def f(x: B) = println(x)
    }

    val y = 10

    val s = spore {
      val yy = y
      (x: A) => {
        x.f(null.asInstanceOf[x.B])
      }
    }
  }

  // This is to test that path-dependent types in which captured arguments are part of the path are
  // transformed correctly.
  @Test
  def pathDependentTypesWithNestedSpores(): Unit = {
    abstract class A {
      val c: {type B}
      def f(s: Spore[A, Unit]) = s(this)
      def g(x: c.B) = println(x)
    }

    val s = spore {
      (a: A) => {
        a.f(spore {
          val cap_a = a
          (a1: A) => {
            val cap_cap_a = cap_a
            a1.g(null.asInstanceOf[a1.c.B])
          }
        })
      }
    }
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

  @Test
  def testIssue4(): Unit = {
    val s = spore {
      val y = 3
      (x: Int) => x * y
    }
    assert(true)
  }
}
