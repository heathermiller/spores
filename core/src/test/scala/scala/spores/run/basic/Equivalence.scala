package scala.spores
package run
package basic

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.spores.SporeConv._

@RunWith(classOf[JUnit4])
class Equivalence {
  @Test
  def `nullary spore equivalence`(): Unit = {
    val v1 = 10
    def s(): NullarySpore[String] = spore {
      val c1 = v1
      () => s"arg: c1: $c1"
    }

    assert(s() == s())
  }

  @Test
  def `spore1 equivalence`(): Unit = {
    def s(v: Int) = spore {
      val c1 = v
      (x: Int) => s"arg: $x, c1: $c1"
    }

    assert(s(10) == s(10))
    assert(s(10) != s(20))
  }

  @Test
  def `spore1 equivalence without enclosed fields`(): Unit = {
    def s() = spore {
      (x: Int) => s"arg: $x"
    }

    assert(s() == s())
  }

  @Test
  def `spore2 equivalence`(): Unit = {
    val v1 = 10
    def s(): Spore2[Int, Int, String] = spore {
      val c1 = v1
      (x: Int, y: Int) => s"args: $x, $y, c1: $c1"
    }

    assert(s() == s())
  }

  @Test
  def `spore3 equivalence`(): Unit = {
    val v1 = 10
    def s(): Spore3[Int, Int, Int, String] = spore {
      val c1 = v1
      (x: Int, y: Int, z: Int) => s"args: $x, $y, $z, c1: $c1"
    }

    assert(s() == s())
  }


}
