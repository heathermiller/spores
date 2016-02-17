package scala.spores
package run
package basic

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.spores.SporeConv._


@RunWith(classOf[JUnit4])
class ExcludedBasic {

  @Test
  def spore2Avoidance(): Unit = {
    val s: Spore2[Int, Unit, Unit] {type Excluded = String} = spore {
      (_: Int, _: Unit) => {}
    }
  }

  @Test
  def spore2AvoidanceMultiple(): Unit = {
    class A {
      type R
    }
    val sA = spore {
      (a: A) => {
        val s: Spore2[Int, Unit, Unit] {type Excluded = a.R} = spore {
          val y = 0.5
          (_: Int, _: Unit) => {}
        }
      }
    }
  }

  @Test
  def spore2AvoidanceMultipleNoCapture(): Unit = {
    class A {
      type R
    }
    val sA = spore {
      (a: A) => {
        val s: Spore2[Int, Unit, Unit] {type Excluded = a.R} = spore {
          (_: Int, _: Unit) => {}
        }
      }
    }
  }

  @Test
  def spore2AvoidanceCapture(): Unit = {
    val s: Spore2[Int, Unit, Unit] {type Excluded = String} = spore {
      val y = 0.5
      (_: Int, _: Unit) => {}
    }
  }


  @Test
  def nullaryAvoidance(): Unit = {
    val s: NullarySpore[Unit] {type Excluded = String} = spore {
      delayed {
        ()
      }
    }
  }

  @Test
  def nullaryAvoidanceCapture(): Unit = {
    val s: NullarySpore[Unit] {type Excluded = String} = spore {
      val y = 10
      delayed {
        ()
      }
    }
  }

  @Test
  def nullaryAvoidanceCapture2(): Unit = {
    val s: NullarySpore[Unit] {type Excluded = String} = spore {
      val y = 10
      val z = 20
      delayed {
        ()
      }
    }
  }

  /**
    * shows why 'self =>' and 'val captured' is needed in spores
    * Without 'self =>', the inner spore is transformed into
    * { class inner... extends Spore[...]... {...}
    *   new inner(...) {type Excluded = captured.B}
    * },
    * but captured refers to inner.captured, while it should refer to
    * outer.captured.
    */
  @Test
  def selfCaptureTypesTest(): Unit = {
    class A{type B}
    val s = spore { // spore with captured : (A)
      val ca = new A{ type B = Int}
      (_: Unit) => {
        val s2 : Spore[Unit, Unit] {type Excluded = ca.B} = spore { // spore with captured : (Unit, Unit)
          val c1 = ()
          val c2 = ()
          (_: Unit) => {}
        }
      }
    }
  }


  // This is to test that path-dependent types in which captured arguments are part of the path are
  // transformed correctly.
  @Test
  def avoidanceTest(): Unit = {
    class A {type B}
    val z: Int = 10
    val s2 : Spore[A, Unit] {type Excluded = String} =  spore {
      val cap_z = z
      (a: A) => {
        val x = 10
      }
    }
  }

  @Test
  def functionArgumentTest(): Unit = {
    class A {type B}
    val z: Int = 10
    def funk(s: Spore[A, Unit] {type Excluded = String}) : Unit = {}
    funk(spore {
      val cap_z = z
      (a: A) => {
        val x = 10
      }
    })
  }

  @Test
  def avoidanceTestImplicit(): Unit = {
    class A {type B}
    val z: Int = 10
    val s2 : Spore[A, Unit] {type Excluded = String} = spore {
      val cap_z = z
      (a: A) => {
        val x = 10
      }
    }
  }

  @Test
  def avoidanceTestImplicitNested(): Unit = {
    class A {
      type B
    }
    val z: Int = 10
    val s3 = spore {
      val cap_z = z
      (a: A) => {
        val s2: Spore[A, Unit] {type Excluded = a.B} = spore {
          val cap_cap_z = cap_z
          (a: A) => {
            val x = 10
          }
        }
        print("hello, world!")
      }
    }
  }
}
