package scala.spores
package run
package pickling

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.pickling._
import Defaults._

import SporePickler._


final case class SelfDescribing(unpicklerClassName: String, blob: Array[Byte]) {
  import binary._

  def result(): Any = {
    val pickle = BinaryPickleArray(blob)
    val reader = pickleFormat.createReader(pickle)

    val unpicklerInst = try {
      Class.forName(unpicklerClassName).newInstance().asInstanceOf[Unpickler[Any]]
    } catch {
      case _: Throwable =>
        scala.concurrent.util.Unsafe.instance.allocateInstance(Class.forName(unpicklerClassName)).asInstanceOf[Unpickler[Any]]
    }

    val typeString = reader.beginEntry()
    reader.hintElidedType(unpicklerInst.tag)
    reader.endEntry()
    unpicklerInst.unpickle(unpicklerInst.tag.key, reader)
  }
}

@RunWith(classOf[JUnit4])
class SelfDescribingSpec {
  import json._

  def mkSelfDesc[T, S](fun: T => S)(implicit pickler: Pickler[Spore[T, S]], unpickler: Unpickler[Spore[T, S]]): SelfDescribing = {
    // pickle spore
    val newBuilder = binary.pickleFormat.createBuilder()
    newBuilder.hintElidedType(pickler.tag)
    pickler.asInstanceOf[Pickler[Any]].pickle(fun, newBuilder)
    val p = newBuilder.result()
    SelfDescribing(unpickler.getClass.getName, p.value)
  }

  def mkSelfDesc2[P <: Spore2[_, _, _]](fun: P)(implicit pickler: Pickler[P], unpickler: Unpickler[P]): SelfDescribing = {
    // pickle spore
    val newBuilder = binary.pickleFormat.createBuilder()
    newBuilder.hintElidedType(pickler.tag)
    pickler.asInstanceOf[Pickler[Any]].pickle(fun, newBuilder)
    val p = newBuilder.result()
    SelfDescribing(unpickler.getClass.getName, p.value)
  }

  @Test def test(): Unit = {
    val s: Spore[Int, String] = spore { (x: Int) =>
      s"x = $x"
    }

    // println(s"pickling spore ${s.getClass.getName}...")
    val sd = mkSelfDesc(s)
    val p = sd.pickle
    // println(p.value)

    // println(s"unpickling...")
    val up = p.unpickle[SelfDescribing]
    val sup = up.result().asInstanceOf[Spore[Int, String]]

    // println(s"res: ${sup(4)}")

    // println(sup.className)
    assert(sup.className != null)
  }

  @Test def test2(): Unit = {
    val maxSize = 20

    val s = spore {
        val chunkSize = maxSize / 2
        val chunkIndex = 0
        (elem: (String, Int), emit: Emitter[String]) =>
          val cond1 = elem._2 >= chunkIndex * chunkSize
          val plusOne = chunkIndex+1
          val cond2 = elem._2 < plusOne * chunkSize
          if (cond1 && cond2) emit.emit(elem._1)
    }

    // println(s"pickling spore ${s.getClass.getName}...")
    val sd = mkSelfDesc2(s)
    val p = sd.pickle
    // println(p.value)

    // println(s"unpickling...")
    val up = p.unpickle[SelfDescribing]
    val sup = up.result().asInstanceOf[Spore2[(String, Int), Emitter[String], Unit]]

    val testEmitter = new TestEmitter
    sup(("hello, " -> 0), testEmitter)
    sup(("world!" -> 9), testEmitter)
    assert(testEmitter.builder.toString == "hello, world!")
  }
}
