package scala.spores

import scala.pickling._
import scala.pickling.pickler.AnyPicklerUnpickler

sealed trait SporeAnyProxy extends Any

trait SporeProxyRuntimePicklers {
  this: SporeRuntimePicklers =>

  implicit object SporeAnyProxyPU
    extends AbstractPicklerUnpickler[SporeAnyProxy] {

    override def pickle(picklee: SporeAnyProxy,
                        builder: PBuilder): Unit = {
      val cls = picklee.getClass.getName.toLowerCase
      if (ReflectionUtils.isSpore(cls)) {
        if (cls.contains("nullary")) {
          val pk = picklee.asInstanceOf[NullarySpore[Any]]
          NullarySporeRuntimePicklerUnpickler.pickle(pk, builder)
        } else if (cls.contains("scala.spores.spore3")) {
          val pk = picklee.asInstanceOf[Spore[Any, Any]]
          SporeRuntimePicklerUnpickler.pickle(pk, builder)
        } else if (cls.contains("spore2")) {
          throw new Exception("No spore2 runtime pickler/unpickler")
        } else if (cls.contains("spore")) {
          throw new Exception("No spore runtime pickler/unpickler")
        } else {
          throw new Exception("Unmatched spore runtime pickler/unpickler")
        }
      } else AnyPicklerUnpickler.pickle(picklee, builder)
    }

    override def unpickle(tag: String,
                          reader: PReader): Any = ???

    override def tag: FastTypeTag[SporeAnyProxy] = ???

  }

}
