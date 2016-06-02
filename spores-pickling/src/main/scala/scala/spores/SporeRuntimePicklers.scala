package scala.spores

import scala.pickling.{AbstractPicklerUnpickler, FastTypeTag, PBuilder, PReader}
import scala.pickling.pickler.GeneratorHelper
import scala.pickling.pickler.AllPicklers.stringPickler
import scala.pickling.pickler.AnyPicklerUnpickler

/** [[SporeRuntimePicklers]] is the responsible to expose a singleton
  * object that will take care of handling spore types at run-time.
  *
  * It is a concrete implementation since the type parameters are replaced
  * by [[scala.Any]] and will use the pickler of the types given at runtime.
  */
private[spores] trait SporeRuntimePicklers extends GeneratorHelper {

  object SporeRuntimePicklerUnpickler
      extends AbstractPicklerUnpickler[Spore[Any, Any]] {

    override def tag = FastTypeTag("scala.spores.Spore[scala.Any,scala.Any]")

    override def pickle(picklee: Spore[Any, Any], builder: PBuilder): Unit = {
      builder.beginEntry(picklee, tag)
      builder.putField("unpicklerClassName", b => {
        stringPickler.pickle(this.getClass.getName, b)
      })
      builder.putField("className", b => {
        stringPickler.pickle(picklee.className, b)
      })
      builder.endEntry()
    }

    override def unpickle(tag: String, reader: PReader): Any = {
      val className = {
        val reader1 = reader.readField("className")
        val tag = reader1.beginEntry()
        val result = stringPickler.unpickle(tag, reader1)
        reader1.endEntry()
        result.asInstanceOf[String]
      }

      ReflectionUtils.createInstance(className)
    }
  }
}
