/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.spores

import scala.reflect.macros.blackbox.Context

import scala.pickling._

trait NullarySporePicklerImpl {

  def genNullarySporePicklerImpl[R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val rtpe = weakTypeOf[R]
    val picklerName = c.freshName(TermName("NullarySporePickler"))

    q"""
      object $picklerName extends scala.pickling.Pickler[scala.spores.NullarySpore[$rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.NullarySpore[$rtpe]]]

        def pickle(picklee: scala.spores.NullarySpore[$rtpe], builder: scala.pickling.PBuilder): Unit = {
          builder.beginEntry(picklee)

          builder.putField("className", b => {
            b.hintTag(scala.pickling.FastTypeTag.String)
            b.hintStaticallyElidedType()
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }

  def genNullarySporeCSPicklerImpl[R: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(cPickler: c.Tree): c.Tree = {
    import c.universe._

    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    def isEffectivelyPrimitive(tpe: c.Type): Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == definitions.ArrayClass && isEffectivelyPrimitive(eltpe) => true
      case _ => false
    }

    val sporeTypeName = TypeName("NullarySporeWithEnv")
    val picklerName = c.freshName(TermName("NullarySporePickler"))

    q"""
      val capturedPickler = $cPickler
      object $picklerName extends scala.pickling.Pickler[scala.spores.NullarySporeWithEnv[$rtpe] { type Captured = $utpe }] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.NullarySporeWithEnv[$rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.NullarySporeWithEnv[$rtpe] { type Captured = $utpe }, builder: scala.pickling.PBuilder): Unit = {
          // println("[genNullarySporeCSPicklerImpl]")
          builder.beginEntry(picklee)

          builder.putField("className", b => {
            b.hintTag(scala.pickling.FastTypeTag.String)
            b.hintStaticallyElidedType()
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("captured", b => {
            b.hintTag(capturedPickler.tag)
            ${if (isEffectivelyPrimitive(utpe)) q"b.hintStaticallyElidedType()" else q""}
            capturedPickler.pickle(picklee.asInstanceOf[$sporeTypeName[$rtpe]].captured.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }

  def genNullarySporeCSUnpicklerImpl[R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val rtpe = weakTypeOf[R]
    val unpicklerName = c.freshName(TermName("NullarySporeUnpickler"))
    val utils = new PicklerUtils[c.type](c)
    val reader = TermName("reader")
    val readClassName = utils.readClassNameTree(reader)

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.NullarySpore[$rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.NullarySpore[$rtpe]]]

        def unpickle(tag: String, $reader: scala.pickling.PReader): Any = {
          val result = $readClassName

          // println("[genNullarySporeCSUnpicklerImpl] creating instance of class " + result)
          val clazz = java.lang.Class.forName(result)
          val sporeInst = (try clazz.newInstance() catch {
            case t: Throwable =>
              val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz)
              val privateClassNameField = clazz.getDeclaredField("_className")
              privateClassNameField.setAccessible(true)
              privateClassNameField.set(inst, result)
              inst
          }).asInstanceOf[scala.spores.NullarySpore[$rtpe]]

          if (sporeInst.isInstanceOf[scala.spores.NullarySporeWithEnv[$rtpe]]) {
            // println("[genNullarySporeCSUnpicklerImpl] spore class is NullarySporeWithEnv")
            val sporeWithEnvInst = sporeInst.asInstanceOf[scala.spores.NullarySporeWithEnv[$rtpe]]
            val reader3 = $reader.readField("captured")
            val tag3 = reader3.beginEntry()
            val value = {
              if (reader3.atPrimitive) {
                reader3.readPrimitive()
              } else {
                val unpickler3 = scala.pickling.runtime.RuntimeUnpicklerLookup.genUnpickler(scala.reflect.runtime.currentMirror, tag3)
                unpickler3.unpickle(tag3, reader3)
              }
            }
            reader3.endEntry()
            sporeWithEnvInst.captured = value.asInstanceOf[sporeWithEnvInst.Captured]
          }

          sporeInst
        }
      }
      $unpicklerName
    """
  }

}
