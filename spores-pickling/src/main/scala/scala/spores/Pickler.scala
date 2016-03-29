/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.spores

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import scala.pickling._


trait SporePickler extends SimpleSporePicklerImpl {
  def genSporePicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(cPickler: c.Tree, cUnpickler: c.Tree): c.Tree = {

    import c.universe._
    import definitions.ArrayClass

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    def isEffectivelyPrimitive(tpe: c.Type): Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && isEffectivelyPrimitive(eltpe) => true
      case _ => false
    }

    //TODO: check if U is a tuple

    val numVarsCaptured = utpe.typeArgs.size
    debug(s"numVarsCaptured = $numVarsCaptured")
    val sporeTypeName = TypeName("SporeWithEnv")

    debug(s"T: $ttpe, R: $rtpe, U: $utpe")

    val picklerUnpicklerName = c.freshName(TermName("SporePicklerUnpickler"))
    val typeFieldName = """$type"""

    // the problem with the following unpickle method is that it doesn't re-initialize the className field correctly.
    q"""
      val capturedPickler = $cPickler
      val capturedUnpickler = $cUnpickler
      object $picklerUnpicklerName extends scala.pickling.Pickler[scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe }]
          with scala.pickling.Unpickler[scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe }] {

        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe },
                   builder: scala.pickling.PBuilder): Unit = {
          builder.beginEntry(picklee, tag)

          builder.putField("className", b => {
            b.hintElidedType(scala.pickling.FastTypeTag.String)
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("captured", b => {
            b.hintElidedType(capturedPickler.tag)
            capturedPickler.pickle(picklee.asInstanceOf[$sporeTypeName[$ttpe, $rtpe]].captured.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader1 = reader.readField($typeFieldName)
          reader1.hintElidedType(scala.pickling.FastTypeTag.String)

          val tag = reader1.beginEntry()
          val result1 = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag, reader1)
          reader1.endEntry()

          val reader2 = reader.readField("className")
          reader2.hintElidedType(scala.pickling.FastTypeTag.String)

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2)
          reader2.endEntry()

          val clazz = java.lang.Class.forName(result.asInstanceOf[String])
          val sporeInst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz).asInstanceOf[$sporeTypeName[$ttpe, $rtpe] { type Captured = $utpe }]

          val reader3 = reader.readField("captured")
          reader3.hintElidedType(capturedUnpickler.tag)
          val tag3 = reader3.beginEntry()
          val result3 = capturedUnpickler.unpickle(tag3, reader3)
          reader3.endEntry()

          val capturedValField = clazz.getDeclaredField("captured")
          capturedValField.setAccessible(true)
          capturedValField.set(sporeInst, result3.asInstanceOf[sporeInst.Captured])

          sporeInst
        }
      }
      $picklerUnpicklerName
    """
  }

  def genSporeCSPicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(cPickler: c.Tree): c.Tree = {
    import c.universe._

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    def isEffectivelyPrimitive(tpe: c.Type): Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == definitions.ArrayClass && isEffectivelyPrimitive(eltpe) => true
      case _ => false
    }

    val sporeTypeName = TypeName("SporeWithEnv")
    val picklerName = c.freshName(TermName("SporePickler"))
    val typeFieldName = """$type"""

    q"""
      val capturedPickler = $cPickler
      object $picklerName extends scala.pickling.Pickler[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }, builder: scala.pickling.PBuilder): Unit = {
          debug("[genSporeCSPickler] creating pickler")
          builder.beginEntry(picklee, tag)

          builder.putField("className", b => {
            b.hintElidedType(scala.pickling.FastTypeTag.String)
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("captured", b => {
            b.hintElidedType(capturedPickler.tag)
            capturedPickler.pickle(picklee.asInstanceOf[$sporeTypeName[$ttpe, $rtpe]].captured.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }

  def genSporeCMPicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(cPickler: c.Tree, cUnpickler: c.Tree): c.Tree = {

    import c.universe._
    import definitions.ArrayClass

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    // assert(utpe.typeArgs.size > 1)

    def isEffectivelyPrimitive(tpe: c.Type): Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && isEffectivelyPrimitive(eltpe) => true
      case _ => false
    }

    //TODO: check if U is a tuple

    val numVarsCaptured = utpe.typeArgs.size
    val sporeTypeName = TypeName("SporeWithEnv")

    debug(s"T: $ttpe, R: $rtpe, U: $utpe")

    val picklerUnpicklerName = c.freshName(TermName("SporePicklerUnpickler"))
    val typeFieldName = """$type"""

    q"""
      val capturedPickler = $cPickler
      val capturedUnpickler = $cUnpickler
      object $picklerUnpicklerName extends scala.pickling.Pickler[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }]
          with scala.pickling.Unpickler[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }] {

        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }, builder: scala.pickling.PBuilder): Unit = {
          debug("[genSporeCMPickler] creating pickler")
          builder.beginEntry(picklee, tag)

          builder.putField("className", b => {
            b.hintElidedType(scala.pickling.FastTypeTag.String)
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("captured", b => {
            b.hintElidedType(capturedPickler.tag)
            capturedPickler.pickle(picklee.asInstanceOf[$sporeTypeName[$ttpe, $rtpe]].captured.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader1 = reader.readField($typeFieldName)
          reader1.hintElidedType(scala.pickling.FastTypeTag.String)

          val tag = reader1.beginEntry()
          val result1 = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag, reader1)
          reader1.endEntry()

          val reader2 = reader.readField("className")
          reader2.hintElidedType(scala.pickling.FastTypeTag.String)

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2)
          reader2.endEntry()

          debug("[genSporeCMPicklerImpl] creating instance of class " + result)
          val clazz = java.lang.Class.forName(result.asInstanceOf[String])
          val sporeInst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz).asInstanceOf[$sporeTypeName[$ttpe, $rtpe] { type Captured = $utpe }]

          val reader3 = reader.readField("captured")
          reader3.hintElidedType(capturedUnpickler.tag)
          val tag3 = reader3.beginEntry()
          val result3 = capturedUnpickler.unpickle(tag3, reader3)
          reader3.endEntry()

          val capturedValField = clazz.getDeclaredField("captured")
          capturedValField.setAccessible(true)
          capturedValField.set(sporeInst, result3.asInstanceOf[sporeInst.Captured])

          sporeInst
        }
      }
      $picklerUnpicklerName
    """
  }

  def genSpore2CMPicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(cPickler: c.Tree, cUnpickler: c.Tree): c.Tree = {

    import c.universe._
    import definitions.ArrayClass

    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    def isEffectivelyPrimitive(tpe: c.Type): Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && isEffectivelyPrimitive(eltpe) => true
      case _ => false
    }

    //TODO: check if U is a tuple

    val numVarsCaptured = utpe.typeArgs.size
    val picklerName = c.freshName(TermName("Spore2CMPickler"))
    val typeFieldName = """$type"""

    q"""
      val capturedPickler = $cPickler
      val capturedUnpickler = $cUnpickler
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }] {

        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }, builder: scala.pickling.PBuilder): Unit = {
          debug("[genSpore2CMPicklerImpl]")
          builder.beginEntry(picklee, tag)

          builder.putField("className", b => {
            b.hintElidedType(scala.pickling.FastTypeTag.String)
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("captured", b => {
            b.hintElidedType(capturedPickler.tag)
            capturedPickler.pickle(picklee.asInstanceOf[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe]].captured.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }

  def genSpore3CMPicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(cPickler: c.Tree, cUnpickler: c.Tree): c.Tree = {

    import c.universe._
    import definitions.ArrayClass

    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val t3tpe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    def isEffectivelyPrimitive(tpe: c.Type): Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && isEffectivelyPrimitive(eltpe) => true
      case _ => false
    }

    //TODO: check if U is a tuple

    val numVarsCaptured = utpe.typeArgs.size
    val picklerName = c.freshName(TermName("Spore3CMPickler"))
    val typeFieldName = """$type"""

    q"""
      val capturedPickler = $cPickler
      val capturedUnpickler = $cUnpickler
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe] { type Captured = $utpe }] {

        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe] { type Captured = $utpe }, builder: scala.pickling.PBuilder): Unit = {
          debug("[genSpore3CMPicklerImpl]")
          builder.beginEntry(picklee, tag)

          builder.putField("className", b => {
            b.hintElidedType(scala.pickling.FastTypeTag.String)
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("captured", b => {
            b.hintElidedType(capturedPickler.tag)
            capturedPickler.pickle(picklee.asInstanceOf[scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe]].captured.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }


  def genSporeCSUnpicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    debug(s"T: $ttpe, R: $rtpe")

    val unpicklerName = c.freshName(TermName("SporeUnpickler"))
    val utils = new PicklerUtils[c.type](c)
    val reader = TermName("reader")
    val readClassName = utils.readClassNameTree(reader)

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.Spore[$ttpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore[$ttpe, $rtpe]]]

        def unpickle(tag: String, $reader: scala.pickling.PReader): Any = {
          val result = $readClassName

          debug("[genSporeCSUnpicklerImpl] creating instance of class " + result)
          val clazz = java.lang.Class.forName(result)
          val sporeInst = (try clazz.newInstance() catch {
            case t: Throwable =>
              val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz)
              val privateClassNameField = clazz.getDeclaredField("_className")
              privateClassNameField.setAccessible(true)
              privateClassNameField.set(inst, result)
              inst
          }).asInstanceOf[scala.spores.Spore[$ttpe, $rtpe]]

          if (sporeInst.isInstanceOf[scala.spores.SporeWithEnv[$ttpe, $rtpe]]) {
            debug("[genSporeCSUnpicklerImpl] spore class is SporeWithEnv")
            val sporeWithEnvInst = sporeInst.asInstanceOf[scala.spores.SporeWithEnv[$ttpe, $rtpe]]
            val reader3 = $reader.readField("captured")

            val tag3 = reader3.beginEntry()
            val value = {
              if (reader3.atPrimitive) {
                reader3.readPrimitive()
              } else {
                import scala.pickling.internal.currentRuntime
                val mirror = currentRuntime.currentMirror
                val unpickler3 = currentRuntime.picklers.genUnpickler(mirror, tag3)
                unpickler3.unpickle(tag3, reader3)
              }
            }

            reader3.endEntry()
            val capturedValField = clazz.getDeclaredField("captured")
            capturedValField.setAccessible(true)
            capturedValField.set(sporeWithEnvInst, value.asInstanceOf[sporeWithEnvInst.Captured])
          }

          sporeInst
        }
      }
      $unpicklerName
    """
  }

  def genSpore2CSUnpicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]

    val unpicklerName = c.freshName(TermName("Spore2CSUnpickler"))
    val utils = new PicklerUtils[c.type](c)
    val reader = TermName("reader")
    val readClassName = utils.readClassNameTree(reader)

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe]]]

        def unpickle(tag: String, $reader: scala.pickling.PReader): Any = {
          val result = $readClassName

          debug("[genSpore2CSUnpicklerImpl] creating instance of class " + result)
          val clazz = java.lang.Class.forName(result)
          val sporeInst = (try clazz.newInstance() catch {
            case t: Throwable =>
              val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz)
              val privateClassNameField = clazz.getDeclaredField("_className")
              privateClassNameField.setAccessible(true)
              privateClassNameField.set(inst, result)
              inst
          }).asInstanceOf[scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe]]

          if (sporeInst.isInstanceOf[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe]]) {
            debug("[genSpore2CSUnpicklerImpl] spore class is Spore2WithEnv")
            val sporeWithEnvInst = sporeInst.asInstanceOf[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe]]
            val reader3 = $reader.readField("captured")
            reader3.hintElidedType(scala.pickling.FastTypeTag.String)

            val tag3 = reader3.beginEntry()
            val value = {
              if (reader3.atPrimitive) {
                reader3.readPrimitive()
              } else {
                import scala.pickling.internal.currentRuntime
                val mirror = currentRuntime.currentMirror
                val unpickler3 = currentRuntime.picklers.genUnpickler(mirror, tag3)
                unpickler3.unpickle(tag3, reader3)
              }
            }
            reader3.endEntry()
            val capturedValField = clazz.getDeclaredField("captured")
            capturedValField.setAccessible(true)
            capturedValField.set(sporeWithEnvInst, value.asInstanceOf[sporeWithEnvInst.Captured])
          }

          sporeInst
        }
      }
      $unpicklerName
    """
  }

  def genSpore2CMUnpicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    import definitions.ArrayClass

    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    def isEffectivelyPrimitive(tpe: c.Type): Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && isEffectivelyPrimitive(eltpe) => true
      case _ => false
    }

    // debug(s"T: $ttpe, R: $rtpe")

    val unpicklerName = c.freshName(TermName("Spore2CMUnpickler"))
    // TODO: the below unpickling method does not correctly restore the spore's _className field

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }]]

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader2 = reader.readField("className")
          reader2.hintElidedType(scala.pickling.FastTypeTag.String)

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2)
          reader2.endEntry()

          debug("creating instance of class " + result)
          val clazz = java.lang.Class.forName(result.asInstanceOf[String])
          val sporeInst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz).asInstanceOf[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe]]

          val reader3 = reader.readField("captured")
          reader3.hintElidedType(scala.pickling.FastTypeTag.String)

          val tag3 = reader3.beginEntry()
          val value = {
            if (reader3.atPrimitive) {
              reader3.readPrimitive()
            } else {
              import scala.pickling.internal.currentRuntime
              val mirror = currentRuntime.currentMirror
              val unpickler3 = currentRuntime.picklers.genUnpickler(mirror, tag3)
              unpickler3.unpickle(tag3, reader3)
            }
          }
          reader3.endEntry()
          val capturedValField = clazz.getDeclaredField("captured")
          capturedValField.setAccessible(true)
          capturedValField.set(sporeInst, value.asInstanceOf[sporeInst.Captured])

          sporeInst
        }
      }
      $unpicklerName
    """
  }

  def genSporeCMUnpicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    import definitions.ArrayClass

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    def isEffectivelyPrimitive(tpe: c.Type): Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && isEffectivelyPrimitive(eltpe) => true
      case _ => false
    }

    debug(s"T: $ttpe, R: $rtpe")

    val unpicklerName = c.freshName(TermName("SporeCMUnpickler"))

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }]]

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader2 = reader.readField("className")
          reader2.hintElidedType(scala.pickling.FastTypeTag.String)

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2)
          reader2.endEntry()

          debug("creating instance of class " + result)
          val clazz = java.lang.Class.forName(result.asInstanceOf[String])
          val sporeInst = (try clazz.newInstance() catch {
            case t: Throwable =>
              val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz)
              val privateClassNameField = clazz.getDeclaredField("_className")
              privateClassNameField.setAccessible(true)
              privateClassNameField.set(inst, result)
              inst
          }).asInstanceOf[scala.spores.SporeWithEnv[$ttpe, $rtpe]]

          val reader3 = reader.readField("captured")
          reader3.hintElidedType(scala.pickling.FastTypeTag.String)

          val tag3 = reader3.beginEntry()
          val value = {
            if (reader3.atPrimitive) {
              reader3.readPrimitive()
            } else {
              import scala.pickling.internal.currentRuntime
              val mirror = currentRuntime.currentMirror
              val unpickler3 = currentRuntime.picklers.genUnpickler(mirror, tag3)
              unpickler3.unpickle(tag3, reader3)
            }
          }
          reader3.endEntry()
          val capturedValField = clazz.getDeclaredField("captured")
          capturedValField.setAccessible(true)
          capturedValField.set(sporeInst, value.asInstanceOf[sporeInst.Captured])

          sporeInst
        }
      }
      $unpicklerName
    """
  }

  def genSpore3CSUnpicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val t3tpe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]

    val unpicklerName = c.freshName(TermName("Spore3Unpickler"))
    val utils = new PicklerUtils[c.type](c)
    val reader = TermName("reader")
    val readClassName = utils.readClassNameTree(reader)

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]]]

        def unpickle(tag: String, $reader: scala.pickling.PReader): Any = {
          val result = $readClassName

          debug("[genSpore3CSUnpicklerImpl] creating instance of class " + result)
          val clazz = java.lang.Class.forName(result)
          val sporeInst = (try clazz.newInstance() catch {
            case t: Throwable =>
              val inst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz)
              val privateClassNameField = clazz.getDeclaredField("_className")
              privateClassNameField.setAccessible(true)
              privateClassNameField.set(inst, result)
              inst
          }).asInstanceOf[scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]]

          if (sporeInst.isInstanceOf[scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe]]) {
            debug("[genSpore3CSUnpicklerImpl] spore class is Spore3WithEnv")
            val sporeWithEnvInst = sporeInst.asInstanceOf[scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe]]
            val reader3 = $reader.readField("captured")
            reader3.hintElidedType(scala.pickling.FastTypeTag.String)

            val tag3 = reader3.beginEntry()
            val value = {
              if (reader3.atPrimitive) {
                reader3.readPrimitive()
              } else {
                import scala.pickling.internal.currentRuntime
                val mirror = currentRuntime.currentMirror
                val unpickler3 = currentRuntime.picklers.genUnpickler(mirror, tag3)
                unpickler3.unpickle(tag3, reader3)
              }
            }
            reader3.endEntry()
            val capturedValField = clazz.getDeclaredField("captured")
            capturedValField.setAccessible(true)
            capturedValField.set(sporeWithEnvInst, value.asInstanceOf[sporeWithEnvInst.Captured])
          }

          sporeInst
        }
      }
      $unpicklerName
    """
  }
}

object SporePickler extends SporePickler {

  /* Unify type for returning both a pickler and unpickler
   * that keeps track of the captured type inside a `Spore` */
  type FullP[T, R, C] = Pickler[Spore[T, R] {type Captured = C}]
  type FullU[T, R, C] = Unpickler[Spore[T, R] {type Captured = C}]
  type FullPU[T, R, C] = FullP[T, R, C] with FullU[T, R, C]

  /* Unify type for returning both a pickler and unpickler
   * that keeps track of the captured type inside a `SporeWithEnv` */
  type FullPE[T, R, C] = Pickler[SporeWithEnv[T, R] {type Captured = C}]
  type FullUE[T, R, C] = Unpickler[SporeWithEnv[T, R] {type Captured = C}]
  type FullPUE[T, R, C] = FullPE[T, R, C] with FullUE[T, R, C]

  // NOTE Should this be implicit?
  def genSporePickler[T, R, U]
    (implicit cPickler: Pickler[U], cUnpickler: Unpickler[U]): FullPU[T, R, U] =
      macro genSporePicklerImpl[T, R, U]

  implicit def genSimpleSporePickler[T, R]: Pickler[Spore[T, R]] =
    macro genSimpleSporePicklerImpl[T, R]

  implicit def genSimpleSpore2Pickler[T1, T2, R]: Pickler[Spore2[T1, T2, R]] =
    macro genSimpleSpore2PicklerImpl[T1, T2, R]

  implicit def genSimpleSpore3Pickler[T1, T2, T3, R]
    : Pickler[Spore3[T1, T2, T3, R]] =
      macro genSimpleSpore3PicklerImpl[T1, T2, T3, R]

  // type `U` </: `Product`?
  implicit def genSporeCSPickler[T, R, U]
    (implicit cPickler: Pickler[U]): Pickler[SporeWithEnv[T, R] { type Captured = U }] =
    macro genSporeCSPicklerImpl[T, R, U]

  // TODO: probably need also implicit macro for Pickler[Spore[T, R] { type Captured = U }]
  // capture > 1 variables
  implicit def genSporeCMPickler[T, R, U <: Product]
    (implicit cPickler: Pickler[U], cUnpickler: Unpickler[U]): FullPUE[T, R, U] =
      macro genSporeCMPicklerImpl[T, R, U]

  // capture > 1 variables
  implicit def genSpore2CMPickler[T1, T2, R, U <: Product](implicit cPickler: Pickler[U], cUnpickler: Unpickler[U])
    : Pickler[Spore2WithEnv[T1, T2, R] { type Captured = U }] = macro genSpore2CMPicklerImpl[T1, T2, R, U]

  // capture > 1 variables
  implicit def genSpore3CMPickler[T1, T2, T3, R, U <: Product](implicit cPickler: Pickler[U], cUnpickler: Unpickler[U])
    : Pickler[Spore3WithEnv[T1, T2, T3, R] { type Captured = U }] = macro genSpore3CMPicklerImpl[T1, T2, T3, R, U]

  // NOTE This is probably never picked because U is not present when we annotate the type for unpickling
  implicit def genSporeCMUnpickler[T, R, U <: Product]: Unpickler[SporeWithEnv[T, R] { type Captured = U }] =
    macro genSporeCMUnpicklerImpl[T, R, U]

  implicit def genSporeCSUnpickler[T, R]: Unpickler[Spore[T, R]/* { type Captured }*/] =
    macro genSporeCSUnpicklerImpl[T, R]

  implicit def genSpore2CSUnpickler[T1, T2, R]: Unpickler[Spore2[T1, T2, R]] =
    macro genSpore2CSUnpicklerImpl[T1, T2, R]

  implicit def genSpore2CMUnpickler[T1, T2, R, U]: Unpickler[Spore2WithEnv[T1, T2, R] { type Captured = U }] =
    macro genSpore2CMUnpicklerImpl[T1, T2, R, U]

  implicit def genSpore3CSUnpickler[T1, T2, T3, R]: Unpickler[Spore3[T1, T2, T3, R]] =
    macro genSpore3CSUnpicklerImpl[T1, T2, T3, R]
}
