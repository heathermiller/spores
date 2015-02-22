package scala.spores

import scala.language.experimental.macros
import scala.reflect.macros.Context

import scala.pickling._


object SporePickler {
  /*implicit*/
  def genSporePickler[T, R, U](implicit tag: FastTypeTag[Spore[T, R]], format: PickleFormat,
                                        cPickler: Pickler[U], cUnpickler: Unpickler[U], cTag: FastTypeTag[U])
        : Pickler[Spore[T, R] { type Captured = U }] = macro genSporePicklerImpl[T, R, U]

  def genSporePicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(tag: c.Tree, format: c.Tree, cPickler: c.Tree, cUnpickler: c.Tree, cTag: c.Tree): c.Tree = {

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
    // println(s"numVarsCaptured = $numVarsCaptured")
    val sporeTypeName = newTypeName("SporeC1")

    debug(s"T: $ttpe, R: $rtpe, U: $utpe")

    val picklerUnpicklerName = c.fresh(newTermName("SporePicklerUnpickler"))
    val typeFieldName = """$type"""

    // the problem with the following unpickle method is that it doesn't re-initialize the className field correctly.
    q"""
      val capturedPickler = $cPickler
      val capturedUnpickler = $cUnpickler
      object $picklerUnpicklerName extends scala.pickling.Pickler[scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe }]
          with scala.pickling.Unpickler[scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe }] {

        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe }, builder: scala.pickling.PBuilder): Unit = {
          builder.beginEntry(picklee)

          builder.putField("className", b => {
            b.hintTag(scala.pickling.FastTypeTag.String)
            b.hintStaticallyElidedType()
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("c1", b => {
          	b.hintTag($cTag)
          	${if (isEffectivelyPrimitive(utpe)) q"b.hintStaticallyElidedType()" else q""}
            capturedPickler.pickle(picklee.asInstanceOf[$sporeTypeName[$ttpe, $rtpe]].c1.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader1 = reader.readField($typeFieldName)
          reader1.hintTag(scala.pickling.FastTypeTag.String)
          reader1.hintStaticallyElidedType()

          val tag = reader1.beginEntry()
          val result1 = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag, reader1)
          reader1.endEntry()

          val reader2 = reader.readField("className")
          reader2.hintTag(scala.pickling.FastTypeTag.String)
          reader2.hintStaticallyElidedType()

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2)
          reader2.endEntry()

          println("[genSporePicklerImpl] creating instance of class " + result)
          val clazz = java.lang.Class.forName(result.asInstanceOf[String])
          val sporeInst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz).asInstanceOf[$sporeTypeName[$ttpe, $rtpe] { type Captured = $utpe }]

          val reader3 = reader.readField("c1")
          reader3.hintTag($cTag)
          ${if (isEffectivelyPrimitive(utpe)) q"reader3.hintStaticallyElidedType()" else q""}
          val tag3 = reader3.beginEntry()
          val result3 = capturedUnpickler.unpickle(tag3, reader3)
          reader3.endEntry()
          sporeInst.c1 = result3.asInstanceOf[sporeInst.C1]

          sporeInst
        }
      }
      $picklerUnpicklerName
    """
  }

/*
  def genSporePickler[T, R, U](implicit tag: FastTypeTag[Spore[T, R]], format: PickleFormat,
                                        cPickler: Pickler[U], cUnpickler: Unpickler[U], cTag: FastTypeTag[U])
        : Pickler[Spore[T, R] { type Captured = U }] = macro genSporePicklerImpl[T, R, U]

  def genSporePicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(tag: c.Tree, format: c.Tree, cPickler: c.Tree, cUnpickler: c.Tree, cTag: c.Tree): c.Tree = {
    import c.universe._
    import definitions.ArrayClass
*/

  implicit def genSimpleSporePickler[T, R]: Pickler[Spore[T, R]] =
    macro genSimpleSporePicklerImpl[T, R]

  def genSimpleSporePicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]

    debug(s"T: $ttpe, R: $rtpe")
    val picklerName = c.fresh(newTermName("SporePickler"))

    q"""
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore[$ttpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore[$ttpe, $rtpe]]]

        def pickle(picklee: scala.spores.Spore[$ttpe, $rtpe], builder: scala.pickling.PBuilder): Unit = {
          println("[genSimpleSporePicklerImpl]")
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

  implicit def genSimpleSpore2Pickler[T1, T2, R]: Pickler[Spore2[T1, T2, R]] =
    macro genSimpleSpore2PicklerImpl[T1, T2, R]

  def genSimpleSpore2PicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]
    val picklerName = c.fresh(newTermName("SimpleSpore2Pickler"))

    q"""
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe]]]

        def pickle(picklee: scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe], builder: scala.pickling.PBuilder): Unit = {
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

  implicit def genSimpleSpore3Pickler[T1, T2, T3, R]: Pickler[Spore3[T1, T2, T3, R]] =
    macro genSimpleSpore3PicklerImpl[T1, T2, T3, R]

  def genSimpleSpore3PicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val t3tpe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]
    val picklerName = c.fresh(newTermName("Spore3Pickler"))

    q"""
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]]]

        def pickle(picklee: scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe], builder: scala.pickling.PBuilder): Unit = {
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

  // TODO: probably need also implicit macro for Pickler[Spore[T, R] { type Captured = U }]
  // capture > 1 variables
  implicit def genSporeCMPickler[T, R, U <: Product](implicit tag: FastTypeTag[Spore[T, R]], format: PickleFormat,
                                                              cPickler: Pickler[U], cUnpickler: Unpickler[U], cTag: FastTypeTag[U])
        : Pickler[SporeWithEnv[T, R] { type Captured = U }] = macro genSporeCMPicklerImpl[T, R, U]

  def genSporeCMPicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(tag: c.Tree, format: c.Tree, cPickler: c.Tree, cUnpickler: c.Tree, cTag: c.Tree): c.Tree = {
    import c.universe._
    import definitions.ArrayClass

    // println("enter genSporeCMPicklerImpl...")

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
    // println(s"numVarsCaptured = $numVarsCaptured")
    val sporeTypeName = newTypeName("SporeWithEnv")

    debug(s"T: $ttpe, R: $rtpe, U: $utpe")

    val picklerUnpicklerName = c.fresh(newTermName("SporePicklerUnpickler"))
    val typeFieldName = """$type"""

    q"""
      val capturedPickler = $cPickler
      val capturedUnpickler = $cUnpickler
      object $picklerUnpicklerName extends scala.pickling.Pickler[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }]
          with scala.pickling.Unpickler[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }] {

        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.SporeWithEnv[$ttpe, $rtpe] { type Captured = $utpe }, builder: scala.pickling.PBuilder): Unit = {
          // println("[genSporeCMPicklerImpl]")
          builder.beginEntry(picklee)

          builder.putField("className", b => {
            b.hintTag(scala.pickling.FastTypeTag.String)
            b.hintStaticallyElidedType()
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("captured", b => {
            b.hintTag($cTag)
            ${if (isEffectivelyPrimitive(utpe)) q"b.hintStaticallyElidedType()" else q""}
            capturedPickler.pickle(picklee.asInstanceOf[$sporeTypeName[$ttpe, $rtpe]].captured.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader1 = reader.readField($typeFieldName)
          reader1.hintTag(scala.pickling.FastTypeTag.String)
          reader1.hintStaticallyElidedType()

          val tag = reader1.beginEntry()
          val result1 = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag, reader1)
          reader1.endEntry()

          val reader2 = reader.readField("className")
          reader2.hintTag(scala.pickling.FastTypeTag.String)
          reader2.hintStaticallyElidedType()

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2)
          reader2.endEntry()

          // println("[genSporeCMPicklerImpl] creating instance of class " + result)
          val clazz = java.lang.Class.forName(result.asInstanceOf[String])
          val sporeInst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz).asInstanceOf[$sporeTypeName[$ttpe, $rtpe] { type Captured = $utpe }]

          val reader3 = reader.readField("captured")
          reader3.hintTag($cTag)
          ${if (isEffectivelyPrimitive(utpe)) q"reader3.hintStaticallyElidedType()" else q""}
          val tag3 = reader3.beginEntry()
          val result3 = capturedUnpickler.unpickle(tag3, reader3)
          reader3.endEntry()
          sporeInst.captured = result3.asInstanceOf[sporeInst.Captured]

          sporeInst
        }
      }
      $picklerUnpicklerName
    """
  }

  // capture > 1 variables
  implicit def genSpore2CMPickler[T1, T2, R, U <: Product](implicit cTag: FastTypeTag[U], cPickler: Pickler[U], cUnpickler: Unpickler[U])
        : Pickler[Spore2WithEnv[T1, T2, R] { type Captured = U }] = macro genSpore2CMPicklerImpl[T1, T2, R, U]

  def genSpore2CMPicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(cTag: c.Tree, cPickler: c.Tree, cUnpickler: c.Tree): c.Tree = {
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
    // println(s"numVarsCaptured = $numVarsCaptured")
    val picklerName = c.fresh(newTermName("Spore2CMPickler"))
    val typeFieldName = """$type"""

    q"""
      val capturedPickler = $cPickler
      val capturedUnpickler = $cUnpickler
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }] {

        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }, builder: scala.pickling.PBuilder): Unit = {
          // println("[genSpore2CMPicklerImpl]")
          builder.beginEntry(picklee)

          builder.putField("className", b => {
            b.hintTag(scala.pickling.FastTypeTag.String)
            b.hintStaticallyElidedType()
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("captured", b => {
            b.hintTag($cTag)
            ${if (isEffectivelyPrimitive(utpe)) q"b.hintStaticallyElidedType()" else q""}
            capturedPickler.pickle(picklee.asInstanceOf[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe]].captured.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }

  // capture > 1 variables
  implicit def genSpore3CMPickler[T1, T2, T3, R, U <: Product](implicit cPickler: Pickler[U], cUnpickler: Unpickler[U])
        : Pickler[Spore3WithEnv[T1, T2, T3, R] { type Captured = U }] = macro genSpore3CMPicklerImpl[T1, T2, T3, R, U]

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
    // println(s"numVarsCaptured = $numVarsCaptured")
    val picklerName = c.fresh(newTermName("Spore3CMPickler"))
    val typeFieldName = """$type"""

    // ${if (isEffectivelyPrimitive(utpe)) q"b.hintStaticallyElidedType()" else q""}

    q"""
      val capturedPickler = $cPickler
      val capturedUnpickler = $cUnpickler
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe] { type Captured = $utpe }] {

        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe] { type Captured = $utpe }]]

        def pickle(picklee: scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe] { type Captured = $utpe }, builder: scala.pickling.PBuilder): Unit = {
          // println("[genSpore3CMPicklerImpl]")
          builder.beginEntry(picklee)

          builder.putField("className", b => {
            b.hintTag(scala.pickling.FastTypeTag.String)
            b.hintStaticallyElidedType()
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.putField("captured", b => {
            b.hintTag(capturedPickler.tag)
            capturedPickler.pickle(picklee.asInstanceOf[scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe]].captured.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }


  implicit def genSporeCSUnpickler[T, R]: Unpickler[Spore[T, R]/* { type Captured }*/] =
    macro genSporeCSUnpicklerImpl[T, R]

  def genSporeCSUnpicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    debug(s"T: $ttpe, R: $rtpe")
    val unpicklerName = c.fresh(newTermName("SporeUnpickler"))

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.Spore[$ttpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore[$ttpe, $rtpe]]]

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader2 = reader.readField("className")
          reader2.hintTag(scala.pickling.FastTypeTag.String)
          reader2.hintStaticallyElidedType()

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2).asInstanceOf[String]
          reader2.endEntry()

          // println("[genSporeCSUnpicklerImpl] creating instance of class " + result)
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
            // println("[genSporeCSUnpicklerImpl] spore class is SporeWithEnv")
            val sporeWithEnvInst = sporeInst.asInstanceOf[scala.spores.SporeWithEnv[$ttpe, $rtpe]]
            val reader3 = reader.readField("captured")
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

  implicit def genSpore2CSUnpickler[T1, T2, R]: Unpickler[Spore2[T1, T2, R]] =
    macro genSpore2CSUnpicklerImpl[T1, T2, R]

  def genSpore2CSUnpicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]

    val unpicklerName = c.fresh(newTermName("Spore2CSUnpickler"))

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe]]]

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader2 = reader.readField("className")
          reader2.hintTag(scala.pickling.FastTypeTag.String)
          reader2.hintStaticallyElidedType()

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2).asInstanceOf[String]
          reader2.endEntry()

          // println("[genSporeCSUnpicklerImpl] creating instance of class " + result)
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
            // println("[genSporeCSUnpicklerImpl] spore class is Spore2WithEnv")
            val sporeWithEnvInst = sporeInst.asInstanceOf[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe]]
            val reader3 = reader.readField("captured")
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

  implicit def genSpore2CMUnpickler[T1, T2, R, U]: Unpickler[Spore2WithEnv[T1, T2, R] { type Captured = U }] =
    macro genSpore2CMUnpicklerImpl[T1, T2, R, U]

  def genSpore2CMUnpicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    import definitions.ArrayClass

    // println("enter genSporeUnpicklerImpl...")

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

    val unpicklerName = c.fresh(newTermName("Spore2CMUnpickler"))
    // TODO: the below unpickling method does not correctly restore the spore's _className field

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe] { type Captured = $utpe }]]

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader2 = reader.readField("className")
          reader2.hintTag(scala.pickling.FastTypeTag.String)
          reader2.hintStaticallyElidedType()

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2)
          reader2.endEntry()

          // println("creating instance of class " + result)
          val clazz = java.lang.Class.forName(result.asInstanceOf[String])
          val sporeInst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz).asInstanceOf[scala.spores.Spore2WithEnv[$t1tpe, $t2tpe, $rtpe]]

          val reader3 = reader.readField("captured")
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
          sporeInst.captured = value.asInstanceOf[sporeInst.Captured]
          sporeInst
        }
      }
      $unpicklerName
    """
  }

  implicit def genSporeCMUnpickler[T, R, U](implicit format: PickleFormat): Unpickler[Spore[T, R] { type Captured = U }] =
    macro genSporeCMUnpicklerImpl[T, R, U]

  def genSporeCMUnpicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(format: c.Tree): c.Tree = {
    import c.universe._
    import definitions.ArrayClass

    // println("enter genSporeUnpicklerImpl...")

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    def isEffectivelyPrimitive(tpe: c.Type): Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && isEffectivelyPrimitive(eltpe) => true
      case _ => false
    }

    debug(s"T: $ttpe, R: $rtpe")

    val unpicklerName = c.fresh(newTermName("SporeUnpickler"))
    // TODO: the below unpickling method does not correctly restore the spore's _className field

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe }] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore[$ttpe, $rtpe] { type Captured = $utpe }]]

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader2 = reader.readField("className")
          reader2.hintTag(scala.pickling.FastTypeTag.String)
          reader2.hintStaticallyElidedType()

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2)
          reader2.endEntry()

          // println("creating instance of class " + result)
          val clazz = java.lang.Class.forName(result.asInstanceOf[String])
          val sporeInst = scala.concurrent.util.Unsafe.instance.allocateInstance(clazz).asInstanceOf[scala.spores.SporeWithEnv[$ttpe, $rtpe]]

          val reader3 = reader.readField("captured")
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
          sporeInst.captured = value.asInstanceOf[sporeInst.Captured]
          sporeInst
        }
      }
      $unpicklerName
    """
  }

  implicit def genSpore3CSUnpickler[T1, T2, T3, R]: Unpickler[Spore3[T1, T2, T3, R]] =
    macro genSpore3CSUnpicklerImpl[T1, T2, T3, R]

  def genSpore3CSUnpicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val t3tpe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]

    val unpicklerName = c.fresh(newTermName("Spore3Unpickler"))

    q"""
      object $unpicklerName extends scala.pickling.Unpickler[scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]]]

        def unpickle(tag: String, reader: scala.pickling.PReader): Any = {
          val reader2 = reader.readField("className")
          reader2.hintTag(scala.pickling.FastTypeTag.String)
          reader2.hintStaticallyElidedType()

          val tag2 = reader2.beginEntry()
          val result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2).asInstanceOf[String]
          reader2.endEntry()

          // println("[genSporeCSUnpicklerImpl] creating instance of class " + result)
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
            // println("[genSporeCSUnpicklerImpl] spore class is Spore3WithEnv")
            val sporeWithEnvInst = sporeInst.asInstanceOf[scala.spores.Spore3WithEnv[$t1tpe, $t2tpe, $t3tpe, $rtpe]]
            val reader3 = reader.readField("captured")
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
