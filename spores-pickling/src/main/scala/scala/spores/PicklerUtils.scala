/*                     __                                               *\
 **     ________ ___   / /  ___     Scala API                            **
 **    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
 **  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
 ** /____/\___/_/ |_/____/_/ | |                                         **
 **                          |/                                          **
\*                                                                      */

package scala.spores

import scala.pickling.FastTypeTag
import scala.reflect.macros.blackbox.Context

private[spores] class PicklerUtils[C <: Context with Singleton](val c: C) {

  import c.universe._

  private[spores] val scalaPath = q"_root_.scala"
  private[spores] val anyType = tq"$scalaPath.Any"
  private[spores] val unitType = tq"$scalaPath.Unit"
  private[spores] val sporesPath = q"$scalaPath.spores"
  private[spores] val predefPath = q"$scalaPath.Predef"
  private[spores] val stringType = tq"$predefPath.String"
  private[spores] val locallyPath = q"$predefPath.locally"
  private[spores] val picklingPath = q"$scalaPath.pickling"
  private[spores] val picklerType = tq"$picklingPath.Pickler"
  private[spores] val unpicklerType = tq"$picklingPath.Unpickler"
  private[spores] val pbuilderType = tq"$picklingPath.PBuilder"
  private[spores] val preaderType = tq"$picklingPath.PReader"
  private[spores] val fastTypeTagType = tq"$picklingPath.FastTypeTag"
  private[spores] val autoRegisterType = tq"$picklingPath.AutoRegister"
  private[spores] val autoRegisterUnpicklerType = tq"$picklingPath.AutoRegisterUnpickler"

  private val capturedField = "captured"
  private val classNameField = "className"
  private val unpicklerClassNameField = "unpicklerClassName"

  private val strTag = q"$picklingPath.FastTypeTag.String"
  private val strPicklerUnpickler = q"$picklingPath.pickler.AllPicklers.stringPickler"

  def readTemplate(reader: TermName,
                   field: String,
                   elidedType: c.Tree,
                   unpickler: c.Tree): c.Tree = {

    val tag = c.freshName(TermName("tag"))
    val reader1 = c.freshName(TermName("reader1"))
    val result = c.freshName(TermName("result"))

    q"""
      val $reader1 = $reader.readField($field)
      // Runtime picklers need the type, don't elide it
      // reader1.hintElidedType(elidedType)
      val $tag = $reader1.beginEntry()
      val $result = $unpickler.unpickle($tag, $reader1)
      $reader1.endEntry()
      $result
    """
  }

  def writeTemplate(builder: TermName,
                    field: String,
                    picklee: c.Tree,
                    elidedType: c.Tree,
                    pickler: c.Tree): c.Tree = {

    q"""
      $builder.putField($field, b => {
        // Runtime picklers need the type, don't elide it
        // b.hintElidedType(elidedType)
        $pickler.pickle($picklee, b)
      })
    """
  }

  def createInstance(className: TermName, tpe: c.Tree): c.Tree = {

    val clazz = c.freshName(TermName("clazz"))
    val instance = c.freshName(TermName("instance"))

    q"""
      val $clazz = java.lang.Class.forName($className)
      val $instance = try ($clazz.newInstance()) catch {
        case t: Throwable =>
          scala.concurrent.util.Unsafe.instance
            .allocateInstance($clazz)
      }

      $instance.asInstanceOf[$tpe]
    """
  }

  def setCapturedInSpore(spore: TermName, captured: TermName): c.Tree = {

    val capturedValField = c.freshName(TermName("capturedValField"))

    q"""
      val $capturedValField = $spore.getClass.getDeclaredField($capturedField)
      $capturedValField.setAccessible(true)
      $capturedValField.set($spore, $captured.asInstanceOf[$spore.Captured])
    """
  }

  def readClassName(reader: TermName): c.Tree = {

    val value = readTemplate(
        reader, classNameField, strTag, strPicklerUnpickler)
    q"$value.asInstanceOf[String]"
  }

  def writeClassName(builder: TermName, picklee: TermName): c.Tree = {

    val target = q"$picklee.className"
    writeTemplate(builder, classNameField, target, strTag, strPicklerUnpickler)
  }

  def readCaptured(reader: TermName, capturedUnpickler: TermName): c.Tree = {

    val unpickler = q"$capturedUnpickler"
    val elidedType = q"$capturedUnpickler.tag"

    readTemplate(reader, capturedField, elidedType, unpickler)
  }

  def writeCaptured(builder: TermName,
                    picklee: TermName,
                    capturedPickler: TermName,
                    tpe: c.Tree,
                    utpe: Type): c.Tree = {

    val pickler = q"$capturedPickler"
    val elidedType = q"$capturedPickler.tag"
    val target = q"$picklee.asInstanceOf[$tpe].captured.asInstanceOf[$utpe]"

    writeTemplate(builder, capturedField, target, elidedType, pickler)
  }

  def readUnpicklerClassName(reader: TermName): c.Tree = {

    val value = readTemplate(
        reader, unpicklerClassNameField, strTag, strPicklerUnpickler)
    q"$value.asInstanceOf[String]"
  }

  def writeUnpicklerClassName(builder: TermName, unpickler: TermName): c.Tree = {

    val target = q"$unpickler.getClass.getName"
    writeTemplate(builder,
      unpicklerClassNameField, target, strTag, strPicklerUnpickler)
  }
}
