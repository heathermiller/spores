/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.spores

import scala.reflect.macros.blackbox.Context

private[spores] class PicklerUtils[C <: Context with Singleton](val c: C) {
  import c.universe._

  final val typeField = "$type"
  final val capturedField = "captured"
  final val classNameField = "className"
  final val unpicklerClassNameField = "unpicklerClassName"

  import scala.pickling.pickler.AllPicklers.stringPickler

  def createInstance(className: TermName, tpe: c.Tree): c.Tree = {
    q"""
      val clazz = java.lang.Class.forName($className)
      val instance = try (clazz.newInstance()) catch {
        case t: Throwable =>
          scala.concurrent.util.Unsafe.instance
            .allocateInstance(clazz)
      }

      instance.asInstanceOf[$tpe]
    """
  }

  def readClassName(reader: TermName): c.Tree = {

    val tag = c.freshName(TermName("tag"))
    val reader1 = c.freshName(TermName("reader1"))
    val result = c.freshName(TermName("result"))

    q"""
      val $reader1 = $reader.readField($classNameField)
      $reader1.hintElidedType(scala.pickling.FastTypeTag.String)
      val $tag = $reader1.beginEntry()
      val $result = stringPickler.unpickle($tag, $reader1)
      $reader1.endEntry()
      $result.asInstanceOf[String]
    """

  }

  def writeClassName(builder: TermName, picklee: TermName): c.Tree = {
    q"""
      $builder.putField($classNameField, b => {
        b.hintElidedType(scala.pickling.FastTypeTag.String)
        stringPickler.pickle($picklee.className, b)
      })
    """
  }

  def readCaptured(reader: TermName, capturedUnpickler: TermName): c.Tree = {

    val tag = c.freshName(TermName("tag"))
    val tag2 = c.freshName(TermName("tag2"))
    val reader1 = c.freshName(TermName("reader1"))
    val result = c.freshName(TermName("result"))

    q"""
      val $reader1 = $reader.readField($capturedField)
      val $tag2 = $capturedUnpickler.tag
      $reader1.hintElidedType($tag2)
      val $tag = $reader1.beginEntry()
      val $result = $capturedUnpickler.unpickle($tag, $reader1)
      $reader1.endEntry()
      $result
    """

  }

  def writeCaptured(builder: TermName, picklee: TermName, capturedPickler: TermName,
                    tpe: c.Tree, utpe: Type): c.Tree = {
    q"""
      $builder.putField($capturedField, b => {
        b.hintElidedType($capturedPickler.tag)
        $capturedPickler.pickle($picklee.asInstanceOf[$tpe].captured
          .asInstanceOf[$utpe], b)
      })
    """
  }

  def setCapturedInSpore(spore: TermName, captured: TermName): c.Tree = {
    q"""
      val capturedValField = $spore.getClass.getDeclaredField("captured")
      capturedValField.setAccessible(true)
      capturedValField.set($spore, $captured.asInstanceOf[$spore.Captured])
    """
  }

  def readTag(reader: TermName): c.Tree = {

    val tag = c.freshName(TermName("tag"))
    val reader1 = c.freshName(TermName("reader1"))
    val result = c.freshName(TermName("result"))

    q"""
      val $reader1 = $reader.readField($typeField)
      $reader1.hintElidedType(scala.pickling.FastTypeTag.String)
      val $tag = $reader1.beginEntry()
      val $result = stringPickler.unpickle($tag, $reader1)
      $reader1.endEntry()
      $result.asInstanceOf[String]
    """

  }

  def readUnpicklerClassName(reader: TermName): c.Tree = {

    val tag = c.freshName(TermName("tag"))
    val reader1 = c.freshName(TermName("reader1"))
    val result = c.freshName(TermName("result"))

    q"""
      val $reader1 = $reader.readField($unpicklerClassNameField)
      $reader1.hintElidedType(scala.pickling.FastTypeTag.String)
      val $tag = $reader1.beginEntry()
      val $result = stringPickler.unpickle($tag, $reader1)
      $reader1.endEntry()
      $result.asInstanceOf[String]
    """

  }

  def writeUnpicklerClassName(builder: TermName, unpickler: TermName): c.Tree = {
    q"""
      $builder.putField($unpicklerClassNameField, b => {
        b.hintElidedType(scala.pickling.FastTypeTag.String)
        stringPickler.pickle($unpickler.getClass.getName, b)
      })
    """
  }

}
