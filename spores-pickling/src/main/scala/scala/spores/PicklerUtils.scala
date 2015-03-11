/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.spores

import scala.reflect.macros.Context


private[spores] class PicklerUtils[C <: Context with Singleton](val c: C) {
  import c.universe._

  def readClassNameTree(reader: TermName): Tree = {
    val result = c.fresh(TermName("result"))
    q"""
      val reader2 = $reader.readField("className")
      reader2.hintTag(scala.pickling.FastTypeTag.String)
      reader2.hintStaticallyElidedType()
      val tag2 = reader2.beginEntry()
      val $result = scala.pickling.pickler.AllPicklers.stringPickler.unpickle(tag2, reader2).asInstanceOf[String]
      reader2.endEntry()
      $result
    """
  }

}
