/*                     __                                               *\
 **     ________ ___   / /  ___     Scala API                            **
 **    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
 **  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
 ** /____/\___/_/ |_/____/_/ | |                                         **
 **                          |/                                          **
\*                                                                      */

package scala.spores

private[spores] object ReflectionUtils {

  def createInstance[T](className: String): T = {

    val clazz = java.lang.Class.forName(className)
    val instance = try (clazz.newInstance()) catch {
      case t: Throwable =>
        scala.concurrent.util.Unsafe.instance
          .allocateInstance(clazz)
    }
    instance.asInstanceOf[T]

  }

}
