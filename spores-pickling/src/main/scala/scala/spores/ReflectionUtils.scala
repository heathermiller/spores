/*                     __                                               *\
 **     ________ ___   / /  ___     Scala API                            **
 **    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
 **  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
 ** /____/\___/_/ |_/____/_/ | |                                         **
 **                          |/                                          **
\*                                                                      */

package scala.spores

/** Helper method to deal with the Java reflection.
  *
  * It is public because macros access it outside the spores package. */
object ReflectionUtils {

  def createInstance[T](className: String): T = {

    val clazz = java.lang.Class.forName(className)
    val instance = try clazz.newInstance() catch {
      case t: Throwable =>
        scala.concurrent.util.Unsafe.instance
          .allocateInstance(clazz)
    }

    // Work around _className being null in any spore
    if (clazz.getSimpleName.contains("anonspore")) {
      val classNameField = clazz.getDeclaredField("_className")
      classNameField.setAccessible(true)
      classNameField.set(instance, className)
    }

    instance.asInstanceOf[T]

  }

}
