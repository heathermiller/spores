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

trait SimpleSporePicklerImpl {

  def genSimpleSporePicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    debug("Using genSimpleSporePicklerImpl")

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    debug(s"T: $ttpe, R: $rtpe")

    val picklerName = c.freshName(TermName("SimpleSporePickler"))

    q"""
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore[$ttpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore[$ttpe, $rtpe]]]

        def pickle(picklee: scala.spores.Spore[$ttpe, $rtpe], builder: scala.pickling.PBuilder): Unit = {
          builder.beginEntry(picklee, tag)

          builder.putField("className", b => {
            b.hintElidedType(scala.pickling.FastTypeTag.String)
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }

  def genSimpleSpore2PicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    debug("Using genSimpleSpore2PicklerImpl")

    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]
    val picklerName = c.freshName(TermName("SimpleSpore2Pickler"))

    q"""
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe]]]

        def pickle(picklee: scala.spores.Spore2[$t1tpe, $t2tpe, $rtpe], builder: scala.pickling.PBuilder): Unit = {
          builder.beginEntry(picklee, tag)

          builder.putField("className", b => {
            b.hintElidedType(scala.pickling.FastTypeTag.String)
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }

  def genSimpleSpore3PicklerImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {

    debug("Using genSimpleSpore3PicklerImpl")

    import c.universe._
    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val t3tpe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]
    val picklerName = c.freshName(TermName("Spore3Pickler"))

    q"""
      object $picklerName extends scala.pickling.Pickler[scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]] {
        def tag =
          implicitly[scala.pickling.FastTypeTag[scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]]]

        def pickle(picklee: scala.spores.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe], builder: scala.pickling.PBuilder): Unit = {
          builder.beginEntry(picklee, tag)

          builder.putField("className", b => {
            b.hintElidedType(scala.pickling.FastTypeTag.String)
            scala.pickling.pickler.AllPicklers.stringPickler.pickle(picklee.className, b)
          })

          builder.endEntry()
        }
      }
      $picklerName
    """
  }

}
