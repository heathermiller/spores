/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.spores

import scala.pickling._
import scala.reflect.macros.blackbox.Context

trait SimpleSporePicklers {

  def genSimpleSporePicklerUnpicklerTemplate(c: Context)
    (seedPicklerName: String, sporeType: c.Tree): c.Tree = {

    import c.universe._

    val utils = new PicklerUtils[c.type](c)
    val reader = c.freshName(TermName("reader"))
    val builder = c.freshName(TermName("builder"))
    val picklee = c.freshName(TermName("picklee"))
    val className = c.freshName(TermName("className"))
    val picklerName = c.freshName(TermName(seedPicklerName))
    import utils.{picklerType, unpicklerType, pbuilderType, preaderType, autoRegisterType}
    import utils.{fastTypeTagType, stringType, anyType, unitType, locallyPath}

    q"""
      $locallyPath {
        implicit object $picklerName
            extends $picklerType[$sporeType] with $unpicklerType[$sporeType]
              with $autoRegisterType[$sporeType] {

          def tag = implicitly[$fastTypeTagType[$sporeType]]

          def pickle($picklee: $sporeType, $builder: $pbuilderType): $unitType = {

            $builder.beginEntry($picklee, tag)
            // Runtime picklers need the type, don't elide it
            // builder.hintElidedType(tag)
            ${utils.writeUnpicklerClassName(builder, picklerName)}
            ${utils.writeClassName(builder, picklee)}
            $builder.endEntry()

          }

          def unpickle(tag: $stringType, $reader: $preaderType): $anyType = {

            val $className = ${utils.readClassName(reader)}
            ${utils.createInstance(className, sporeType)}

          }
        }

        $picklerName
      }
    """

  }

  def genSimpleSporePicklerImpl
      [T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    debug(s"T: $ttpe, R: $rtpe")

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val sporeType = tq"$sporesPath.Spore[$ttpe, $rtpe]"
    genSimpleSporePicklerUnpicklerTemplate(c)("SimpleSporePickler", sporeType)

  }

  def genSimpleSpore2PicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag]
      (c: Context): c.Tree = {

    import c.universe._

    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]
    debug(s"T1: $t1tpe, T2: $t2tpe, R: $rtpe")

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val sporeType = tq"$sporesPath.Spore2[$t1tpe, $t2tpe, $rtpe]"
    genSimpleSporePicklerUnpicklerTemplate(c)("SimpleSpore2Pickler", sporeType)

  }

  def genSimpleSpore3PicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag]
      (c: Context): c.Tree = {

    import c.universe._

    val t1tpe = weakTypeOf[T1]
    val t2tpe = weakTypeOf[T2]
    val t3tpe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]
    debug(s"T1: $t1tpe, T2: $t2tpe, T3: $t3tpe, R: $rtpe")

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val sporeType = tq"$sporesPath.Spore3[$t1tpe, $t2tpe, $t3tpe, $rtpe]"
    genSimpleSporePicklerUnpicklerTemplate(c)("SimpleSpore3Pickler", sporeType)

  }

}
