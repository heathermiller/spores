package scala.spores

import scala.language.experimental.macros
import scala.reflect.macros.Context

import scala.pickling._
import json._

object SporePickler {
  /*implicit*/
  def genSporePickler[T, R, U](implicit tag: FastTypeTag[Spore[T, R]], format: PickleFormat, cPickler: SPickler[U], cTag: FastTypeTag[U])
        : SPickler[Spore[T, R] { type Captured = U }] = macro genSporePicklerImpl[T, R, U]

  def genSporePicklerImpl[T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
        (c: Context)(tag: c.Tree, format: c.Tree, cPickler: c.Tree, cTag: c.Tree): c.Tree = {
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

    debug(s"T: $ttpe, R: $rtpe, U: $utpe")

    val picklerUnpicklerName = c.fresh(newTermName("SporePicklerUnpickler"))

    q"""
      object $picklerUnpicklerName extends scala.pickling.SPickler[Spore[$ttpe, $rtpe] { type Captured = $utpe }] {
        val format = implicitly[${format.tpe}]

        def pickle(picklee: Spore[$ttpe, $rtpe] { type Captured = $utpe }, builder: PBuilder): Unit = {
          builder.hintTag($tag)
          builder.beginEntry(picklee)

          builder.putField("className", b => {
            b.hintTag(implicitly[FastTypeTag[String]])
            b.hintStaticallyElidedType()
            scala.pickling.SPickler.stringPicklerUnpickler.pickle(picklee.className, b)
          })

          builder.putField("c1", b => {
          	b.hintTag($cTag)
          	${if (isEffectivelyPrimitive(utpe)) q"b.hintStaticallyElidedType()" else q""}
            $cPickler.pickle(picklee.asInstanceOf[SporeC1[$ttpe, $rtpe]].c1.asInstanceOf[$utpe], b)
          })

          builder.endEntry()
        }
      }
      $picklerUnpicklerName
    """
  }
}
