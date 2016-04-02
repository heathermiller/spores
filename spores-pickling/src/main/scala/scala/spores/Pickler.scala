/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.spores

import scala.pickling._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait SporePickler extends SimpleSporePicklerImpl {

  /** The current implementation doesn't really use this method but
    * there can be corner cases or future bugs that could be fixed
    * using it. It's a good way to check if some type is primitive.
    */
  def isEffectivelyPrimitive(c: Context)(tpe: c.Type): Boolean = {

    import c.universe._
    import definitions.ArrayClass

    @scala.annotation.tailrec
    def checkPrimitiveRec(tpe: c.Type, isArrayOrPrimitive: Boolean): Boolean = {
      (tpe, isArrayOrPrimitive) match {
        case (TypeRef(_, sym: ClassSymbol, _), true) =>
          checkPrimitiveRec(tpe, sym.isPrimitive)
        case (TypeRef(_, sym, eltpe :: Nil), true) =>
          checkPrimitiveRec(eltpe, sym == ArrayClass)
        case (_, false) => false
      }
    }

    checkPrimitiveRec(tpe, true)

  }

  /** Generates both `Pickler`s and `Unpickler`s for `Spore`s and `SporeWithEnv`s.
    * 
    * The `Pickler`s will be used by default and the `Unpickler`s will be invoked
    * by the `UnpicklerFetcher` defined below. Fortunately, we don't have to keep
    * type information of `Captured` in the pickled message since we already know
    * which is the correct tag when `unpickle` is invoked (this is kind of a hack).
    */
  def genSporePicklerUnpicklerTemplate[U: c.WeakTypeTag](c: Context)
      (cPickler: c.Tree, cUnpickler: c.Tree, sporeType: c.Tree): c.Tree = {

    import c.universe._

    val utpe = weakTypeOf[U]
    val utpeStr = utpe.toString

    val reader = c.freshName(TermName("reader"))
    val builder = c.freshName(TermName("builder"))
    val picklee = c.freshName(TermName("picklee"))
    val className = c.freshName(TermName("className"))
    val unpickledSpore = c.freshName(TermName("spore"))
    val unpickledCapture = c.freshName(TermName("capture"))
    val capturedPickler = c.freshName(TermName("capturedPickler"))
    val capturedUnpickler = c.freshName(TermName("capturedUnpickler"))
    val picklerUnpicklerName = c.freshName(TermName("SporePicklerUnpickler"))
    val utils = new PicklerUtils[c.type](c)

    q"""
      val $capturedPickler = $cPickler

      object $picklerUnpicklerName
          extends scala.pickling.Pickler[$sporeType]
             with scala.pickling.Unpickler[$sporeType] {

        def tag = implicitly[scala.pickling.FastTypeTag[$sporeType]]

        def pickle($picklee: $sporeType, $builder: scala.pickling.PBuilder): Unit = {

          $builder.beginEntry($picklee, tag)
          $builder.hintElidedType(tag)
          ${utils.writeUnpicklerClassName(builder, picklerUnpicklerName)}
          ${utils.writeClassName(builder, picklee)}
          ${utils.writeCaptured(builder, picklee, capturedPickler, sporeType, utpe)}
          $builder.endEntry()

        }

        def unpickle(tag: String, $reader: scala.pickling.PReader): Any = {

          val $className = ${utils.readClassName(reader)}
          val $unpickledSpore = ${utils.createInstance(className, sporeType)}

          /* Ask for the unpickler at this point, since if we pass a var outside
           * this scope, the unpickler will be null. This is a safe operation
           * since such an `Unpickler` exists because we got it as an implicit */
          
          val $capturedUnpickler = implicitly[scala.pickling.Unpickler[$utpe]]
          if($capturedUnpickler == null) println("Unpickler[" + $utpeStr + "] is null")
          val $unpickledCapture = ${utils.readCaptured(reader, capturedUnpickler)}

          ${utils.setCapturedInSpore(unpickledSpore, unpickledCapture)}

          $unpickledSpore

        }
      }

      $picklerUnpicklerName
    """
  }

  def genSporePicklerUnpicklerImpl
      [T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: Context)(cPickler: c.Tree, cUnpickler: c.Tree): c.Tree = {

      import c.universe._

      val ttpe = weakTypeOf[T]
      val rtpe = weakTypeOf[R]
      val utpe = weakTypeOf[U]

      val numVarsCaptured = utpe.typeArgs.size
      debug(s"numVarsCaptured = $numVarsCaptured")
      debug(s"T: $ttpe, R: $rtpe, U: $utpe")

      val sporeType = tq"scala.spores.SporeWithEnv[$ttpe, $rtpe] {type Captured = $utpe}"
      genSporePicklerUnpicklerTemplate[U](c)(cPickler, cUnpickler, sporeType)

  }

  def genSpore2PicklerUnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: Context)(cPickler: c.Tree, cUnpickler: c.Tree): c.Tree = {

      import c.universe._

      val t1pe = weakTypeOf[T1]
      val t2pe = weakTypeOf[T2]
      val rtpe = weakTypeOf[R]
      val utpe = weakTypeOf[U]

      val numVarsCaptured = utpe.typeArgs.size
      debug(s"numVarsCaptured = $numVarsCaptured")
      debug(s"T1: $t1pe, T2: $t2pe, R: $rtpe, U: $utpe")

      val sporeType = tq"scala.spores.Spore2WithEnv[$t1pe, $t2pe, $rtpe] {type Captured = $utpe}"
      genSporePicklerUnpicklerTemplate[U](c)(cPickler, cUnpickler, sporeType)

  }

  def genSpore3PicklerUnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: Context)(cPickler: c.Tree, cUnpickler: c.Tree): c.Tree = {

      import c.universe._

      val t1pe = weakTypeOf[T1]
      val t2pe = weakTypeOf[T2]
      val t3pe = weakTypeOf[T3]
      val rtpe = weakTypeOf[R]
      val utpe = weakTypeOf[U]

      val numVarsCaptured = utpe.typeArgs.size
      debug(s"numVarsCaptured = $numVarsCaptured")
      debug(s"T1: $t1pe, T2: $t2pe, T3: $t3pe, R: $rtpe, U: $utpe")

      val sporeType = tq"scala.spores.Spore3WithEnv[$t1pe, $t2pe, $t3pe, $rtpe] {type Captured = $utpe}"
      genSporePicklerUnpicklerTemplate[U](c)(cPickler, cUnpickler, sporeType)

  }

  /** Generates an `Unpickler` that will be able to get the classname of another
    * `Unpickler` and successfully instantiate it and delegate his responsability.
    *
    * This is used to call `Unpickler`s that are generated at the same time as
    * the `Pickler`s but that cannot be run when unpickling because we don't have
    * enough information, e.g. the user does not specify that a `Spore` is indeed
    * `SporeWithEnv`. Also, it's not allowed to include the type refinements of
    * `Captured` in the type annotation of the method `unpickle`, so that
    * prevents them from being selected in the implicit search.
    */
  def genSporeUnpicklerFetcherTemplate(c: Context)(sporeType: c.Tree): c.Tree = {

    import c.universe._

    val utils = new PicklerUtils[c.type](c)
    val reader = c.freshName(TermName("reader"))
    val unpicklerType = tq"scala.pickling.Unpickler[$sporeType]"
    val unpicklerName = c.freshName(TermName("UnpicklerFetcher"))
    val unpicklerClassName = TermName("unpicklerClassName")

    q"""
      object $unpicklerName extends $unpicklerType {

        def tag = implicitly[scala.pickling.FastTypeTag[$sporeType]]

        def unpickle(tag: String, $reader: scala.pickling.PReader): Any = {

          val unpicklerClassName = ${utils.readUnpicklerClassName(reader)}
          debug("[genFetcherOfUnpickler] instantiate " + unpicklerClassName)
          val sporeUnpickler = ${utils.createInstance(unpicklerClassName, unpicklerType)}

          sporeUnpickler.unpickle(tag, $reader).asInstanceOf[$sporeType]

        }
      }

      $unpicklerName
    """

  }

  def genSporeUnpicklerImpl
      [T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]

    val sporeType = tq"scala.spores.Spore[$ttpe, $rtpe]"
    genSporeUnpicklerFetcherTemplate(c)(sporeType)

  }

  def genSpore2UnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag]
      (c: Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]

    val sporeType = tq"scala.spores.Spore2[$t1pe, $t2pe, $rtpe]"
    genSporeUnpicklerFetcherTemplate(c)(sporeType)

  }

  def genSpore3UnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag]
      (c: Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val t3pe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]

    val sporeType = tq"scala.spores.Spore3[$t1pe, $t2pe, $t3pe, $rtpe]"
    genSporeUnpicklerFetcherTemplate(c)(sporeType)

  }

}

object SporePickler extends SporePickler {

  /* Unify type for returning both a pickler and unpickler
   * that keeps track of the captured type inside a `Spore` */
  type FullP[T, R] = Pickler[Spore[T, R]]
  type FullU[T, R] = Unpickler[Spore[T, R]]

  type FullP2[T1, T2, R] = Pickler[Spore2[T1, T2, R]]
  type FullU2[T1, T2, R] = Unpickler[Spore2[T1, T2, R]]

  type FullP3[T1, T2, T3, R] = Pickler[Spore3[T1, T2, T3, R]]
  type FullU3[T1, T2, T3, R] = Unpickler[Spore3[T1, T2, T3, R]]

  /* Unify type for returning both a pickler and unpickler
   * that keeps track of the captured type inside a `SporeWithEnv` */
  type FullPE[T, R, C] = Pickler[SporeWithEnv[T, R] {type Captured = C}]
  type FullUE[T, R, C] = Unpickler[SporeWithEnv[T, R] {type Captured = C}]
  type FullPUE[T, R, C] = FullPE[T, R, C] with FullUE[T, R, C]

  type FullPE2[T1, T2, R, C] = Pickler[Spore2WithEnv[T1, T2, R] {type Captured = C}]
  type FullUE2[T1, T2, R, C] = Unpickler[Spore2WithEnv[T1, T2, R] {type Captured = C}]
  type FullPUE2[T1, T2, R, C] = FullPE2[T1, T2, R, C] with FullUE2[T1, T2, R, C]

  type FullPE3[T1, T2, T3, R, C] = Pickler[Spore3WithEnv[T1, T2, T3, R] {type Captured = C}]
  type FullUE3[T1, T2, T3, R, C] = Unpickler[Spore3WithEnv[T1, T2, T3, R] {type Captured = C}]
  type FullPUE3[T1, T2, T3, R, C] = FullPE3[T1, T2, T3, R, C] with FullUE3[T1, T2, T3, R, C]

  /********************* PICKLERS ***********************/

  /* `SporeWithEnv` has their own picklers and unpicklers and these unpicklers
   * can be picked in the implicit search if we don't look them up for the
   * subclass `Spore`. Otherwise, a `SporeWithEnv` can be disguised as a
   * `Spore` and in those cases we use the general `Unpickler` defined below. */

  implicit def genSporePicklerUnpickler[T, R, U]
    (implicit cPickler: Pickler[U], cUnpickler: Unpickler[U]): FullPUE[T, R, U] =
      macro genSporePicklerUnpicklerImpl[T, R, U]

  implicit def genSpore2PicklerUnpickler[T1, T2, R, U]
    (implicit cPickler: Pickler[U], cUnpickler: Unpickler[U]): FullPUE2[T1, T2, R, U] =
      macro genSpore2PicklerUnpicklerImpl[T1, T2, R, U]

  implicit def genSpore3PicklerUnpickler[T1, T2, T3, R, U]
    (implicit cPickler: Pickler[U], cUnpickler: Unpickler[U]): FullPUE3[T1, T2, T3, R, U] =
      macro genSpore3PicklerUnpicklerImpl[T1, T2, T3, R, U]

  /* Don't make these Picklers to have an `Unpickler` in the return type
   * since this will screw up the implicit search for any given `Spore` */

  implicit def genSimpleSporePickler[T, R]: FullP[T, R] =
    macro genSimpleSporePicklerImpl[T, R]

  implicit def genSimpleSpore2Pickler[T1, T2, R]: FullP2[T1, T2, R] =
    macro genSimpleSpore2PicklerImpl[T1, T2, R]

  implicit def genSimpleSpore3Pickler[T1, T2, T3, R]: FullP3[T1, T2, T3, R] =
      macro genSimpleSpore3PicklerImpl[T1, T2, T3, R]

  /********************* UNPICKLERS ***********************/

  /* These `Unpickler`s are meant to serialize both `Spore`s and `SporeWitEnv`s.
   * The subtyping relation between them forces us to have an intermediate
   * `Unpickler` which will get the correct `Unpickler` for the actual spore type. */

  implicit def genSporeUnpickler[T, R]: FullU[T, R] =
    macro genSporeUnpicklerImpl[T, R]

  implicit def genSpore2Unpickler[T1, T2, R]: FullU2[T1, T2, R] =
    macro genSpore2UnpicklerImpl[T1, T2, R]

  implicit def genSpore3Unpickler[T1, T2, T3, R]: FullU3[T1, T2, T3, R] =
    macro genSpore3UnpicklerImpl[T1, T2, T3, R]

}
