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
import scala.pickling.pickler.GeneratorRegistry
import scala.reflect.macros.blackbox

trait SporePicklers extends SimpleSporePicklers {

  /** The current implementation doesn't really use this method but
    * there can be corner cases or future bugs that could be fixed
    * using it. It's a good way to check if some type is primitive.
    */
  def isEffectivelyPrimitive(c: blackbox.Context)(tpe: c.Type): Boolean = {

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
  def genSporePicklerUnpicklerTemplate[U: c.WeakTypeTag]
      (c: blackbox.Context)(sporeType: c.Tree, sporeSubType: c.Tree): c.Tree = {

    import c.universe._

    val utpe = weakTypeOf[U].dealias
    debug(s"Captured type is: $utpe")

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
    import utils.{absPicklerUnpicklerType, picklerType, unpicklerType, pbuilderType, preaderType, scalaPath}
    import utils.{fastTypeTagType, stringType, anyType, unitType, autoRegisterType, locallyPath}
    val resultType = tq"$absPicklerUnpicklerType[$sporeType]"

    q"""
      $locallyPath[$resultType] {
        val $capturedPickler = implicitly[$picklerType[$utpe]]
        val $capturedUnpickler = implicitly[$unpicklerType[$utpe]]

        implicit object $picklerUnpicklerName
            extends $absPicklerUnpicklerType[$sporeType]
              with $autoRegisterType[$sporeType] {

          def tag = implicitly[$fastTypeTagType[$sporeType]]

          def pickle($picklee: $sporeType, $builder: $pbuilderType): $unitType = {

            $builder.beginEntry($picklee, tag)
            // Runtime picklers need the type, don't elide it
            // builder.hintElidedType(tag)
            ${utils.writeUnpicklerClassName(builder, picklerUnpicklerName)}
            ${utils.writeClassName(builder, picklee)}
            if($picklee.isInstanceOf[$sporeSubType]) {
              ${utils.writeCaptured(builder, picklee, capturedPickler,
                sporeSubType, utpe)}
            }
            $builder.endEntry()

          }

          def unpickle(tag: $stringType, $reader: $preaderType): $anyType = {

            val $className = ${utils.readClassName(reader)}
            val $unpickledSpore = ${utils.createInstance(className, sporeType)}

            if($unpickledSpore.isInstanceOf[$sporeSubType]) {

              /* Ask for the unpickler at this point, since if we pass a var outside
               * this scope, the unpickler will be null. This is a safe operation
               * since such an `Unpickler` exists because we got it as an implicit */

              val $capturedUnpickler = implicitly[$unpicklerType[$utpe]]
              val $unpickledCapture = ${utils.readCaptured(reader, capturedUnpickler)}
              ${utils.setCapturedInSpore(unpickledSpore, unpickledCapture, utpe)}

            }

            $unpickledSpore

          }
        }

        $picklerUnpicklerName: $resultType
      }
    """
  }

  def genNullarySporePicklerUnpicklerImpl
      [T: c.WeakTypeTag, U: c.WeakTypeTag](c: blackbox.Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]
    val utpe = weakTypeOf[U].dealias

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val numVarsCaptured = utpe.typeArgs.size
    debug(s"numVarsCaptured = $numVarsCaptured")
    debug(s"T: $ttpe, U: $utpe")

    val subSporeType = tq"$sporesPath.NullarySporeWithEnv[$ttpe] {type Captured = $utpe}"
    val mySporeType = tq"$sporesPath.NullarySpore[$ttpe] {type Captured = $utpe}"
    genSporePicklerUnpicklerTemplate[U](c)(mySporeType, subSporeType)

  }

  def genSporePicklerUnpicklerImpl
      [T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U].dealias

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val numVarsCaptured = utpe.typeArgs.size
    debug(s"numVarsCaptured = $numVarsCaptured")
    debug(s"T: $ttpe, R: $rtpe, U: $utpe")

    val subSporeType = tq"$sporesPath.SporeWithEnv[$ttpe, $rtpe] {type Captured = $utpe}"
    val mySporeType = tq"$sporesPath.Spore[$ttpe, $rtpe] {type Captured = $utpe}"
    genSporePicklerUnpicklerTemplate[U](c)(mySporeType, subSporeType)

  }

  def genSporeWithEnvPicklerUnpicklerImpl
      [T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U].dealias

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val numVarsCaptured = utpe.typeArgs.size
    debug(s"numVarsCaptured = $numVarsCaptured")
    debug(s"T: $ttpe, R: $rtpe, U: $utpe")

    val sporeTpe = tq"$sporesPath.SporeWithEnv[$ttpe, $rtpe] {type Captured = $utpe}"
    genSporePicklerUnpicklerTemplate[U](c)(sporeTpe, sporeTpe)

  }

  def genNullarySporeWithEnvPicklerUnpicklerImpl
      [T: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]
    val utpe = weakTypeOf[U].dealias

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val numVarsCaptured = utpe.typeArgs.size
    debug(s"numVarsCaptured = $numVarsCaptured")
    debug(s"T: $ttpe, U: $utpe")

    val sporeTpe = tq"$sporesPath.NullarySporeWithEnv[$ttpe] {type Captured = $utpe}"
    genSporePicklerUnpicklerTemplate[U](c)(sporeTpe, sporeTpe)

  }

  def genSpore2PicklerUnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U].dealias

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val numVarsCaptured = utpe.typeArgs.size
    debug(s"numVarsCaptured = $numVarsCaptured")
    debug(s"T1: $t1pe, T2: $t2pe, R: $rtpe, U: $utpe")

    val sporeType = tq"$sporesPath.Spore2[$t1pe, $t2pe, $rtpe] {type Captured = $utpe}"
    val subSporeType = tq"$sporesPath.Spore2WithEnv[$t1pe, $t2pe, $rtpe] {type Captured = $utpe}"
    genSporePicklerUnpicklerTemplate[U](c)(sporeType, subSporeType)

  }

  def genSpore2WithEnvPicklerUnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U].dealias

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val numVarsCaptured = utpe.typeArgs.size
    debug(s"numVarsCaptured = $numVarsCaptured")
    debug(s"T1: $t1pe, T2: $t2pe, R: $rtpe, U: $utpe")

    val sporeType = tq"$sporesPath.Spore2WithEnv[$t1pe, $t2pe, $rtpe] {type Captured = $utpe}"
    genSporePicklerUnpicklerTemplate[U](c)(sporeType, sporeType)

  }
  
  def genSpore3PicklerUnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val t3pe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U].dealias

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val numVarsCaptured = utpe.typeArgs.size
    debug(s"numVarsCaptured = $numVarsCaptured")
    debug(s"T1: $t1pe, T2: $t2pe, T3: $t3pe, R: $rtpe, U: $utpe")

    val sporeType = tq"$sporesPath.Spore3[$t1pe, $t2pe, $t3pe, $rtpe] {type Captured = $utpe}"
    val subSporeType = tq"$sporesPath.Spore3WithEnv[$t1pe, $t2pe, $t3pe, $rtpe] {type Captured = $utpe}"
    genSporePicklerUnpicklerTemplate[U](c)(sporeType, subSporeType)

  }

  def genSpore3WithEnvPicklerUnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val t3pe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U].dealias

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val numVarsCaptured = utpe.typeArgs.size
    debug(s"numVarsCaptured = $numVarsCaptured")
    debug(s"T1: $t1pe, T2: $t2pe, T3: $t3pe, R: $rtpe, U: $utpe")

    val sporeType = tq"$sporesPath.Spore3WithEnv[$t1pe, $t2pe, $t3pe, $rtpe] {type Captured = $utpe}"
    genSporePicklerUnpicklerTemplate[U](c)(sporeType, sporeType)

  }

  /** Generates an `Unpickler` that will be able to get the classname of another
    * `Unpickler` and successfully instantiate it and delegate its responsability.
    *
    * This is used to call `Unpickler`s that are generated at the same time as
    * the `Pickler`s but that cannot be run when unpickling because we don't have
    * enough information, e.g. the user does not specify that a `Spore` is indeed
    * `SporeWithEnv`. Also, it's not allowed to include the type refinements of
    * `Captured` in the type annotation of the method `unpickle`, so that
    * prevents them from being selected in the implicit search.
    */
  def genSporeUnpicklerFetcherTemplate(c: blackbox.Context)
                                      (sporeType: c.Tree): c.Tree = {

    import c.universe._

    val utils = new PicklerUtils[c.type](c)
    val reader = c.freshName(TermName("reader"))
    val unpicklerName = c.freshName(TermName("UnpicklerFetcher"))
    val unpicklerClassName = TermName("unpicklerClassName")
    import utils.{unpicklerType, preaderType, stringType}
    import utils.{fastTypeTagType, anyType, locallyPath, autoRegisterUnpicklerType}
    val unpicklerTpe = tq"$unpicklerType[$sporeType]"

    q"""
      $locallyPath {
        implicit object $unpicklerName extends $unpicklerTpe
            with $autoRegisterUnpicklerType[$sporeType] {

          def tag = implicitly[$fastTypeTagType[$sporeType]]

          def unpickle(tag: $stringType, $reader: $preaderType): $anyType = {

            val unpicklerClassName = ${utils.readUnpicklerClassName(reader)}
            val sporeUnpickler = ${utils.createInstance(unpicklerClassName, unpicklerTpe)}
            sporeUnpickler.unpickle(tag, $reader).asInstanceOf[$sporeType]

          }
        }

        $unpicklerName
      }
    """

  }

  def genSporeUnpicklerImpl
    [T: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]

    val utils = new PicklerUtils[c.type](c)
    val sporeType = tq"${utils.sporesPath}.Spore[$ttpe, $rtpe]"
    genSporeUnpicklerFetcherTemplate(c)(sporeType)

  }

  def genNullarySporeUnpicklerImpl
    [T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]

    val utils = new PicklerUtils[c.type](c)
    val sporeType = tq"${utils.sporesPath}.NullarySpore[$ttpe]"
    genSporeUnpicklerFetcherTemplate(c)(sporeType)

  }

  def genSporeCapturedUnpicklerImpl
      [T: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    val utils = new PicklerUtils[c.type](c)
    import utils.{sporesPath, unpicklerType}
    val sporeType = tq"$sporesPath.Spore[$ttpe, $rtpe] {type Captured = $utpe}"

    if(utils.isNothing(utpe.typeSymbol.asType.toType)) {
      // Trick to reuse our unpickler for simple spores
      q"""
        ${genSporeUnpicklerImpl[T, R](c)}
          .asInstanceOf[$unpicklerType[$sporeType]]
      """
    } else {
      genSporeUnpicklerFetcherTemplate(c)(sporeType)
    }

  }

  def genNullarySporeCapturedUnpicklerImpl
      [T: c.WeakTypeTag, U: c.WeakTypeTag](c: blackbox.Context): c.Tree = {

    import c.universe._

    val ttpe = weakTypeOf[T]
    val utpe = weakTypeOf[U]

    val utils = new PicklerUtils[c.type](c)
    import utils.{sporesPath, unpicklerType}
    val sporeType = tq"$sporesPath.NullarySpore[$ttpe] {type Captured = $utpe}"

    if(utils.isNothing(utpe.typeSymbol.asType.toType)) {
      // Trick to reuse our unpickler for simple spores
      q"""
        ${genNullarySporeUnpicklerImpl[T](c)}
          .asInstanceOf[$unpicklerType[$sporeType]]
      """
    } else {
      genSporeUnpicklerFetcherTemplate(c)(sporeType)
    }

  }

  def genSpore2UnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val sporeType = tq"$sporesPath.Spore2[$t1pe, $t2pe, $rtpe]"
    genSporeUnpicklerFetcherTemplate(c)(sporeType)

  }

  def genSpore2CapturedUnpicklerImpl
  [T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
  (c: blackbox.Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    val utils = new PicklerUtils[c.type](c)
    import utils.{sporesPath, unpicklerType}
    val sporeType = tq"$sporesPath.Spore2[$t1pe, $t2pe, $rtpe] {type Captured = $utpe}"

    if(utils.isNothing(utpe.typeSymbol.asType.toType)) {
      // Trick to reuse our unpickler for simple spores
      q"""
        ${genSpore2UnpicklerImpl[T1, T2, R](c)}
          .asInstanceOf[$unpicklerType[$sporeType]]
      """
    } else {
      genSporeUnpicklerFetcherTemplate(c)(sporeType)
    }

  }

  def genSpore3UnpicklerImpl
      [T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag]
      (c: blackbox.Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val t3pe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]

    val utils = new PicklerUtils[c.type](c)
    import utils.sporesPath
    val sporeType = tq"$sporesPath.Spore3[$t1pe, $t2pe, $t3pe, $rtpe]"
    genSporeUnpicklerFetcherTemplate(c)(sporeType)

  }

  def genSpore3CapturedUnpicklerImpl
  [T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag, U: c.WeakTypeTag]
  (c: blackbox.Context): c.Tree = {

    import c.universe._

    val t1pe = weakTypeOf[T1]
    val t2pe = weakTypeOf[T2]
    val t3pe = weakTypeOf[T3]
    val rtpe = weakTypeOf[R]
    val utpe = weakTypeOf[U]

    val utils = new PicklerUtils[c.type](c)
    import utils.{sporesPath, unpicklerType}
    val sporeType = tq"$sporesPath.Spore3[$t1pe, $t2pe, $t3pe, $rtpe] {type Captured = $utpe}"

    if(utils.isNothing(utpe.typeSymbol.asType.toType)) {
      // Trick to reuse our unpickler for simple spores
      q"""
        ${genSpore3UnpicklerImpl[T1, T2, T3, R](c)}
          .asInstanceOf[$unpicklerType[$sporeType]]
      """
    } else {
      genSporeUnpicklerFetcherTemplate(c)(sporeType)
    }

  }

}

object SporePicklers extends SporePicklers
    with SporeRuntimePicklers with GeneratorRegistry {

  /* Unify type for returning both a pickler and unpickler
   * that keeps track of the captured type inside a `Spore` */
  type FullPN[T] = Pickler[NullarySpore[T]]
  type FullUN[T] = Unpickler[NullarySpore[T]]

  type FullP[T, R] = Pickler[Spore[T, R]]
  type FullU[T, R] = Unpickler[Spore[T, R]]

  type FullP2[T1, T2, R] = Pickler[Spore2[T1, T2, R]]
  type FullU2[T1, T2, R] = Unpickler[Spore2[T1, T2, R]]

  type FullP3[T1, T2, T3, R] = Pickler[Spore3[T1, T2, T3, R]]
  type FullU3[T1, T2, T3, R] = Unpickler[Spore3[T1, T2, T3, R]]

  /* These guys also enable to find picklers and unpicklers when
   * the captured type is specified for any kind of spore. */
  type FullPCN[T, C] = Pickler[NullarySpore[T] {type Captured = C}]
  type FullUCN[T, C] = Unpickler[NullarySpore[T] {type Captured = C}]

  type FullPC[T, R, C] = Pickler[Spore[T, R] {type Captured = C}]
  type FullUC[T, R, C] = Unpickler[Spore[T, R] {type Captured = C}]

  type FullPC2[T1, T2, R, C] = Pickler[Spore2[T1, T2, R] {type Captured = C}]
  type FullUC2[T1, T2, R, C] = Unpickler[Spore2[T1, T2, R] {type Captured = C}]

  type FullPC3[T1, T2, T3, R, C] = Pickler[Spore3[T1, T2, T3, R] {type Captured = C}]
  type FullUC3[T1, T2, T3, R, C] = Unpickler[Spore3[T1, T2, T3, R] {type Captured = C}]

  /* Unify type for returning both a pickler and unpickler
   * that keeps track of the captured type inside a `SporeWithEnv` */
  type FullPE[T, R, C] = Pickler[SporeWithEnv[T, R] {type Captured = C}]
  type FullUE[T, R, C] = Unpickler[SporeWithEnv[T, R] {type Captured = C}]
  type FullPUE[T, R, C] = FullPE[T, R, C] with FullUE[T, R, C]

  type FullPEN[T, C] = Pickler[NullarySporeWithEnv[T] {type Captured = C}]
  type FullUEN[T, C] = Unpickler[NullarySporeWithEnv[T] {type Captured = C}]
  type FullPUEN[T, C] = FullPEN[T, C] with FullUEN[T, C]

  type FullPE2[T1, T2, R, C] = Pickler[Spore2WithEnv[T1, T2, R] {type Captured = C}]
  type FullUE2[T1, T2, R, C] = Unpickler[Spore2WithEnv[T1, T2, R] {type Captured = C}]
  type FullPUE2[T1, T2, R, C] = FullPE2[T1, T2, R, C] with FullUE2[T1, T2, R, C]

  type FullPE3[T1, T2, T3, R, C] = Pickler[Spore3WithEnv[T1, T2, T3, R] {type Captured = C}]
  type FullUE3[T1, T2, T3, R, C] = Unpickler[Spore3WithEnv[T1, T2, T3, R] {type Captured = C}]
  type FullPUE3[T1, T2, T3, R, C] = FullPE3[T1, T2, T3, R, C] with FullUE3[T1, T2, T3, R, C]

  /********************************* Picklers *********************************/

  /* `SporeWithEnv` has their own picklers and unpicklers and these unpicklers
   * can be picked in the implicit search if we don't look them up for the
   * subclass `Spore`. Otherwise, a `SporeWithEnv` can be disguised as a
   * `Spore` and in those cases we use the general `Unpickler` defined below.
   *
   * Note that we could ask for the implicits of pickler and unpickler of `U`
   * here but we don't. The reason is that the scala compiler will interpret
   * the type `U` to be `this.Captured` instead of the dealiased type. This
   * spoils the type signatures and makes scala pickling to generate new
   * picklers for `this.Captured` every time, slowing down the compilation time.
   */

  implicit def genNullarySporePicklerUnpickler[T, U]: FullPCN[T, U] =
    macro genNullarySporePicklerUnpicklerImpl[T, U]

  implicit def genSporePicklerUnpickler[T, R, U]: FullPC[T, R, U] =
    macro genSporePicklerUnpicklerImpl[T, R, U]

  implicit def genNullarySporePicklerUnpicklerWithEnv[T, U]: FullPUEN[T, U] =
    macro genNullarySporeWithEnvPicklerUnpicklerImpl[T, U]

  implicit def genSporePicklerUnpicklerWithEnv[T, R, U]: FullPUE[T, R, U] =
    macro genSporeWithEnvPicklerUnpicklerImpl[T, R, U]

  implicit def genSpore2PicklerUnpickler[T1, T2, R, U]: FullPC2[T1, T2, R, U] =
    macro genSpore2PicklerUnpicklerImpl[T1, T2, R, U]

  implicit def genSpore2PicklerUnpicklerWithEnv[T1, T2, R, U]: FullPUE2[T1, T2, R, U] =
    macro genSpore2WithEnvPicklerUnpicklerImpl[T1, T2, R, U]

  implicit def genSpore3PicklerUnpickler[T1, T2, T3, R, U]: FullPC3[T1, T2, T3, R, U] =
    macro genSpore3PicklerUnpicklerImpl[T1, T2, T3, R, U]

  implicit def genSpore3PicklerUnpicklerWithEnv[T1, T2, T3, R, U]: FullPUE3[T1, T2, T3, R, U] =
    macro genSpore3WithEnvPicklerUnpicklerImpl[T1, T2, T3, R, U]

  /* Don't make these Picklers to have an `Unpickler` in the return type
   * since this will screw up the implicit search for any given `Spore` */

  implicit def genNullarySporePickler[T]: FullPN[T] =
    macro genNullarySporePicklerImpl[T]

  implicit def genSimpleSporePickler[T, R]: FullP[T, R] =
    macro genSimpleSporePicklerImpl[T, R]

  implicit def genSimpleSpore2Pickler[T1, T2, R]: FullP2[T1, T2, R] =
    macro genSimpleSpore2PicklerImpl[T1, T2, R]

  implicit def genSimpleSpore3Pickler[T1, T2, T3, R]: FullP3[T1, T2, T3, R] =
    macro genSimpleSpore3PicklerImpl[T1, T2, T3, R]

  /******************************** Unpicklers ********************************/

  /* These `Unpickler`s are meant to unpickle both `Spore`s and `SporeWithEnv`s.
   * The subtyping relation between them forces us to have an intermediate
   * `Unpickler` which will get the correct `Unpickler` for the actual spore type. */

  implicit def genNullarySporeUnpickler[T]: FullUN[T] =
    macro genNullarySporeUnpicklerImpl[T]

  implicit def genSporeUnpickler[T, R]: FullU[T, R] =
    macro genSporeUnpicklerImpl[T, R]

  implicit def genNullarySporeCapturedUnpickler[T, C]: FullUCN[T, C] =
    macro genNullarySporeCapturedUnpicklerImpl[T, C]

  implicit def genSporeCapturedUnpickler[T, R, C]: FullUC[T, R, C] =
    macro genSporeCapturedUnpicklerImpl[T, R, C]

  implicit def genSpore2Unpickler[T1, T2, R]: FullU2[T1, T2, R] =
    macro genSpore2UnpicklerImpl[T1, T2, R]

  implicit def genSpore2CapturedUnpickler[T1, T2, R, C]: FullUC2[T1, T2, R, C] =
    macro genSpore2CapturedUnpicklerImpl[T1, T2, R, C]

  implicit def genSpore3Unpickler[T1, T2, T3, R]: FullU3[T1, T2, T3, R] =
    macro genSpore3UnpicklerImpl[T1, T2, T3, R]

  implicit def genSpore3CapturedUnpickler[T1, T2, T3, R, C]: FullUC3[T1, T2, T3, R, C] =
    macro genSpore3CapturedUnpicklerImpl[T1, T2, T3, R, C]

  /********************** Runtime picklers and unpicklers *********************/

  def registerRuntimePicklerUnpickler(): Unit =
    registerPicklerAsGen(SporeRuntimePicklerUnpickler)

  locally {
    registerRuntimePicklerUnpickler()
  }

}
