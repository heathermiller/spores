/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.language.experimental.macros
import scala.reflect.macros.Context

package object spores {

  def capture[T](x: T): T = x

  /**
   *  Usage:
   *
   *  spore {
   *    val x = outerX
   *    val y = outerY
   *    (p: T) => <body>
   *  }
   *
   *  Check that body only accesses x, y, p, and variables local to (owned by) the
   *  closure.
   */
  def spore[T, R](fun: T => R): Spore[T, R] = macro sporeImpl[T, R]

  def spore[T1, T2, R](fun: (T1, T2) => R): Spore2[T1, T2, R] = macro spore2Impl[T1, T2, R]

  def spore[T1, T2, T3, R](fun: (T1, T2, T3) => R): Spore3[T1, T2, T3, R] = macro spore3Impl[T1, T2, T3, R]

  implicit def mkSpore[T, R](fun: T => R): Spore[T, R] = macro sporeImpl[T, R]

  def delayed[T](body: T): Function0[T] = macro delayedImpl[T]

  def delayedImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[Function0[T]] = {
    import c.universe._
    // check Spore constraints
    //check(c)(fun.tree)

    reify {
      () => body.splice
    }
  }

  // TOGGLE DEBUGGING
  private val isDebugEnabled = System.getProperty("spores.debug", "false").toBoolean
  private[spores] def debug(s: => String): Unit =
    if (isDebugEnabled) println(s)

  /**
     spore {
       val x = outer
       (y: T) => { ... }
     }
   */
  private def check(c: Context)(funTree: c.Tree, ttpe: c.Type, rtpe: c.Type): c.Tree = {
    import c.universe._

    // traverse body of `fun` and check that the free vars access only allowed things
    val (validEnv, funLiteral) = funTree match {
      case Block(stmts, expr) =>
        val validVarSyms = stmts.toList flatMap { stmt =>
          stmt match {
            case vd @ ValDef(mods, name, tpt, rhs) => List(vd.symbol)
            case _ =>
              c.error(stmt.pos, "Only val defs allowed at this position")
              List()
          }
        }
        validVarSyms foreach { sym => debug("valid: " + sym) }
        (validVarSyms, expr)

      case expr =>
        (List(), expr)
    }

    val captureSym = typeOf[spores.`package`.type].member(newTermName("capture"))

    val (paramSym, retTpe, funBody) = funLiteral match {
      case fun @ Function(vparams, body) =>

        // contains all symbols found in `capture` syntax
        var capturedSyms = List[Symbol]()

        // is the use of symbol s allowed via spore rules? (in the spore body)
        def isSymbolValid(s: Symbol): Boolean =
          validEnv.contains(s) ||
          capturedSyms.contains(s) ||
          s.owner == fun.symbol ||
          s.isStatic || {
            c.error(s.pos, "invalid reference to " + s)
            false
          }

        // is tree t a path with only components that satisfy pred? (eg stable or lazy)
        def isPathWith(t: Tree)(pred: TermSymbol => Boolean): Boolean = t match {
          case sel @ Select(s, _) =>
            isPathWith(s)(pred) && pred(sel.symbol.asTerm)
          case id: Ident =>
            pred(id.symbol.asTerm)
          case th: This =>
            true
          // we can't seem to have a super in paths because of S-1938, pity
          // https://issues.scala-lang.org/browse/SI-1938
          // case supr: Super =>
          //   true
          case _ =>
            false
        }

        // traverse the spore body and collect symbols in `capture` invocations
        val collectCapturedTraverser = new Traverser {
          override def traverse(tree: Tree): Unit = tree match {
            case app @ Apply(fun, List(captured)) if (fun.symbol == captureSym) =>
              debug("found capture: " + app)
              if (!isPathWith(captured)(_.isStable))
                c.error(captured.pos, "Only stable paths can be captured")
              else if (!isPathWith(captured)(!_.isLazy))
                c.error(captured.pos, "A captured path cannot contain lazy members")
              else
                capturedSyms ::= captured.symbol
            case _ =>
              super.traverse(tree)
          }
        }
        debug("collecting captured symbols")
        collectCapturedTraverser.traverse(body)

        debug(s"checking $body...")
        // check the spore body, ie for all identifiers, check that they are valid according to spore rules
        // ie, either declared locally or captured via a `capture` invocation
        val traverser = new Traverser {
          override def traverse(tree: Tree) {
            tree match {
              case id: Ident =>
                debug("checking ident " + id)
                isSymbolValid(id.symbol)

              // x.m().s
              case sel @ Select(app: Apply, _) =>
                debug("checking select (app)" + sel)
                if (app.symbol.isStatic) {
                  debug("OK, fun static")
                } else c.error(sel.pos, "the fun is not static")

              case sel @ Select(pre, _) =>
                debug("checking select " + sel)
                isSymbolValid(sel.symbol)

              case _ =>
                super.traverse(tree)
            }
          }
        }

        traverser.traverse(body)
        (vparams.head.symbol, body.tpe, body)

      case _ =>
        c.error(funLiteral.pos, "Incorrect usage of `spore`: function literal expected")
        (null, null, null)
    }

    val res = c.inferImplicitValue(typeOf[Property[_]], silent = true)
    debug(s"result of inferring property: $res")
    // type of inferred implicit value
    val optPropTpe = if (res != EmptyTree) {
      val propTpe = res.tpe
      debug(s"type of property: $propTpe, type args: ${propTpe.typeArgs}")

      // type of implicit val to be added to spore
      val instTpe = propTpe.typeArgs.head
      debug(s"adding instances of base type: $instTpe")

      // example:
      val intInstTpe = appliedType(instTpe.typeConstructor, typeOf[Int])
      debug(s"example Int instance: $intInstTpe")

      Some(instTpe.typeConstructor)
    } else
      None

    if (paramSym != null) {
      val applyParamName = c.fresh(newTermName("x"))
      val id = Ident(applyParamName)
      val applyName = newTermName("apply")

      val applyParamValDef = ValDef(Modifiers(Flag.PARAM), applyParamName, TypeTree(paramSym.typeSignature), EmptyTree)
      val applyParamSymbol = applyParamValDef.symbol

      val symtable = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]

      if (validEnv.isEmpty) {
        // replace reference to paramSym with reference to applyParamSymbol
        val substituter = new symtable.TreeSubstituter(List(paramSym.asInstanceOf[symtable.Symbol]), List(id.asInstanceOf[symtable.Tree]))
        val newFunBody = substituter.transform(funBody.asInstanceOf[symtable.Tree])

        val nfBody = c.resetLocalAttrs(newFunBody.asInstanceOf[c.universe.Tree])

        val applyDefDef: DefDef = {
          val applyVParamss = List(List(applyParamValDef))
          DefDef(NoMods, applyName, Nil, applyVParamss, TypeTree(retTpe), nfBody)
        }

        val sporeClassName = c.fresh(newTypeName("anonspore"))

        q"""
          class $sporeClassName extends Spore[$ttpe, $rtpe] {
            val className: String = ${sporeClassName.toString}
            $applyDefDef
          }
          new $sporeClassName
        """
      } else {
        // replace reference to paramSym with reference to applyParamSymbol
        // and reference to captured variable to new field
        val capturedTypes = validEnv.map(sym => sym.typeSignature)
        if (capturedTypes.size > 1) ???
        else {
          val fieldName = newTermName("c1")
          val fieldId = Ident(fieldName)

          val symsToReplace = List(paramSym, validEnv.head).map(_.asInstanceOf[symtable.Symbol])
          val idsToSubstitute = List(id, fieldId).map(_.asInstanceOf[symtable.Tree])

          val substituter = new symtable.TreeSubstituter(symsToReplace, idsToSubstitute)
          val newFunBody = substituter.transform(funBody.asInstanceOf[symtable.Tree])

          val nfBody = c.resetLocalAttrs(newFunBody.asInstanceOf[c.universe.Tree])
          val applyDefDef: DefDef = {
            val applyVParamss = List(List(applyParamValDef))
            DefDef(NoMods, applyName, Nil, applyVParamss, TypeTree(retTpe), nfBody)
          }

          val rhs = (funTree match {
            case Block(stmts, expr) =>
              stmts.toList flatMap { stmt =>
                stmt match {
                  case vd @ ValDef(mods, name, tpt, rhs) => List(rhs)
                  case _ =>
                    c.error(stmt.pos, "Only val defs allowed at this position")
                    List()
                }
              }
          }).head

          val sporeClassName = c.fresh(newTypeName("anonspore"))
          val initializerName = c.fresh(newTermName("initialize"))
          val capturedTpe = capturedTypes.head

          q"""
            class $sporeClassName(val $fieldName: ${capturedTypes.head}) extends SporeC1[$ttpe, $rtpe] {
              type Captured = $capturedTpe
              val className: String = ${sporeClassName.toString}
              $applyDefDef
            }
            val $initializerName = $rhs
            new $sporeClassName($initializerName)
          """
        }
      }
    } else {
      ???
    }
  }


  def sporeImpl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(fun: c.Expr[T => R]): c.Expr[Spore[T, R]] = {
    import c.universe._

    // check Spore constraints
    val tree = check(c)(fun.tree, weakTypeOf[T], weakTypeOf[R])

    c.Expr[Spore[T, R]](tree)
  }

  def spore2Impl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(fun: c.Expr[(T1, T2) => R]): c.Expr[Spore2[T1, T2, R]] = {
    import c.universe._

    // check Spore constraints
    check(c)(fun.tree, null, null)

    reify {
      new Spore2Impl(fun.splice)
    }
  }

  def spore3Impl[T1: c.WeakTypeTag, T2: c.WeakTypeTag, T3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(fun: c.Expr[(T1, T2, T3) => R]): c.Expr[Spore3[T1, T2, T3, R]] = {
    import c.universe._

    // check Spore constraints
    check(c)(fun.tree, null, null)

    reify {
      new Spore3Impl(fun.splice)
    }
  }

  // type constraint checking idea
  private def checkTc(c: Context)(funTree: c.Tree): List[c.Type] = {
    import c.universe._

    // traverse body of `fun` and check that the free vars access only allowed things
    val (validEnv, funLiteral) = funTree match {
      case Block(stmts, expr) =>
        val validVarSyms: List[(Symbol, Type)] = stmts.toList flatMap { stmt =>
          stmt match {
            case vd @ ValDef(mods, name, tpt, rhs) =>
              List(vd.symbol -> tpt.tpe)
            case td @ TypeDef(mods, name, tparams, rhs) =>
              println(s"found type def $name with rhs: $rhs")
              if (name.toString == "Constraint") {
                rhs match {
                  case tpt @ TypeTree() =>
                    println(s"found TypeTree, tpe: ${tpt.tpe}")
                  case _ =>
                    println(s"rhs is something else: ${rhs.getClass}")
                }
              }
              List()
            case _ =>
              c.error(stmt.pos, "Only val defs allowed at this position")
              List()
          }
        }
        validVarSyms foreach { p => debug("valid: " + p) }
        (validVarSyms, expr)

      case expr =>
        (List(), expr)
    }

    funLiteral match {
      case fun @ Function(vparams, body) =>

        def isSymbolValid(s: Symbol): Boolean =
          validEnv.map(_._1).contains(s) ||
          s.owner == fun.symbol ||
          s.isStatic || {
            c.error(s.pos, "invalid reference to " + s)
            false
          }

        debug(s"checking $body...")
        val traverser = new Traverser {
          override def traverse(tree: Tree) {
            tree match {
              case id: Ident =>
                debug("checking ident " + id)
                isSymbolValid(id.symbol)

              case sel @ Select(app: Apply, _) =>
                debug("checking select (app)" + sel)
                if (app.symbol.isStatic) {
                  debug("OK, fun static")
                } else c.error(sel.pos, "the fun is not static")

              case sel @ Select(pre, _) =>
                debug("checking select " + sel)
                isSymbolValid(sel.symbol)

              case _ =>
                super.traverse(tree)
            }
          }
        }

        traverser.traverse(body)
      case _ =>
        c.error(funLiteral.pos, "Function literal expected")
    }

    validEnv map { _._2 }
  }

  // sketch involved with type constraints, probably unneeded
  def sporeWith[T, R](fun: T => R): Spore[T, R] = macro sporeTcImpl[T, R]

  def sporeTcImpl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(fun: c.Expr[T => R]): c.Expr[Spore[T, R]] = {
    import c.universe._

    // check Spore constraints
    val tpes = checkTc(c)(fun.tree)
    println("captured types: " + tpes)

    reify {
      val f = fun.splice
      new Spore[T, R] {
        def apply(x: T): R = f(x)
      }
    }
  }

}