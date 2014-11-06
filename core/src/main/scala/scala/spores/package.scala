/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.language.experimental.macros
import scala.language.implicitConversions

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

    def ownerChainContains(sym: Symbol, owner: Symbol): Boolean = {
      sym != null && (sym.owner == owner || {
        sym.owner != NoSymbol && ownerChainContains(sym.owner, owner)
      })
    }

    // traverse body of `fun` and check that the free vars access only allowed things
    // `validEnv` == symbols declared in the spore header
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
          validEnv.contains(s) ||              // is `s` declared in the spore header?
          capturedSyms.contains(s) ||          // is `s` captured using the `capture` syntax?
          ownerChainContains(s, fun.symbol) || // is `s` declared within `fun`?
          s == NoSymbol ||                     // is `s` == `_`?
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
              case sel @ Select(app @ Apply(fun0, args0), _) =>
                debug("checking select (app): " + sel)
                if (app.symbol.isStatic) {
                  debug("OK, fun static")
                } else fun0 match {
                  case Select(obj, _) =>
                    if (ownerChainContains(obj.symbol, fun.symbol)) debug(s"OK, selected on local object $obj")
                    else c.error(sel.pos, "the fun is not static")
                  case _ =>
                    c.error(sel.pos, "the fun is not static")
                }

              case sel @ Select(pre, _) =>
                debug("checking select " + sel)
                if (!sel.symbol.isMethod)
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
      } else if (validEnv.size == 1) { // TODO: simplify
        // replace reference to paramSym with reference to applyParamSymbol
        // and references to captured variables to new fields
        val capturedTypes = validEnv.map(sym => sym.typeSignature)
        // println(s"capturedTypes: ${capturedTypes.mkString(",")}")

        val fieldNames = (1 to capturedTypes.size).map(i => newTermName(s"c$i")).toList
        val fieldIds   = fieldNames.map(n => Ident(n))

        val symsToReplace = (paramSym :: validEnv).map(_.asInstanceOf[symtable.Symbol])
        val idsToSubstitute = (id :: fieldIds).map(_.asInstanceOf[symtable.Tree])

        val substituter = new symtable.TreeSubstituter(symsToReplace, idsToSubstitute)
        val newFunBody = substituter.transform(funBody.asInstanceOf[symtable.Tree])

        val nfBody = c.resetLocalAttrs(newFunBody.asInstanceOf[c.universe.Tree])
        val applyDefDef: DefDef = {
          val applyVParamss = List(List(applyParamValDef))
          DefDef(NoMods, applyName, Nil, applyVParamss, TypeTree(retTpe), nfBody)
        }

        val rhss = funTree match {
          case Block(stmts, expr) =>
            stmts.toList flatMap { stmt =>
              stmt match {
                case vd @ ValDef(mods, name, tpt, rhs) => List(rhs)
                case _ =>
                  c.error(stmt.pos, "Only val defs allowed at this position")
                  List()
              }
            }
        }

        val sporeClassName = c.fresh(newTypeName("anonspore"))
        val initializerNames = (1 to capturedTypes.size).map(i => c.fresh(newTermName(s"initialize$i")))

        val initializerName = c.fresh(newTermName(s"initialize"))
        val initializerTrees = fieldNames.zip(rhss).zipWithIndex.map {
          case ((n, rhs), i) =>
            val t = newTypeName("C" + (i+1))
            q"$initializerName.$n = $rhs.asInstanceOf[$initializerName.$t]"
        }

        val fieldTrees = fieldNames.zipWithIndex.map {
          case (n, i) =>
            val t = newTypeName("C" + (i+1))
            q"var $n: $t = _"
        }

        val superclassName = newTypeName(s"SporeC${capturedTypes.size}")
        val captureTypeTree = (if (capturedTypes.size == 1) q"type Captured = ${capturedTypes(0)}"
          else if (capturedTypes.size == 2) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)})"
          else if (capturedTypes.size == 3) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)})"
          else if (capturedTypes.size == 4) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)})").asInstanceOf[c.Tree]

        val cTypeTrees = capturedTypes.zipWithIndex.map {
          case (t, i) =>
            val n = newTypeName("C" + (i + 1))
            q"type $n = $t"
        }

        q"""
          final class $sporeClassName extends $superclassName[$ttpe, $rtpe] {
            $captureTypeTree
            ..$cTypeTrees
            ..$fieldTrees
            val className: String = this.getClass.getName
            $applyDefDef
          }
          val $initializerName = new $sporeClassName
          ..$initializerTrees
          $initializerName
        """
      } else { // validEnv.size > 1
        // replace reference to paramSym with reference to applyParamSymbol
        // and references to captured variables to new fields
        val capturedTypes = validEnv.map(sym => sym.typeSignature)
        // println(s"capturedTypes: ${capturedTypes.mkString(",")}")

        val symsToReplace = (paramSym :: validEnv).map(_.asInstanceOf[symtable.Symbol])
        val newTrees = (1 to validEnv.size).map(i => Select(Ident(newTermName("captured")), newTermName(s"_$i"))).toList
        val treesToSubstitute = (id :: newTrees).map(_.asInstanceOf[symtable.Tree])

        val substituter = new symtable.TreeSubstituter(symsToReplace, treesToSubstitute)
        val newFunBody = substituter.transform(funBody.asInstanceOf[symtable.Tree])

        val nfBody = c.resetLocalAttrs(newFunBody.asInstanceOf[c.universe.Tree])
        val applyDefDef: DefDef = {
          val applyVParamss = List(List(applyParamValDef))
          DefDef(NoMods, applyName, Nil, applyVParamss, TypeTree(retTpe), nfBody)
        }

        val rhss = funTree match {
          case Block(stmts, expr) =>
            stmts.toList flatMap { stmt =>
              stmt match {
                case vd @ ValDef(mods, name, tpt, rhs) => List(rhs)
                case _ =>
                  c.error(stmt.pos, "Only val defs allowed at this position")
                  List()
              }
            }
        }

        val sporeClassName  = c.fresh(newTypeName("anonspore"))
        val initializerName = c.fresh(newTermName(s"initialize"))
        val initializerTree = q"$initializerName.captured = (..$rhss)"
        val superclassName  = newTypeName(s"SporeWithEnv")

        val captureTypeTree = (if (capturedTypes.size == 2) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)})"
          else if (capturedTypes.size == 3) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)})"
          else if (capturedTypes.size == 4) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)})"
          else if (capturedTypes.size == 5) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)})"
          else if (capturedTypes.size == 6) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)})"
          else if (capturedTypes.size == 7) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)}, ${capturedTypes(6)})"
          else if (capturedTypes.size == 8) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)}, ${capturedTypes(6)}, ${capturedTypes(7)})").asInstanceOf[c.Tree]

        q"""
          final class $sporeClassName extends $superclassName[$ttpe, $rtpe] {
            $captureTypeTree
            val className: String = this.getClass.getName
            $applyDefDef
          }
          val $initializerName = new $sporeClassName
          $initializerTree
          $initializerName
        """
      }
    } else {
      ???
    }
  }


  def sporeImpl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(fun: c.Expr[T => R]): c.Expr[Spore[T, R]] = {
    import c.universe._

    // check Spore constraints
    // TODO: the last 2 arguments could be passed implicitly
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