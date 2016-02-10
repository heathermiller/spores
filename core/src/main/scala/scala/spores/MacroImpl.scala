/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.spores

import scala.reflect.macros.whitebox.Context


private[spores] class MacroImpl[C <: Context with Singleton](val c: C) {
  import c.universe._

  /* Checks whether the owner chain of `sym` contains `owner`.
   *
   * @param sym   the symbol to be checked
   * @param owner the owner symbol that we try to find
   * @return      whether `owner` is a direct or indirect owner of `sym`
   */
  def ownerChainContains(sym: Symbol, owner: Symbol): Boolean =
    sym != null && (sym.owner == owner || {
      sym.owner != NoSymbol && ownerChainContains(sym.owner, owner)
    })

  /* Checks whether `member` is selected from a static selector, or whether
   * its selector is transitively selected from a static symbol.
   */
  def selectorIsStatic(member: Tree): Boolean = member match {
    case Select(selector, member0) =>
      val selStatic = selector.symbol.isStatic
      debug(s"checking whether $selector is static...$selStatic")
      selStatic || selectorIsStatic(selector)
    case _ => false
  }

  def conforms(funTree: c.Tree): (List[Symbol], Type, Tree, List[Symbol]) = {
    // traverse body of `fun` and check that the free vars access only allowed things
    // `validEnv` == symbols declared in the spore header
    val (validEnv, funLiteral) = funTree match {
      case Block(stmts, expr) =>
        val validVarSyms = stmts.toList flatMap {
          case vd @ ValDef(mods, name, tpt, rhs) =>
            List(vd.symbol)
          case stmt =>
            c.error(stmt.pos, "Only val defs allowed at this position")
            List()
        }
        validVarSyms foreach { sym => debug("valid: " + sym) }
        (validVarSyms, expr)

      case expr =>
        (List(), expr)
    }

    val captureSym = typeOf[spores.`package`.type].member(TermName("capture"))
    val delayedSym = typeOf[spores.`package`.type].member(TermName("delayed"))

    val (fun, vparams, body) = funLiteral match {
      case fun0 @ Function(vparams0, body0) =>
        // non-nullary spore
        (fun0, vparams0, body0)
      case Apply(fun0, List(arg)) if fun0.symbol == delayedSym =>
        // nullary spore
        (null, List(), arg)
      case _ =>
        c.error(funLiteral.pos, "Incorrect usage of `spore`: function literal or `delayed` expression expected")
        (null, null, null)
    }

    if (body == null) {
      (null, null, null, validEnv)
    } else {
      // contains all symbols found in `capture` syntax
      var capturedSyms = List[Symbol]()
      var declaredSyms = List[Symbol]()

      // is the use of symbol s allowed via spore rules? (in the spore body)
      def isSymbolValid(s: Symbol): Boolean =
        validEnv.contains(s) ||              // is `s` declared in the spore header?
        capturedSyms.contains(s) ||          // is `s` captured using the `capture` syntax?
        (fun != null && ownerChainContains(s, fun.symbol)) || // is `s` declared within `fun`?
        declaredSyms.contains(s) ||
        s == NoSymbol ||                     // is `s` == `_`?
        s.isStatic ||
        s.owner == definitions.PredefModule

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

      def isPathValid(tree: Tree): (Boolean, Option[Tree]) = {
        debug(s"checking isPathValid for $tree [${tree.symbol}]...")
        debug(s"tree class: ${tree.getClass.getName}")
        if (tree.symbol != null && isSymbolValid(tree.symbol)) (true, None)
        else tree match {
          case Select(pre, sel) =>
            debug(s"case 1: Select($pre, $sel)")
            isPathValid(pre)
          case Apply(Select(pre, _), _) =>
            debug(s"case 2: Apply(Select, _)")
            isPathValid(pre)
          case TypeApply(Select(pre, _), _) =>
            debug("case 3: TypeApply(Select, _)")
            isPathValid(pre)
          case TypeApply(fun, _) =>
            debug("case 4: TypeApply")
            isPathValid(fun)
          case Literal(Constant(_)) | New(_) =>
            (true, None)
          case id: Ident =>
            (isSymbolValid(id.symbol), None)
          case _ =>
            debug("case 7: _")
            (false, Some(tree))
        }
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
      // check the spore body, i.e., for each identifier, check that it is valid according to spore rules
      // i.e., either declared locally or captured via a `capture` invocation
      val traverser = new Traverser {
        override def traverse(tree: Tree) {
          tree match {
            case vd @ ValDef(mods, name, tpt, rhs) =>
              super.traverse(tree)
              declaredSyms = vd.symbol :: declaredSyms

            case id: Ident =>
              debug("checking ident " + id)
              if (!isSymbolValid(id.symbol))
                c.error(tree.pos, "invalid reference to " + id.symbol)

            case th: This =>
              c.error(tree.pos, "invalid reference to " + th.symbol)

            // x.m().s
            case sel @ Select(app @ Apply(fun0, args0), _) =>
              debug("checking select (app): " + sel)
              if (app.symbol.isStatic) {
                debug(s"OK, invocation of '$app' is static")
              } else fun0 match {
                case Select(obj, _) =>
                  if (fun != null && ownerChainContains(obj.symbol, fun.symbol)) debug(s"OK, selected on local object $obj")
                  else {
                    // the invocation is OK if `obj` is transitively selected from a top-level object
                    debug(s"checking whether $obj is transitively selected from a top-level object...")
                    val objIsStatic = obj.symbol.isStatic || selectorIsStatic(obj)
                    debug(s"$obj.symbol.isStatic: $objIsStatic")
                    if (!objIsStatic)
                      c.error(sel.pos, s"the invocation of '$fun0' is not static")
                  }

                case _ =>
                  c.error(sel.pos, s"the invocation of '$fun0' is not static")
              }

            case sel @ Select(pre, _) =>
              debug("checking select " + sel)

              isPathValid(sel) match {
                case (false, None) =>
                  c.error(tree.pos, "invalid reference to " + sel.symbol)
                case (false, Some(subtree)) =>
                  traverse(subtree)
                case (true, None) =>
                  // do nothing
                case (true, Some(subtree)) =>
                  // do nothing
              }

            case _ =>
              super.traverse(tree)
          }
        }
      }

      traverser.traverse(body)
      (vparams.map(_.symbol), body.tpe, body, validEnv)
    }
  }

  def check2(funTree: c.Tree, tpes: List[c.Type]): c.Tree = {
    debug(s"SPORES: enter check2")

    val (paramSyms, retTpe, funBody, validEnv) = conforms(funTree)

    val applyParamNames = for (i <- 0 until paramSyms.size) yield c.freshName(TermName("x" + i))
    val ids = for (name <- applyParamNames.toList) yield Ident(name)

    val applyParamValDefs = for ((applyParamName, paramSym) <- applyParamNames.zip(paramSyms))
      yield ValDef(Modifiers(Flag.PARAM), applyParamName, TypeTree(paramSym.typeSignature), EmptyTree)
    val applyParamSymbols = for (applyParamValDef <- applyParamValDefs)
      yield applyParamValDef.symbol

    def mkApplyDefDef(body: Tree): DefDef = {
      val applyVParamss = List(applyParamValDefs.toList)
      DefDef(NoMods, TermName("apply"), Nil, applyVParamss, TypeTree(retTpe), body)
    }

    val symtable = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]

    def processFunctionBody(substituter: symtable.TreeSubstituter, funBody: Tree): DefDef = {
      val newFunBody = substituter.transform(funBody.asInstanceOf[symtable.Tree])
      val nfBody     = c.untypecheck(newFunBody.asInstanceOf[Tree])
      mkApplyDefDef(nfBody)
    }

    val sporeClassName = c.freshName(TypeName("anonspore"))

    if (validEnv.isEmpty) {
      // replace references to paramSyms with references to applyParamSymbols
      val substituter = new symtable.TreeSubstituter(paramSyms.map(_.asInstanceOf[symtable.Symbol]), ids.toList.map(_.asInstanceOf[symtable.Tree]))
      val applyDefDef = processFunctionBody(substituter, funBody)

      if (paramSyms.size == 2) {
        q"""
          class $sporeClassName extends scala.spores.Spore2[${tpes(1)}, ${tpes(2)}, ${tpes(0)}] {
            this._className = this.getClass.getName
            $applyDefDef
          }
          new $sporeClassName
        """
      } else if (paramSyms.size == 3) {
        q"""
          class $sporeClassName extends scala.spores.Spore3[${tpes(1)}, ${tpes(2)}, ${tpes(3)}, ${tpes(0)}] {
            this._className = this.getClass.getName
            $applyDefDef
          }
          new $sporeClassName
        """
      } else ???
    } else { // validEnv.size > 1 (TODO: size == 1)
        // replace references to paramSyms with references to applyParamSymbols
        // and references to captured variables to new fields
        val capturedTypes = validEnv.map(_.typeSignature)
        debug(s"capturedTypes: ${capturedTypes.mkString(",")}")

        val symsToReplace     = (paramSyms ::: validEnv).map(_.asInstanceOf[symtable.Symbol])
        val newTrees          = (1 to validEnv.size).map(i => Select(Ident(TermName("captured")), TermName(s"_$i"))).toList
        val treesToSubstitute = (ids ::: newTrees).map(_.asInstanceOf[symtable.Tree])
        val substituter       = new symtable.TreeSubstituter(symsToReplace, treesToSubstitute)
        val applyDefDef       = processFunctionBody(substituter, funBody)

        val rhss = funTree match {
          case Block(stmts, expr) =>
            stmts.toList flatMap {
              case ValDef(_, _, _, rhs) => List(rhs)
              case stmt =>
                c.error(stmt.pos, "Only val defs allowed at this position")
                List()
            }
        }

        val constructorParams = List(List(toTuple(rhss)))

        val captureTypeTreeDefinition = (if (capturedTypes.size == 2) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)})"
          else if (capturedTypes.size == 3) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)})"
          else if (capturedTypes.size == 4) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)})"
          else if (capturedTypes.size == 5) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)})"
          else if (capturedTypes.size == 6) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)})"
          else if (capturedTypes.size == 7) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)}, ${capturedTypes(6)})"
          else if (capturedTypes.size == 8) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)}, ${capturedTypes(6)}, ${capturedTypes(7)})").asInstanceOf[c.Tree]

        val q"type $_ = $captureTypeTree" = captureTypeTreeDefinition


      if (paramSyms.size == 2) {
          q"""
            final class $sporeClassName(val captured: $captureTypeTree) extends scala.spores.Spore2WithEnv[${tpes(1)}, ${tpes(2)}, ${tpes(0)}] {
              $captureTypeTreeDefinition
              this._className = this.getClass.getName
              $applyDefDef
            }
            new $sporeClassName(...$constructorParams)
          """
        } else if (paramSyms.size == 3) {
          q"""
            final class $sporeClassName(val captured: $captureTypeTree) extends scala.spores.Spore3WithEnv[${tpes(1)}, ${tpes(2)}, ${tpes(3)}, ${tpes(0)}] {
              $captureTypeTreeDefinition
              this._className = this.getClass.getName
              $applyDefDef
            }
            new $sporeClassName(...$constructorParams)
          """
        } else ???
    }
  }

  /**
     spore {
       val x = outer
       delayed { ... }
     }
   */
  def checkNullary(funTree: c.Tree, rtpe: c.Type): c.Tree = {
    debug(s"SPORES: enter checkNullary")

    val (paramSyms, retTpe, funBody, validEnv) = conforms(funTree)

    val applyName = TermName("apply")
    val symtable = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
    val sporeClassName = c.freshName(TypeName("anonspore"))

    if (validEnv.isEmpty) {
      val newFunBody = c.untypecheck(funBody)
      val applyDefDef = DefDef(NoMods, applyName, Nil, List(List()), TypeTree(retTpe), newFunBody)

      q"""
        class $sporeClassName extends scala.spores.NullarySpore[$rtpe] {
          this._className = this.getClass.getName
          $applyDefDef
        }
        new $sporeClassName
      """
    } else {
      val capturedTypes = validEnv.map(_.typeSignature)
      debug(s"capturedTypes: ${capturedTypes.mkString(",")}")

      // replace references to captured variables with references to new fields
      val symsToReplace = validEnv
      val newTrees =
        if (validEnv.size == 1) List(Ident(TermName("captured")))
        else (1 to validEnv.size).map(i => Select(Ident( TermName("captured")), TermName(s"_$i"))).toList
      val treesToSubstitute = newTrees
      val symsToTrees = symsToReplace.zip(treesToSubstitute).toMap
      val namesToTrees = symsToReplace.map(_.name.toString).zip(treesToSubstitute).toMap
      val newFunBody = transformTypes(symsToTrees,
        namesToTrees
      ) (funBody)

      val nfBody = c.untypecheck(newFunBody.asInstanceOf[c.universe.Tree])
      val applyDefDef = DefDef(NoMods, applyName, Nil, List(List()), TypeTree(retTpe), nfBody)

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
      assert(rhss.size == validEnv.size)

      val constructorParams = List(List(toTuple(rhss)))

      val superclassName = TypeName("NullarySporeWithEnv")

      val captureTypeTreeDefinition = (if (capturedTypes.size == 1) q"type Captured = ${capturedTypes(0)}"
        else if (capturedTypes.size == 2) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)})"
        else if (capturedTypes.size == 3) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)})"
        else if (capturedTypes.size == 4) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)})"
        else if (capturedTypes.size == 5) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)})"
        else if (capturedTypes.size == 6) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)})"
        else if (capturedTypes.size == 7) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)}, ${capturedTypes(6)})"
        else if (capturedTypes.size == 8) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)}, ${capturedTypes(6)}, ${capturedTypes(7)})").asInstanceOf[c.Tree]

      val q"type $_ = $captureTypeTree" = captureTypeTreeDefinition

      q"""
        final class $sporeClassName(val captured: $captureTypeTree) extends $superclassName[$rtpe] {
          $captureTypeTreeDefinition
          this._className = this.getClass.getName
          $applyDefDef
        }
        new $sporeClassName(...$constructorParams)
      """
    }
  }

  /* Constructs a function that replaces all occurrences of symbols in m with trees in m and that changes the 'origin'
   * field to fix path-dependent types.
   */
  def transformTypes(m: Map[c.universe.Symbol, Tree], nameMap: Map[String, Tree]) : Tree => Tree = {

    // recycled from TreeSubstituter in Trees.scala and
    // https://github.com/non/spire/blob/master/macros/src/main/scala/spire/macros/Syntax.scala
    // https://github.com/non/spire/blob/master/macros/src/main/scala_2.11/spire/macros/compat.scala
    class TypeTransformer(val m: Map[c.universe.Symbol, Tree],
                          val nameMap: Map[String, Tree]
                         ) extends Transformer {
      override def transform(tree: Tree): Tree = {

        tree match {
          case ident@Ident(_) =>
            if (m.contains(tree.symbol)) m(tree.symbol)
            else tree
          case tt: TypeTree if tt.original != null =>
            super.transform(c.universe.internal.setOriginal(TypeTree(), super.transform(tt.original)))
          case tt: TypeTree if tt.original == null =>
            if (showRaw(tree) == "TypeTree()" &&
              nameMap.keys.exists( key => s"${tree.tpe}".contains(key.toString))) {
              debug(s"${showRaw(tree)}")
              debug(s"${showRaw(tree.tpe)}")
              debug(s"${tree.tpe}")


              def constructOriginal(tp: c.Type) : c.Tree = {
                // Example: tp =
                // TypeRef(
                //   SingleType(
                //     SingleType(NoPrefix, TermName("lit5_ui")),
                //     TermName("uref")),
                //   TypeName("R"),
                //   List())
                def matchTypeName(tn: c.Symbol) : c.TypeName =  {
                  if (showRaw(tn).startsWith("TypeName(")) TypeName(tn.name.toString)
                  else null
                }
                def matchTermNameNoPrefixCase(tn: c.Symbol) : Tree =  {
                  if (showRaw(tn).startsWith("TermName(")) {
                    val sym_name = tn.name.toString
                    if (nameMap.contains(sym_name)) nameMap(sym_name) else Ident(TermName(sym_name))
                  }
                  else null
                }

                def matchTermName(tn: c.Symbol) : TermName =  {
                  if (showRaw(tn).startsWith("TermName(")) {
                    TermName(tn.name.toString)
                  }
                  else null
                }
                tp match {
                  case TypeRef(tr, tns, List()) =>
                    debug(s"tns = $tns,\nshowRaw(tns) = ${showRaw(tns)}")
                    val tnsTypeName = matchTypeName(tns)
                    if (tnsTypeName != null) {
                      val tr_rec = constructOriginal(tr)
                      if (tr_rec != null) Select(tr_rec, tnsTypeName)
                      else null
                    }
                    else null
                  case SingleType(NoPrefix, tns) =>
                    matchTermNameNoPrefixCase(tns)
                  case SingleType(pre, tns) =>
                    val tnsTypeName = matchTermName(tns)
                    val pre_rec = constructOriginal(pre)
                    if (pre_rec != null) Select(pre_rec, tnsTypeName)
                    else null
                  case _ => null
                }
              }

              val new_orig = constructOriginal(tree.tpe)
              val res = if (new_orig != null)
                  c.universe.internal.setOriginal(TypeTree(), new_orig)
                else
                  tree
              res
            }
            else tree
          case _ => super.transform(tree)
        }
      }
    }
    new TypeTransformer(m, nameMap).transform(_: Tree)
  }

  def toTuple(lst: List[c.Tree]) : c.Tree = {
    if (lst.size == 1) lst(0)
    else if (lst.size == 2) q"(${lst(0)}, ${lst(1)})"
    else if (lst.size == 3) q"(${lst(0)}, ${lst(1)}, ${lst(2)})"
    else if (lst.size == 4) q"(${lst(0)}, ${lst(1)}, ${lst(2)}, ${lst(3)})"
    else if (lst.size == 5) q"(${lst(0)}, ${lst(1)}, ${lst(2)}, ${lst(3)}, ${lst(4)})"
    else if (lst.size == 6) q"(${lst(0)}, ${lst(1)}, ${lst(2)}, ${lst(3)}, ${lst(4)}, ${lst(5)})"
    else if (lst.size == 7) q"(${lst(0)}, ${lst(1)}, ${lst(2)}, ${lst(3)}, ${lst(4)}, ${lst(5)}, ${lst(6)})"
    else if (lst.size == 8) q"(${lst(0)}, ${lst(1)}, ${lst(2)}, ${lst(3)}, ${lst(4)}, ${lst(5)}, ${lst(6)}, ${lst(7)})"
    else ???
  }

  /**
     spore {
       val x = outer
       (y: T) => { ... }
     }
   */
  def check(funTree: c.Tree, ttpe: c.Type, rtpe: c.Type): c.Tree = {
    debug(s"SPORES: enter check, tree:\n$funTree")

    val (paramSyms, retTpe, funBody, validEnv) = conforms(funTree)
    val paramSym = paramSyms.head
    val oldName = paramSym.asInstanceOf[c.universe.TermSymbol].name

    if (paramSym != null) {
      val applyParamName = c.freshName(TermName("x"))
      val id = Ident(applyParamName)
      val applyName = TermName("apply")

      val applyParamValDef = ValDef(Modifiers(Flag.PARAM), applyParamName, TypeTree(paramSym.typeSignature), EmptyTree)

      val symtable = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
      val sporeClassName = c.freshName(TypeName("anonspore"))

      if (validEnv.isEmpty) {
        val typeTransformer = transformTypes(Map(paramSym -> id),
          Map(oldName.toString -> id)
        )
        val newFunBody = typeTransformer(funBody)

       
        val nfBody = c.untypecheck(newFunBody.asInstanceOf[c.universe.Tree])

        val applyDefDef: DefDef = {
          val applyVParamss = List(List(applyParamValDef))
          DefDef(NoMods, applyName, Nil, applyVParamss, TypeTree(retTpe), nfBody)
        }

        q"""
          class $sporeClassName extends scala.spores.Spore[$ttpe, $rtpe] {
            this._className = this.getClass.getName
            $applyDefDef
          }
          new $sporeClassName
        """
      } else {
        // replace reference to paramSym with reference to applyParamSymbol
        // and references to captured variables with references to new fields
        val capturedTypes = validEnv.map(_.typeSignature)
        debug(s"capturedTypes: ${capturedTypes.mkString(",")}")

        val symsToReplace = paramSym :: validEnv
        val newTrees =
          if (validEnv.size == 1) List(Ident(TermName("captured")))
          else (1 to validEnv.size).map(i => Select(Ident(TermName("captured")), TermName(s"_$i"))).toList

        val treesToSubstitute = id :: newTrees
        val symsToTrees = symsToReplace.zip(treesToSubstitute).toMap
        val namesToTrees = symsToReplace.map(_.name.toString).zip(treesToSubstitute).toMap
        val newFunBody = transformTypes(symsToTrees,
          namesToTrees
        ) (funBody)

        val nfBody = c.untypecheck(newFunBody.asInstanceOf[c.universe.Tree])
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
        assert(rhss.size == validEnv.size)

        val initializerName = c.freshName(TermName("initialize"))
        val constructorParams = List(List(toTuple(rhss)))
        val superclassName = TypeName("SporeWithEnv")

        val capturedTypeDefinition = (if (capturedTypes.size == 1) q"type Captured = ${capturedTypes(0)}"
        else if (capturedTypes.size == 2) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)})"
        else if (capturedTypes.size == 3) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)})"
        else if (capturedTypes.size == 4) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)})"
        else if (capturedTypes.size == 5) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)})"
        else if (capturedTypes.size == 6) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)})"
        else if (capturedTypes.size == 7) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)}, ${capturedTypes(6)})"
        else if (capturedTypes.size == 8) q"type Captured = (${capturedTypes(0)}, ${capturedTypes(1)}, ${capturedTypes(2)}, ${capturedTypes(3)}, ${capturedTypes(4)}, ${capturedTypes(5)}, ${capturedTypes(6)}, ${capturedTypes(7)})").asInstanceOf[c.Tree]

        val q"type $_ = $capturedTypeTree" = capturedTypeDefinition
        
        q"""
          class $sporeClassName(val captured : $capturedTypeTree) extends $superclassName[$ttpe, $rtpe] {
            $capturedTypeDefinition
            this._className = this.getClass.getName
            $applyDefDef
          }
          new $sporeClassName(...$constructorParams)
        """
      }
    } else {
      ???
    }
  }

}
