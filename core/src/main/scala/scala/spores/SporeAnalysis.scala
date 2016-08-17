package scala.spores

import scala.reflect.macros.whitebox

protected class SporeAnalysis[C <: whitebox.Context with Singleton](val ctx: C) {

  import ctx.universe._
  import ctx.universe.Flag._

  def stripSporeStructure(tree: Tree): (List[Symbol], Tree) = {
    def isCorrectHeader(valDef: ValDef) = !valDef.mods.hasFlag(MUTABLE)

    tree match {
      case Block(stmts, expr) =>
        (stmts flatMap {
          case vd: ValDef if isCorrectHeader(vd) => List(vd.symbol)
          case stmt => ctx.abort(stmt.pos, Feedback.IncorrectSporeHeader)
        }) -> expr
      case expr => (List.empty, expr)
    }
  }

  val SporesDefinition = typeOf[spores.`package`.type]
  val delayedSym = SporesDefinition.member(TermName("delayed"))

  def readSporeBody(tree: Tree): (Option[Function], List[Tree], Tree) = {
    tree match {
      case f @ Function(params, body) => (Some(f), params, body) // Non-nullary
      case Apply(f, List(arg))
        if f.symbol == delayedSym => (None, List(), arg) // Nullary spore
      case _ => ctx.abort(tree.pos, Feedback.IncorrectSporeBody)
    }
  }

  def checkSpore(spore: ctx.Tree) = {}
}

object SporeAnalysis {
  // TODO(jvican): Use for clarity
  type Env = List[Symbol]
}
