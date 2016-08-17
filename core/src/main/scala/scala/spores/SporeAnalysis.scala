package scala.spores

import scala.reflect.macros.whitebox

protected class SporeAnalysis[C <: whitebox.Context with Singleton](val ctx: C) {

  import ctx.universe._
  import ctx.universe.Flag._

  def stripSporeStructure(tree: Tree): (List[Symbol], ctx.Tree) = {
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

  def checkSpore(spore: ctx.Tree) = {}
}

object SporeAnalysis {
  // TODO(jvican): Use for clarity
  type Env = List[Symbol]
}
