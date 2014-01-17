package reopp.workers.strategies

import reopp.common.guardedcommands.{Formula, GCSolution}
import reopp.common.{Constraints, Solution}


class OneStepStrategy[S<:Solution,C<:Constraints[S,C]] extends Strategy[S,C,OneStepStrategy[S,C]] {
  // Find the next nodes (from the fringe) to expand to.
  def nextNodes = if (!fringe.isEmpty) Set(fringe.head) else Set()

  // Find the initial nodes based on a prefered node "n".
  def initNodes(n: Nd) = Set(n)

  // Checks if it makes sense to search now for a solution.
  def canSolve = true

  // merges the information from another traversal
  override def merge(s: OneStepStrategy[S, C]) {
    super.merge(s)
    owned ++= s.owned
    fringe ++= s.fringe
    fringe --= owned
  }
}

object OneStepStrategy {
  implicit object OneStepStrategyBuilder
    extends StrategyBuilder[GCSolution, Formula, OneStepStrategy[GCSolution, Formula]] {
    def apply = new OneStepStrategy[GCSolution, Formula]()
  }
}