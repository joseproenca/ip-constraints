package workers.strategies

import common.beh.{Constraints, Solution}
import common.beh.guardedcommands.{GuardedCommands, GCSolution}


class OneStepStrategy[S<:Solution,C<:Constraints[S,C]] extends Strategy[S,C,OneStepStrategy[S,C]] {
  // Find the next nodes (from the fringe) to expand to.
  def nextNodes = if (!fringe.isEmpty) Set(fringe.head) else Set()

  // Find the initial nodes based on a prefered node "n".
  def initNodes(n: Nd) = Set(n)

  // Checks if it makes sense to search now for a solution.
  def canSolve = true

  // merges the information from another traversal
  def merge(s: OneStepStrategy[S, C]) {
    owned ++= s.owned
    fringe ++= s.fringe
    fringe --= owned
  }
}

object OneStepStrategyBuilder
  extends StrategyBuilder[GCSolution, GuardedCommands, OneStepStrategy[GCSolution, GuardedCommands]] {
    def apply = new OneStepStrategy[GCSolution, GuardedCommands]()
  }