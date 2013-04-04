package workers.strategies

import common.guardedcommands.{Formula, GCSolution}
import common.{Constraints, Solution}


class CompleteStrategy[S <: Solution, C <: Constraints[S, C]] extends Strategy[S, C, CompleteStrategy[S, C]] {
  override def merge(s: CompleteStrategy[S, C]) {
    throw new RuntimeException("CompleteStrategy cannot be merged")
  }

  override def canSolve = true

  override def initNodes(n: Nd): Iterable[Nd] = {
    var included = Set(n)
    var missing: Set[Nd] = n.getNeighbours.toSet
    while (!missing.isEmpty) {
      val node = missing.head
      missing = missing.tail
      included += node
      missing ++= node.getNeighbours.filterNot(x => included contains x)
        //node.neighbours.filterNot(x => included contains x)
    }
    included
  }

  override def nextNodes: Iterable[Nd] = List()
}

object CompleteStrategy {
  implicit object CompleteStrategyBuilder
    extends StrategyBuilder[GCSolution, Formula, CompleteStrategy[GCSolution, Formula]] {
    def apply = new CompleteStrategy[GCSolution, Formula]()

  }
}
