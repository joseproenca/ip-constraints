package workers.strategies

import common.beh.{Solution, Constraints}
import common.beh.choco.{ChoConstraints, ChoSolution}

// UNDER CONSTRUCTION

class HybridStrategy[S <: Solution, C <: Constraints[S, C]] extends Strategy[S, C, HybridStrategy[S, C]] {

  // keep track of nodes that must be included before the next claim
  private var priorityQueue = List[Nd]()

  def collectAll(n: Nd) {
    var included = Set(n)
    var missing: List[Nd] = n.neighbours
    while (!missing.isEmpty) {
      val node = missing.head
      missing = missing.tail
      included += node
      missing :::= node.neighbours.filterNot(x => included contains x)
    }
    included
  }

  // Find the initial nodes based on a prefered node "n".
  def initNodes(n: Nd) = Set(n)

  // Find the next nodes (from the fringe) to expand to.
  def nextNodes: Iterable[Nd] = {
    for (n <- priorityQueue)
      if (fringe contains n) {
        priorityQueue filterNot (n ==)
        return Set(n)
      }
    if (!fringe.isEmpty) Set(fringe.head) else Set()
  }



  // Checks if it makes sense to search now for a solution.
  def canSolve = priorityQueue.isEmpty

  // merges the information from another traversal
  def merge(s: HybridStrategy[S, C]) {
    owned ++= s.owned
    fringe ++= s.fringe
    priorityQueue :::= s.priorityQueue
    fringe --= owned
    priorityQueue = priorityQueue filterNot (owned contains)
  }

//  override def register(nds:Iterable[Nd]) {
//    for (nd <- nds)
//      priorityQueue.....
//    owned ++= nds
//    fringe --= nds
//    for (nd <- nds)
//      extendFringe(nd.neighbours)
//  }
//
//  def register(nd:Nd) {
//    owned += nd
//    fringe -= nd
//    extendFringe(nd.neighbours)
//  }
}



object HybridStrategy
  extends StrategyBuilder[ChoSolution, ChoConstraints, HybridStrategy[ChoSolution, ChoConstraints]] {
  def apply = new HybridStrategy[ChoSolution, ChoConstraints]()
}
