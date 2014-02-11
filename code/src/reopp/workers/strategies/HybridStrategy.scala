package reopp.workers.strategies

import reopp.common.guardedcommands.{Formula, GCSolution}
import reopp.common.{Constraints, Solution}

// UNDER CONSTRUCTION

// Idea: there is a queue of nodes - priorityQueue - that must be empty before a solution can be fetch.
// - extend priorityQueue: when registering a new node -> add dependencies
// - shrink priority Queue: when registering a new node as well... -> remove it from the queue

class HybridStrategy[S <: Solution, C <: Constraints[S, C]] extends Strategy[S,C,HybridStrategy[S,C]] {
                                                         // extends OneStepStrategy[S, C] {
                                                         // extends Strategy[S,C,OneStepStrategy[S,C]]

  // keep track of nodes that must be included before the next claim
  private var priorityQueue = List[Nd]() // STACK, not a queue any more...
  private var pausedQueue   = Set[Nd]()


  // Find the initial nodes based on a prefered node "n".
  def initNodes(n: Nd): Set[Nd] = {
    val nbs = n.getNeighbours
    // set all neigbours as priority! (and fringe), even before owning anything.
    fringe ++= nbs
//    priorityQueue ++= n.neighbours
    priorityQueue :::= nbs.toList
    Set(n)
  }

  // Find the next nodes (from the fringe) to expand to.
  override def nextNodes: Iterable[Nd] = {
    if (!priorityQueue.isEmpty) Set(priorityQueue.head)
    else if (!fringe.isEmpty) Set(fringe.head) else Set()
         //super.nextNodes
  }

  // Checks if it makes sense to search now for a solution.
  override def canSolve: Boolean = {
//    debug("can solve? "+priorityQueue.mkString(",")+s" (onwned - ${owned.mkString(",")})")
    priorityQueue.isEmpty
  }

  // merges the information from another traversal
  override def merge(s: HybridStrategy[S, C]) {
    super.merge(s)
    owned ++= s.owned
    fringe ++= s.fringe
    fringe --= owned
//    super.merge(s)
//    priorityQueue ++= s.priorityQueue // :::= s.priorityQueue
    priorityQueue :::=  s.priorityQueue
    priorityQueue = priorityQueue filterNot (owned contains)
//    debug("merged strat. prior: "+priorityQueue.mkString(",")+" / owned: "+owned.mkString(","))
  }


  override def register(nds:Iterable[Nd]) {
//    println("Registering. Owned so far: "+owned.mkString("[",",","]"))
    super.register(nds)
    for (nd <- nds)
      updatePriorityQueue(nd)
  }
//
  override def register(nd:Nd) {
//    println("Registering node. Owned so far: "+owned.mkString("[",",","]"))
    super.register(nd)
    updatePriorityQueue(nd)
  }

  // again (assume priority subseteq fringe!):
  // - check dependencies of selected node, and add them to the priorityQueue (NOT SURE).
  // - go for each neighbour
  // - check if it is owned (in the traversal)
  // - if so, enqueue fresh dependencies from it (in fringe, not in queue)
  def updatePriorityQueue(nd:Nd) {
//    debug("updating prior for "+nd+": "+priorityQueue.mkString(","))
    val deps = nd.guessRequirements(nd)
    for (dep <- deps)
      if ((fringe contains dep) && !(priorityQueue contains dep))
            priorityQueue ::= dep
    
    if (!(priorityQueue.isEmpty))
      if (priorityQueue.head == nd)
//        priorityQueue.dequeue()
        priorityQueue = priorityQueue.tail

    for ((_,neighbs) <- nd.invConnections; neighb <- neighbs) {
      if (owned contains neighb) {
        val deps = nd.guessRequirements(neighb)
        for (dep <- deps)
          if ((fringe contains dep) && !(priorityQueue contains dep))
            priorityQueue ::= dep
        //            priorityQueue += dep
      }
//      priorityQueue = priorityQueue filterNot(nd ==)
//    debug("updated prior: "+priorityQueue.mkString(",")+s" (owned - ${owned.mkString(",")})")
    }
//    println("UPDATED priority queue after claiming "+nd.hashCode()+" - "+priorityQueue.mkString("[",",","]"))
  }

  override def dropFromFringe(nd:Nd) {
//    println("dropping from fringe (and PQ) "+nd.hashCode())
    super.dropFromFringe(nd)
    priorityQueue
    val isPriority = priorityQueue contains nd
    priorityQueue = priorityQueue filterNot(nd ==) //dequeueFirst(nd ==) //filterNot(nd ==)
    if (isPriority) pausedQueue += nd
//    println("dropped - "+priorityQueue.map(_.hashCode()).mkString("[",",","]") +" / "+fringe.map(_.hashCode()).mkString("[",",","]"))
  }

  override def restore2fringe(nd:Nd) {
    super.restore2fringe(nd)
    if (pausedQueue contains nd) {
//      priorityQueue += nd
      priorityQueue ::= nd
      pausedQueue -= nd
    }
  }

  override def restore2fringe {
    super.restore2fringe
//    priorityQueue ++= pausedQueue
    for (n <- pausedQueue)
      priorityQueue ::= n
    pausedQueue = Set()
  }
}



object HybridStrategy {
  implicit object HybridStrategyBuilder
    extends StrategyBuilder[GCSolution, Formula, HybridStrategy[GCSolution, Formula]] {
    def apply = new HybridStrategy[GCSolution, Formula]()
  }
}

