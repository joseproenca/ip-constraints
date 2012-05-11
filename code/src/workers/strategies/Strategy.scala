package workers.strategies

import common.beh.{Constraints, Solution}
import workers.Node


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 14:22
 * To change this template use File | Settings | File Templates.
 */

abstract class Strategy[S<:Solution,C<:Constraints[S,C],St<:Strategy[S,C, St]] {
  type Nd = Node[S,C]

  val owned = scala.collection.mutable.Set[Nd]()
  val fringe = scala.collection.mutable.Set[Nd]()

  // abstract methods //

  // Find the next nodes (from the fringe) to expand to.
  def nextNodes: Iterable[Nd]
  // Find the initial nodes based on a prefered node "n".
  def initNodes(n:Nd): Iterable[Nd]
  // Checks if it makes sense to search now for a solution.
  def canSolve: Boolean
  // merges the information from another traversal
  def merge(s:St)

  // aux functions
  def solve: Option[S] = {
//    var beh = new Behaviour[S,C](val ends: List[String],val uid: Int)
    if (owned.isEmpty) return None

    // get first node and behaviour
    val init = owned.head
    val beh = init.behaviour

    // collect it's constraints + neighbour constraints
//    var  (c,included)  = neighbourConstr(init,Set(),beh.constraints)
    var c = neighbourConstr(init,beh.constraints)

    // collect the constraints + neighbour constraints of owned ports,
    // avoiding adding repeated neighbours (via "included") -- DROPPED (common neighbours of 2 nodes must be added 2x)
    for (n <- (owned - init)) {
      c = c ++ n.behaviour.constraints
      c = neighbourConstr(n,c)
//      val pair = neighbourConstr(n,included,c)
//      c = pair._1; included = pair._2
    }

    c.solve
  }

  private def neighbourConstr(node:Nd, basec:C): C = {
    var c = basec
//    var i = included
    for (n <- node.neighbours) {
//      i += n
      // node connected to n!
      if (owned contains n) c = node.behaviour.sync(n,c)
      // node makes border with possible sync region
      else c = node.behaviour.border(n,c)
    }
    c
  }

  def register(nds:Iterable[Nd]) {
    owned ++= nds
    fringe --= nds
    for (nd <- nds)
      extendFringe(nd.neighbours)
  }

  def register(nd:Nd) {
    owned += nd
    fringe -= nd
    extendFringe(nd.neighbours)
  }

  private def extendFringe(nd:Iterable[Nd]) {
    for (n <- nd)
      if (!(owned contains n)) fringe += n
  }

}


abstract class StrategyBuilder[S <: Solution, C <: Constraints[S, C], St <: Strategy[S, C, St]] {
  def apply: St
}

