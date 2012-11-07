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

trait Strategy[S<:Solution,C<:Constraints[S,C],St<:Strategy[S,C, St]] {
  type Nd = Node[S,C]

  val owned = scala.collection.mutable.Set[Nd]()
  val fringe = scala.collection.mutable.Set[Nd]()
  var droppedFringe = Set[Nd]()

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
    var c = neighbourConstr(init,beh.getConstraints)

    // collect the constraints + neighbour constraints of owned ports,
    // avoiding adding repeated neighbours (via "included") -- DROPPED (common neighbours of 2 nodes must be added 2x)
    for (n <- (owned - init)) {
      c = c ++ n.behaviour.getConstraints
      c = neighbourConstr(n,c)
//      val pair = neighbourConstr(n,included,c)
//      c = pair._1; included = pair._2
    }

    c.solve
  }

  // TODO: BROKEN!!! behaviour.sync connects local end "a" to neighbour ends "b" by "b:=a". Need the right order!
  private def neighbourConstr(node:Nd, basec:C): C = {
    var c = basec
//    var i = included
    for ((end,ns) <- node.invConnections; n <- ns) {
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
    for (nd <- nds; (_,nbs) <- nd.invConnections; nb <- nbs)
      extendFringe(nb)
  }

  def register(nd:Nd) {
    owned += nd
    fringe -= nd
    for ((_,ns) <- nd.invConnections; n <- ns) extendFringe(n)
  }

  private def extendFringe(n:Nd) {
//    for (n <- nd)
      if (!(owned contains n)) fringe += n
  }

  def dropFromFringe(nd:Nd) {
    fringe -= nd
    droppedFringe += nd
  }

  def restore2fringe(nd:Nd) {
    droppedFringe -= nd
    fringe += nd
  }

  def restore2fringe {
    fringe ++= droppedFringe
    droppedFringe = Set()
  }

}


abstract class StrategyBuilder[S <: Solution, C <: Constraints[S, C], St <: Strategy[S, C, St]] {
  def apply: St
}

