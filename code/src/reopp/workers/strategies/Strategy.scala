package reopp.workers.strategies

import reopp.workers.Node
import reopp.common._


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
  def solve(implicit builder:CBuilder[S,C]): OptionSol[S] = {
//    var beh = new Behaviour[S,C](val ends: List[String],val uid: Int)
    if (owned.isEmpty) return NoneSol()

    // get first node and behaviour
    val init = owned.head
    val behh = init.behaviour

    // collect it's constraints + neighbour constraints
//    var  (c,included)  = neighbourConstr(init,Set(),beh.constraints)
    var c = neighbourConstr(init,behh.getConstraints)(builder)

    // collect the constraints + neighbour constraints of owned ports,
    // avoiding adding repeated neighbours (via "included") -- DROPPED (reopp.common neighbours of 2 nodes must be added 2x)
    for (n <- (owned - init)) {
      c ++= n.behaviour.getConstraints
      c = neighbourConstr(n,c)(builder)
//      val pair = neighbourConstr(n,included,c)
//      c = pair._1; included = pair._2
    }

//    println("solving: "+c)
    c.solve
  }

  // TODO: BROKEN!!! behaviour.sync connects local end "a" to neighbour ends "b" by "b:=a". Need the right order!
  private def neighbourConstr(node:Nd, basec:C)(builder:CBuilder[S,C]): C = {
    var c = basec
//    var i = included
    for ((end,ns) <- node.invConnections; n <- ns) {
//      i += n
      // node connected to n!
      if (owned contains n) c = //node.behaviour.sync(n,c)
        sync(node,n,c)(builder)

      // node makes border with possible sync region
      else c = //node.behaviour.border(n,c)
        border(node,n,c)(builder)
    }
    c
  }


  private def sync(n1:Nd,n2:Nd, basec: C)(implicit cbuilder: CBuilder[S,C]): C = {
    val uid1 = n1.behaviour.uid
    val uid2 = n2.behaviour.uid
    var res = basec

    for ((e1,u1,e2,u2) <- n1.flowconn)
      if (u2 == uid2) res ++= cbuilder.sync(e1,u1,e2,u2)
    for ((e2,u2,e1,u1) <- n2.flowconn)
      if (u1 == uid1) res ++= cbuilder.sync(e2,u2,e1,u1)
    res
  }

  // n1 owend, n2 not owned -> border n1.ends inters. n2.ends
  private def border(n1:Nd,n2:Nd, basec: C)(implicit cbuilder: CBuilder[S,C]): C = {
    val uid1 = n1.behaviour.uid
    var res = basec

    if (n1.connections contains n2)
      for ((end,_,_) <- n1.connections(n2))
        res ++= cbuilder.noflow(end,uid1)

    //    println("added borded. New constraints: "+c.commands.mkString(","))
    res
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

  def restore2fringe() {
    fringe ++= droppedFringe
    droppedFringe = Set()
  }

}


abstract class StrategyBuilder[S <: Solution, C <: Constraints[S, C], St <: Strategy[S, C, St]] {
  def apply: St
}

