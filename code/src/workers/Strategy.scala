package workers

import common.beh.{Behaviour, Solution, Constraints}
import xml.dtd.NotationDecl
import common.beh.choco.{ChoConstraints, ChoSolution}


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
  
  // abstract methods
  def nextNodes: Iterable[Nd]
  def initNodes(n:Nd): Iterable[Nd]
  def canSolve: Boolean
  def merge(s:St): St
  
  // aux functions
  def solve: Option[S] = {
//    var beh = new Behaviour[S,C](val ends: List[String],val uid: Int)
    if (owned.isEmpty) return None
    
    // get first node and behaviour
    val init = owned.head
    val beh = init.behaviour
    
    // collect it's constraints + neighbour constraints
    var  (c,included)  = neighbourConstr(init,Set(),beh.constraints)

    // collect the constraints + neighbour constraints of owned ports,
    // avoiding adding repeated neighbours (via "included")
    for (n <- (owned - init)) {
      c = c ++ n.behaviour.constraints
      val pair = neighbourConstr(n,included,c)
      c = pair._1; included = pair._2
    }
    
    c.solve
  }
  
  private def neighbourConstr(node:Nd, included:Set[Nd], basec:C): (C, Set[Nd]) = {
    var c = basec
    var i = included
    for (n <- node.neighbours) {
      if (!(i contains n)) {
        i += n
        if (owned contains n)
          // node connected to n!
          c = node.behaviour.sync(n,c)
        else 
          // node makes border with possible sync region
          c = node.behaviour.border(n,c)
      }
    }
    (c, i)
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

class CompleteStrategy[S<:Solution,C<:Constraints[S,C]] extends Strategy[S,C,CompleteStrategy[S,C]] {
  override def merge(s: CompleteStrategy[S, C]) =
    throw new RuntimeException("CompleteStrategy cannot be merged")

  override def canSolve = true

  override def initNodes(n: Nd): Iterable[Nd] = {
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

  override def nextNodes: Iterable[Nd] = List()
}

abstract class StrategyBuilder[S<:Solution,C<:Constraints[S,C],St<:Strategy[S,C, St]] {
  def apply: St
}

object CompleteStrategyBuilder
extends StrategyBuilder[ChoSolution,ChoConstraints,CompleteStrategy[ChoSolution,ChoConstraints]] {
  def apply = new CompleteStrategy[ChoSolution,ChoConstraints]()
}

