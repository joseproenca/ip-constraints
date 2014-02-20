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
  var triedSol: Option[NoneSol] = None // managed by the workers


  // abstract methods //

  // Find the next nodes (from the fringe) to expand to.
  def nextNodes: Iterable[Nd]
//  // Find the initial nodes based on a prefered node "n".
//  def initNodes(n:Nd): Iterable[Nd]
  // Checks if it makes sense to search now for a solution.
  def canSolve: Boolean
  /** Merges the information from another traversal. Should be extended by subclasses.
   *  Right now it only merges the previous buffer. */
  def merge(s:St) {
    (triedSol,s.triedSol) match {
      case (None,Some(NoneSol(Some(_)))) => triedSol = s.triedSol
      case (Some(NoneSol(None)),Some(NoneSol(Some(_)))) => triedSol = s.triedSol
      case (Some(NoneSol(Some(b1))),Some(NoneSol(Some(b2)))) => b1.safeImport(b2)
      case _ => {}
    }
  }

  def solve(implicit builder:CBuilder[S,C]): OptionSol[S] = solve(triedSol)
  
  // aux functions
  private def solve(tried:Option[NoneSol])(implicit builder:CBuilder[S,C]): OptionSol[S] = {
//    debug("solving - "+owned.mkString(","))
//    var beh = new Behaviour[S,C](val ends: List[String],val uid: Int)
    if (owned.isEmpty) return NoneSol()

    // get first node and behaviour
    val fstNode = owned.head
    val fstConn = fstNode.connector

    //println("building neighbour constraints = border + sync")
    var c = neighbourConstr(fstNode,fstConn.getConstraints)(builder)

    // collect the constraints + neighbour constraints of owned ports,
    // avoiding adding repeated neighbours (via "included") -- DROPPED (reopp.common neighbours of 2 nodes must be added 2x)
    for (n <- (owned - fstNode)) {
      c ++= n.connector.getConstraints
      c = neighbourConstr(n,c)(builder)
    }

    //println("solving: "+c)
    val res = c.solve(tried)
    //if (res.isDefined) println("-------------\nSolved:\n"+res.get)
    //else println("failed")
    res
  }

  private def neighbourConstr(node:Nd, basec:C)(builder:CBuilder[S,C]): C = {
    var c = basec
//    var i = included
    for (n <- node.getNeighbours) {
//      i += n
      // node connected to n!
      if (owned contains n) {
        c = sync(node,n,c)(builder)
      }
      // node makes border with possible sync region
      else {
        c = border(node,n,c)(builder)
      }
        
    }
    c
  }


  private def sync(n1:Nd,n2:Nd, basec: C)(implicit cbuilder: CBuilder[S,C]): C = {
    val uid1 = n1.connector.uid
    val uid2 = n2.connector.uid
    var res = basec

    for (ends <- n1.getConnectedEndsTo(n2))
      if (n1 hasSourceEnd ends._1) {
    	  res ++= cbuilder.sync(ends._2,uid2,ends._1,uid1)
      }
    for (ends <- n2.getConnectedEndsTo(n1))
      if (n2 hasSourceEnd ends._1) {
    	  res ++= cbuilder.sync(ends._2,uid1,ends._1,uid2)
      }    	  
//    for ((e1,u1,e2,u2) <- n1.flowconn)
//      if (u2 == uid2) res ++= cbuilder.sync(e1,u1,e2,u2)
//    for ((e2,u2,e1,u1) <- n2.flowconn)
//      if (u1 == uid1) res ++= cbuilder.sync(e2,u2,e1,u1)
    res
  }

  // n1 owend, n2 not owned -> border n1.ends inters. n2.ends
  private def border(n1:Nd,n2:Nd, basec: C)(implicit cbuilder: CBuilder[S,C]): C = {
    val uid1 = n1.connector.uid
    var res = basec

    if (n1 connectedTo n2) {
      //println(s"connected: $n1 -- $n2")
      for (end <- n1.getConnectedEndsTo(n2)) {
        //println("noflow at "+end._1)
        res ++= cbuilder.noflow(end._1,uid1)
      }
    }

    //    println("added borded. New constraints: "+c.commands.mkString(","))
    res
  }

  def register(nds:Iterable[Nd]) {
    owned ++= nds
    fringe --= nds
    for (nd <- nds; nb <- nd.getNeighbours)
      extendFringe(nb)
  }

  def register(nd:Nd) {
    owned += nd
    fringe -= nd
    for (n <- nd.getNeighbours)
      extendFringe(n)
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

  protected def debug(s:String) {
//    println(s"str[${hashCode.toString.substring(5)}] $s")
  }
  

}


abstract class StrategyBuilder[S <: Solution, C <: Constraints[S, C], St <: Strategy[S, C, St]] {
  def apply: St
}

