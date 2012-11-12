package common.beh

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 08:52
 * To change this template use File | Settings | File Templates.
 */

abstract class Connector[S<: Solution, C <: Constraints[S,C]](val ends: List[String],val uid: Int = 0) {

  /**
   * Collect the constraints and returns them, ready to be solved.
   */
  def getConstraints: C

  /**
   * Given a solution updates the current state.
   * @param s solution
   */
  def update(s: S) {} // default: do nothing


  /**
   * Returns yes if, when updated, it should start a new round of constraint solving
   * @return
   */
  def isProactive = false // default: false


  /**
   * Combine two connectors, resulting in a composed connector.
   *
   * @param other The other connector to be composed
   * @return The composed connector
   */
  def ++(other: Connector[S,C]): Connector[S,C]


  // CONFIGURING POWER OF CONSTRAINTS!
  // not sure if it is the way to go...
  var useData = false
  var useCC3 = false


  ///////////////////////////////////////////////////////////////
  // FROM HERE it should probably be moved to other classes... //
  ///////////////////////////////////////////////////////////////
//
//
//  def compat(other: Connector[S,C]) =
//    useData == other.useData && useCC3 == other.useCC3
//
//
//  //////////////////////////////////////////////////////
//  // Connector as an independent piece (no shared ports)
//
//  // required by dreams or workers... connections from local ends to remote ends
//  var connections: Map[AnyRef,Set[(String,String,Int)]] = Map() // neighbours to pairs of sync'd ends
//
//  // adds to "c" the sync constraints wrt the ends shared with "from"
//  // required if connected to other nodes.
//  def sync(from:AnyRef,c:C): C
//
//  // adds to "c" the border constraints wrt the ends shared with "from"
//  def border(from:AnyRef,c:C): C


  ///////
  // Hooks for convenience, used by writers and readers.
  // Adding observers
  var listeners: List[ () => Unit ] = Nil

  def listen(listener: () => Unit) {
    listeners ::= listener
  }

  def notifyflow() { for (l <- listeners) l() }

}

