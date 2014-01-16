package reopp.common

/**
 * Abstract representation of a state machine with state given by
 * a collection of constraints `C` and an `update` method.
 * Connectors must be composable via a `++` method.
 *
 * Created by jose on 02/05/12.
 */

abstract class Connector[S<: Solution, C <: Constraints[S,C]](val ends: List[String],val uid: Int = 0) {

  /**
   * Collect the constraints and returns them, ready to be solved.
   */
  def getConstraints: C

  /**
   * Given a (possible) solution updates the current state.
   * @param s solution
   */
  def update(s: OptionSol[S]) {} // default: do nothing


  /**
   * Returns yes if, when updated, it should start a new round of constraint solving.
   * It probably should be moved to [[reopp.dreams]] or [[reopp.workers]], for example.
   *
   * @return if it is proactive.
   */
  def isProactive = false // default: false


  /**
   * Combine two connectors, resulting in a composed connector.
   *
   * @param other The other connector to be composed
   * @return The composed connector
   */
  def ++(other: Connector[S,C]): Connector[S,C]


  /**
   * Collect constraints, solve them, and update connector.
   * Default solver for the constraints is used.
   * @return Possible solution for the current step
   */
  def doStep: OptionSol[S] = {
    val s = getConstraints.solve
    update(s)
//    println("-- step done --\n"+s)
    s
  }


  /**
   * Keeps performing steps until no dataflow is possible.
   */
  def run() {
    if (doStep.isDefined) run()
  }


  // CONFIGURING POWER OF CONSTRAINTS!
  // not sure if it is the way to go...
  /** Used by the constructor of specific connectors */
  protected var useData = false
  /** Used by the constructor of specific connectors */
  protected var useCC3 = false


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
//  // required by reopp.dreams or reopp.workers... connections from local ends to remote ends
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
  private var listeners: List[ () => Unit ] = Nil

  /**
   * Adds a listener.
   * @param listener is a function that is executed whenever `notifyflow()` is called.
   */
  def listen(listener: () => Unit) {
    listeners ::= listener
  }

  /**
   * Removes a listener
   * @param listener is a function that is executed whenever `notifyflow()` is called.
   */
  def ignore(listener: () => Unit) {
    listeners = listeners.filterNot(_==listener)
  }

  /**
   * Notifies all listeners by executing the associated functions
   */
  def notifyflow() { for (l <- listeners) l() }

}

