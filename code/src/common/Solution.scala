package common

/**
 * Representation of a solution for the [[common.Constraints]].
 * Corresponds exactly to a synchronous step, and must capture all information
 * needed to update the state (using [[common.Connector.update()]]).
 *
 * Created by jose on 02/05/12.
 */

trait Solution {
  /** If an end (variable) has dataflow. */
  def hasFlowOn(end: String): Boolean
  /** What data flow on a given end (variable). */
  def getDataOn(end: String): Option[Any]
  //def pretty: String
}

/**
 * Represents an empty solution. Needed to give empty solutions as implicit parameters.
 * @tparam S is the precise type for the solution, subtype of [[common.Solution]]
 */
trait EmptySol[S <: Solution] {
  def sol: S
}