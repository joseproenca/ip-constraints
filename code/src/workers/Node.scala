package workers

import common.beh.{Behaviour, Solution, Constraints}
import actors.OutputChannel


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 04/05/12
 * Time: 13:18
 * To change this template use File | Settings | File Templates.
 */

abstract class Node[S<:Solution, C<:Constraints[S,C]]
    (deployer: OutputChannel[Any]) {

  // abstract method:
  val behaviour: Behaviour[S, C]

  var neighbours = List[Node[S,C]]() // order indicates possible preference
//  var neighbours = Set[OutputChannel[Any]]()

  // shared lock
  var owner: Option[OutputChannel[Any]] = None

  def init() {
    println("INIT? "+behaviour.isProactive)
    if (behaviour.isProactive) deployer ! this
  }

//  def update(s:S) // to be overriden
//  def update(s:S) {
//    behaviour.update(s)
//    init
//  }
}
