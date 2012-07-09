package workers.connectors

import actors.OutputChannel
import workers.Node
import common.beh.choco.{ChoConstraints, ChoSolution}
import common.beh.choco.connectors.ChoLossy

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 15:37
 * To change this template use File | Settings | File Templates.
 */

class Lossy(deployer: OutputChannel[Any]) extends Node[ChoSolution, ChoConstraints](deployer) {
  val uid = hashCode
  val behaviour = new ChoLossy("a","b",uid)

//  // what ends depend on "end" - just a guess to decide when to search for a solution
//  def dependsOn(end: String) = if (end == "a") Set("b") else Set()

  // suggests which ends must have dataflow if "end" has also dataflow
  // "b" requires "a", but not vice-versa!
  def guessRequirements(nd: Node[ChoSolution, ChoConstraints]): Set[Node[ChoSolution,ChoConstraints]] =
    if (behaviour.connections contains nd) { // if the node nd is actually connected to nd
      for ((myend,_,_) <- behaviour.connections(nd)) {// set of ends
        if (myend == "a") return invConnections("b")
        else if (myend == "b") return invConnections("a")
      }
      Set()
    }
    else Set()
//    if (neighbours.tail.head == nd) Set(neighbours.head)
//    else Set(neighbours.head)
}
