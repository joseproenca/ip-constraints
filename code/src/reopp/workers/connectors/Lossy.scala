package reopp.workers.connectors

import actors.OutputChannel
import reopp.workers.Node
import reopp.common.guardedcommands.{Formula, GCSolution}
import reopp.common.guardedcommands.dataconnectors.GCLossy

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 15:37
 * To change this template use File | Settings | File Templates.
 */

class Lossy(deployer: OutputChannel[Any]) extends Node[GCSolution, Formula](deployer) {
//  val uid = hashCode
  val connector = new GCLossy("a","b",uid)

//  // what ends depend on "end" - just a guess to decide when to search for a solution
//  def dependsOn(end: String) = if (end == "a") Set("b") else Set()

  // suggests which ends must have dataflow if "end" has also dataflow
  // "b" requires "a", but not vice-versa!
  def guessRequirements(nd: Node[GCSolution, Formula]): Set[Node[GCSolution,Formula]] =
    if (connections contains nd) { // if the node nd is actually connected to nd
      for ((myend,_,_) <- connections(nd)) {// set of ends
        if (myend == "a") return invConnections("b")
        else if (myend == "b") return invConnections("a")
      }
      Set()
    }
    else Set()
//    if (neighbours.tail.head == nd) Set(neighbours.head)
//    else Set(neighbours.head)
}
