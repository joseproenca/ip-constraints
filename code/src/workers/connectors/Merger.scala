package workers.connectors

import actors.OutputChannel
import workers.Node
import scala.Predef._
import common.guardedcommands.{GuardedCommands, GCSolution}
import common.guardedcommands.dataconnectors.GCMerger

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 15/05/12
 * Time: 15:23
 * To change this template use File | Settings | File Templates.
 */

class Merger (deployer: OutputChannel[Any]) extends Node[GCSolution, GuardedCommands](deployer) {
//  val uid = hashCode
  val behaviour = new GCMerger("a","b","c",uid)


  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(nd: Node[GCSolution, GuardedCommands]): Set[Node[GCSolution,GuardedCommands]] =
    if (connections contains nd) { // if the node nd is actually connected to nd
      for ((myend,_,_) <- connections(nd)) {// set of ends
        if (myend == "a" || myend == "b") return invConnections("c")
        else if (myend == "c") return invConnections("a")
      }
      Set()
    }
    else Set()


  //    if (neighbours.head == nd || neighbours.tail.head == nd) Set(neighbours.tail.tail.head)
//    else Set(neighbours.head) // priority to first end.
}