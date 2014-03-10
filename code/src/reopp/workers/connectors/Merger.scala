package reopp.workers.connectors

import actors.OutputChannel
import reopp.workers.Node
import scala.Predef._
import reopp.common.guardedcommands.{Formula, GCSolution}
import reopp.common.guardedcommands.dataconnectors.GCMerger

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 15/05/12
 * Time: 15:23
 * To change this template use File | Settings | File Templates.
 */

class Merger extends Node[GCSolution, Formula] {
//  val uid = hashCode
  val connector = new GCMerger("a","b","c")


  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(nd: Node[GCSolution, Formula]): Set[Node[GCSolution,Formula]] =
    if (connections contains nd) { // if the node nd is actually connected to nd
//      for ((myend,_,_) <- connections(nd)) {// set of ends
      for ((myend,_) <- getConnectedEndsTo(nd)) {
        if (myend == "a" || myend == "b") return invConnections("c")
        else if (myend == "c") return invConnections("a")
      }
      Set()
    }
    else Set()


  //    if (neighbours.head == nd || neighbours.tail.head == nd) Set(neighbours.tail.tail.head)
//    else Set(neighbours.head) // priority to first end.
}