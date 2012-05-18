package workers.connectors

import actors.OutputChannel
import workers.Node
import common.beh.choco.{ChoConstraints, ChoSolution}
import common.beh.choco.connectors.{ChoMerger, ChoLossy}
import scala.Predef._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 15/05/12
 * Time: 15:23
 * To change this template use File | Settings | File Templates.
 */

class Merger (deployer: OutputChannel[Any]) extends Node[ChoSolution, ChoConstraints](deployer) {
  val uid = hashCode
  val behaviour = new ChoMerger("a","b","c",uid)


  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(nd: Node[ChoSolution, ChoConstraints]) =
    if (neighbours.head == nd || neighbours.tail.head == nd) Set(neighbours.tail.tail.head)
    else Set(neighbours.head) // priority to first end.
}