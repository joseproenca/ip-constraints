package workers.connectors

import workers.Node
import common.beh.choco.{ChoSolution, ChoConstraints}
import actors.OutputChannel
import common.beh.choco.connectors.ChoWriter

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 13:03
 * To change this template use File | Settings | File Templates.
 */

class Writer(var n:Int,deployer: OutputChannel[Any]) extends Node[ChoSolution, ChoConstraints](deployer) {

  val uid = hashCode()

  val behaviour = new ChoWriter("w",uid,n)

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(nd: Node[ChoSolution, ChoConstraints]) = Set()
}
