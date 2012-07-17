package workers.connectors

import actors.OutputChannel
import workers.Node
import common.beh.choco.{ChoConstraints, ChoSolution}
import common.beh.choco.connectors.ChoReaderPassive

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 17:50
 * To change this template use File | Settings | File Templates.
 */

class Reader (var n:Int,deployer: OutputChannel[Any]) extends Node[ChoSolution, ChoConstraints](deployer) {

  val uid = hashCode()

  val behaviour = new ChoReaderPassive("r",uid,n)

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(nd: Node[ChoSolution, ChoConstraints]) = Set()
}
