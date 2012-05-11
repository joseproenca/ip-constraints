package workers.connectors

import actors.OutputChannel
import workers.Node
import common.beh.choco.{ChoConstraints, ChoSolution}
import common.beh.choco.connectors.ChoReader

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 17:50
 * To change this template use File | Settings | File Templates.
 */

class Reader (var n:Int,deployer: OutputChannel[Any]) extends Node[ChoSolution, ChoConstraints](deployer) {

  val uid = hashCode()

  val behaviour = new ChoReader("r",uid,n)

  // what ends depend on "end" - just a guess to decide when to search for a solution
  def dependsOn(end: String) = Set()
}
