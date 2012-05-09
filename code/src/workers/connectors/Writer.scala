package workers.connectors

import workers.Node
import common.beh.choco.{ChoSolution, ChoConstraints}
import workers.Deployer
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

  val behaviour = new ChoWriter("a",uid,n)

}
