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
}
