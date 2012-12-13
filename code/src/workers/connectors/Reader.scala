package workers.connectors

import actors.OutputChannel
import workers.Node
import common.guardedcommands.{GCSolution, GuardedCommands}
import common.guardedcommands.dataconnectors.GCReader

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 17:50
 * To change this template use File | Settings | File Templates.
 */

class Reader (var n:Int,deployer: OutputChannel[Any]) extends Node[GCSolution, GuardedCommands](deployer) {

//  val uid = hashCode()

  val behaviour = //new ChoReaderPassive("r",uid,n)
    new GCReader("r",uid,n) { override def isProactive = false }

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(nd: Node[GCSolution, GuardedCommands]) = Set()
}
