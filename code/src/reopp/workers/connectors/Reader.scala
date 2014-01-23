package reopp.workers.connectors

import actors.OutputChannel
import reopp.workers.Node
import reopp.common.guardedcommands.{GCSolution, Formula}
import reopp.common.guardedcommands.dataconnectors.GCReader

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 17:50
 * To change this template use File | Settings | File Templates.
 */

class Reader (var n:Int,deployer: OutputChannel[Any]) extends Node[GCSolution, Formula](deployer) {

//  val uid = hashCode()

  val connector = //new ChoReaderPassive("r",uid,n)
    new GCReader("r",uid,n) { override def isProactive = false }

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(nd: Node[GCSolution, Formula]) = Set()
}
