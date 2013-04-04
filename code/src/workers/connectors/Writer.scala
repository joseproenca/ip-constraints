package workers.connectors

import workers.Node
import actors.OutputChannel
import common.guardedcommands.{Formula, GCSolution}
import common.guardedcommands.dataconnectors.GCWriter

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 13:03
 * To change this template use File | Settings | File Templates.
 */

class Writer(var n:Int,deployer: OutputChannel[Any]) extends Node[GCSolution, Formula](deployer) {

  //val uid = hashCode()

  val behaviour = new GCWriter("w",uid,(1 to n).map(Int.box(_)).toList)

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(nd: Node[GCSolution, Formula]) = Set()
}
