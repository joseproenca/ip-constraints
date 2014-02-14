package reopp.workers.connectors

import reopp.workers.Node
import actors.OutputChannel
import reopp.common.guardedcommands.{Formula, GCSolution}
import reopp.common.guardedcommands.dataconnectors.GCWriter

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 13:03
 * To change this template use File | Settings | File Templates.
 */

class Writer(var n:Int) extends Node[GCSolution, Formula] {

  //val uid = hashCode()

  val connector = new GCWriter("w",uid,(1 to n).map(Int.box(_)).toList)

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(nd: Node[GCSolution, Formula]) = Set()
}
