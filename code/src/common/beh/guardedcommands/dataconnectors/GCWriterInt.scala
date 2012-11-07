package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 17:29
 * To change this template use File | Settings | File Templates.
 */

@deprecated
class GCWriterInt(val x: String, uid: Int, var data: List[Int]) extends GCConnector(List(x), uid) {
  val xv = Var(flowVar(x,uid))

  val nfConstr = GuardedCommands(!xv)

  def getConstraints: GuardedCommands = {
    if (!data.isEmpty) {
      if(useData) xv := data.head  //GuardedCommands(xv --> IntAssgn(dataVar(x,uid),data.head)) //(xv := data.head))
      else if (useCC3) throw new Exception("CC3 not implemented")
      else GuardedCommands()
    }
    else nfConstr
  }

  override def update(s: GCSolution) {
    if (s.hasFlow(flowVar(x, uid))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
//      constraints = loadConstraints
      data = data.tail
    }
  }

  override def isProactive: Boolean = !data.isEmpty

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

}
