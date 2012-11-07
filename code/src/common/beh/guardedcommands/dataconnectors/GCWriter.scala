package common.beh.guardedcommands.dataconnectors

import common.beh.guardedcommands._
import common.beh.Utils._
import common.beh.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:15
 * To change this template use File | Settings | File Templates.
 */

class GCWriter (val x: String, uid: Int, var data: List[Any]) extends GCConnector(List(x), uid) {
  val xv = Var(flowVar(x,uid))

  private val nfConstr: GuardedCommands = !xv

  //var constraints = loadConstraints

  def getConstraints = {
    if (!data.isEmpty) {
      if(useData)
        xv --> (xv := data.head)//(Var(flowVar(x,uid)) --> DataAssgn(dataVar(x,uid),data.head))

      else if (useCC3) throw new Exception("CC3 not implemented")
      else GuardedCommands()
    }
    else nfConstr
  }

  override def update(s: GCSolution) {
    if (s.hasFlow(flowVar(x, uid))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
      // update state
      data = data.tail
      // update constraints
      // constraints = loadConstraints --- done by getConstraints by updating state
    }
  }

  override def isProactive: Boolean = !data.isEmpty

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

}

