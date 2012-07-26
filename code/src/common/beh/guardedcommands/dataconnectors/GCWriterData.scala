package common.beh.guardedcommands.dataconnectors

import common.beh.guardedcommands._
import common.beh.Utils._
import common.beh.guardedcommands.Neg
import common.beh.guardedcommands.Var
import common.beh.guardedcommands.SGuard
import common.beh.guardedcommands.IntAssgn

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:15
 * To change this template use File | Settings | File Templates.
 */

class GCWriterData (val x: String, uid: Int, var data: List[Any]) extends GCBehaviour(List(x), uid) {
  //  val flowConstr = ChoConstraints(TrueC)
  val nfConstr = GuardedCommands(True --> SGuard(Neg((Var(flowVar(x, uid))))))

  var constraints = loadConstraints

  protected def loadConstraints = {
    if (!data.isEmpty) {
      if(useData) GuardedCommands(Var(flowVar(x,uid)) --> DataAssgn(dataVar(x,uid),data.head))
      else if (useCC3) throw new Exception("CC3 not implemented")
      else GuardedCommands()
    }
    else nfConstr
  }

  override def update(s: GCSolution) {
    if (s.hasFlow(flowVar(x, uid))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
      constraints = loadConstraints
      data = data.tail
    }
  }

  override def isProactive: Boolean = !data.isEmpty

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

}

