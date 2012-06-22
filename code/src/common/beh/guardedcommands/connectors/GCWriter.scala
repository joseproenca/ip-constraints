package common.beh.guardedcommands.connectors

import common.beh.Utils._
import common.beh.guardedcommands._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 17:29
 * To change this template use File | Settings | File Templates.
 */

class GCWriter(val x: String, uid: Int, var data: List[Int]) extends GCBehaviour(List(x), uid) {
  //  val flowConstr = ChoConstraints(TrueC)
  val nfConstr = GuardedCommands(True --> SGuard(Neg((Var(flowVar(x, uid))))))

  var constraints = loadConstraints

  protected def loadConstraints = {
    if (!data.isEmpty) GuardedCommands(Var(flowVar(x,uid)) --> DataAssgn(dataVar(x,uid),data.head))
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
