package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 17:34
 * To change this template use File | Settings | File Templates.
 */

class GCReader(x: String, uid: Int, var size: Int) extends GCBehaviour(List(x), uid) {
  //  val flowConstr = ChoConstraints(TrueC)
  val nfConstr = GuardedCommands(Neg((Var(flowVar(x, uid)))))

  var constraints = loadConstraints

  protected def loadConstraints =
    if (size > 0) GuardedCommands()
    else nfConstr

  override def update(s: GCSolution) {
    if (s.hasFlow(flowVar(x, uid))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
      constraints = loadConstraints
      size -= 1
    }
  }

  override def isProactive: Boolean = size > 0

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

  if (useCC3) throw new Exception("CC3 not implemented")
}