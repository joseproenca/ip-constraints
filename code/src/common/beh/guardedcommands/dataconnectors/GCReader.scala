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

class GCReader(x: String, uid: Int, var size: Int) extends GCConnector(List(x), uid) {
  //  val flowConstr = ChoConstraints(TrueC)
  val nfConstr = GuardedCommands(Neg((Var(flowVar(x, uid)))))

  def getConstraints =
    if (size > 0) GuardedCommands()
    else nfConstr

  override def update(s: GCSolution) {
    if (s hasFlowOn flowVar(x, uid)) {
      println("//////////////////")
      println("// Got data - "+x+": "+s.getDataOn(dataVar(x,uid)).get)
//      println("// new size: "+size)
      println("//////////////////")
      notifyflow()
//      constraints = loadConstraints
      size -= 1
    }
  }

  override def isProactive: Boolean = size > 0

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

  if (useCC3) throw new Exception("CC3 not implemented")
}