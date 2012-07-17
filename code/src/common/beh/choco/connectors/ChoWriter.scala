package common.beh.choco.connectors

import common.beh.choco._
import common.beh.Utils


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 03/05/12
 * Time: 10:54
 * To change this template use File | Settings | File Templates.
 */

class ChoWriter(x:String,uid:Int,var size:Int) extends ChoBehaviour(List(x),uid) {

  useData = false
  useCC3 = false

  //  val flowConstr = ChoConstraints(Var(ConstrBuilder.flowVar(x,uid)))
//  val nfConstr = ChoConstraints(FalseC)
  val nfConstr = ChoConstraints(Neg(Var(Utils.flowVar(x,uid))))

  var constraints = loadConstraints

  protected def loadConstraints = if (size>0) ChoConstraints(TrueC) else nfConstr

  override def update(s:ChoSolution) {
    if (s.hasFlow(Utils.flowVar(x,uid))) {
      size -= 1
//      println("Writer: FLOW! new size: "+size)
      notifyflow()
      constraints = loadConstraints
    }
  }

  override def isProactive: Boolean = size > 0

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()
}
