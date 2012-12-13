package common.choco.connectors

import common.choco._
import common.Utils


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 03/05/12
 * Time: 10:54
 * To change this template use File | Settings | File Templates.
 */

class ChoWriter(x:String,uid:Int,var size:Int) extends ChoConnector(List(x),uid) {

  useData = false
  useCC3 = false

  //  val flowConstr = ChoConstraints(Var(ConstrBuilder.flowVar(x,uid)))
//  val nfConstr = ChoConstraints(FalseC)
  val nfConstr = ChoConstraints(Neg(Var(Utils.flowVar(x,uid))))

  def getConstraints = if (size>0) ChoConstraints(TrueC) else nfConstr


  override def update(s:ChoSolution) {
    if (s hasFlowOn Utils.flowVar(x,uid)) {
      size -= 1
//      println("Writer: FLOW! new size: "+size)
      notifyflow()
//      constraints = loadConstraints
    }
  }

  override def isProactive: Boolean = size > 0

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()
}
