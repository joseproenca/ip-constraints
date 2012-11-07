package common.beh.choco.connectors

import common.beh.choco._
import common.beh.Utils

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 17:12
 * To change this template use File | Settings | File Templates.
 */

// So far Readers and Writers are equal
class ChoReader(x:String,uid:Int,var size:Int) extends ChoConnector(List(x),uid) {

  useData = false
  useCC3 = false

  //  val flowConstr = ChoConstraints(Var(ConstrBuilder.flowVar(x,uid)))
//  val nfConstr = ChoConstraints(FalseC)
  val flowConstr = ChoConstraints(TrueC)
  val nfConstr = ChoConstraints(Neg(Var(Utils.flowVar(x,uid))))

  def getConstraints = if (size>0) flowConstr else nfConstr

  override def update(s:ChoSolution) {
//    println("Reader: updating? (has flow?) "+s.getVal(ConstrBuilder.flowVar(x,uid)))
    if (s hasFlow Utils.flowVar(x, uid)) {
      size -= 1
//      println("Reader: FLOW! new size: "+size)
      notifyflow()
//      constraints = loadConstraints
    }
  }

  override def isProactive: Boolean = size > 0

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

}