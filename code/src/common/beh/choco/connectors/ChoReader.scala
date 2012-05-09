package common.beh.choco.connectors

import common.beh.choco._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 17:12
 * To change this template use File | Settings | File Templates.
 */

// So far Readers and Writers are equal
class ChoReader(x:String,uid:Int,var size:Int) extends ChoBehaviour(List(x),uid) {

//  val flowConstr = ChoConstraints(Var(ConstrBuilder.flowVar(x,uid)))
//  val nfConstr = ChoConstraints(FalseC)
  val flowConstr = ChoConstraints(TrueC)
  val nfConstr = ChoConstraints(Neg(Var(ConstrBuilder.flowVar(x,uid))))


  var constraints = loadConstraints

  private def loadConstraints = if (size>0) flowConstr else nfConstr

  override def update(s:ChoSolution) {
    if (s.hasFlow(ConstrBuilder.flowVar(x,uid))) {
      size -= 1
      constraints = loadConstraints
    }
  }

  override def isProactive: Boolean = size > 0

}