package common.beh.choco.dataconnectors

import common.beh.choco._
import common.beh.Utils._


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 10:54
 * To change this template use File | Settings | File Templates.
 */

class ChoWriter(x: String, uid: Int, var data: List[Int]) extends ChoDataBehaviour(List(x), uid) {

  //  val flowConstr = ChoConstraints(Var(ConstrBuilder.flowVar(x,uid)))
  //  val nfConstr = ChoConstraints(FalseC)
//  val flowConstr = ChoConstraints(TrueC)
  val nfConstr = ChoConstraints(Neg(Var(flowVar(x, uid))))

  var constraints = loadConstraints

  protected def loadConstraints =
    if (!data.isEmpty) ChoConstraints(Var(flowVar(x,uid)) --> DataAssgn(dataVar(x,uid),data.head))
    else nfConstr

  override def update(s: ChoSolution) {
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
