package common.beh.choco.dataconnectors

import common.beh.choco._
import common.beh.choco.Utils.{dataVar,flowVar}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:12
 * To change this template use File | Settings | File Templates.
 */

// So far Readers and Writers are equal
class ChoReader(x: String, uid: Int, var size: Int) extends ChoDataBehaviour(List(x), uid) {

  //  val flowConstr = ChoConstraints(Var(ConstrBuilder.flowVar(x,uid)))
  //  val nfConstr = ChoConstraints(FalseC)
  val flowConstr = ChoConstraints(TrueC)
  val nfConstr = ChoConstraints(Neg(Var(Utils.flowVar(x, uid))))


  var constraints = loadConstraints

  private def loadConstraints = if (size > 0) flowConstr else nfConstr

  override def update(s: ChoSolution) {
    //    println("Reader: updating? (has flow?) "+s.getVal(ConstrBuilder.flowVar(x,uid)))
    if (s hasFlow flowVar(x, uid)) {
      size -= 1
      //      println("Reader: FLOW! new size: "+size)
      println("DataReader: FLOW ("+s.getVal(dataVar(x,uid))+") - new size: "+size)
      notifyflow()
      constraints = loadConstraints
    }
  }

  override def isProactive: Boolean = size > 0

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

}