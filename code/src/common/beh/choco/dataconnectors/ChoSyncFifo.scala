package common.beh.choco.dataconnectors

import common.beh.Utils._
import common.beh.choco._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:28
 * To change this template use File | Settings | File Templates.
 */

class ChoSyncFifo(a: String, b: String, var data: Option[Int], uid: Int) extends ChoDataConnector(List(a,b), uid) {

  useData = true
  useCC3 = false

  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))

  val emptyFifo = ChoConstraints(Neg(bv))
  def fullFifo = ChoConstraints(List(
    av --> bv,
    bv --> DataAssgn(dataVar(b,uid),data.get)
  ))



  def getConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: ChoSolution) {
    if (s.hasFlowOn(flowVar(a, uid))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
      data = s.getVal(flowVar(a,uid))
//      constraints = loadConstraints
    }
  }

  override def isProactive: Boolean = data.isDefined

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()
}
