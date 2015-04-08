package reopp.common.choco.connectors

import reopp.common.{OptionSol, Utils}
import Utils._
import reopp.common.choco._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:21
 * To change this template use File | Settings | File Templates.
 */

class ChoFifoA(a: String, var data: Option[Int], uid: Int) extends ChoDataConnector(List(a), uid) {

  useData = false
  useCC3 = false

  val av = Var(flowVar(a, uid))

  val emptyFifo = ChoConstraints(TrueC)

  def fullFifo = ChoConstraints(Neg(av))

  def getConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: OptionSol[ChoSolution]) {
    if (s.isDefined)
    if (s.get.hasFlowOn(flowVar(a, uid))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
      data = Some(0)
//      constraints = loadConstraints
    }
  }

  override def isProactive: Boolean = !data.isDefined

  // suggests which ends must have dataflow if "end" has also dataflow
  //  def guessRequirements(end: String) = Set()
}
