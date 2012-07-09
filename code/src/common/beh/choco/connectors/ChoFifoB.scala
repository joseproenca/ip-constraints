package common.beh.choco.connectors

import common.beh.Utils._
import common.beh.choco._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:21
 * To change this template use File | Settings | File Templates.
 */

class ChoFifoB(b: String, var data: Option[Int], uid: Int) extends ChoDataBehaviour(List(b), uid) {

  useData = false
  useCC3 = false

  val bv = Var(flowVar(b, uid))

  val emptyFifo = ChoConstraints(Neg(bv))

  def fullFifo = ChoConstraints(TrueC)

  var constraints = loadConstraints

  protected def loadConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: ChoSolution) {
    if (s.hasFlow(flowVar(b, uid))) {
      notifyflow()
      data = None
      constraints = emptyFifo
    }
  }

  override def isProactive: Boolean = data.isDefined

  // suggests which ends must have dataflow if "end" has also dataflow
  //  def guessRequirements(end: String) = Set()
}
