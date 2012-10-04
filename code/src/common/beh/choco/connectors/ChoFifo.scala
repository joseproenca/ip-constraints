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

class ChoFifo(a: String, b: String, var data: Option[Int], uid: Int) extends ChoDataConnector(List(a, b), uid) {

  useData = false
  useCC3 = false

  val av = Var(flowVar(a, uid))
  val bv = Var(flowVar(b, uid))

  val emptyFifo = ChoConstraints(Neg(bv))

  def fullFifo = ChoConstraints(
    Neg(av)
  )

  var constraints = loadConstraints

  protected def loadConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: ChoSolution) {
    if (s.hasFlow(flowVar(a, uid))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
      data = Some(0)
      constraints = loadConstraints
    }
    if (s.hasFlow(flowVar(b, uid))) {
      notifyflow()
      data = None
      constraints = loadConstraints
    }
  }

  override def isProactive: Boolean = true //data.isDefined

  // suggests which ends must have dataflow if "end" has also dataflow
//  def guessRequirements(end: String) = Set()
}
