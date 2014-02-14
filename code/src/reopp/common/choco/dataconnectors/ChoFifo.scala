package reopp.common.choco.dataconnectors

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

class ChoFifo(a: String, b: String, dt: Option[Int], uid: Int) extends connectors.ChoFifo(a,b, dt, uid) {

  useData = true
  useCC3 = false

  override def fullFifo = super.fullFifo ++ ChoConstraints(
    bv --> DataAssgn(dataVar(b,uid),data.get)
  )

  override def update(s: OptionSol[ChoSolution]) {
    if (s.isDefined) {
    if (s.get.hasFlowOn(flowVar(a, uid))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
      data = s.get.getVal(flowVar(a,uid))
//      constraints = loadConstraints
    }
    else if (s.get.hasFlowOn(flowVar(b, uid))) {
      notifyflow()
      data = None
//      constraints = loadConstraints
    }
    }
  }

}
