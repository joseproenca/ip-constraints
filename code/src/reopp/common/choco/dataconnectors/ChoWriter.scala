package reopp.common.choco.dataconnectors

import reopp.common.choco._
import reopp.common.Utils
import Utils._


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 10:54
 * To change this template use File | Settings | File Templates.
 */

class ChoWriter(val x: String, uid: Int, var data: List[Int]) extends connectors.ChoWriter(x, uid, data.size) {

  useData = true
  useCC3 = false

  override def getConstraints =
    if (!data.isEmpty) ChoConstraints(Var(flowVar(x,uid)) --> DataAssgn(dataVar(x,uid),data.head))
    else nfConstr

  override def update(s: Option[ChoSolution]) {
    if (s.isDefined)
    if (s.get.hasFlowOn(flowVar(x, uid))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
//      constraints = loadConstraints
      data = data.tail
    }
  }

  override def isProactive: Boolean = !data.isEmpty

}
