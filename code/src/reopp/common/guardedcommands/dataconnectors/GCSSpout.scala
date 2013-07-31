package reopp.common.guardedcommands.dataconnectors

import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 12/06/12
 * Time: 14:27
 * To change this template use File | Settings | File Templates.
 */

class GCSSpout(a: String, b: String, uid: Int) extends GCConnector(List(a,b), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))

  def getConstraints =  av <-> bv

  if (useCC3) throw new Exception("CC3 not implemented")
}
