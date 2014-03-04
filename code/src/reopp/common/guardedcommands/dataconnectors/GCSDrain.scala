package reopp.common.guardedcommands.dataconnectors

import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:56
 * To change this template use File | Settings | File Templates.
 */

class GCSDrain(a: String, b: String, uid: Int) extends GCConnector(List(a,b), uid) {

  def getConstraints =  a <-> b

  if (useCC3) throw new Exception("CC3 not implemented")

}
