package reopp.common.guardedcommands.dataconnectors

import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 16:44
 * To change this template use File | Settings | File Templates.
 */

class GCLossy(a: String, b: String, uid: Int) extends GCConnector(List(a,b), uid) {

  private def constraints : Formula = b --> a


  private def dataConstraints = constraints ++
      (b --> (b := a)) //VarAssgn(dataVar(y,uid),dataVar(x,uid))

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = if (useData) dataConstraints else constraints
}
