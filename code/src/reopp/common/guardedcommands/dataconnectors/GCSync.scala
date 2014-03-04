package reopp.common.guardedcommands.dataconnectors

import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:59
 * To change this template use File | Settings | File Templates.
 */

class GCSync(a: String, b: String, uid: Int) extends GCConnector(List(a,b), uid) {

  private def constraints = Formula(
    a <-> b
  )

  private def dataConstraints = constraints ++
    //    av --> (bv := av) //VarAssgn(dataVar(b,uid), dataVar(a,uid))
    (b := a)

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = if (useData) dataConstraints else constraints
}
